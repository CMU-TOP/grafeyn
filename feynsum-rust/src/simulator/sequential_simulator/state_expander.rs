use std::cmp;
use std::fmt::{self, Display, Formatter};

use crate::circuit::{Gate, PullApplyOutput, PushApplicable, PushApplyOutput};
use crate::config::Config;
use crate::types::{BasisIdx, Complex, Real};
use crate::utility;

use super::super::{Compactifiable, SimulatorError};
use super::state::{DenseStateTable, SparseStateTable, State, Table};

pub enum ExpandMethod {
    Sparse,
    PushDense,
    #[allow(dead_code)]
    PullDense,
}

impl Display for ExpandMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExpandMethod::Sparse => write!(f, "push sparse"),
            ExpandMethod::PushDense => write!(f, "push dense"),
            ExpandMethod::PullDense => write!(f, "pull dense"),
        }
    }
}

pub struct ExpandResult<B: BasisIdx> {
    pub state: State<B>,
    pub num_nonzeros: usize,
    pub num_gate_apps: usize,
    pub method: ExpandMethod,
}

pub fn expand<B: BasisIdx>(
    gates: Vec<&Gate<B>>,
    config: &Config,
    num_qubits: usize,
    prev_num_nonzeros: usize,
    state: State<B>,
) -> Result<ExpandResult<B>, SimulatorError> {
    let expected_cost = expected_cost(num_qubits, state.num_nonzeros(), prev_num_nonzeros);

    let all_gates_pullable = gates.iter().all(|gate| gate.is_pullable());

    assert!(config.dense_threshold <= config.pull_threshold);

    if expected_cost < config.dense_threshold {
        expand_sparse(gates, state)
    } else if expected_cost >= config.pull_threshold && all_gates_pullable {
        expand_pull_dense(gates, num_qubits, state)
    } else {
        expand_push_dense(gates, num_qubits, state)
    }
}

fn expected_cost(num_qubits: usize, num_nonzeros: usize, prev_num_nonzeros: usize) -> Real {
    let max_num_states = 1 << num_qubits;
    let rate = Real::max(1.0, num_nonzeros as Real / prev_num_nonzeros as Real);
    let expected = cmp::min(max_num_states, (rate * num_nonzeros as Real) as i64);
    let expected_density = expected as Real / max_num_states as Real;
    let current_density = num_nonzeros as Real / max_num_states as Real;
    expected_density.max(current_density)
}

fn expand_sparse<B: BasisIdx>(
    gates: Vec<&Gate<B>>,
    state: State<B>,
) -> Result<ExpandResult<B>, SimulatorError> {
    let mut table = SparseStateTable::<B>::new();

    let num_gate_apps = state
        .compactify()
        .map(|(bidx, weight)| apply_gates(&gates, &mut table, bidx, weight))
        .sum();

    let num_nonzeros = table.num_nonzeros();

    Ok(ExpandResult {
        state: State::Sparse(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::Sparse,
    })
}

fn expand_push_dense<B: BasisIdx>(
    gates: Vec<&Gate<B>>,
    num_qubits: usize,
    state: State<B>,
) -> Result<ExpandResult<B>, SimulatorError> {
    let mut table = DenseStateTable::new(num_qubits);

    let num_gate_apps = state
        .compactify()
        .map(|(bidx, weight)| apply_gates(&gates, &mut table, bidx, weight))
        .sum();

    let num_nonzeros = table.num_nonzeros();

    Ok(ExpandResult {
        state: State::Dense(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::PushDense,
    })
}

fn expand_pull_dense<B: BasisIdx>(
    gates: Vec<&Gate<B>>,
    num_qubits: usize,
    state: State<B>,
) -> Result<ExpandResult<B>, SimulatorError> {
    let mut table = DenseStateTable::new(num_qubits);

    let capacity = 1 << num_qubits;

    let (num_gate_apps, num_nonzeros) =
        (0..capacity).try_fold((0, 0), |(num_gate_apps, num_nonzeros), idx| {
            let bidx = B::from_idx(idx);
            let (weight, num_gate_apps_here) = apply_pull_gates(&gates, &state, &bidx)?;

            let num_nonzeros_here = if utility::is_nonzero(weight) { 1 } else { 0 };
            table.put(bidx, weight);

            Ok::<(usize, usize), SimulatorError>((
                num_gate_apps + num_gate_apps_here,
                num_nonzeros + num_nonzeros_here,
            ))
        })?;

    Ok(ExpandResult {
        state: State::Dense(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::PullDense,
    })
}

fn apply_gates<B: BasisIdx>(
    gates: &[&Gate<B>],
    table: &mut impl Table<B>,
    bidx: B,
    weight: Complex,
) -> usize {
    if utility::is_zero(weight) {
        return 0;
    }
    if gates.is_empty() {
        table.put(bidx, weight);
        return 0;
    }

    match gates[0].push_apply(bidx, weight) {
        PushApplyOutput::Nonbranching(new_bidx, new_weight) => {
            1 + apply_gates(&gates[1..], table, new_bidx, new_weight)
        }
        PushApplyOutput::Branching((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            let num_gate_apps_1 = apply_gates(&gates[1..], table, new_bidx1, new_weight1);
            let num_gate_apps_2 = apply_gates(&gates[1..], table, new_bidx2, new_weight2);
            1 + num_gate_apps_1 + num_gate_apps_2
        }
    }
}

fn apply_pull_gates<B: BasisIdx>(
    gates: &[&Gate<B>],
    prev_state: &State<B>,
    bidx: &B,
) -> Result<(Complex, usize), SimulatorError> {
    if gates.is_empty() {
        let weight = prev_state
            .get(bidx)
            .map_or(Complex::new(0.0, 0.0), Clone::clone);
        return Ok((weight, 0));
    }

    match gates[0].pull_action.as_ref().unwrap()(*bidx) {
        // FIXME: No clone
        PullApplyOutput::Nonbranching(neighbor, multiplier) => {
            let (weight, num_gate_apps) = apply_pull_gates(&gates[1..], prev_state, &neighbor)?;
            Ok((weight * multiplier, 1 + num_gate_apps))
        }
        PullApplyOutput::Branching((neighbor1, multiplier1), (neighbor2, multiplier2)) => {
            let (weight1, num_gate_apps_1) = apply_pull_gates(&gates[1..], prev_state, &neighbor1)?;
            let (weight2, num_gate_apps_2) = apply_pull_gates(&gates[1..], prev_state, &neighbor2)?;

            Ok((
                weight1 * multiplier1 + weight2 * multiplier2,
                1 + num_gate_apps_1 + num_gate_apps_2,
            ))
        }
    }
}
