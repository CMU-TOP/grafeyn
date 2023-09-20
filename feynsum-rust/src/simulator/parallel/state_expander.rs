use std::cmp;
use std::fmt::{self, Display, Formatter};

use rayon::prelude::*;

use crate::circuit::{Gate, PullApplicable, PullApplyOutput, PushApplicable, PushApplyOutput};
use crate::config::Config;
use crate::types::{BasisIdx, Complex, Real};
use crate::utility;

use super::super::Compactifiable;
use super::state::{DenseStateTable, SparseStateTable, State, Table};

pub enum ExpandMethod {
    Sparse,
    PushDense,
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

pub struct ExpandResult {
    pub state: State,
    pub num_nonzeros: usize,
    pub num_gate_apps: usize,
    pub method: ExpandMethod,
}

pub fn expand(
    gates: Vec<&Gate>,
    config: &Config,
    num_qubits: usize,
    prev_num_nonzeros: usize,
    state: State,
) -> ExpandResult {
    let expected_cost = expected_cost(num_qubits, state.num_nonzeros(), prev_num_nonzeros);

    let all_gates_pullable = gates.iter().all(|gate| gate.is_pullable());

    assert!(config.dense_threshold <= config.pull_threshold);

    if expected_cost < config.dense_threshold {
        expand_sparse(gates, state)
    } else if expected_cost >= config.pull_threshold && all_gates_pullable {
        expand_pull_dense(gates, config, num_qubits, state)
    } else {
        unsafe { expand_push_dense(gates, config, num_qubits, state) }
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

fn expand_sparse(gates: Vec<&Gate>, state: State) -> ExpandResult {
    let mut table = SparseStateTable::new();

    let num_gate_apps = state
        .compactify()
        .map(|(bidx, weight)| apply_gates_seq(&gates, &mut table, bidx, weight))
        .sum();

    // We don't store zeros in the sparse table, so the number of nonzeros is
    // always the size of the hash table
    let num_nonzeros = table.table.len();

    ExpandResult {
        state: State::Sparse(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::Sparse,
    }
}

unsafe fn expand_push_dense(
    gates: Vec<&Gate>,
    config: &Config,
    num_qubits: usize,
    state: State,
) -> ExpandResult {
    // FIXME: Remove code duplicate by defining and use par_compactify().
    // This is difficult because we cannot easily write the type of the parallel
    // iterator
    let block_size = config.block_size;
    let table = DenseStateTable::new(num_qubits);

    let num_gate_apps = match state {
        State::Sparse(prev_table) => prev_table
            .table
            .into_iter()
            .collect::<Vec<_>>()
            .par_chunks(block_size)
            .map(|chunk| {
                chunk
                    .iter()
                    .copied()
                    .map(|(bidx, weight)| {
                        apply_gates(&gates, &table as *const DenseStateTable, bidx, weight)
                    })
                    .sum::<usize>()
            })
            .sum(),
        State::Dense(prev_table) => prev_table
            .array
            .par_chunks(block_size)
            .enumerate()
            .map(|(chunk_idx, chunk)| {
                chunk
                    .iter()
                    .enumerate()
                    .map(|(idx, atomic)| unsafe {
                        let (re, im) = utility::unpack_complex(*atomic.as_ptr());
                        apply_gates(
                            &gates,
                            &table as *const DenseStateTable,
                            BasisIdx::from_idx(block_size * chunk_idx + idx),
                            Complex::new(re, im),
                        )
                    })
                    .sum::<usize>()
            })
            .sum(),
    };

    let num_nonzeros = table.num_nonzeros();

    ExpandResult {
        state: State::Dense(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::PushDense,
    }
}

fn expand_pull_dense(
    gates: Vec<&Gate>,
    config: &Config,
    num_qubits: usize,
    state: State,
) -> ExpandResult {
    let table = DenseStateTable::new(num_qubits);

    let capacity = 1 << num_qubits;

    let block_size = config.block_size;

    let (num_gate_apps, num_nonzeros) = (0..capacity)
        .into_par_iter()
        .chunks(block_size)
        .map(|chunk| {
            chunk
                .iter()
                .fold((0, 0), |(num_gate_apps, num_nonzeros), idx| {
                    let bidx = BasisIdx::from_idx(*idx);
                    unsafe {
                        let (weight, num_gate_apps_here) = apply_pull_gates(&gates, &state, &bidx);
                        table.unsafe_put(bidx, weight);
                        (
                            num_gate_apps + num_gate_apps_here,
                            if utility::is_nonzero(weight) {
                                num_nonzeros + 1
                            } else {
                                num_nonzeros
                            },
                        )
                    }
                })
        })
        .reduce(
            || (0, 0),
            |(num_gate_apps, num_nonzeros), chunk_result| {
                let (num_gate_apps_in_chunk, num_nonzeros_in_chunk) = chunk_result;
                (
                    (num_gate_apps + num_gate_apps_in_chunk),
                    (num_nonzeros + num_nonzeros_in_chunk),
                )
            },
        );

    ExpandResult {
        state: State::Dense(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::PullDense,
    }
}

// NOTE: Some gate applications are still performed in a sequential manner
fn apply_gates_seq(
    gates: &[&Gate],
    table: &mut impl Table,
    bidx: BasisIdx,
    weight: Complex,
) -> usize {
    if gates.is_empty() {
        if !utility::is_zero(weight) {
            table.put(bidx, weight);
        }
        return 0;
    }

    match gates[0].push_apply(bidx, weight) {
        PushApplyOutput::Nonbranching(new_bidx, new_weight) => {
            1 + apply_gates_seq(&gates[1..], table, new_bidx, new_weight)
        }
        PushApplyOutput::Branching((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            let num_gate_apps_1 = apply_gates_seq(&gates[1..], table, new_bidx1, new_weight1);
            let num_gate_apps_2 = apply_gates_seq(&gates[1..], table, new_bidx2, new_weight2);
            1 + num_gate_apps_1 + num_gate_apps_2
        }
    }
}

unsafe fn apply_gates(
    gates: &[&Gate],
    table: *const DenseStateTable,
    bidx: BasisIdx,
    weight: Complex,
) -> usize {
    if utility::is_zero(weight) {
        return 0;
    }
    if gates.is_empty() {
        (*table).atomic_put(bidx, weight);
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

unsafe fn apply_pull_gates(
    gates: &[&Gate],
    prev_state: &State,
    bidx: &BasisIdx,
) -> (Complex, usize) {
    if gates.is_empty() {
        let weight = prev_state
            .unsafe_get(bidx)
            .unwrap_or(Complex::new(0.0, 0.0));
        return (weight, 0);
    }

    match gates[0].pull_apply(*bidx) {
        PullApplyOutput::Nonbranching(neighbor, multiplier) => {
            let (weight, num_gate_apps) = apply_pull_gates(&gates[1..], prev_state, &neighbor);
            (weight * multiplier, 1 + num_gate_apps)
        }
        PullApplyOutput::Branching((neighbor1, multiplier1), (neighbor2, multiplier2)) => {
            let (weight1, num_gate_apps_1) = apply_pull_gates(&gates[1..], prev_state, &neighbor1);
            let (weight2, num_gate_apps_2) = apply_pull_gates(&gates[1..], prev_state, &neighbor2);

            (
                weight1 * multiplier1 + weight2 * multiplier2,
                1 + num_gate_apps_1 + num_gate_apps_2,
            )
        }
    }
}
