use std::cmp;
use std::fmt::{self, Display, Formatter};
use std::sync::{atomic::Ordering, Arc};

use rayon::prelude::*;

use crate::circuit::{Gate, PullApplicable, PullApplyOutput, PushApplicable, PushApplyOutput};
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
) -> Result<ExpandResult, SimulatorError> {
    let expected_cost = expected_cost(num_qubits, state.num_nonzeros(), prev_num_nonzeros);

    let all_gates_pullable = gates.iter().all(|gate| gate.is_pullable());

    assert!(config.dense_threshold <= config.pull_threshold);

    if expected_cost < config.dense_threshold {
        expand_sparse(gates, state)
    } else if expected_cost >= config.pull_threshold && all_gates_pullable {
        expand_pull_dense(gates, config, num_qubits, state)
    } else {
        expand_push_dense(gates, config, num_qubits, state)
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

fn expand_sparse(gates: Vec<&Gate>, state: State) -> Result<ExpandResult, SimulatorError> {
    let mut table = SparseStateTable::new();

    let num_gate_apps = state
        .compactify()
        .try_fold(0, |num_gate_apps, (bidx, weight)| {
            Ok::<usize, SimulatorError>(
                num_gate_apps + apply_gates_seq(&gates, &mut table, bidx, weight)?,
            )
        })?;

    let num_nonzeros = table.num_nonzeros();

    Ok(ExpandResult {
        state: State::Sparse(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::Sparse,
    })
}

fn expand_push_dense(
    gates: Vec<&Gate>,
    config: &Config,
    num_qubits: usize,
    state: State,
) -> Result<ExpandResult, SimulatorError> {
    // FIXME: Remove code duplicate by defining and use par_compactify().
    // This is difficult because we cannot easily write the type of the parallel
    // iterator
    let block_size = config.block_size;
    let table = Arc::new(DenseStateTable::new(num_qubits));

    let num_gate_apps = match state {
        State::Sparse(prev_table) => prev_table
            .table
            .into_iter()
            .collect::<Vec<_>>()
            .par_chunks(block_size)
            .map(|chunk| {
                chunk
                    .to_owned()
                    .into_iter()
                    .map(|(bidx, weight)| apply_gates(&gates, Arc::clone(&table), bidx, weight))
                    .sum::<Result<usize, SimulatorError>>()
            })
            .sum::<Result<usize, SimulatorError>>()?,
        State::Dense(prev_table) => prev_table
            .array
            .par_chunks(block_size)
            .enumerate()
            .map(|(chunk_idx, chunk)| {
                chunk
                    .iter()
                    .enumerate()
                    .map(|(idx, v)| {
                        let (re, im) = utility::unpack_complex(v.load(Ordering::Relaxed));
                        apply_gates(
                            &gates,
                            Arc::clone(&table),
                            BasisIdx::from_idx(block_size * chunk_idx + idx),
                            Complex::new(re, im),
                        )
                    })
                    .sum::<Result<usize, SimulatorError>>()
            })
            .sum::<Result<usize, SimulatorError>>()?,
    };
    let table = Arc::<DenseStateTable>::try_unwrap(table)
        .expect("all other references to table should have been dropped");

    let num_nonzeros = table.num_nonzeros();

    Ok(ExpandResult {
        state: State::Dense(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::PushDense,
    })
}

fn expand_pull_dense(
    gates: Vec<&Gate>,
    config: &Config,
    num_qubits: usize,
    state: State,
) -> Result<ExpandResult, SimulatorError> {
    let table = Arc::new(DenseStateTable::new(num_qubits));

    let capacity = 1 << num_qubits;

    let block_size = config.block_size;

    let num_blocks = ((capacity as f32) / (block_size as f32)).ceil() as usize;

    let (num_gate_apps, num_nonzeros) =
        (0..num_blocks)
            .into_par_iter()
            .map(|chunk_idx| {
                ((chunk_idx * block_size)..(((chunk_idx + 1) * block_size).min(capacity - 1)))
                    .try_fold((0, 0), |(num_gate_apps, num_nonzeros), idx| {
                        let bidx = BasisIdx::from_idx(idx);
                        let (weight, num_gate_apps_here) = apply_pull_gates(&gates, &state, &bidx)?;
                        unsafe {
                            table.unsafe_put(bidx, weight);
                        }
                        Ok::<(usize, usize), SimulatorError>((
                            num_gate_apps + num_gate_apps_here,
                            if utility::is_nonzero(weight) {
                                num_nonzeros + 1
                            } else {
                                num_nonzeros
                            },
                        ))
                    })
            })
            .try_reduce(
                || (0, 0),
                |(num_gate_apps, num_nonzeros), chunk_result| {
                    let (num_gate_apps_in_chunk, num_nonzeros_in_chunk) = chunk_result;
                    Ok((
                        (num_gate_apps + num_gate_apps_in_chunk),
                        (num_nonzeros + num_nonzeros_in_chunk),
                    ))
                },
            )?;

    Ok(ExpandResult {
        state: State::Dense(
            Arc::into_inner(table).expect("all other references to table should have been dropped"),
        ),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::PullDense,
    })
}

// NOTE: Some gate applications are still performed in a sequential manner
fn apply_gates_seq(
    gates: &[&Gate],
    table: &mut impl Table,
    bidx: BasisIdx,
    weight: Complex,
) -> Result<usize, SimulatorError> {
    if utility::is_zero(weight) {
        return Ok(0);
    }
    if gates.is_empty() {
        table.put(bidx, weight);
        return Ok(0);
    }

    match gates[0].push_apply(bidx, weight)? {
        PushApplyOutput::Nonbranching(new_bidx, new_weight) => {
            Ok(1 + apply_gates_seq(&gates[1..], table, new_bidx, new_weight)?)
        }
        PushApplyOutput::Branching((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            let num_gate_apps_1 = apply_gates_seq(&gates[1..], table, new_bidx1, new_weight1)?;
            let num_gate_apps_2 = apply_gates_seq(&gates[1..], table, new_bidx2, new_weight2)?;
            Ok(1 + num_gate_apps_1 + num_gate_apps_2)
        }
    }
}

fn apply_gates(
    gates: &[&Gate],
    table: Arc<DenseStateTable>,
    bidx: BasisIdx,
    weight: Complex,
) -> Result<usize, SimulatorError> {
    if utility::is_zero(weight) {
        return Ok(0);
    }
    if gates.is_empty() {
        table.atomic_put(bidx, weight);
        return Ok(0);
    }

    match gates[0].push_apply(bidx, weight)? {
        PushApplyOutput::Nonbranching(new_bidx, new_weight) => {
            Ok(1 + apply_gates_internal(&gates[1..], &table, new_bidx, new_weight)?)
        }
        PushApplyOutput::Branching((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            let num_gate_apps_1 =
                apply_gates_internal(&gates[1..], &table, new_bidx1, new_weight1)?;
            let num_gate_apps_2 =
                apply_gates_internal(&gates[1..], &table, new_bidx2, new_weight2)?;
            Ok(1 + num_gate_apps_1 + num_gate_apps_2)
        }
    }
}

// used to avoid unnecessary Arc::clone() within a thread
fn apply_gates_internal(
    gates: &[&Gate],
    table: &Arc<DenseStateTable>,
    bidx: BasisIdx,
    weight: Complex,
) -> Result<usize, SimulatorError> {
    if utility::is_zero(weight) {
        return Ok(0);
    }
    if gates.is_empty() {
        table.atomic_put(bidx, weight);
        return Ok(0);
    }

    match gates[0].push_apply(bidx, weight)? {
        PushApplyOutput::Nonbranching(new_bidx, new_weight) => {
            Ok(1 + apply_gates_internal(&gates[1..], table, new_bidx, new_weight)?)
        }
        PushApplyOutput::Branching((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            let num_gate_apps_1 = apply_gates_internal(&gates[1..], table, new_bidx1, new_weight1)?;
            let num_gate_apps_2 = apply_gates_internal(&gates[1..], table, new_bidx2, new_weight2)?;
            Ok(1 + num_gate_apps_1 + num_gate_apps_2)
        }
    }
}

fn apply_pull_gates(
    gates: &[&Gate],
    prev_state: &State,
    bidx: &BasisIdx,
) -> Result<(Complex, usize), SimulatorError> {
    if gates.is_empty() {
        let weight = prev_state.get(bidx).unwrap_or(Complex::new(0.0, 0.0));
        return Ok((weight, 0));
    }

    match gates[0].pull_apply(bidx.clone())? {
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
