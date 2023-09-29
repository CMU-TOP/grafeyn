use std::cmp;
use std::fmt::{self, Display, Formatter};
//use std::ops::Deref;
use std::sync::{atomic::Ordering, atomic::AtomicBool};
use std::sync::Arc;

use rayon::prelude::*;

use crate::circuit::{Gate, PullApplicable, PullApplyOutput, PushApplicable, PushApplyOutput};
use crate::config::Config;
use crate::types::{BasisIdx, Complex, Real};
use crate::utility;

use super::super::Compactifiable;
use super::state::{DenseStateTable, SparseStateTable, ConcurrentSparseStateTable, SparseStateTableInserion, State};

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

fn try_put(table: &mut ConcurrentSparseStateTable, bidx: BasisIdx, weight: Complex) -> SparseStateTableInserion {
    let max_load: Real = 123.0;
    let n = table.capacity();
    let probably_longest_probe = ((n as Real).log2() / (max_load - 1.0 - max_load.log2())).ceil() as usize;
    let tolerance = 4 * std::cmp::max(10, probably_longest_probe);
    let tolerance = std::cmp::min(tolerance, n);
    table.insertAddWeightsLimitProbes(tolerance, bidx, weight)
}

pub enum SuccessorsResult {
    AllSucceeded,
    SomeFailed(Vec<(BasisIdx,Complex,usize)>),
}

fn apply_gates1(
    gatenum: usize,
    gates: &[&Gate],
    table: &mut ConcurrentSparseStateTable,
    bidx: BasisIdx,
    weight: Complex,
    full: &mut AtomicBool,
    apps: usize,
) -> (usize, SuccessorsResult) {
    if utility::is_zero(weight) {
	return (apps, SuccessorsResult::AllSucceeded);
    }
    if gatenum >= gates.len() {
	match (full.load(Ordering::Relaxed), try_put(table, bidx, weight)) {
	    (false, SparseStateTableInserion::Success) => {
		return (apps, SuccessorsResult::AllSucceeded);
	    }
	    _ => {
		if !full.load(Ordering::Relaxed) {
		    full.store(true, Ordering::SeqCst);
		}
		let mut v = vec![(bidx, weight, gatenum)];
		return (apps, SuccessorsResult::SomeFailed(v));
	    }
	}
    }
    match gates[gatenum].push_apply(bidx, weight) {
        PushApplyOutput::Nonbranching(new_bidx, new_weight) => {
            return apply_gates1(gatenum + 1, gates, table, new_bidx, new_weight, full, apps + 1)
        }
        PushApplyOutput::Branching((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            return apply_gates2(gatenum + 1, gates, table, new_bidx1, new_weight1, new_bidx2, new_weight2, full, apps + 1)
        }
    }
}

fn apply_gates2(
    gatenum: usize,
    gates: &[&Gate],
    table: &mut ConcurrentSparseStateTable,
    bidx1: BasisIdx,
    weight1: Complex,
    bidx2: BasisIdx,
    weight2: Complex,
    full: &mut AtomicBool,
    apps: usize,
) -> (usize, SuccessorsResult) {
    match apply_gates1(gatenum, gates, table, bidx1, weight1, full, apps) {
	(apps2, SuccessorsResult::AllSucceeded) => {
	    apply_gates1(gatenum, gates, table, bidx2, weight2, full, apps)
	}
	(apps2, SuccessorsResult::SomeFailed(v)) => {
	    let mut v2 = v.clone(); // probably we can do something more efficient here
	    v2.push((bidx1, weight1, gatenum));
	    (apps2, SuccessorsResult::SomeFailed(v2))
	}
    }
}

fn expand_sparse2(gates: Vec<&Gate>, config: &Config, state: State) -> usize {
    let mut table = ConcurrentSparseStateTable::new();
    let n : usize = state.num_nonzeros();
    let block_size = std::cmp::min(n / 1000, config.block_size);
    let block_size = std::cmp::max(100, block_size);
    let num_blocks = n / block_size; // TODO: need ceiling division
    let block_start = |b: usize| block_size * b;
    let block_stop = |b: usize| std::cmp::min(n, block_size + block_start(b));
    let mut block_remaining_starts: Vec<usize> = (0..num_blocks).map(|b| block_start(b)).collect();
    let block_has_remaining = |b: usize| block_remaining_starts[b] < block_stop(b);
    let mut block_pending: Vec<Vec<(BasisIdx, Complex, usize)>> = vec![vec![]; num_blocks];
    let block_has_pending = |b: usize| !block_pending[b].is_empty();
    let mut remaining_blocks: Vec<usize> = (0..n).map(|b| b).collect();
    let mut apps = 0;

    while !remaining_blocks.is_empty() {
	let mut full: AtomicBool = AtomicBool::new(false);
	for i in 0..remaining_blocks.len() { // process each block b, i.e., workOnBlock()
	    let b = remaining_blocks[i];
	    let mut clear_pending = |apps0: usize, block_pending0: Vec<(BasisIdx, Complex, usize)>| {
		let mut apps = apps0;
		let mut block_pending1 = block_pending0.clone();
		let mut block_pending2: Vec<(BasisIdx, Complex, usize)> = vec![];
		while !block_pending1.is_empty() {
		    if let Some((idx, weight, gatenum)) = block_pending1.pop() {
			match apply_gates1(gatenum, &gates, &mut table, idx, weight, &mut full, apps) {
			    (apps2, SuccessorsResult::AllSucceeded) => {
				apps = apps2;
			    }
			    (apps2, SuccessorsResult::SomeFailed(vfs)) => {
				block_pending2.extend(vfs);
				apps = apps2;
				break;
			    }
			}
		    }
		}
		block_pending2.extend(block_pending1);
		(apps, block_pending2)
	    };
	    let (apps0, pending_cleared) = clear_pending(0, block_pending.swap_remove(b));
	    let mut appsb = apps0;
	    if !pending_cleared.is_empty() {
		block_pending[b].extend(pending_cleared);
		break;
	    }
	    let mut j = block_remaining_starts[b];
	    loop { // process each item j in block b
		if j >= block_stop(b) {
		    block_remaining_starts[b] = block_stop(b);
		    break
		}
		let idx = BasisIdx::from_idx(j);
		match State::get(&state, &idx) {
		    Some(weight) => {
			match apply_gates1(0, &gates, &mut table, idx, weight, &mut full, appsb) {
			    (apps2, SuccessorsResult::AllSucceeded) => {
				j = j + 1;
				appsb = apps2;
			    }
			    (apps2, SuccessorsResult::SomeFailed(vfs)) => {
				block_remaining_starts[b] = j + 1;
				block_pending[b].extend(vfs);
				appsb = apps2;
			    }
			}
		    }
		    None => {
			j = j + 1;
		    }
		}
	    }
	    apps = apps + appsb;
	}
	//let remaining_blocks_next: Vec<usize> = remaining_blocks.iter().filter(|&&b| block_has_pending(b) || block_has_remaining(b)).cloned().collect();
    }
    0
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

fn expand_push_dense(
    gates: Vec<&Gate>,
    config: &Config,
    num_qubits: usize,
    state: State,
) -> ExpandResult {
    let block_size = config.block_size;
    let table = DenseStateTable::new(num_qubits, block_size);

    let num_gate_apps = match state {
        State::Sparse(prev_table) => prev_table
            .table
            .into_iter()
            .collect::<Vec<_>>()
            .par_chunks(block_size)
            .map(|chunk| {
                chunk
                    .iter()
                    .map(|(bidx, weight)| apply_gates(&gates, &table, *bidx, *weight))
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
                    .map(|(idx, v)| {
                        let (re, im) = utility::unpack_complex(v.load(Ordering::Relaxed));
                        apply_gates(
                            &gates,
                            &table,
                            BasisIdx::from_idx(block_size * chunk_idx + idx),
                            Complex::new(re, im),
                        )
                    })
                    .sum::<usize>()
            })
            .sum(),
    };

    let num_nonzeros = table.num_nonzeros(block_size);

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
    let block_size = config.block_size;
    let table = DenseStateTable::new(num_qubits, block_size);
    let capacity = 1 << num_qubits;

    let (num_gate_apps, num_nonzeros) = (0..capacity)
        .into_par_iter()
        .fold_chunks(
            block_size,
            || (0, 0),
            |acc, idx| {
                let bidx = BasisIdx::from_idx(idx);
                let (weight, num_gate_apps_here) = apply_pull_gates(&gates, &state, &bidx);
                table.atomic_put(bidx, weight);
                (
                    acc.0 + num_gate_apps_here,
                    acc.1 + if utility::is_nonzero(weight) { 1 } else { 0 },
                )
            },
        )
        .reduce(|| (0, 0), |a, b| (a.0 + b.0, a.1 + b.1));

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
    table: &mut SparseStateTable,
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

fn apply_gates(gates: &[&Gate], table: &DenseStateTable, bidx: BasisIdx, weight: Complex) -> usize {
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

fn apply_pull_gates(gates: &[&Gate], prev_state: &State, bidx: &BasisIdx) -> (Complex, usize) {
    if gates.is_empty() {
        let weight = prev_state.get(bidx).unwrap_or(Complex::new(0.0, 0.0));
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
