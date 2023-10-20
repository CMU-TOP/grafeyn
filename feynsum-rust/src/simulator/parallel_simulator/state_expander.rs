use std::cmp;
use std::fmt::{self, Display, Formatter};
use std::sync::{atomic::AtomicBool, atomic::Ordering};

use rayon::prelude::*;

use crate::circuit::{Gate, PullApplyOutput, PushApplicable, PushApplyOutput};
use crate::config::Config;
use crate::simulator::Compactifiable;
use crate::types::{BasisIdx, Complex, Real};
use crate::utility;

use super::state::{
    ConcurrentSparseStateTable, DenseStateTable, SparseStateTable, SparseStateTableInsertion, State,
};

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
        expand_sparse2(gates, config, &state)
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
    Real::max(expected_density, current_density)
}

fn try_put(
    table: &ConcurrentSparseStateTable,
    bidx: BasisIdx,
    weight: Complex,
) -> SparseStateTableInsertion {
    let max_load: Real = 0.9;
    let n = table.capacity();
    let probably_longest_probe =
        ((n as Real).log2() / (max_load - 1.0 - max_load.log2())).ceil() as usize;
    let tolerance = std::cmp::min(4 * std::cmp::max(10, probably_longest_probe), n);
    table.insert_add_weights_limit_probes(tolerance, bidx, weight)
}

pub enum SuccessorsResult {
    AllSucceeded,
    SomeFailed(Vec<(BasisIdx, Complex, usize)>),
}

fn apply_gates1(
    gatenum: usize,
    gates: &[&Gate],
    table: &ConcurrentSparseStateTable,
    bidx: BasisIdx,
    weight: Complex,
    full: &AtomicBool,
    apps: usize,
) -> (usize, SuccessorsResult) {
    if utility::is_zero(weight) {
        return (apps, SuccessorsResult::AllSucceeded);
    }
    if gatenum >= gates.len() {
        if !full.load(Ordering::Relaxed) {
            match try_put(table, bidx, weight) {
                SparseStateTableInsertion::Success => {
                    return (apps, SuccessorsResult::AllSucceeded)
                }
                SparseStateTableInsertion::Full => (),
            }
        }
        if !full.load(Ordering::Relaxed) {
            full.store(true, Ordering::SeqCst);
        }
        return (
            apps,
            SuccessorsResult::SomeFailed(vec![(bidx, weight, gatenum)]),
        );
    }
    match gates[gatenum].push_apply(bidx, weight) {
        PushApplyOutput::Nonbranching(new_bidx, new_weight) => apply_gates1(
            gatenum + 1,
            gates,
            table,
            new_bidx,
            new_weight,
            full,
            apps + 1,
        ),
        PushApplyOutput::Branching((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            apply_gates2(
                gatenum + 1,
                gates,
                table,
                new_bidx1,
                new_weight1,
                new_bidx2,
                new_weight2,
                full,
                apps + 1,
            )
        }
    }
}

fn apply_gates2(
    gatenum: usize,
    gates: &[&Gate],
    table: &ConcurrentSparseStateTable,
    bidx1: BasisIdx,
    weight1: Complex,
    bidx2: BasisIdx,
    weight2: Complex,
    full: &AtomicBool,
    apps: usize,
) -> (usize, SuccessorsResult) {
    match apply_gates1(gatenum, gates, table, bidx1, weight1, full, apps) {
        (apps, SuccessorsResult::AllSucceeded) => {
            apply_gates1(gatenum, gates, table, bidx2, weight2, full, apps)
        }
        (apps, SuccessorsResult::SomeFailed(v)) => {
            let mut v2 = v.clone();
            v2.push((bidx2, weight2, gatenum));
            (apps, SuccessorsResult::SomeFailed(v2))
        }
    }
}

fn expand_sparse2(gates: Vec<&Gate>, config: &Config, state: &State) -> ExpandResult {
    let mut table = ConcurrentSparseStateTable::new();
    let n: usize = match state {
        State::ConcurrentSparse(prev_table) => prev_table.num_nonzeros(),
        State::Dense(prev_table) => prev_table.capacity(),
        State::Sparse(_) => panic!(),
    };
    let block_size = std::cmp::max(100, std::cmp::min(n / 1000, config.block_size));
    let num_blocks = (n as f64 / block_size as f64).ceil() as usize;
    let block_start = |b: usize| block_size * b;
    let block_stop = |b: usize| std::cmp::min(n, block_size + block_start(b));
    let mut blocks: Vec<(usize, usize, Vec<(BasisIdx, Complex, usize)>)> = (0..num_blocks)
        .into_par_iter()
        .map(|b| (b, block_start(b), vec![]))
        .collect();
    let get = |i: usize| match state {
        State::Dense(prev_table) => {
            let v = prev_table.array[i].load(Ordering::Relaxed);
            let weight = utility::unpack_complex(v);
            (BasisIdx::from_idx(i), weight)
        }
        State::ConcurrentSparse(prev_table) => {
            let j = prev_table.nonzeros[i];
            let idx = BasisIdx::from_u64(prev_table.keys[j].load(Ordering::Relaxed));
            let weight =
                utility::unpack_complex(prev_table.packed_weights[j].load(Ordering::Relaxed));
            (idx, weight)
        }
        State::Sparse(_) => panic!(),
    };

    while !blocks.is_empty() {
        let full: AtomicBool = AtomicBool::new(false);
        let mut blocks_next = blocks
            .par_iter()
            // We process a given block b in two steps:
            .map(|(b, s, ps)| {
                // (1) we clear any gate applications postponed from resizing the state vector
                let mut s2 = *s;
                let mut ps2 = ps.clone();
                loop {
                    if full.load(Ordering::Relaxed) {
                        return (*b, s2, ps2);
                    }
                    match ps2.pop() {
                        None => {
                            break;
                        }
                        Some((idx, weight, gatenum)) => {
                            match apply_gates1(gatenum, &gates, &table, idx, weight, &full, 0) {
                                (_, SuccessorsResult::AllSucceeded) => {}
                                (_, SuccessorsResult::SomeFailed(fs)) => {
                                    ps2.extend(fs);
                                    return (*b, s2, ps2);
                                }
                            }
                        }
                    }
                }
                // (2) we apply remaining gate applications in the block
                for i in *s..block_stop(*b) {
                    if full.load(Ordering::Relaxed) {
                        s2 = i;
                        break;
                    }
                    let (idx, weight) = get(i);
                    match apply_gates1(0, &gates, &table, idx, weight, &full, 0) {
                        (_, SuccessorsResult::AllSucceeded) => {}
                        (_, SuccessorsResult::SomeFailed(fs)) => {
                            s2 = i + 1;
                            ps2.extend(fs);
                            break;
                        }
                    }
                    if i + 1 == block_stop(*b) {
                        s2 = i + 1
                    }
                }
                (*b, s2, ps2)
            })
            // We keep any block that is not fully processed.
            .filter(|(b, s, ps)| s < &block_stop(*b) || !ps.is_empty())
            .collect();
        std::mem::swap(&mut blocks, &mut blocks_next);
        if !blocks.is_empty() {
            let mut table2 = table.increase_capacity_by_factor(1.5);
            std::mem::swap(&mut table, &mut table2);
        }
    }
    table.make_nonzero_shortcuts();
    let num_nonzeros = table.num_nonzeros();
    let num_gate_apps = 0;
    ExpandResult {
        state: State::ConcurrentSparse(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::Sparse,
    }
}

fn expand_sparse(gates: Vec<&Gate>, state: State) -> ExpandResult {
    let mut table = SparseStateTable::new();

    let num_gate_apps = state
        .compactify()
        .map(|(bidx, weight)| apply_gates_seq(&gates, &mut table, bidx, weight))
        .sum();

    let num_nonzeros = table.num_nonzeros();

    ExpandResult {
        state: State::Sparse(table),
        num_nonzeros,
        num_gate_apps,
        method: ExpandMethod::Sparse,
    }
}

fn expand_push_dense(gates: Vec<&Gate>, num_qubits: usize, state: State) -> ExpandResult {
    let table = DenseStateTable::new(num_qubits);

    let num_gate_apps = match state {
        State::Sparse(prev_table) => prev_table
            .table
            .into_par_iter()
            .map(|(bidx, weight)| apply_gates(&gates, &table, bidx, weight))
            .sum(),
        State::Dense(prev_table) => prev_table
            .array
            .into_par_iter()
            .enumerate()
            .map(|(idx, v)| {
                let weight = utility::unpack_complex(v.load(Ordering::Relaxed));
                apply_gates(&gates, &table, BasisIdx::from_idx(idx), weight)
            })
            .sum(),
        // FIXME: There should be better way to parallelize iteration over nonzeros of State::Sparse
        // FIXME: Refactor this iterator generation
        State::ConcurrentSparse(prev_table) => prev_table
            .nonzeros
            .into_par_iter()
            .map(move |i: usize| {
                let idx = prev_table.keys[i].load(Ordering::Relaxed) as usize;
                let weight =
                    utility::unpack_complex(prev_table.packed_weights[i].load(Ordering::Relaxed));
                (BasisIdx::from_idx(idx), weight)
            })
            .map(|(bidx, weight)| apply_gates(&gates, &table, bidx, weight))
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

fn expand_pull_dense(gates: Vec<&Gate>, num_qubits: usize, state: State) -> ExpandResult {
    let table = DenseStateTable::new(num_qubits);
    let capacity = 1 << num_qubits;

    let (num_gate_apps, num_nonzeros) = (0..capacity)
        .into_par_iter()
        .fold(
            || (0, 0),
            |acc, idx| {
                let bidx = BasisIdx::from_idx(idx);
                let (weight, num_gate_apps_here) = apply_pull_gates(&gates, &state, bidx);
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
        table.atomic_put(bidx, weight);
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

fn apply_pull_gates(gates: &[&Gate], prev_state: &State, bidx: BasisIdx) -> (Complex, usize) {
    if gates.is_empty() {
        let weight = prev_state.get(&bidx).unwrap_or(Complex::new(0.0, 0.0));
        return (weight, 0);
    }

    match gates[0].pull_action.as_ref().unwrap()(bidx) {
        PullApplyOutput::Nonbranching(neighbor, multiplier) => {
            let (weight, num_gate_apps) = apply_pull_gates(&gates[1..], prev_state, neighbor);
            (weight * multiplier, 1 + num_gate_apps)
        }
        PullApplyOutput::Branching((neighbor1, multiplier1), (neighbor2, multiplier2)) => {
            let (weight1, num_gate_apps_1) = apply_pull_gates(&gates[1..], prev_state, neighbor1);
            let (weight2, num_gate_apps_2) = apply_pull_gates(&gates[1..], prev_state, neighbor2);

            (
                weight1 * multiplier1 + weight2 * multiplier2,
                1 + num_gate_apps_1 + num_gate_apps_2,
            )
        }
    }
}
