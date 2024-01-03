use std::fmt::{self, Display, Formatter};

use crate::circuit::{Gate, Unitary};
use crate::config::Config;
use crate::futhark::{self, Context, FutharkVector};
use crate::simulator::parallel_simulator::SparseStateTable;
use crate::types::{AtomicBasisIdx, BasisIdx, Complex};

use super::super::expected_cost;
use super::super::parallel_simulator;
use super::State;

pub enum ExpandMethod {
    Sparse,
    Dense,
}

impl Display for ExpandMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExpandMethod::Sparse => write!(f, "sparse"),
            ExpandMethod::Dense => write!(f, "dense"),
        }
    }
}
pub struct ExpandResult<'a, B: BasisIdx, AB: AtomicBasisIdx<B>> {
    pub state: State<'a, B, AB>,
    pub num_nonzeros: usize,
    pub method: ExpandMethod,
}

pub fn expand<'a, B: BasisIdx, AB: AtomicBasisIdx<B>>(
    futhark_ctx: &'a Context,
    gate: &Gate<B>,
    config: &Config,
    num_qubits: usize,
    num_nonzeros: usize,
    prev_num_nonzeros: usize,
    state: State<'a, B, AB>,
) -> ExpandResult<'a, B, AB> {
    let (expected_density, expected_num_nonzeros) =
        expected_cost(num_qubits, num_nonzeros, prev_num_nonzeros);

    if expected_density < config.dense_threshold {
        expand_sparse(gate, config, num_qubits, expected_num_nonzeros, state)
    } else {
        expand_dense(futhark_ctx, gate, num_qubits, state)
    }
}

fn expand_sparse<'a, B: BasisIdx, AB: AtomicBasisIdx<B>>(
    gate: &Gate<B>,
    config: &Config,
    num_qubits: usize,
    expected_num_nonzeros: usize,
    state: State<'a, B, AB>,
) -> ExpandResult<'a, B, AB> {
    let parallel_simulator_state = match state {
        State::Sparse(table) => parallel_simulator::State::Sparse(table),
        State::Dense(futhark_vec) => parallel_simulator::State::Sparse(
            SparseStateTable::new_from_dense(num_qubits, futhark_vec.into_vec()),
        ),
    };
    let parallel_simulator::ExpandResult {
        state,
        num_nonzeros,
        method: parallel_simulator_expand_method,
        ..
    } = parallel_simulator::expand_sparse(
        vec![gate],
        num_qubits,
        config,
        expected_num_nonzeros,
        &parallel_simulator_state,
    );

    let state = match state {
        parallel_simulator::State::Sparse(table) => State::Sparse(table),
        _ => unreachable!("Result of parallel_simulator::expand_sparse should be sparse"),
    };

    let method = match parallel_simulator_expand_method {
        parallel_simulator::ExpandMethod::Sparse => ExpandMethod::Sparse,
        parallel_simulator::ExpandMethod::PushDense
        | parallel_simulator::ExpandMethod::PullDense => ExpandMethod::Dense,
    };

    ExpandResult {
        state,
        num_nonzeros,
        method,
    }
}

fn expand_dense<'a, B: BasisIdx, AB: AtomicBasisIdx<B>>(
    futhark_ctx: &'a Context,
    gate: &Gate<B>,
    num_qubits: usize,
    state: State<'a, B, AB>,
) -> ExpandResult<'a, B, AB> {
    let futhark_vec = create_futhark_vec(futhark_ctx, num_qubits, state);

    let new_state = futhark::apply_vec(futhark_ctx, futhark_vec, gate.unitary());

    let num_nonzeros = 1 << num_qubits;
    // FIXME: Temporarily we use num_nonzeros = 1 << num_qubits, just to treat that the new state is dense
    // We need a way to compute num_nonzeros of the new state without actually retrieving the data from the GPU

    ExpandResult {
        state: State::Dense(new_state),
        num_nonzeros,
        method: ExpandMethod::Dense,
    }
}

fn create_futhark_vec<'a, B: BasisIdx, AB: AtomicBasisIdx<B>>(
    ctx: &'a Context,
    num_qubits: usize,
    state: State<'a, B, AB>,
) -> FutharkVector<'a> {
    match state {
        State::Sparse(table) => {
            let dim = 1 << num_qubits;
            // TODO: Optimize this
            let vec = (0..dim)
                .into_iter()
                .map(|i| {
                    let bidx = B::from_idx(i);
                    match table.get(&bidx) {
                        Some(weight) => weight,
                        None => Complex::new(0.0, 0.0),
                    }
                })
                .collect::<Vec<_>>();
            FutharkVector::new(ctx, vec)
        }
        State::Dense(futhark_vec) => futhark_vec,
    }
}
