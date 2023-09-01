use crate::circuit::Gate;

#[derive(Debug)]
pub enum State {
    Sparse(SparseState),
    #[allow(dead_code)]
    Dense(DenseState), // TODO: use this
    #[allow(dead_code)]
    DenseKnownNonzeroSize(DenseState, usize), // TODO: use this
}
// TODO: maybe better to use trait objects instead of enums

#[derive(Debug)]
pub struct SparseState {}

impl SparseState {
    pub fn singleton() -> Self {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct DenseState {}

pub struct ExpandResult {
    pub state: State,
    pub method: String,
    pub num_nonzero: usize,
    pub num_gate_apps: usize,
}

pub fn expand(
    _gates: Vec<Gate>,
    _num_qubits: usize,
    _state: State,
    _prev_nonzero_size: usize,
) -> ExpandResult {
    unimplemented!()
}
