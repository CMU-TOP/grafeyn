use std::collections::HashSet;

use log::debug;

use super::GateScheduler;
use crate::types::{GateIndex, QubitIndex};

pub struct GreedyFinishQubitGateScheduler<'a> {
    _frontier: Vec<GateIndex>,
    pub num_gates: usize,
    _num_qubits: usize,
    _gate_touches: Vec<&'a HashSet<QubitIndex>>,
    _gate_is_branching: Vec<bool>,
    _max_branching_stride: usize,
}

impl<'a> GateScheduler for GreedyFinishQubitGateScheduler<'a> {
    fn pick_next_gates(&mut self) -> Vec<GateIndex> {
        todo!()
    }
}

impl<'a> GreedyFinishQubitGateScheduler<'a> {
    pub fn new(
        num_gates: usize,
        num_qubits: usize,
        _gate_touches: Vec<&'a HashSet<QubitIndex>>,
        _gate_is_branching: Vec<bool>,
    ) -> Self {
        debug!(
            "initializing greedy finish qubit gate scheduler with {} gates and {} qubits",
            num_gates, num_qubits
        );

        todo!()
    }
}
