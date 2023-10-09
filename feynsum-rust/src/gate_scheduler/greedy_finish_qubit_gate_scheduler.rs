use log::debug;

use super::{utility, GateScheduler};
use crate::types::{GateIndex, QubitIndex};

pub struct GreedyFinishQubitGateScheduler<'a> {
    frontier: Vec<GateIndex>,
    num_gates: usize,
    num_qubits: usize,
    gate_touches: Vec<&'a [QubitIndex]>,
}

impl<'a> GateScheduler for GreedyFinishQubitGateScheduler<'a> {
    fn pick_next_gates(&mut self) -> Vec<GateIndex> {
        let unfinished_qubit = (0..self.num_qubits).find(|qi| self.frontier[*qi] < self.num_gates);

        match unfinished_qubit {
            Some(qi) => self.make_progress_on_qubit(qi),
            None => Vec::new(),
        }
    }
}

impl<'a> GreedyFinishQubitGateScheduler<'a> {
    pub fn new(num_gates: usize, num_qubits: usize, gate_touches: Vec<&'a [QubitIndex]>) -> Self {
        debug!(
            "initializing greedy finish qubit gate scheduler with {} gates and {} qubits",
            num_gates, num_qubits
        );

        GreedyFinishQubitGateScheduler {
            frontier: (0..num_qubits)
                .map(|qi| utility::next_touch(num_gates, &gate_touches, qi, 0))
                .collect(),
            num_gates,
            num_qubits,
            gate_touches,
        }
    }

    fn make_progress_on_qubit(&mut self, qi: QubitIndex) -> Vec<GateIndex> {
        let desired_gate = self.frontier[qi];

        if utility::okay_to_visit(
            self.num_gates,
            &self.gate_touches,
            &self.frontier,
            desired_gate,
        ) {
            utility::mark_as_visit(
                self.num_gates,
                &self.gate_touches,
                &mut self.frontier,
                desired_gate,
            );
            vec![desired_gate]
        } else {
            let dependency = self.gate_touches[desired_gate]
                .iter()
                .find(|qj| self.frontier[**qj] < desired_gate)
                .expect("since desired_gate is not okay to visit, there must be a dependency");

            self.make_progress_on_qubit(*dependency)
        }
    }
}
