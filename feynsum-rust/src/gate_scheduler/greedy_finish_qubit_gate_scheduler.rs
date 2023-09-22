use std::collections::HashSet;

use log::debug;

use super::GateScheduler;
use crate::types::{GateIndex, QubitIndex};

pub struct GreedyFinishQubitGateScheduler<'a> {
    frontier: Vec<GateIndex>,
    num_gates: usize,
    num_qubits: usize,
    gate_touches: Vec<&'a HashSet<QubitIndex>>,
}

impl<'a> GateScheduler for GreedyFinishQubitGateScheduler<'a> {
    fn pick_next_gates(&mut self) -> Vec<GateIndex> {
        let unfinished_qubit = (0..self.num_qubits)
            .filter(|qi| self.frontier[*qi] < self.num_gates)
            .next();

        match unfinished_qubit {
            Some(qi) => self.make_progress_on_qubit(qi),
            None => Vec::new(),
        }
    }
}

impl<'a> GreedyFinishQubitGateScheduler<'a> {
    pub fn new(
        num_gates: usize,
        num_qubits: usize,
        gate_touches: Vec<&'a HashSet<QubitIndex>>,
    ) -> Self {
        debug!(
            "initializing greedy finish qubit gate scheduler with {} gates and {} qubits",
            num_gates, num_qubits
        );

        GreedyFinishQubitGateScheduler {
            frontier: (0..num_qubits)
                .map(|qi| next_touch(num_gates, &gate_touches, qi, 0))
                .collect(),
            num_gates,
            num_qubits,
            gate_touches,
        }
    }

    fn make_progress_on_qubit(&mut self, qi: QubitIndex) -> Vec<GateIndex> {
        let desired_gate = self.frontier[qi];

        if self.okay_to_visit(desired_gate) {
            self.visit(desired_gate);
            vec![desired_gate]
        } else {
            let dependency = self.gate_touches[desired_gate]
                .iter()
                .filter(|qj| self.frontier[**qj] < desired_gate)
                .next()
                .expect("since desired_gate is not okay to visit, there must be a dependency");

            self.make_progress_on_qubit(*dependency)
        }
    }

    fn okay_to_visit(&self, gi: GateIndex) -> bool {
        gi < self.num_gates
            && self.gate_touches[gi]
                .iter()
                .all(|qi| self.frontier[*qi] == gi)
    }

    fn visit(&mut self, gi: GateIndex) {
        debug!("visiting gate: {}", gi);
        assert!(self.okay_to_visit(gi));
        for qi in self.gate_touches[gi] {
            let next = next_touch(self.num_gates, &self.gate_touches, *qi, gi + 1);

            self.frontier[*qi] = next;
            debug!("updated frontier[{}] to {}", qi, self.frontier[*qi]);
        }
    }
}

fn next_touch(
    num_gates: usize,
    gate_touches: &[&HashSet<QubitIndex>],
    qi: QubitIndex,
    gi: GateIndex,
) -> GateIndex {
    if gi >= num_gates {
        num_gates
    } else if gate_touches[gi].contains(&qi) {
        gi
    } else {
        next_touch(num_gates, gate_touches, qi, gi + 1)
    }
}
