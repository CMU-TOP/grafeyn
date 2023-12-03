use super::{utility, GateScheduler};
use crate::types::{GateIndex, QubitIndex};

pub struct GreedyNonbranchingGateScheduler<'a> {
    frontier: Vec<GateIndex>,
    num_gates: usize,
    num_qubits: usize,
    gate_touches: Vec<&'a [QubitIndex]>,
    gate_is_branching: Vec<bool>,
    max_branching_stride: usize,
    disable_gate_fusion: bool,
}

impl<'a> GateScheduler for GreedyNonbranchingGateScheduler<'a> {
    fn pick_next_gates(&mut self) -> Vec<GateIndex> {
        if self.disable_gate_fusion {
            match self.visit_nonbranching() {
                Some(gi) => vec![gi],
                None => match self.visit_branching() {
                    Some(gi) => vec![gi],
                    None => vec![],
                },
            }
        } else {
            let mut num_branching_so_far = 0;
            let mut next_gates = Vec::<GateIndex>::new();

            while num_branching_so_far < self.max_branching_stride {
                next_gates.append(&mut self.visit_maximal_nonbranching_run());

                if let Some(next_gate) = self.visit_branching() {
                    num_branching_so_far += 1;
                    next_gates.push(next_gate);
                } else {
                    break;
                }
            }

            // each gate in next_gates should be marked as already visited
            assert!(next_gates.iter().all(|gi| !utility::okay_to_visit(
                self.num_gates,
                &self.gate_touches,
                &self.frontier,
                *gi
            )));

            log::debug!("next gates: {:?}", next_gates);

            next_gates
        }
    }
}

impl<'a> GreedyNonbranchingGateScheduler<'a> {
    pub fn new(
        num_gates: usize,
        num_qubits: usize,
        gate_touches: Vec<&'a [QubitIndex]>,
        gate_is_branching: Vec<bool>,
        disable_gate_fusion: bool,
    ) -> Self {
        log::debug!(
            "initializing greedy nonbranching gate scheduler with {} gates and {} qubits",
            num_gates,
            num_qubits
        );
        let scheduler = Self {
            frontier: (0..num_qubits)
                .map(|qi| next_touch(num_gates, &gate_touches, qi, 0))
                .collect(),
            num_gates,
            num_qubits,
            gate_touches,
            gate_is_branching,
            max_branching_stride: 2,
            disable_gate_fusion,
        };

        assert_eq!(scheduler.frontier.len(), num_qubits);
        assert_eq!(scheduler.gate_touches.len(), num_gates);
        assert_eq!(scheduler.gate_is_branching.len(), num_gates);

        log::debug!("initial frontier: {:?}", scheduler.frontier);

        scheduler
    }

    fn visit_nonbranching(&mut self) -> Option<GateIndex> {
        let candidate = self
            .frontier
            .iter()
            .filter(|gi| {
                *gi < &self.num_gates
                    && !self.gate_is_branching[**gi]
                    && utility::okay_to_visit(
                        self.num_gates,
                        &self.gate_touches,
                        &self.frontier,
                        **gi,
                    )
            })
            .nth(0)
            .cloned();

        if let Some(gi) = candidate {
            utility::mark_as_visit(self.num_gates, &self.gate_touches, &mut self.frontier, gi);
        }
        candidate
    }

    fn visit_maximal_nonbranching_run(&mut self) -> Vec<GateIndex> {
        let mut non_branching_gates = Vec::new();

        loop {
            let mut selection = Vec::<GateIndex>::new();

            for qi in 0..self.num_qubits {
                loop {
                    let next_gi = self.frontier[qi];
                    if next_gi >= self.num_gates
                        || self.gate_is_branching[next_gi]
                        || !utility::okay_to_visit(
                            self.num_gates,
                            &self.gate_touches,
                            &self.frontier,
                            next_gi,
                        )
                    {
                        break;
                    } else {
                        assert!(utility::okay_to_visit(
                            self.num_gates,
                            &self.gate_touches,
                            &self.frontier,
                            next_gi
                        ));
                        utility::mark_as_visit(
                            self.num_gates,
                            &self.gate_touches,
                            &mut self.frontier,
                            next_gi,
                        );
                        selection.push(next_gi);
                    }
                }
            }

            if selection.is_empty() {
                break;
            } else {
                non_branching_gates.append(&mut selection);
            }
        }
        non_branching_gates
    }

    fn visit_branching(&mut self) -> Option<GateIndex> {
        let result = self
            .frontier
            .iter()
            .filter(|gi| {
                *gi < &self.num_gates
                    && self.gate_is_branching[**gi]
                    && utility::okay_to_visit(
                        self.num_gates,
                        &self.gate_touches,
                        &self.frontier,
                        **gi,
                    )
            })
            .nth(0)
            .copied();

        if let Some(gi) = result {
            utility::mark_as_visit(self.num_gates, &self.gate_touches, &mut self.frontier, gi);
        }

        result
    }
}

fn next_touch(
    num_gates: usize,
    gate_touches: &[&[QubitIndex]],
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
