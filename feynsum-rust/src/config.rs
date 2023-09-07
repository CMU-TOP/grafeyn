use std::collections::HashSet;

use log::info;

use crate::gate_scheduler::{
    GateScheduler, GateSchedulingPolicy, GreedyNonbranchingGateScheduler, NaiveGateScheduler,
};
use crate::options::Options;
use crate::types::{QubitIndex, Real};

pub struct Config {
    #[allow(dead_code)]
    block_size: u32,
    #[allow(dead_code)]
    maxload: Real,
    gate_scheduling_policy: GateSchedulingPolicy, // TODO: Add denseThreshold, pullThreshold
}

impl Config {
    pub fn new(options: &Options) -> Self {
        Self {
            block_size: 0, // FIXME
            maxload: 0.0,  // FIXME
            gate_scheduling_policy: options.gate_schduling_policy,
        }
    }

    pub fn create_gate_scheduler<'a>(
        &self,
        num_gates: usize,
        num_qubits: usize,
        gate_touches: Vec<&'a HashSet<QubitIndex>>,
        gate_is_branching: Vec<bool>,
    ) -> Box<dyn GateScheduler + 'a> {
        match self.gate_scheduling_policy {
            GateSchedulingPolicy::Naive => {
                info!("using naive gate scheduler");
                Box::new(NaiveGateScheduler::new(num_gates))
            }
            GateSchedulingPolicy::GreedyNonbranching => {
                info!("using greedy nonbranching gate scheduler");
                Box::new(GreedyNonbranchingGateScheduler::new(
                    num_gates,
                    num_qubits,
                    gate_touches,
                    gate_is_branching,
                ))
            }
        }
    }
}
