use crate::gate_scheduler::{GateScheduler, GateSchedulingPolicy, NaiveGateScheduler};
use crate::options::Options;
use crate::types::Real;

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

    pub fn create_gate_scheduler(&self, num_gates: usize) -> impl GateScheduler {
        match self.gate_scheduling_policy {
            GateSchedulingPolicy::Naive => NaiveGateScheduler::new(num_gates),
        }
    }
}
