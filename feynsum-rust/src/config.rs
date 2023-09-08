use crate::gate_scheduler::GateSchedulingPolicy;
use crate::options::Options;
use crate::types::Real;

pub struct Config {
    #[allow(dead_code)]
    block_size: u32,
    #[allow(dead_code)]
    maxload: Real,
    pub gate_scheduling_policy: GateSchedulingPolicy, // TODO: Add pullThreshold
    pub dense_threshold: Real,
    pub pull_threshold: Real,
}

impl Config {
    pub fn new(options: &Options) -> Self {
        Self {
            block_size: 0, // FIXME
            maxload: 0.0,  // FIXME
            gate_scheduling_policy: options.gate_schduling_policy,
            dense_threshold: options.dense_threshold,
            pull_threshold: options.pull_threshold,
        }
    }
}
