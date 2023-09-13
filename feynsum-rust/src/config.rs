use crate::gate_scheduler::GateSchedulingPolicy;
use crate::options::Options;
use crate::types::Real;

pub struct Config {
    pub block_size: usize,
    #[allow(dead_code)]
    maxload: Real,
    pub gate_scheduling_policy: GateSchedulingPolicy, // TODO: Add pullThreshold
    pub dense_threshold: Real,
    pub pull_threshold: Real,
}

impl Config {
    pub fn new(options: &Options) -> Self {
        Self {
            block_size: options.block_size,
            maxload: 0.0, // FIXME
            gate_scheduling_policy: options.gate_schduling_policy,
            dense_threshold: options.dense_threshold,
            pull_threshold: options.pull_threshold,
        }
    }
}

// used for testing
impl Default for Config {
    fn default() -> Self {
        Self {
            block_size: 10_000,
            maxload: 0.0, // FIXME
            gate_scheduling_policy: GateSchedulingPolicy::GreedyNonbranching,
            dense_threshold: 0.25,
            pull_threshold: 0.8,
        }
    }
}
