use std::str::FromStr;

mod naive_gate_scheduler;

pub use naive_gate_scheduler::NaiveGateScheduler;

#[derive(Debug, Copy, Clone)]
pub enum GateSchedulingPolicy {
    Naive,
}

impl FromStr for GateSchedulingPolicy {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "naive" => Ok(GateSchedulingPolicy::Naive),
            _ => Err(format!(
                "unknown gate scheduling policy: {}; valid values are: naive",
                s
            )),
        }
    }
}

pub trait GateScheduler {
    fn new(num_gates: usize) -> Self;
    fn pick_next_gates(&mut self) -> Vec<usize>;
}
