use std::str::FromStr;

mod greedy_nonbranching_gate_scheduler;
mod naive_gate_scheduler;

pub use greedy_nonbranching_gate_scheduler::GreedyNonbranchingGateScheduler;
pub use naive_gate_scheduler::NaiveGateScheduler;

#[derive(Debug, Copy, Clone)]
pub enum GateSchedulingPolicy {
    Naive,
    GreedyNonbranching,
}

impl FromStr for GateSchedulingPolicy {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "naive" => Ok(GateSchedulingPolicy::Naive),
            "greedy-nonbranching" | "gnb" => Ok(GateSchedulingPolicy::GreedyNonbranching),
            _ => Err(format!(
                "unknown gate scheduling policy: {}; valid values are: naive, gnb",
                s
            )),
        }
    }
}

pub trait GateScheduler {
    fn pick_next_gates(&mut self) -> Vec<usize>;
}
