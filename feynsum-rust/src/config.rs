use crate::types::Real;

pub struct Config {
    #[allow(dead_code)]
    block_size: u32,
    #[allow(dead_code)]
    maxload: Real,
    gate_scheduling_policy: GateSchedulingPolicy, // TODO: Add denseThreshold, pullThreshold
}

impl Config {
    pub fn new() -> Self {
        Self {
            block_size: 0, // FIXME
            maxload: 0.0,  // FIXME
            gate_scheduling_policy: GateSchedulingPolicy::Naive,
        }
    }

    pub fn create_gate_scheduler(&self, num_gates: usize) -> Box<dyn GateScheduler> {
        match self.gate_scheduling_policy {
            GateSchedulingPolicy::Naive => Box::new(NaiveGateScheduler { next: 0, num_gates }),
        }
    }
}

pub enum GateSchedulingPolicy {
    Naive,
}

pub trait GateScheduler {
    fn pick_next_gates(&mut self) -> Vec<usize>;
}

struct NaiveGateScheduler {
    pub next: usize,
    pub num_gates: usize,
}

impl GateScheduler for NaiveGateScheduler {
    fn pick_next_gates(&mut self) -> Vec<usize> {
        if self.next >= self.num_gates {
            vec![]
        } else {
            let gate_idx = self.next;
            self.next += 1;
            vec![gate_idx]
        }
    }
}
