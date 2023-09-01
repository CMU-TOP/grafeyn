use crate::types::Real;

use crate::circuit::Gate;

#[allow(dead_code)]

pub struct Config {
    #[allow(dead_code)]
    block_size: u32,
    #[allow(dead_code)]
    maxload: Real,
    pub gate_scheduler: Box<dyn GateScheduler>,
    // TODO: Add denseThreshold, pullThreshold
}

impl Config {
    pub fn new() -> Self {
        Self {
            block_size: 0,
            maxload: 0.0,
            gate_scheduler: Box::new(NaiveGateScheduler {}),
        }
    }
}

pub trait GateScheduler {
    fn pick_next_gates(&self) -> Vec<Gate>;
}

struct NaiveGateScheduler {}

impl GateScheduler for NaiveGateScheduler {
    fn pick_next_gates(&self) -> Vec<Gate> {
        unimplemented!()
    }
}
