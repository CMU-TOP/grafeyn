pub mod parallel_simulator;
pub mod sequential_simulator;

use crate::types::{BasisIdx, Complex};

pub trait Compactifiable {
    fn compactify(self) -> Box<dyn Iterator<Item = (BasisIdx, Complex)>>;
}

#[derive(Debug)]
pub enum SimulatorError {
    TooManyQubits,
}
