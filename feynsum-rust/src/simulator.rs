pub mod parallel_simulator;
pub mod sequential_simulator;

use crate::types::{BasisIdx64, Complex};

pub trait Compactifiable {
    fn compactify(self) -> Box<dyn Iterator<Item = (BasisIdx64, Complex)>>;
}

#[derive(Debug)]
pub enum SimulatorError {
    TooManyQubits,
}
