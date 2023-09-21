pub mod parallel;
pub mod sequential;

use crate::types::{BasisIdx, Complex};

pub trait Compactifiable {
    fn compactify(self) -> Box<dyn Iterator<Item = (BasisIdx, Complex)>>;
}

#[derive(Debug)]
pub enum SimulatorError {
    TooManyQubits,
}
