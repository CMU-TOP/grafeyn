pub mod dense_simulator;
pub mod parallel_simulator;
pub mod sequential_simulator;

use crate::types::{BasisIdx, Complex};

pub trait Compactifiable<B: BasisIdx> {
    fn compactify(self) -> Box<dyn Iterator<Item = (B, Complex)>>;
}
