use std::str::FromStr;

pub mod dense_simulator;
pub mod parallel_simulator;
pub mod sequential_simulator;

use crate::types::{BasisIdx, Complex};

pub trait Compactifiable<B: BasisIdx> {
    fn compactify(self) -> Box<dyn Iterator<Item = (B, Complex)>>;
}

#[derive(Debug)]
pub enum Simulator {
    Sequential,
    Parallel,
    Dense,
}

impl FromStr for Simulator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "sequential" | "seq" => Ok(Simulator::Sequential),
            "parallel" | "par" => Ok(Simulator::Parallel),
            "dense" => Ok(Simulator::Dense),
            _ => Err(format!(
                "unknown simulator: {}; valid values are: sequential, parallel, and dense",
                s
            )),
        }
    }
}
