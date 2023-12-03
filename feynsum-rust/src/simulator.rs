use std::cmp;
use std::str::FromStr;

pub mod dense_simulator;
pub mod hybrid_simulator;
pub mod parallel_simulator;
pub mod sequential_simulator;

use crate::types::{BasisIdx, Complex, Real};

pub trait Compactifiable<B: BasisIdx> {
    fn compactify(self) -> Box<dyn Iterator<Item = (B, Complex)>>;
}

#[derive(Debug)]
pub enum Simulator {
    Sequential,
    Parallel,
    Dense,
    Hybrid,
}

impl FromStr for Simulator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "sequential" | "seq" => Ok(Simulator::Sequential),
            "parallel" | "par" => Ok(Simulator::Parallel),
            "dense" => Ok(Simulator::Dense),
            "hybrid" => Ok(Simulator::Hybrid),
            _ => Err(format!(
                "unknown simulator: {}; valid values are: sequential, parallel, and dense",
                s
            )),
        }
    }
}

/// computes the expected density and num nozeros after applying a (fused) gate
pub fn expected_cost(
    num_qubits: usize,
    num_nonzeros: usize,
    prev_num_nonzeros: usize,
) -> (Real, usize) {
    let max_num_states = 1 << num_qubits;
    let rate = Real::max(1.0, num_nonzeros as Real / prev_num_nonzeros as Real);
    let expected_num_nonzeros =
        cmp::min(max_num_states, (rate * num_nonzeros as Real) as i64) as usize;
    let expected_density = expected_num_nonzeros as Real / max_num_states as Real;
    let current_density = num_nonzeros as Real / max_num_states as Real;

    (expected_density.max(current_density), expected_num_nonzeros)
}
