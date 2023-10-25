use crate::types::{BasisIdx64, Complex};
use crate::utility;

use super::Table;

#[derive(Debug)]
pub struct DenseStateTable {
    pub array: Vec<Complex>,
}

impl DenseStateTable {
    pub fn new(num_qubits: usize) -> Self {
        let capacity = 1 << num_qubits;

        Self {
            array: vec![Complex::new(0.0, 0.0); capacity],
        }
    }

    pub fn num_nonzeros(&self) -> usize {
        self.array
            .iter()
            .filter(|c| utility::is_nonzero(**c))
            .count()
    }

    pub fn get(&self, bidx: &BasisIdx64) -> Option<&Complex> {
        self.array.get(bidx.into_idx())
    }
}

impl Table for DenseStateTable {
    fn put(&mut self, bidx: BasisIdx64, weight: Complex) {
        let idx = bidx.into_idx();

        self.array[idx] += weight;
    }
}
