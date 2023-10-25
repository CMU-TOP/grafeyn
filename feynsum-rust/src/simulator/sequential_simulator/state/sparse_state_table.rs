use std::collections::HashMap;

use crate::types::{BasisIdx64, Complex};
use crate::utility;

use super::Table;

#[derive(Debug)]
pub struct SparseStateTable {
    pub table: HashMap<BasisIdx64, Complex>,
}

impl SparseStateTable {
    pub fn singleton(bidx: BasisIdx64, weight: Complex) -> Self {
        Self {
            table: HashMap::from([(bidx, weight)]),
        }
    }

    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn num_nonzeros(&self) -> usize {
        self.table
            .iter()
            .filter(|(_, w)| utility::is_nonzero(**w))
            .count()
    }

    pub fn get(&self, bidx: &BasisIdx64) -> Option<&Complex> {
        self.table.get(bidx)
    }
}

impl Table for SparseStateTable {
    fn put(&mut self, bidx: BasisIdx64, weight: Complex) {
        self.table
            .entry(bidx)
            .and_modify(|w| *w += weight)
            .or_insert(weight);
    }
}
