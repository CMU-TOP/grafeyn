use std::collections::HashMap;

use crate::types::{BasisIdx, Complex};

#[derive(Debug)]
pub struct SparseStateTable {
    table: HashMap<BasisIdx, Complex>,
}

impl SparseStateTable {
    pub fn singleton(bidx: BasisIdx, weight: Complex) -> Self {
        Self {
            table: HashMap::from([(bidx, weight)]),
        }
    }

    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn compactify(self) -> impl Iterator<Item = (BasisIdx, Complex)> {
        self.table.into_iter()
    }

    pub fn put(&mut self, bidx: BasisIdx, weight: Complex) {
        self.table
            .entry(bidx)
            .and_modify(|w| *w += weight)
            .or_insert(weight);
    }

    pub fn size(&self) -> usize {
        self.table.len()
    }
}

#[derive(Debug)]
pub struct DenseStateTable {}
