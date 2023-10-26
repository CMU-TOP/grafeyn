use std::collections::HashMap;

use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Table;

#[derive(Debug)]
pub struct SparseStateTable<B: BasisIdx> {
    pub table: HashMap<B, Complex>,
}

impl<B: BasisIdx> SparseStateTable<B> {
    pub fn singleton(bidx: B, weight: Complex) -> Self {
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

    pub fn get(&self, bidx: &B) -> Option<&Complex> {
        self.table.get(bidx)
    }
}

impl<B: BasisIdx> Table<B> for SparseStateTable<B> {
    fn put(&mut self, bidx: B, weight: Complex) {
        self.table
            .entry(bidx)
            .and_modify(|w| *w += weight)
            .or_insert(weight);
    }
}
