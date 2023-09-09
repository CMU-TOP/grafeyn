use std::collections::HashMap;

use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Table;

#[derive(Debug)]
pub struct SparseStateTable {
    pub(super) table: HashMap<BasisIdx, Complex>,
    // NOTE: We use `pub(super)` to allow access from `impl State`, to reduce
    // boilerplate and unnecessary dynamic dispatch
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

    pub fn num_nonzeros(&self) -> usize {
        self.table
            .iter()
            .filter(|(_, w)| utility::is_nonzero(**w))
            .count()
    }
}

impl Table for SparseStateTable {
    fn put(&mut self, bidx: BasisIdx, weight: Complex) {
        self.table
            .entry(bidx)
            .and_modify(|w| *w += weight)
            .or_insert(weight);
    }
}