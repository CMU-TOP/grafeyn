use std::collections::HashMap;
use std::hash::{BuildHasher, Hasher};

use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Table;

pub struct TrivialHashser(u64);

impl Hasher for TrivialHashser {
    fn write(&mut self, _bytes: &[u8]) {
        unreachable!()
    }
    fn write_u64(&mut self, n: u64) {
        self.0 = n;
    }
    fn finish(&self) -> u64 {
        self.0
    }
}

pub struct BuildTrivialHashser;

impl BuildHasher for BuildTrivialHashser {
    type Hasher = TrivialHashser;
    fn build_hasher(&self) -> Self::Hasher {
        TrivialHashser(0)
    }
}

#[derive(Debug)]
pub struct DenseStateTable {
    pub(super) table: HashMap<BasisIdx, Complex, BuildTrivialHashser>,
    // NOTE: We use `pub(super)` to allow access from `impl State`, to reduce
    // boilerplate and unnecessary dynamic dispatch
}

impl DenseStateTable {
    pub fn new(num_qubits: usize) -> Self {
        let capacity = 1 << num_qubits;
        Self {
            table: HashMap::with_capacity_and_hasher(capacity, BuildTrivialHashser),
        }
    }

    pub fn num_nonzeros(&self) -> usize {
        self.table
            .iter()
            .filter(|(_, w)| utility::is_nonzero(**w))
            .count()
    }
}

impl Table for DenseStateTable {
    fn put(&mut self, bidx: BasisIdx, weight: Complex) {
        self.table
            .entry(bidx)
            .and_modify(|w| *w += weight)
            .or_insert(weight);
    }
}
