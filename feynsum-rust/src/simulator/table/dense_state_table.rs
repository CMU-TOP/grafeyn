use std::collections::HashMap;
use std::hash::{BuildHasher, Hasher};

use crate::types::{BasisIdx, Complex};

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
    table: HashMap<BasisIdx, Complex, BuildTrivialHashser>,
}

impl DenseStateTable {
    pub fn new(num_qubits: usize) -> Self {
        let capacity = 1 << num_qubits;
        Self {
            table: HashMap::with_capacity_and_hasher(capacity, BuildTrivialHashser),
        }
    }
}
