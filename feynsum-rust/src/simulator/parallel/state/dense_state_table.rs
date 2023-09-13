use std::sync::atomic::Ordering;

use atomic_float::AtomicF64;

use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Table;

#[derive(Debug)]
pub struct DenseStateTable {
    pub array: Vec<(AtomicF64, AtomicF64)>,
}

impl DenseStateTable {
    pub fn new(num_qubits: usize) -> Self {
        let capacity = 1 << num_qubits;
        // TODO: Check if the initialization is performance bottleneck
        Self {
            array: (0..capacity)
                .map(|_| (AtomicF64::new(0.0), AtomicF64::new(0.0)))
                .collect(),
        }
    }

    pub fn num_nonzeros(&self) -> usize {
        self.array
            .iter()
            .filter(|&(re, im)| {
                utility::is_real_nonzero(re.load(Ordering::Relaxed))
                    || utility::is_real_nonzero(im.load(Ordering::Relaxed))
            })
            .count()
    }
}

impl Table for DenseStateTable {
    fn put(&mut self, bidx: BasisIdx, weight: Complex) {
        let idx = bidx.into_idx();

        atomic_add(&self.array[idx].0, weight.re);
        atomic_add(&self.array[idx].1, weight.re);
    }
}

fn atomic_add(num: &AtomicF64, adder: f64) {
    let mut old = num.load(Ordering::Relaxed);
    let mut new = old + adder;
    while let Err(actual) = num.compare_exchange(old, new, Ordering::Relaxed, Ordering::Relaxed) {
        old = actual;
        new = old + adder;
    }
}
