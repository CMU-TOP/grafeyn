use std::sync::atomic::Ordering;

use atomic_float::AtomicF64;

use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Table;

#[derive(Debug)]
pub struct DenseStateTable {
    pub(super) array: Vec<(AtomicF64, AtomicF64)>, // NOTE: We use `pub(super)` to allow access from `impl State`, to reduce
                                                   // boilerplate and unnecessary dynamic dispatch
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

        self.array[idx].0.fetch_add(weight.re, Ordering::Relaxed);
        self.array[idx].1.fetch_add(weight.im, Ordering::Relaxed);
    }
}
