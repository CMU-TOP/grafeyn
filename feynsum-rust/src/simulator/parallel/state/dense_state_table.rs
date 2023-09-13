use std::sync::atomic::Ordering;

use atomic_float::AtomicF32;

use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Table;

#[derive(Debug)]
pub struct DenseStateTable {
    pub array: Vec<(AtomicF32, AtomicF32)>,
}

impl DenseStateTable {
    pub fn new(num_qubits: usize) -> Self {
        let capacity = 1 << num_qubits;
        // TODO: Check if the initialization is performance bottleneck
        Self {
            array: (0..capacity)
                .map(|_| (AtomicF32::new(0.0), AtomicF32::new(0.0)))
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
    pub fn atomic_put(&self, bidx: BasisIdx, weight: Complex) {
        // FIXME: We can use `put` method instead of `atomic_put` method if we
        // change the signature of `put` method from `&mut self` to &self
        let idx = bidx.into_idx();

        atomic_add(&self.array[idx].0, weight.re);
        atomic_add(&self.array[idx].1, weight.im);
    }
}

impl Table for DenseStateTable {
    fn put(&mut self, bidx: BasisIdx, weight: Complex) {
        let idx = bidx.into_idx();

        atomic_add(&self.array[idx].0, weight.re);
        atomic_add(&self.array[idx].1, weight.im);
    }
}

fn atomic_add(num: &AtomicF32, adder: f32) {
    let mut old = num.load(Ordering::Relaxed);
    let mut new = old + adder;
    while let Err(actual) = num.compare_exchange(old, new, Ordering::Relaxed, Ordering::Relaxed) {
        old = actual;
        new = old + adder;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atomic_put() {
        let table = DenseStateTable::new(2);
        table.atomic_put(BasisIdx::from_idx(2), Complex::new(1.0, 0.0));

        assert_eq!(table.array[2].0.load(Ordering::Relaxed), 1.0);
        assert_eq!(table.array[2].1.load(Ordering::Relaxed), 0.0);
    }
}
