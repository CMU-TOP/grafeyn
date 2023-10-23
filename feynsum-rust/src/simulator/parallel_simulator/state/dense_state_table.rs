use rayon::prelude::*;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::types::{BasisIdx, Complex};
use crate::utility;

#[derive(Debug)]
pub struct DenseStateTable {
    pub array: Vec<AtomicU64>,
}

impl DenseStateTable {
    pub fn new(num_qubits: usize) -> Self {
        let capacity = 1 << num_qubits;
        let mut array = Vec::with_capacity(capacity);
        (0..capacity)
            .into_par_iter()
            .map(|_| AtomicU64::new(0))
            .collect_into_vec(&mut array);
        Self { array }
    }

    pub fn capacity(&self) -> usize {
        self.array.len()
    }
    pub fn num_nonzeros(&self) -> usize {
        self.array
            .par_iter()
            .filter(|v| {
                let weight = utility::unpack_complex(v.load(Ordering::Relaxed));
                utility::is_nonzero(weight)
            })
            .count()
    }
    pub fn atomic_put(&self, bidx: BasisIdx, weight: Complex) {
        // FIXME: We can use `put` method instead of `atomic_put` method if we
        // change the signature of `put` method from `&mut self` to &self
        let idx = bidx.into_idx();

        atomic_put(&self.array[idx], weight);
    }

    pub fn get(&self, bidx: &BasisIdx) -> Option<Complex> {
        self.array
            .get(bidx.into_idx())
            .map(|v| utility::unpack_complex(v.load(Ordering::Relaxed)))
    }
}

fn atomic_put(to: &AtomicU64, v: Complex) {
    loop {
        let old = to.load(Ordering::Relaxed);
        let weight = utility::unpack_complex(old);
        let new = utility::pack_complex(Complex::new(weight.re + v.re, weight.im + v.im));

        if to
            .compare_exchange(old, new, Ordering::SeqCst, Ordering::Acquire)
            .is_ok()
        {
            return;
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_atomic_put() {
        let a = AtomicU64::new(0);

        atomic_put(&a, Complex::new(1.0, 2.0));

        let weight = utility::unpack_complex(a.load(Ordering::Relaxed));
        assert_eq!(weight, Complex::new(1.0, 2.0));
    }
}
