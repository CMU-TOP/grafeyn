use rayon::prelude::*;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Table;

#[derive(Debug)]
pub struct DenseStateTable {
    pub array: Vec<AtomicU64>,
}

impl DenseStateTable {
    pub fn new(num_qubits: usize) -> Self {
        let capacity = 1 << num_qubits;
        // TODO: Check if the initialization is performance bottleneck
        Self {
            array: (0..capacity).map(|_| AtomicU64::new(0)).collect(),
        }
    }

    pub fn num_nonzeros(&self, block_size: usize) -> usize {
        self.array
            .par_chunks(block_size)
            .map(|chunk| {
                chunk
                    .iter()
                    .filter(|v| {
                        let (re, im) = utility::unpack_complex(v.load(Ordering::Relaxed));
                        utility::is_real_nonzero(re) || utility::is_real_nonzero(im)
                    })
                    .count()
            })
            .reduce(|| 0, |acc, n| acc + n)
    }
    pub fn atomic_put(&self, bidx: BasisIdx, weight: Complex) {
        // FIXME: We can use `put` method instead of `atomic_put` method if we
        // change the signature of `put` method from `&mut self` to &self
        let idx = bidx.into_idx();

        atomic_put(&self.array[idx], weight);
    }

    pub fn get(&self, bidx: &BasisIdx) -> Option<Complex> {
        self.array.get(bidx.into_idx()).map(|v| {
            let (re, im) = utility::unpack_complex(v.load(Ordering::Relaxed));
            Complex::new(re, im)
        })
    }
}

impl Table for DenseStateTable {
    fn put(&mut self, _bidx: BasisIdx, _weight: Complex) {
        unreachable!()
        // FIXME
    }
}

fn atomic_put(to: &AtomicU64, v: Complex) {
    loop {
        let old = to.load(Ordering::Relaxed);
        let (re, im) = utility::unpack_complex(old);
        let new = utility::pack_complex(re + v.re, im + v.im);

        match to.compare_exchange(old, new, Ordering::SeqCst, Ordering::Acquire) {
            Ok(_) => return,
            Err(_) => (),
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

        let (re, im) = utility::unpack_complex(a.load(Ordering::Relaxed));
        assert_eq!(Complex::new(re, im), Complex::new(1.0, 2.0));
    }
}
