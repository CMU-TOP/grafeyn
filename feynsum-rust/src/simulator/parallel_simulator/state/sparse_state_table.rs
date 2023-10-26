use rayon::prelude::*;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

use crate::types::{AtomicComplex, AtomicReal, BasisIdx, BasisIdx64, Complex, Real};
use crate::utility;

use std::sync::{atomic::AtomicU64, atomic::Ordering};

use super::SparseStateTableInsertion;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

pub struct SparseStateTable<B: BasisIdx> {
    pub keys: Vec<AtomicU64>,
    pub weights: Vec<AtomicComplex>,
    _mark: PhantomData<B>,
}

impl<B: BasisIdx> SparseStateTable<B> {
    fn new_with_capacity(capacity: usize) -> Self {
        let keys: Vec<AtomicU64> = (0..capacity)
            .into_par_iter()
            .map(|_i| AtomicU64::new(B::empty_key().into_u64()))
            .collect();
        let weights: Vec<AtomicComplex> = (0..capacity)
            .into_par_iter()
            .map(|_i| (AtomicReal::new(0.0), AtomicReal::new(0.0)))
            .collect();
        Self {
            keys,
            weights,
            _mark: Default::default(),
        }
    }
    pub fn new(maxload: Real, expected: i64) -> Self {
        let capacity = (1.1 * (1.0 / maxload) * (expected as Real)).ceil() as usize;
        Self::new_with_capacity(capacity)
    }
    pub fn singleton(bidx: B, weight: Complex, maxload: Real, expected: i64) -> Self {
        let t = SparseStateTable::new(maxload, expected);
        t.force_insert_unique(bidx, weight);
        t
    }
    pub fn capacity(&self) -> usize {
        self.keys.len()
    }
    pub fn num_nonzeros(&self) -> usize {
        self.keys
            .iter()
            .enumerate()
            .filter(|&(i, k)| {
                let k = k.load(Ordering::Relaxed);
                k != B::empty_key().into_u64() && utility::is_nonzero(self.get_value_at(i))
            })
            .count()
    }
    fn put_value_at(&self, i: usize, v: Complex) {
        self.weights[i].0.fetch_add(v.re, Ordering::SeqCst);
        self.weights[i].1.fetch_add(v.im, Ordering::SeqCst);
    }
    fn put_value_at_nonatomic(&self, i: usize, v: Complex) {
        let v0 = self.get_value_at(i);
        self.weights[i].0.store(v0.re + v.re, Ordering::Relaxed);
        self.weights[i].1.store(v0.im + v.im, Ordering::Relaxed);
    }
    pub fn get_value_at(&self, i: usize) -> Complex {
        Complex::new(
            self.weights[i].0.load(Ordering::Relaxed),
            self.weights[i].1.load(Ordering::Relaxed),
        )
    }
    fn force_insert_unique(&self, x: impl BasisIdx, v: Complex) {
        let n = self.keys.len();
        let start = calculate_hash(&x) as usize % n;
        let mut i: usize = start;
        let y = x.into_u64();
        loop {
            if i >= n {
                i = 0;
                continue;
            }
            let k = self.keys[i].load(Ordering::Relaxed);
            if k == B::empty_key().into_u64()
                && self.keys[i]
                    .compare_exchange(k, y, Ordering::SeqCst, Ordering::Acquire)
                    .is_ok()
            {
                self.put_value_at_nonatomic(i, v);
                break;
            }
            assert!(k != y); // duplicate key
            i += 1;
            assert!(i != start);
        }
    }
    pub fn insert_add_weights_limit_probes(
        &self,
        tolerance: usize,
        x: B,
        v: Complex,
    ) -> SparseStateTableInsertion {
        let n = self.keys.len();
        let mut i: usize = calculate_hash(&x) as usize % n;
        let y = x.into_u64();
        let mut probes: usize = 0;
        loop {
            if probes >= tolerance {
                return SparseStateTableInsertion::Full;
            }
            if i >= n {
                i = 0;
                continue;
            }
            let k = self.keys[i].load(Ordering::Relaxed);
            if k == B::empty_key().into_u64() {
                match self.keys[i].compare_exchange(k, y, Ordering::SeqCst, Ordering::Acquire) {
                    Ok(_) => {
                        self.put_value_at(i, v);
                        break;
                    }
                    Err(_) => continue,
                }
            } else if k == y {
                self.put_value_at(i, v);
                break;
            } else {
                i += 1;
                probes += 1;
            }
        }
        SparseStateTableInsertion::Success
    }
    pub fn get(&self, x: &B) -> Option<Complex> {
        let n = self.keys.len();
        let mut i: usize = calculate_hash(&x) as usize % n;
        let y = x.into_u64();
        loop {
            let k = self.keys[i].load(Ordering::Relaxed);
            if k == B::empty_key().into_u64() {
                return None;
            } else if k == y {
                return Some(self.get_value_at(i));
            } else {
                i = (i + 1) % n
            }
        }
    }
    pub fn increase_capacity_by_factor(&self, alpha: f32) -> Self {
        let new_capacity = (alpha * self.keys.len() as f32).ceil() as usize;
        let new_table = Self::new_with_capacity(new_capacity);
        self.keys
            .par_iter()
            .zip(self.weights.par_iter())
            .for_each(|(k, pw)| {
                let w = Complex::new(pw.0.load(Ordering::Relaxed), pw.1.load(Ordering::Relaxed));
                if utility::is_nonzero(w) {
                    new_table
                        .force_insert_unique(BasisIdx64::from_u64(k.load(Ordering::Relaxed)), w)
                }
            });
        new_table
    }
    pub fn nonzeros(&self) -> Vec<(B, Complex)> {
        (0..self.capacity())
            .into_par_iter()
            .map(|i| {
                let bidx = B::from_u64(self.keys[i].load(Ordering::Relaxed));
                let weight = self.get_value_at(i);
                (bidx, weight)
            })
            .filter(|(bidx, weight)| bidx != &B::empty_key() && utility::is_nonzero(*weight))
            .collect()
    }
}
