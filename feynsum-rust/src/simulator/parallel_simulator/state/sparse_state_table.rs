use rayon::prelude::*;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::types::{AtomicComplex, AtomicReal, BasisIdx, Complex};
use crate::utility;

use std::sync::{atomic::AtomicU64, atomic::Ordering};

use super::SparseStateTableInsertion;

const EMPTY_KEY: BasisIdx = BasisIdx::flip_unsafe(&BasisIdx::from_u64(0), 63);

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

pub struct ConcurrentSparseStateTable {
    pub keys: Vec<AtomicU64>,
    pub weights: Vec<AtomicComplex>,
}

impl ConcurrentSparseStateTable {
    fn new_with_capacity(capacity: usize) -> Self {
        let keys: Vec<AtomicU64> = (0..capacity)
            .into_par_iter()
            .map(|_i| AtomicU64::new(BasisIdx::into_u64(EMPTY_KEY)))
            .collect();
        let weights: Vec<AtomicComplex> = (0..capacity)
            .into_par_iter()
            .map(|_i| (AtomicReal::new(0.0), AtomicReal::new(0.0)))
            .collect();
        Self { keys, weights }
    }
    pub fn new() -> Self {
        let capacity = 100;
        Self::new_with_capacity(capacity)
    }
    pub fn singleton(bidx: BasisIdx, weight: Complex) -> Self {
        let mut t = ConcurrentSparseStateTable::new();
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
                k != BasisIdx::into_u64(EMPTY_KEY) && utility::is_nonzero(self.get_value_at(i))
            })
            .count()
    }
    fn put_value_at(&self, i: usize, v: Complex) {
        self.weights[i].0.fetch_add(v.re, Ordering::SeqCst);
        self.weights[i].1.fetch_add(v.im, Ordering::SeqCst);
    }
    pub fn get_value_at(&self, i: usize) -> Complex {
        Complex::new(
            self.weights[i].0.load(Ordering::Relaxed),
            self.weights[i].1.load(Ordering::Relaxed),
        )
    }
    fn force_insert_unique(&self, x: BasisIdx, v: Complex) {
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
            if k == BasisIdx::into_u64(EMPTY_KEY)
                && self.keys[i]
                    .compare_exchange(k, y, Ordering::SeqCst, Ordering::Acquire)
                    .is_ok()
            {
                self.put_value_at(i, v); // later: optimize by using non atomic add
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
        x: BasisIdx,
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
            if k == BasisIdx::into_u64(EMPTY_KEY) {
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
    pub fn get(&self, x: &BasisIdx) -> Option<Complex> {
        let n = self.keys.len();
        let mut i: usize = calculate_hash(&x) as usize % n;
        let y = x.into_u64();
        loop {
            let k = self.keys[i].load(Ordering::Relaxed);
            if k == BasisIdx::into_u64(EMPTY_KEY) {
                return None;
            } else if k == y {
                return Some(self.get_value_at(i));
            } else {
                i += 1
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
                    new_table.force_insert_unique(BasisIdx::from_u64(k.load(Ordering::Relaxed)), w)
                }
            });
        new_table
    }
    pub fn nonzeros(&self) -> Vec<(BasisIdx, Complex)> {
        (0..self.capacity())
            .into_par_iter()
            .map(|i| {
                let bidx = BasisIdx::from_u64(self.keys[i].load(Ordering::Relaxed));
                let weight = self.get_value_at(i);
                (bidx, weight)
            })
            .filter(|(bidx, weight)| bidx != &EMPTY_KEY && utility::is_nonzero(*weight))
            .collect()
    }
}

#[derive(Debug)]
pub struct SparseStateTable {
    pub table: HashMap<BasisIdx, Complex>,
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

    pub fn put(&mut self, bidx: BasisIdx, weight: Complex) {
        self.table
            .entry(bidx)
            .and_modify(|w| *w += weight)
            .or_insert(weight);
    }

    pub fn get(&self, bidx: &BasisIdx) -> Option<Complex> {
        self.table.get(bidx).map(Clone::clone)
    }
}
