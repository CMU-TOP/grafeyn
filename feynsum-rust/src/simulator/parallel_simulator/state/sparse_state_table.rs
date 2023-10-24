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
    pub nonzeros: Vec<usize>, // indices of keys with nonzero weights
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
        let nonzeros = Vec::new();
        Self {
            keys,
            weights,
            nonzeros,
        }
    }
    pub fn new() -> Self {
        let capacity = 100;
        Self::new_with_capacity(capacity)
    }
    pub fn singleton(bidx: BasisIdx, weight: Complex) -> Self {
        let mut t = ConcurrentSparseStateTable::new();
        t.force_insert_unique(bidx, weight);
        t.make_nonzero_shortcuts();
        t
    }
    pub fn capacity(&self) -> usize {
        self.keys.len()
    }
    pub fn num_nonzeros(&self) -> usize {
        self.nonzeros.len()
    }
    fn put_value_at(&self, i: usize, v: Complex) {
        self.weights[i].0.fetch_add(v.re, Ordering::SeqCst);
        self.weights[i].1.fetch_add(v.im, Ordering::SeqCst);
    }
    pub fn get_value_at(weights: &[AtomicComplex], i: usize) -> Complex {
        Complex::new(
            weights[i].0.load(Ordering::Relaxed),
            weights[i].1.load(Ordering::Relaxed),
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
                return Some(ConcurrentSparseStateTable::get_value_at(&self.weights, i));
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
    pub fn make_nonzero_shortcuts(&mut self) {
        let mut nonzeros = (0..self.keys.len())
            .into_par_iter()
            .filter(|&i| {
                if self.keys[i].load(Ordering::Relaxed) == BasisIdx::into_u64(EMPTY_KEY) {
                    return false;
                }
                utility::is_nonzero(ConcurrentSparseStateTable::get_value_at(&self.weights, i))
            })
            .collect();
        std::mem::swap(&mut self.nonzeros, &mut nonzeros);
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
