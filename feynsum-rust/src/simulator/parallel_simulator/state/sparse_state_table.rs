use rayon::prelude::*;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use crate::types::{AtomicBasisIdx, AtomicComplex, AtomicReal, BasisIdx, Complex, Real};
use crate::utility;

use std::sync::atomic::Ordering;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

pub struct SparseStateTable<B: BasisIdx, AB: AtomicBasisIdx<B>> {
    pub keys: Vec<AB>,
    pub weights: Vec<AtomicComplex>,
    num_qubits: usize,
    empty_key: B,
}

impl<B: BasisIdx, AB: AtomicBasisIdx<B>> SparseStateTable<B, AB> {
    fn new_with_capacity(num_qubits: usize, capacity: usize) -> Self {
        let keys: Vec<AB> = (0..capacity)
            .into_par_iter()
            .map(|_i| AB::empty_key(num_qubits))
            .collect();
        let weights: Vec<AtomicComplex> = (0..capacity)
            .into_par_iter()
            .map(|_i| (AtomicReal::new(0.0), AtomicReal::new(0.0)))
            .collect();
        Self {
            keys,
            weights,
            num_qubits,
            empty_key: B::empty_key(num_qubits),
        }
    }
    pub fn new(num_qubits: usize, maxload: Real, expected: i64) -> Self {
        let capacity = (1.1 * (1.0 / maxload) * (expected as Real)).ceil() as usize;
        Self::new_with_capacity(num_qubits, capacity)
    }
    pub fn singleton(
        num_qubits: usize,
        bidx: B,
        weight: Complex,
        maxload: Real,
        expected: i64,
    ) -> Self {
        let t = SparseStateTable::new(num_qubits, maxload, expected);
        t.force_insert_unique(bidx, weight);
        println!("singleton nonzeros: {}", t.num_nonzeros());
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
                let k = k.load();
                k != self.empty_key && utility::is_nonzero(self.get_value_at(i))
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
    fn force_insert_unique(&self, x: B, v: Complex) {
        let n = self.keys.len();
        let start = calculate_hash(&x) as usize % n;
        let mut i: usize = start;
        let y = x;
        loop {
            if i >= n {
                i = 0;
                continue;
            }
            let k = self.keys[i].load();
            if k == self.empty_key && self.keys[i].compare_exchange(k.clone(), y.clone()).is_ok() {
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
    ) -> Result<(), ()> {
        let n = self.keys.len();
        let mut i: usize = calculate_hash(&x) as usize % n;
        let y = x;
        let mut probes: usize = 0;
        loop {
            if probes >= tolerance {
                return Err(());
            }
            if i >= n {
                i = 0;
                continue;
            }
            let k = self.keys[i].load();
            if k == self.empty_key {
                match self.keys[i].compare_exchange(k, y.clone()) {
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
        Ok(())
    }
    pub fn get(&self, x: &B) -> Option<Complex> {
        let n = self.keys.len();
        let mut i: usize = calculate_hash(&x) as usize % n;
        let y = x;
        loop {
            let k = self.keys[i].load();
            if k == self.empty_key {
                return None;
            } else if k == *y {
                return Some(self.get_value_at(i));
            } else {
                i = (i + 1) % n
            }
        }
    }
    pub fn increase_capacity_by_factor(&self, alpha: f32) -> Self {
        let new_capacity = (alpha * self.keys.len() as f32).ceil() as usize;
        let new_table = Self::new_with_capacity(self.num_qubits, new_capacity);
        self.keys
            .par_iter()
            .zip(self.weights.par_iter())
            .for_each(|(k, pw)| {
                let w = Complex::new(pw.0.load(Ordering::Relaxed), pw.1.load(Ordering::Relaxed));
                if utility::is_nonzero(w) {
                    new_table.force_insert_unique(k.load(), w)
                }
            });
        new_table
    }
    pub fn try_put(&self, bidx: B, weight: Complex, maxload: Real) -> Result<(), ()> {
        let n = self.capacity();
        let probably_longest_probe =
            ((n as Real).log2() / (maxload - 1.0 - maxload.log2())).ceil() as usize;
        let tolerance = std::cmp::min(4 * std::cmp::max(10, probably_longest_probe), n);
        self.insert_add_weights_limit_probes(tolerance, bidx, weight)
    }
    pub fn nonzeros(&self) -> Vec<(B, Complex)> {
        (0..self.capacity())
            .into_par_iter()
            .map(|i| {
                let bidx = self.keys[i].load();
                let weight = self.get_value_at(i);
                (bidx, weight)
            })
            .filter(|(bidx, weight)| bidx != &self.empty_key && utility::is_nonzero(*weight))
            .collect()
    }
}
