use std::alloc::{alloc, dealloc, Layout};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::types::{BasisIdx, Complex};
use crate::utility;

use std::sync::{atomic::AtomicU64, atomic::Ordering};

use super::SparseStateTableInserion;

const EMPTY_KEY: BasisIdx = BasisIdx::flip_unsafe(&BasisIdx::from_u64(0), 63);

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

pub struct ConcurrentSparseStateTable {
    pub keys: Vec<AtomicU64>,
    pub packed_weights: Vec<AtomicU64>,
    pub nonzeros: Vec<usize>,
}

unsafe impl Sync for ConcurrentSparseStateTable {}

impl<'a> ConcurrentSparseStateTable {
    pub fn new2(capacity: usize) -> Self {
        let mut keys = Vec::with_capacity(capacity);
        for _i in 0..capacity {
            keys.push(AtomicU64::new(BasisIdx::into_u64(EMPTY_KEY)));
        }
        let mut packed_weights = Vec::with_capacity(capacity);
        for _i in 0..capacity {
            packed_weights.push(AtomicU64::new(0));
        }
        assert!(keys.len() == capacity);
        assert!(packed_weights.len() == capacity);
        let nonzeros = Vec::new();
        Self {
            keys,
            packed_weights,
            nonzeros,
        }
    }
    pub fn new() -> Self {
        let capacity = 100000;
        Self::new2(capacity)
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
        self.keys
            .iter()
            .enumerate()
            .filter(|(i, k)| {
                if k.load(Ordering::Relaxed) != BasisIdx::into_u64(EMPTY_KEY) {
                    let (re, im) =
                        utility::unpack_complex(self.packed_weights[*i].load(Ordering::Relaxed));
                    utility::is_real_nonzero(re) || utility::is_real_nonzero(im)
                } else {
                    false
                }
            })
            .count()
    }
    fn put_value_at(&self, i: usize, v: Complex) {
        loop {
            let old: u64 = self.packed_weights[i].load(Ordering::Relaxed);
            let (re, im) = utility::unpack_complex(old);
            let new = utility::pack_complex(re + v.re, im + v.im);
            match self.packed_weights[i].compare_exchange(
                old,
                new,
                Ordering::SeqCst,
                Ordering::Acquire,
            ) {
                Ok(_) => return,
                Err(_) => (),
            }
        }
    }
    fn force_insert_unique(
        &self,
        x: BasisIdx,
        v: Complex
    ) {
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
            if k == BasisIdx::into_u64(EMPTY_KEY) {
                match self.keys[i].compare_exchange(k, y, Ordering::SeqCst, Ordering::Acquire) {
                    Ok(_) => {
                        self.put_value_at(i, v); // later: optimize by using non atomic add
                        break;
                    }
                    Err(_) => assert!(false),
                }
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
    ) -> SparseStateTableInserion {
        let n = self.keys.len();
        let mut i: usize = calculate_hash(&x) as usize % n;
        let y = x.into_u64();
        let mut probes: usize = 0;
        loop {
            if probes >= tolerance {
                return SparseStateTableInserion::Full;
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
        SparseStateTableInserion::Success
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
                let old: u64 = self.packed_weights[i].load(Ordering::Relaxed);
                let (re, im) = utility::unpack_complex(old);
                return Some(Complex::new(re, im));
            } else {
                i += 1
            }
        }
    }
    pub fn increase_capacity_by_factor(&self, alpha: f32) -> Self {
        let new_capacity = (alpha * self.keys.len() as f32).ceil() as usize;
        println!("oldcap {} newcap {}", self.capacity(),new_capacity);
        let new_table = Self::new2(new_capacity);
        for i in 0..self.keys.len() {
            let k = BasisIdx::from_u64(self.keys[i].load(Ordering::Relaxed));
            let (re, im) = utility::unpack_complex(self.packed_weights[i].load(Ordering::Relaxed));
            let c =  Complex::new(re, im);
            if utility::is_nonzero(c) {
                new_table.force_insert_unique(k, c)
            }
        }
        new_table
    }
    pub fn make_nonzero_shortcuts(&mut self) {
        let mut nonzeros = (0..self.keys.len())
            .filter(|&i| {
                if self.keys[i].load(Ordering::Relaxed) == BasisIdx::into_u64(EMPTY_KEY) {
                    return false;
                }
                let (re, im) =
                    utility::unpack_complex(self.packed_weights[i].load(Ordering::Relaxed));
                utility::is_real_nonzero(re) || utility::is_real_nonzero(im)
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
        self.table.get(&bidx).map(Clone::clone)
    }
}
