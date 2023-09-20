use std::collections::HashMap;
use std::alloc::{alloc, dealloc, Layout};

use crate::types::{BasisIdx, Real, Complex};
use crate::utility;

use std::sync::{atomic::Ordering, atomic::AtomicU64};

pub struct HeapArray<T> {
    ptr: *mut T,
    len: usize
}

impl<T> HeapArray<T> {
    pub fn new(len: usize) -> Self {
        let ptr = unsafe {
            let layout = Layout::from_size_align_unchecked(len, std::mem::size_of::<T>());
            alloc(layout) as *mut T
        };
        Self { ptr, len }
    }
    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx < self.len {
            unsafe { Some(&*(self.ptr.add(idx))) }
        } else {
            None
        }
    }
    pub fn get_mut(&self, idx: usize) -> Option<&mut T> {
        if idx < self.len {
            unsafe { Some(&mut *(self.ptr.add(idx))) }
        } else {
            None
        }
    }
    pub fn len(&self) -> usize {
        self.len
    }
}

impl<T> Drop for HeapArray<T> {
    fn drop(&mut self) {
        unsafe {
            dealloc(
                self.ptr as *mut u8,
                Layout::from_size_align_unchecked(self.len, std::mem::size_of::<T>()),
            )
        };
    }
}

impl<T> std::ops::Index<usize> for HeapArray<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}
impl<T> std::ops::IndexMut<usize> for HeapArray<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

pub struct ConcurrentSparseTable {
    keys : HeapArray<AtomicU64>,
    packedWeights : HeapArray<AtomicU64>
}

const capacity: usize = 123;
const emptykey: BasisIdx = BasisIdx::flip_unsafe(&BasisIdx::from_u64(0), 63);

impl ConcurrentSparseTable {
    pub fn new() -> Self {
	let mut keys = HeapArray::<AtomicU64>::new(capacity);
	for i in 0..capacity {
	    keys[i] = AtomicU64::new(0);
	};
	let mut packedWeights = HeapArray::<AtomicU64>::new(2 * capacity);
	for i in 0..(2 * capacity) {
	    packedWeights[i] = AtomicU64::new(0);
	};
        Self { keys, packedWeights }
    }
    fn atomicAdd(&self, i: usize, v: Complex) {
	loop {
	    let old: u64 = self.packedWeights[i].load(Ordering::Relaxed);
	    let (re, im) = utility::unpack_complex(old);
	    let new = utility::pack_complex(re + v.re, im + v.im);
	    match self.packedWeights[i].compare_exchange(old, new,
						      Ordering::SeqCst,
						      Ordering::Acquire) {
		    Ok(_) => return,
		    Err(_) => ()
	    }
	}
    }
    pub fn insertAddWeightsLimitProbes(&self, tolerance: usize, x: BasisIdx, v: Complex) {
	let mut probes: usize = 0;
	let mut i: usize = 0;
	let new = x.into_u64();
	loop {
	    if probes >= tolerance {
		return
	    }
	    let old = self.keys[i].load(Ordering::Relaxed);
	    if old == BasisIdx::into_u64(emptykey) {
		match self.keys[i].compare_exchange(old, new,
						    Ordering::SeqCst,
						    Ordering::Acquire) {
		    Ok(_) => break,
		    Err(_) => ()
		}
	    }
	    i = i + 1;
	    probes = probes + 1;
	};
	self.atomicAdd(2 * i, v)
    }
}

use super::Table;

#[derive(Debug)]
pub struct SparseStateTable {
    pub table: HashMap<BasisIdx, Complex>,
}

impl SparseStateTable {
    pub fn singleton(bidx: BasisIdx, weight: Complex) -> Self {
        Self {
	    table: HashMap::from([(bidx, weight)])
        }
    }

    pub fn new() -> Self {
        Self {
	    table: HashMap::new()
        }
    }

    pub fn num_nonzeros(&self) -> usize {
	self.table
            .iter()
            .filter(|(_, w)| utility::is_nonzero(**w))
            .count()

    }


    /*
    #[cfg(test)]
    pub fn get(&self, bidx: &BasisIdx) -> Option<&Complex> {
	self.table.get(&bidx)
    } */

    pub fn get(&self, bidx: &BasisIdx) -> Option<Complex> {
        self.table.get(&bidx).map(Clone::clone)
    }
}

impl Table for SparseStateTable {
    fn put(&mut self, bidx: BasisIdx, weight: Complex) {
	self.table
            .entry(bidx)
            .and_modify(|w| *w += weight)
            .or_insert(weight);
    }
}
