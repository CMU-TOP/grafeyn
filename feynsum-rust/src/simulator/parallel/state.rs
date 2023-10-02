use std::sync::atomic::Ordering;

use crate::types::{BasisIdx, Complex};
use crate::utility;

mod dense_state_table;
mod sparse_state_table;

pub use dense_state_table::DenseStateTable;
pub use sparse_state_table::ConcurrentSparseStateTable;
pub use sparse_state_table::SparseStateTable;

use super::super::Compactifiable;

#[derive(Debug)]
pub enum State {
    Sparse(SparseStateTable),
    Dense(DenseStateTable),
}

impl State {
    pub fn num_nonzeros(&self) -> usize {
        match self {
            State::Sparse(table) => table.num_nonzeros(),
            State::Dense(table) => table.num_nonzeros(),
        }
    }

    pub fn get(&self, bidx: &BasisIdx) -> Option<Complex> {
        match self {
            State::Sparse(table) => table.get(bidx),
            State::Dense(table) => table.get(bidx),
        }
    }
}

impl Compactifiable for State {
    fn compactify(self) -> Box<dyn Iterator<Item = (BasisIdx, Complex)>> {
        match self {
            State::Sparse(table) => Box::new(table.table.into_iter()),
            State::Dense(table) => Box::new(table.array.into_iter().enumerate().map(|(idx, v)| {
                let (re, im) = utility::unpack_complex(v.load(Ordering::Relaxed));
                (BasisIdx::from_idx(idx), Complex::new(re, im))
            })),
        }
    }
}

pub enum SparseStateTableInserion {
    Success,
    Full,
}
