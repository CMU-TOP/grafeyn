use std::sync::atomic::Ordering;

use crate::types::{BasisIdx, Complex};
use crate::utility;

mod dense_state_table;
mod sparse_state_table;

pub use dense_state_table::DenseStateTable;
pub use sparse_state_table::SparseStateTable;

use super::super::Compactifiable;

pub trait Table {
    fn put(&mut self, bidx: BasisIdx, weight: Complex);
}

#[derive(Debug)]
pub enum State {
    Sparse(SparseStateTable),
    #[allow(dead_code)]
    Dense(DenseStateTable),
    #[allow(dead_code)]
    DenseKnownNonzeroSize(DenseStateTable, usize), // TODO: use this
}

impl State {
    pub fn num_nonzeros(&self) -> usize {
        match self {
            State::Sparse(table) => table.num_nonzeros(),
            State::Dense(table) => table.num_nonzeros(),
            _ => panic!("TODO"),
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
            _ => panic!("TODO"),
        }
    }
}
