use crate::types::{BasisIdx, Complex};
use crate::utility;

mod dense_state_table;
mod sparse_state_table;

pub use dense_state_table::DenseStateTable;
pub use sparse_state_table::SparseStateTable;

use super::super::Compactifiable;

pub trait Table<B: BasisIdx> {
    fn put(&mut self, bidx: B, weight: Complex);
}

#[derive(Debug)]
pub enum State<B: BasisIdx> {
    Sparse(SparseStateTable<B>),
    Dense(DenseStateTable),
}

impl<B: BasisIdx> State<B> {
    pub fn num_nonzeros(&self) -> usize {
        match self {
            State::Sparse(table) => table.num_nonzeros(),
            State::Dense(table) => table.num_nonzeros(),
        }
    }

    pub fn get(&self, bidx: &B) -> Option<&Complex> {
        match self {
            State::Sparse(table) => table.get(bidx),
            State::Dense(table) => table.get(bidx),
        }
    }
}

impl<B: BasisIdx> Compactifiable<B> for State<B> {
    fn compactify(self) -> Box<dyn Iterator<Item = (B, Complex)>> {
        match self {
            State::Sparse(table) => Box::new(
                table
                    .table
                    .into_iter()
                    .filter(|(_b, c)| utility::is_nonzero(*c)),
            ),
            State::Dense(table) => {
                Box::new(table.array.into_iter().enumerate().filter_map(|(idx, c)| {
                    if utility::is_nonzero(c) {
                        Some((B::from_idx(idx), c))
                    } else {
                        None
                    }
                }))
            }
        }
    }
}
