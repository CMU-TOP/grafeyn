use crate::types::{BasisIdx, Complex};

mod dense_state_table;
mod sparse_state_table;

pub use dense_state_table::DenseStateTable;
pub use sparse_state_table::SparseStateTable;

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
    pub fn compactify(self) -> impl Iterator<Item = (BasisIdx, Complex)> {
        match self {
            State::Sparse(table) => table.table.into_iter(),
            State::Dense(table) => table.table.into_iter(),
            _ => panic!("TODO"),
        }
    }

    pub fn num_nonzeros(&self) -> usize {
        match self {
            State::Sparse(table) => table.num_nonzeros(),
            State::Dense(table) => table.num_nonzeros(),
            _ => panic!("TODO"),
        }
    }
}
