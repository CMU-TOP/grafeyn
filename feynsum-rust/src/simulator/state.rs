use crate::types::{BasisIdx, Complex};

use super::table::{DenseStateTable, SparseStateTable};

#[derive(Debug)]
pub enum State {
    Sparse(SparseStateTable),
    #[allow(dead_code)]
    Dense(DenseStateTable), // TODO: use this
    #[allow(dead_code)]
    DenseKnownNonzeroSize(DenseStateTable, usize), // TODO: use this
}

impl State {
    pub fn compactify(self) -> impl Iterator<Item = (BasisIdx, Complex)> {
        match self {
            State::Sparse(table) => table.compactify(),
            _ => panic!("TODO"),
        }
    }

    pub fn num_nonzero(&self) -> usize {
        match self {
            State::Sparse(table) => table.num_nonzero(),
            _ => panic!("TODO"),
        }
    }
}
