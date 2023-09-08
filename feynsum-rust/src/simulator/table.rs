mod dense_state_table;
mod sparse_state_table;

use crate::types::{BasisIdx, Complex};

pub use dense_state_table::DenseStateTable;
pub use sparse_state_table::SparseStateTable;

pub trait Table {
    fn put(&mut self, bidx: BasisIdx, weight: Complex);
}
