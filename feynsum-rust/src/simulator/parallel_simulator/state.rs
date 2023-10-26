use std::convert::Infallible;
use std::marker::PhantomData;

use std::sync::atomic::Ordering;

use crate::types::{AtomicBasisIdx, BasisIdx, Complex};
use crate::utility;

mod dense_state_table;
mod sparse_state_table;

pub use dense_state_table::DenseStateTable;
pub use sparse_state_table::SparseStateTable;

use super::super::Compactifiable;

//#[derive(Debug)]
pub enum State<B: BasisIdx, AB: AtomicBasisIdx<B>> {
    Sparse(SparseStateTable<B, AB>),
    Dense(DenseStateTable),
    // Used to avoid a compiler error that says B is not used.  Refer to
    // https://github.com/rust-lang/rust/issues/23246 for more details.
    #[allow(dead_code)]
    Never(Infallible, PhantomData<B>),
}

impl<B: BasisIdx, AB: AtomicBasisIdx<B>> State<B, AB> {
    pub fn num_nonzeros(&self) -> usize {
        match self {
            State::Sparse(table) => table.num_nonzeros(),
            State::Dense(table) => table.num_nonzeros(),
            State::Never(_, _) => unreachable!(),
        }
    }

    pub fn get(&self, bidx: &B) -> Option<Complex> {
        match self {
            State::Sparse(table) => table.get(bidx),
            State::Dense(table) => table.get(bidx),
            State::Never(_, _) => unreachable!(),
        }
    }
}

impl<B: BasisIdx, AB: AtomicBasisIdx<B>> Compactifiable<B> for State<B, AB> {
    fn compactify(self) -> Box<dyn Iterator<Item = (B, Complex)>> {
        match self {
            State::Sparse(table) => Box::new(table.nonzeros().into_iter()),
            State::Dense(table) => Box::new(table.array.into_iter().enumerate().map(|(idx, v)| {
                let weight = utility::unpack_complex(v.load(Ordering::Relaxed));
                (B::from_idx(idx), weight)
            })),
            State::Never(_, _) => unreachable!(),
        }
    }
}

pub enum SparseStateTableInsertion {
    Success,
    Full,
}
