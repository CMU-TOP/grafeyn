use std::convert::Infallible;
use std::marker::PhantomData;

use std::sync::atomic::Ordering;

use crate::types::{BasisIdx, Complex};
use crate::utility;

mod dense_state_table;
mod sparse_state_table;

pub use dense_state_table::DenseStateTable;
pub use sparse_state_table::SparseStateTable;

use super::super::Compactifiable;

//#[derive(Debug)]
pub enum State<B: BasisIdx> {
    Sparse(SparseStateTable<B>),
    Dense(DenseStateTable),
    // Used to avoid a compiler error that says B is not used.  Refer to
    // https://github.com/rust-lang/rust/issues/23246 for more details.
    #[allow(dead_code)]
    Never(Infallible, PhantomData<B>),
}

impl<B: BasisIdx> State<B> {
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

impl<B: BasisIdx> Compactifiable<B> for State<B> {
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
