use crate::circuit::Circuit;
use crate::config::Config;
use crate::futhark::FutharkVector;
use crate::types::{AtomicBasisIdx, BasisIdx, Complex};

use super::Compactifiable;

use super::parallel_simulator::SparseStateTable;

pub enum State<'a, B: BasisIdx, AB: AtomicBasisIdx<B>> {
    Sparse(SparseStateTable<B, AB>),
    Dense(FutharkVector<'a>),
}

impl<'a, B: BasisIdx, AB: AtomicBasisIdx<B>> Compactifiable<B> for State<'a, B, AB> {
    fn compactify(self) -> Box<dyn Iterator<Item = (B, Complex)>> {
        match self {
            State::Sparse(table) => Box::new(table.nonzeros().into_iter()),
            State::Dense(futhark_vec) => Box::new(
                futhark_vec
                    .into_vec()
                    .into_iter()
                    .enumerate()
                    .map(|(idx, weight)| (B::from_idx(idx), weight)),
            ) as Box<dyn Iterator<Item = (B, Complex)>>,
        }
    }
}

pub fn run<B: BasisIdx, AB: AtomicBasisIdx<B>>(
    _config: &Config,
    _circuit: Circuit<B>,
) -> State<B, AB> {
    todo!()
}
