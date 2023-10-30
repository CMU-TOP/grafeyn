use crate::circuit::Circuit;
use crate::config::Config;
use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Compactifiable;

type State = Vec<Complex>;

impl<B: BasisIdx> Compactifiable<B> for State {
    fn compactify(self) -> Box<dyn Iterator<Item = (B, Complex)>> {
        Box::new(self.into_iter().enumerate().filter_map(|(i, v)| {
            if utility::is_nonzero(v) {
                Some((B::from_idx(i), v))
            } else {
                None
            }
        }))
    }
}

pub fn run<B: BasisIdx>(_config: &Config, _circuit: Circuit<B>) -> State {
    todo!()
}
