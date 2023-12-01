use crate::circuit::Circuit;
use crate::config::Config;
use crate::types::BasisIdx;

use super::Compactifiable;

pub struct State {
    // TODO
}

impl<B: BasisIdx> Compactifiable<B> for State {
    fn compactify(self) -> Box<dyn Iterator<Item = (B, crate::types::Complex)>> {
        todo!()
    }
}

pub fn run<B: BasisIdx>(_config: &Config, _circuit: Circuit<B>) -> State {
    todo!()
}
