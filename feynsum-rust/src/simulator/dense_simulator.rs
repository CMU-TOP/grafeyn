use crate::circuit::Circuit;
use crate::config::Config;
use crate::futhark;
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

pub fn run<B: BasisIdx>(_config: &Config, circuit: Circuit<B>) -> State {
    let unitary = vec![
        vec![Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
        vec![Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)],
    ];
    let init_state = vec![vec![Complex::new(1.0, 0.0)], vec![Complex::new(0.0, 0.0)]];

    let result = futhark::matmul(unitary, init_state);
    let result_dim = result.len();

    let linearized: Vec<Complex> = result.into_iter().flatten().collect();
    assert!(result_dim == linearized.len());
    linearized
}
