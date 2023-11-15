mod unitary;

use log;

use crate::circuit::{Circuit, Gate};
use crate::config::Config;
use crate::futhark;
use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Compactifiable;
use unitary::{Unitary, UnitaryMatrix};

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
    let dim = 1 << circuit.num_qubits;

    let init_state = (0..dim)
        .into_iter()
        .map(|i| {
            if i == 0 {
                Complex::new(1.0, 0.0)
            } else {
                Complex::new(0.0, 0.0)
            }
        })
        .collect::<Vec<Complex>>();

    let result = circuit
        .gates
        .into_iter()
        .fold(init_state, |acc, gate: Gate<B>| -> Vec<Complex> {
            log::debug!("applying gate: {:?}", gate);
            let UnitaryMatrix { mat, qubit_indices } = gate.unitary();
            futhark::apply_vec(acc, mat, qubit_indices)
        });

    assert!(result.len() == dim);
    result
}
