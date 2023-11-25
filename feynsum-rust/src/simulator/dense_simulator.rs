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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{circuit::Circuit, parser, test_case, types::BasisIdx64};
    use std::collections::HashMap;
    use std::{fs, str::FromStr};

    #[test]
    fn test_run() {
        let config = Config::default();
        let source = fs::read_to_string(test_case!("adder_n10.qasm")).unwrap();
        let program = parser::parse_program(&source).unwrap();
        let circuit = Circuit::<BasisIdx64>::new(program).unwrap();

        let result = run(&config, circuit);
        let expected = fs::read_to_string(test_case!("adder_n10_expected.txt"))
            .unwrap()
            .lines()
            .map(|line| {
                let (bidx, w) = line.split_once(" ").unwrap();
                let c = Complex::from_str(w).unwrap();
                let bidx = BasisIdx64::from_str(bidx).unwrap();

                (bidx, c)
            })
            .collect::<HashMap<_, _>>();
        result
            .into_iter()
            .enumerate()
            .filter(|(_idx, w)| utility::is_nonzero(*w))
            .for_each(|(idx, w)| {
                let bidx = BasisIdx64::from_idx(idx);
                let expected_w = expected.get(&bidx).expect("expected must be nonzero");
                assert!(
                    (w - expected_w).norm() < 1e-6,
                    "expected: {}, actual: {}",
                    expected_w,
                    w
                );
            });
    }
}
