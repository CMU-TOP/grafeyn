mod unitary;

use log;

use crate::circuit::Circuit;
use crate::config::Config;
use crate::futhark::{self, FutharkVector};
use crate::gate_scheduler;
use crate::profile;
use crate::types::{BasisIdx, Complex};
use crate::utility;

use super::Compactifiable;
use unitary::Unitary;
pub use unitary::UnitaryMatrix;

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

pub fn run<B: BasisIdx>(config: &Config, circuit: Circuit<B>) -> State {
    let dim = 1 << circuit.num_qubits;

    let mut gate_scheduler = gate_scheduler::create_gate_scheduler(config, &circuit);

    let mut num_gates_visited = 0;

    let futhark_context = futhark::create_context();

    let mut state = FutharkVector::new(
        &futhark_context,
        (0..dim)
            .map(|i| {
                if i == 0 {
                    Complex::new(1.0, 0.0)
                } else {
                    Complex::new(0.0, 0.0)
                }
            })
            .collect::<Vec<Complex>>(),
    );

    let (duration, _) = profile!(loop {
        let these_gates = gate_scheduler.pick_next_gates();
        if these_gates.is_empty() {
            break;
        }
        assert!(these_gates.len() == 1);
        let gate = &circuit.gates[these_gates[0]];

        log::debug!("applying gate: {:?}", gate);

        let num_gates_visited_here = 1;

        let (duration, new_state) =
            profile!(futhark::apply_vec(&futhark_context, state, gate.unitary()));

        println!(
            "gate: {:<3} density: ????????? nonzero: ??????????? hop:  {} dense(gpu) time: {:.4}s",
            num_gates_visited,
            num_gates_visited_here,
            duration.as_secs_f32(),
        );

        state = new_state;
        num_gates_visited += num_gates_visited_here;
    });

    let result = state.into_vec();
    let num_nonzeros = result.iter().filter(|&&v| utility::is_nonzero(v)).count();

    println!(
        "gate: {:<2} density: ????????? nonzero: {:>10}\n time: {}s",
        num_gates_visited,
        num_nonzeros,
        duration.as_secs_f32()
    );
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
        let config = Config {
            disable_gate_fusion: true,
            ..Config::default()
        };
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
                    (w - expected_w).norm() < 1e-4, // FIXME: improve precision
                    "expected: {}, actual: {}",
                    expected_w,
                    w
                );
            });
    }
}
