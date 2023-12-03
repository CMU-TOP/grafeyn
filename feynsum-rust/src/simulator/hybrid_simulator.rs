use crate::circuit::Circuit;
use crate::config::Config;
use crate::futhark::{self, FutharkVector};
use crate::gate_scheduler;
use crate::profile;
use crate::types::{AtomicBasisIdx, BasisIdx, Complex};

use super::parallel_simulator::SparseStateTable;

mod state_expander;
use state_expander::ExpandResult;

pub enum State<'a, B: BasisIdx, AB: AtomicBasisIdx<B>> {
    Sparse(SparseStateTable<B, AB>),
    Dense(FutharkVector<'a>),
}

pub fn run<B: BasisIdx, AB: AtomicBasisIdx<B>>(
    config: &Config,
    circuit: Circuit<B>,
) -> Box<dyn Iterator<Item = (B, Complex)>> {
    let num_qubits = circuit.num_qubits;

    let mut gate_scheduler = gate_scheduler::create_gate_scheduler(config, &circuit);

    let mut num_gates_visited = 0;

    let futhark_context = futhark::create_context();

    let mut state = State::Sparse(SparseStateTable::<B, AB>::singleton(
        num_qubits,
        B::zeros(),
        Complex::new(1.0, 0.0),
        config.maxload,
        1,
    )); // initial state
    let mut num_nonzeros = 1;
    let mut prev_num_nonzeros = 1;

    log::info!("starting gate application loop.");

    let (duration, _) = profile!(loop {
        let these_gates = gate_scheduler.pick_next_gates();
        if these_gates.is_empty() {
            break;
        }
        assert!(these_gates.len() == 1);
        let gate = &circuit.gates[these_gates[0]];

        log::debug!("applying gate: {:?}", gate);

        let num_gates_visited_here = 1;

        let (
            duration,
            ExpandResult {
                state: new_state,
                num_nonzeros: new_num_nonzeros,
                method,
            },
        ) = profile!(state_expander::expand(
            &futhark_context,
            gate,
            config,
            num_qubits,
            num_nonzeros,
            prev_num_nonzeros,
            state,
        ));

        println!(
            "gate: {:<3} density: ????????? nonzero: ??????????? hop:  {} {} time: {:.4}s",
            // FIXME: Print density and nonzero
            num_gates_visited,
            num_gates_visited_here,
            method,
            duration.as_secs_f32(),
        );

        num_gates_visited += num_gates_visited_here;
        state = new_state;
        prev_num_nonzeros = num_nonzeros;
        num_nonzeros = new_num_nonzeros;
    });

    println!(
        "gate: {:<2} density: ????????? nonzero: ???????????\ngate app count: ???, time: {}s",
        num_gates_visited,
        duration.as_secs_f32()
    );

    match state {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{circuit::Circuit, parser, test_case, types::BasisIdx64, utility};
    use std::collections::HashMap;
    use std::sync::atomic::AtomicU64;
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

        let result = run::<BasisIdx64, AtomicU64>(&config, circuit);
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
            .filter(|(_bidx, w)| utility::is_nonzero(*w))
            .for_each(|(bidx, w)| {
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
