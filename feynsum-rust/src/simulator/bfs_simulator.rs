use log::{debug, error, info};

use crate::circuit::Circuit;
use crate::config::Config;
use crate::profile;
use crate::types::basis_idx::MAX_QUBITS;
use crate::types::{BasisIdx, Complex};

use super::state::State;
use super::state_expander::{self, ExpandResult};
use super::table::SparseStateTable;
use super::SimulatorError;

pub fn run(config: &Config, circuit: Circuit) -> Result<State, SimulatorError> {
    let num_gates = circuit.num_gates();
    let num_qubits = circuit.num_qubits;

    if num_qubits > MAX_QUBITS {
        error!("Too many qubits: {}", num_qubits);
        return Err(SimulatorError::TooManyQubits);
    }

    let mut num_gates_visited = 0;
    let mut state = State::Sparse(SparseStateTable::singleton(
        BasisIdx::zeros(num_qubits),
        Complex::new(1.0, 0.0),
    )); // initial state
    let mut num_nonzero = 1;
    let mut num_gate_apps = 0;

    let gate_touches = circuit.gates.iter().map(|gate| &gate.touches).collect();
    let gate_is_branching = circuit
        .gates
        .iter()
        .map(|gate| gate.is_branching())
        .collect();

    let mut gate_scheduler =
        config.create_gate_scheduler(num_gates, num_qubits, gate_touches, gate_is_branching);

    info!("starting gate application loop.");

    let (duration, _) = profile!(loop {
        let these_gates = gate_scheduler
            .pick_next_gates()
            .into_iter()
            .map(|idx| &circuit.gates[idx])
            .collect::<Vec<_>>();

        debug!("applying gates: {:?}", these_gates);

        if these_gates.is_empty() {
            break;
        }

        let num_gates_visited_here = these_gates.len();

        let (
            duration,
            ExpandResult {
                state: new_state,
                num_nonzero: new_num_nonzero,
                num_gate_apps: num_gate_apps_here,
            },
        ) = profile!(state_expander::expand(these_gates, num_qubits, state)?);

        let density = {
            let max_num_states = 1 << num_qubits;
            num_nonzero as f64 / max_num_states as f64
        };

        println!(
            "gate: {:<2} density: {:.8} nonzero: {:>10} hop: {:<2}  time: {:.4}s",
            num_gates_visited,
            density,
            num_nonzero,
            num_gates_visited_here,
            duration.as_secs_f64(),
        );

        num_gates_visited += num_gates_visited_here;
        num_gate_apps += num_gate_apps_here;
        num_nonzero = new_num_nonzero;
        state = new_state;
    });

    let final_density = {
        let max_num_states = 1 << num_qubits;
        num_nonzero as f64 / max_num_states as f64
    };

    println!(
        "gate: {:<2} density: {:.8} nonzero: {:>10}\ngate app count: {}, time: {}s",
        num_gates_visited,
        final_density,
        num_nonzero,
        num_gate_apps,
        duration.as_secs_f64()
    );

    assert!(num_gates_visited >= num_gates);
    Ok(state)
}
