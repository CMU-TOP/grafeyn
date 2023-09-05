use log::{error, info};

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
    let depth = circuit.depth();
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

    let mut gate_scheduler = config.create_gate_scheduler(depth);
    info!("gate scheduler initialized. starting gate application loop.");

    loop {
        let these_gates = gate_scheduler
            .pick_next_gates()
            .into_iter()
            .map(|idx| &circuit.gates[idx])
            .collect::<Vec<_>>();

        if these_gates.len() == 0 {
            info!("no more gates to apply. terminating loop.");
            break;
        }

        let num_gates_visited_here = these_gates.len();

        let (
            duration,
            ExpandResult {
                state: new_state,
                num_nonzero,
            },
        ) = profile!(state_expander::expand(these_gates, num_qubits, state)?);

        let density = {
            let max_num_states = 1 << num_qubits;
            num_nonzero as f64 / max_num_states as f64
        };

        info!(
            "gate: {:<2} hop: {:<2} density: {:.8} nonzero: {:<10} time: {:.4}s",
            num_gates_visited + 1,
            num_gates_visited_here,
            density,
            num_nonzero,
            duration.as_secs_f64(),
        );

        num_gates_visited += num_gates_visited_here;
        state = new_state;
    }

    assert!(num_gates_visited >= depth);
    Ok(state)
}
