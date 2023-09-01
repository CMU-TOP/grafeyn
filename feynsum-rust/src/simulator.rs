use log::{debug, error, info};

use crate::circuit::Circuit;
use crate::config::Config;
use crate::state_expander::{self, ExpandResult, SparseState, State};

const MAX_QUBITS: usize = 63;

pub enum SimulatorError {
    TooManyQubits,
}

pub fn run(config: &Config, circuit: Circuit) -> Result<(), SimulatorError> {
    let depth = circuit.depth();
    let num_qubits = circuit.num_qubits;

    if num_qubits > MAX_QUBITS {
        error!("Too many qubits: {}", num_qubits);
        return Err(SimulatorError::TooManyQubits);
    }

    let mut num_gates_visited = 0;
    let mut num_gate_apps = 0;
    let mut state = State::Sparse(SparseState::singleton()); // initial state
    let mut prev_nonzero_size = 0;

    while num_gates_visited < depth {
        let these_gates = config.gate_scheduler.pick_next_gates();
        let num_gates_visited_here = these_gates.len();

        let ExpandResult {
            state: new_state,
            method,
            num_nonzero,
            num_gate_apps: num_gate_apps_here,
        } = state_expander::expand(these_gates, num_qubits, state, prev_nonzero_size);
        debug!(
            "state expanded: new_state: {:?}, method: {}, num_nonzero: {}, num_gate_apps: {}",
            new_state, method, num_nonzero, num_gate_apps_here
        );

        num_gates_visited += num_gates_visited_here;
        num_gate_apps += num_gate_apps_here;
        prev_nonzero_size = num_nonzero;
        state = new_state;

        // TODO: calculate and log density
    }

    assert!(num_gates_visited >= depth);
    info!("run complete: {} gate applications", num_gate_apps);
    Ok(())
}
