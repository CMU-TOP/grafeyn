use log::{debug, error, info};

use crate::circuit::Circuit;
use crate::config::Config;
use crate::state_expander::{self, ExpandResult, State};
use crate::table::SparseStateTable;
use crate::types::basis_idx::MAX_QUBITS;
use crate::types::{BasisIdx, Complex};

use super::SimulatorError;

pub fn run(config: &Config, circuit: Circuit) -> Result<State, SimulatorError> {
    let depth = circuit.depth();
    let num_qubits = circuit.num_qubits;

    if num_qubits > MAX_QUBITS {
        error!("Too many qubits: {}", num_qubits);
        return Err(SimulatorError::TooManyQubits);
    }

    let mut num_gates_visited = 0;
    let mut num_gate_apps = 0;
    let mut state = State::Sparse(SparseStateTable::singleton(
        BasisIdx::zeros(),
        Complex::new(1.0, 0.0),
    )); // initial state

    let mut gate_scheduler = config.create_gate_scheduler(depth);

    loop {
        let these_gates = gate_scheduler
            .pick_next_gates()
            .into_iter()
            .map(|idx| &circuit.gates[idx])
            .collect::<Vec<_>>();
        if these_gates.len() == 0 {
            break;
        }

        let num_gates_visited_here = these_gates.len();

        let ExpandResult {
            state: new_state,
            num_nonzero,
            num_gate_apps: num_gate_apps_here,
        } = state_expander::expand(these_gates, num_qubits, state);
        debug!(
            "state expanded: new_state: {:?}, num_nonzero: {}, num_gate_apps: {}",
            new_state, num_nonzero, num_gate_apps_here
        );

        num_gates_visited += num_gates_visited_here;
        num_gate_apps += num_gate_apps_here;
        state = new_state;

        // TODO: calculate and log density
    }

    assert!(num_gates_visited >= depth);
    info!("run complete: {} gate applications", num_gate_apps);
    Ok(state)
}
