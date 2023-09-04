use log::debug;

use crate::circuit::{GateDefn, MaybeBranchingOutput};

use super::state::State;
use super::table::SparseStateTable;

pub struct ExpandResult {
    pub state: State,
    pub num_nonzero: usize,
    pub num_gate_apps: usize,
}

pub fn expand(gates: Vec<&GateDefn>, _num_qubits: usize, state: State) -> ExpandResult {
    let mut num_gate_apps = 0;
    let mut num_nonzero = 0;

    let prev_entries = state.compactify();
    debug!("prev entries compactified");

    let mut table = SparseStateTable::new();

    for (bidx, weight) in prev_entries {
        for gate in gates.iter() {
            debug!("applying gate {:?} to idx {}", gate, bidx);
            match gate.apply(&bidx, &weight) {
                MaybeBranchingOutput::OuptutOne((new_bidx, new_weight)) => {
                    debug!("gate applied. one output: ({}, {})", new_bidx, new_weight);
                    table.put(new_bidx, new_weight);
                    num_nonzero += 1;
                }
                MaybeBranchingOutput::OutputTwo(
                    (new_bidx1, new_weight1),
                    (new_bidx2, new_weight2),
                ) => {
                    debug!(
                        "gate applied. two outputs: ({}, {}), ({}, {})",
                        new_bidx1, new_weight1, new_bidx2, new_weight2
                    );
                    table.put(new_bidx1, new_weight1);
                    table.put(new_bidx2, new_weight2);
                    num_nonzero += 2;
                }
            }
        }
        num_gate_apps += 1;
    }

    ExpandResult {
        state: State::Sparse(table), // TODO: use DenseStateTable if necessary
        num_nonzero,
        num_gate_apps,
    }
}
