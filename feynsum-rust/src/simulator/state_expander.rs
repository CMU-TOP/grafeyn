use crate::circuit::{Gate, MaybeBranchingOutput, PushApplicable};

use super::state::State;
use super::table::SparseStateTable;
use super::SimulatorError;

pub struct ExpandResult {
    pub state: State,
    pub num_nonzero: usize,
}

pub fn expand(
    gates: Vec<&Gate>,
    _num_qubits: usize,
    state: State,
) -> Result<ExpandResult, SimulatorError> {
    let mut num_nonzero = 0;

    let prev_entries = state.compactify();

    let mut table = SparseStateTable::new();

    for (bidx, weight) in prev_entries {
        for gate in gates.iter() {
            match gate.push_apply(&bidx, &weight)? {
                MaybeBranchingOutput::OuptutOne((new_bidx, new_weight)) => {
                    table.put(new_bidx, new_weight);
                    num_nonzero += 1;
                }
                MaybeBranchingOutput::OutputTwo(
                    (new_bidx1, new_weight1),
                    (new_bidx2, new_weight2),
                ) => {
                    table.put(new_bidx1, new_weight1);
                    table.put(new_bidx2, new_weight2);
                    num_nonzero += 2;
                }
            }
        }
    }

    Ok(ExpandResult {
        state: State::Sparse(table), // TODO: use DenseStateTable if necessary
        num_nonzero,
    })
}
