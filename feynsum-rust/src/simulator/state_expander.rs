use crate::circuit::{Gate, MaybeBranchingOutput, PushApplicable};
use crate::types::{BasisIdx, Complex};
use crate::utility::is_zero;

use super::state::State;
use super::table::SparseStateTable;
use super::SimulatorError;

pub struct ExpandResult {
    pub state: State,
    pub num_nonzero: usize,
}

fn apply_gates(
    gates: &[&Gate],
    table: &mut SparseStateTable,
    bidx: BasisIdx,
    weight: Complex,
) -> Result<(), SimulatorError> {
    if is_zero(weight) {
        return Ok(());
    }
    if gates.is_empty() {
        table.put(bidx, weight);
        return Ok(());
    }

    match gates[0].push_apply(&bidx, &weight)? {
        MaybeBranchingOutput::OuptutOne((new_bidx, new_weight)) => {
            apply_gates(&gates[1..], table, new_bidx, new_weight)
        }
        MaybeBranchingOutput::OutputTwo((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            apply_gates(&gates[1..], table, new_bidx1, new_weight1)?;
            apply_gates(&gates[1..], table, new_bidx2, new_weight2)
        }
    }
}

pub fn expand(
    gates: Vec<&Gate>,
    _num_qubits: usize,
    state: State,
) -> Result<ExpandResult, SimulatorError> {
    let prev_entries = state.compactify();

    let mut table = SparseStateTable::new();

    for (bidx, weight) in prev_entries {
        apply_gates(&gates, &mut table, bidx, weight)?;
    }

    let num_nonzero = table.size();

    Ok(ExpandResult {
        state: State::Sparse(table), // TODO: use DenseStateTable if necessary
        num_nonzero,
    })
}
