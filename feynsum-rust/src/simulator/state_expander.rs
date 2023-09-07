use crate::circuit::{Gate, MaybeBranchingOutput, PushApplicable};
use crate::types::{BasisIdx, Complex};
use crate::utility::is_zero;

use super::state::State;
use super::table::SparseStateTable;
use super::SimulatorError;

pub struct ExpandResult {
    pub state: State,
    pub num_nonzero: usize,
    pub num_gate_apps: usize,
}

fn apply_gates(
    gates: &[&Gate],
    table: &mut SparseStateTable,
    bidx: BasisIdx,
    weight: Complex,
) -> Result<usize, SimulatorError> {
    if is_zero(weight) {
        return Ok(0);
    }
    if gates.is_empty() {
        table.put(bidx, weight);
        return Ok(0);
    }

    match gates[0].push_apply(bidx, weight)? {
        MaybeBranchingOutput::OuptutOne((new_bidx, new_weight)) => {
            Ok(1 + apply_gates(&gates[1..], table, new_bidx, new_weight)?)
        }
        MaybeBranchingOutput::OutputTwo((new_bidx1, new_weight1), (new_bidx2, new_weight2)) => {
            let num_gate_apps_1 = apply_gates(&gates[1..], table, new_bidx1, new_weight1)?;
            let num_gate_apps_2 = apply_gates(&gates[1..], table, new_bidx2, new_weight2)?;
            Ok(1 + num_gate_apps_1 + num_gate_apps_2)
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

    let mut num_gate_apps = 0;

    for (bidx, weight) in prev_entries {
        num_gate_apps += apply_gates(&gates, &mut table, bidx, weight)?;
    }

    let num_nonzero = table.num_nonzero();

    Ok(ExpandResult {
        state: State::Sparse(table), // TODO: use DenseStateTable if necessary
        num_nonzero,
        num_gate_apps,
    })
}
