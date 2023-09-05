pub mod bfs_simulator;
mod state;
mod state_expander;
mod table;

use crate::circuit::GateApplyErr;

#[derive(Debug)]
pub enum SimulatorError {
    TooManyQubits,
    GateApplyErr(GateApplyErr),
}

impl From<GateApplyErr> for SimulatorError {
    fn from(err: GateApplyErr) -> Self {
        SimulatorError::GateApplyErr(err)
    }
}
