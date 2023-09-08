pub mod bfs_simulator;
mod state;
mod state_expander;

use crate::circuit::GateApplyErr;
pub use state::State;

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
