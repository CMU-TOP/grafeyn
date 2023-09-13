pub mod parallel;
pub mod sequential;

use crate::circuit::GateApplyErr;
use crate::types::{BasisIdx, Complex};

pub trait Compactifiable {
    fn compactify(self) -> Box<dyn Iterator<Item = (BasisIdx, Complex)>>;
}

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
