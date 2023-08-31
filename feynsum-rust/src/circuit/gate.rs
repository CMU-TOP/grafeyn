use crate::types::{QubitIndex, Real};

#[derive(Debug)]
pub enum GateDefn {
    PauliY(QubitIndex),
    PauliZ(QubitIndex),
    Hadamard(QubitIndex),
    T(QubitIndex),
    SqrtX(QubitIndex),
    SqrtY(QubitIndex),
    SqrtW(QubitIndex),
    X(QubitIndex),
    CX {
        control: QubitIndex,
        target: QubitIndex,
    },
    CZ {
        control: QubitIndex,
        target: QubitIndex,
    },
    CCX {
        control1: QubitIndex,
        control2: QubitIndex,
        target: QubitIndex,
    },
    CPhase {
        control: QubitIndex,
        target: QubitIndex,
        rot: Real,
    },
    FSim {
        left: QubitIndex,
        right: QubitIndex,
        theta: Real,
        phi: Real,
    },
    RZ {
        rot: Real,
        target: QubitIndex,
    },
    RY {
        rot: Real,
        target: QubitIndex,
    },
    CSwap {
        control: QubitIndex,
        target1: QubitIndex,
        target2: QubitIndex,
    },
    U {
        target: QubitIndex,
        theta: Real,
        phi: Real,
        lambda: Real,
    },
    Other {
        name: String,
        params: Vec<Real>,
        args: Vec<QubitIndex>,
    },
}

pub struct Gate {
    pub name: String,
    pub params: Vec<String>,
    pub args: Vec<QubitIndex>,
}

impl Gate {
    pub fn _apply(&self, _basis: ()) {
        unimplemented!()
    }
}
