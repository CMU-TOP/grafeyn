use crate::types::{QubitIndex, Real};

enum GateDefn {
    PauliY {
        qubit_idx: QubitIndex,
    },
    PauliZ {
        qubit_idx: QubitIndex,
    },
    Hadamard {
        qubit_idx: QubitIndex,
    },
    T {
        qubit_idx: QubitIndex,
    },
    SqrtY {
        qubit_idx: QubitIndex,
    },
    SqrtX {
        qubit_idx: QubitIndex,
    },
    SqrtW {
        qubit_idx: QubitIndex,
    },
    X {
        qubit_idx: QubitIndex,
    },
    CX {
        control: QubitIndex,
        target: QubitIndex,
    },
    CZ {
        control: QubitIndex,
        target: QubitIndex,
    },
    CCX {
        control: QubitIndex,
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

pub struct Gate {}

impl Gate {
    fn apply(&self, basis: ()) {
        unimplemented!()
    }
}

struct Circuit {
    num_cubits: u32,
    gates: Vec<Gate>,
}
