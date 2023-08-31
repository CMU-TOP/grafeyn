mod gate;

use std::collections::HashMap;

use crate::parser::{Argument, Expression, QasmStatement};
use crate::types::{QubitIndex, Real};
pub use gate::{Gate, GateDefn};

pub struct Circuit {
    pub num_qubits: usize,
    pub gates: Vec<GateDefn>,
}

impl Circuit {
    pub fn new(statements: Vec<QasmStatement>) -> Result<Self, ()> {
        let mut num_qubits_so_far: usize = 0;
        let mut qregs = HashMap::<String, (QubitIndex, QubitIndex)>::new();
        let mut gates = Vec::<GateDefn>::new();

        for statement in statements {
            match statement {
                QasmStatement::QReg { name, size } => {
                    num_qubits_so_far += size;
                    qregs.insert(name, (num_qubits_so_far, num_qubits_so_far + size));
                }
                QasmStatement::GateCall { name, params, args } => {
                    let param_arity = params.len();
                    let arg_arity = args.len();

                    let get_index = |arg: Argument| -> QubitIndex {
                        match arg {
                            Argument::Id(_) => panic!("unsupported gate arg: missing qubit index"),
                            Argument::Item(name, index) => {
                                let (start, stop) = qregs
                                    .get(&name)
                                    .expect(format!("unknown qreg {}", name.as_str()).as_str());
                                if index >= (stop - start) {
                                    // NOTE: index is always nonzero as it is of type usize
                                    panic!() // TODO: Error handling
                                } else {
                                    start + index
                                }
                            }
                            _ => panic!("unsupported gate arg"),
                        }
                    };

                    let args: Vec<QubitIndex> = args.into_iter().map(get_index).collect();
                    let params: Vec<Real> = params.into_iter().map(eval).collect();

                    let gate = match (name.as_str(), param_arity, arg_arity) {
                        ("h", 0, 1) => GateDefn::Hadamard(args[0]),
                        ("y", 0, 1) => GateDefn::PauliY(args[0]),
                        ("z", 0, 1) => GateDefn::PauliZ(args[0]),
                        ("t", 0, 1) => GateDefn::T(args[0]),
                        ("x", 0, 1) => GateDefn::X(args[0]),
                        ("sx", 0, 1) => GateDefn::SqrtX(args[0]),
                        ("cx", 0, 2) => GateDefn::CX {
                            control: args[0],
                            target: args[1],
                        },
                        ("cz", 0, 2) => GateDefn::CZ {
                            control: args[0],
                            target: args[1],
                        },
                        ("ccx", 0, 3) => GateDefn::CCX {
                            control1: args[0],
                            control2: args[1],
                            target: args[2],
                        },
                        ("cphase", 1, 2) => GateDefn::CPhase {
                            control: args[0],
                            target: args[1],
                            rot: params[0],
                        },
                        ("ry", 1, 1) => GateDefn::RY {
                            rot: params[0],
                            target: args[0],
                        },
                        ("rz", 1, 1) => GateDefn::RZ {
                            rot: params[0],
                            target: args[0],
                        },
                        ("cswap", 0, 3) => GateDefn::CSwap {
                            control: args[0],
                            target1: args[1],
                            target2: args[2],
                        },
                        ("u", 3, 1) => GateDefn::U {
                            target: args[0],
                            theta: params[0],
                            phi: params[1],
                            lambda: params[2],
                        },
                        ("u2", 2, 1) => GateDefn::U {
                            target: args[0],
                            theta: std::f64::consts::PI / 2.0,
                            phi: params[0],
                            lambda: params[1],
                        },
                        ("u1", 1, 1) => GateDefn::U {
                            target: args[0],
                            theta: 0.0,
                            phi: 0.0,
                            lambda: params[0],
                        },
                        _ => panic!("Unexpected gate"),
                    };

                    gates.push(gate);
                }
            }
        }

        Ok(Circuit {
            num_qubits: num_qubits_so_far,
            gates,
        })
    }
}

fn eval(exp: Expression) -> Real {
    unimplemented!()
}
