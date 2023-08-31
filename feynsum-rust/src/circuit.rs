mod gate;

use core::panic;
use std::collections::HashMap;

use crate::parser::{Argument, Expression, OpCode, QasmStatement};
use crate::types::{QubitIndex, Real};
pub use gate::{Gate, GateDefn};

#[derive(Debug)]
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
                        ("sy", 0, 1) => GateDefn::SqrtY(args[0]),
                        ("sw", 0, 1) => GateDefn::SqrtW(args[0]),
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
                        ("fsim", 0, 0) => unimplemented!(),
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
    match exp {
        Expression::Pi => std::f64::consts::PI,
        Expression::Real(x) => x,
        Expression::Int(x) => x as f64,
        Expression::Op(opcode, e1, e2) => {
            let v1 = eval(*e1);
            let v2 = eval(*e2);
            match opcode {
                OpCode::Add => v1 + v2,
                OpCode::Sub => v1 - v2,
                OpCode::Mul => v1 * v2,
                OpCode::Div => v1 / v2,
                OpCode::Pow => v1.powf(v2),
                _ => panic!("unsupported opcode"),
            }
        }
        Expression::Id(_) => panic!("unsupported identifier"),
        _ => panic!("unsupported expression"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;

    #[test]
    fn test_circuit_from_source() {
        let source = r#"
        OPENQASM 2.0;
        include "qelib1.inc";
        qreg q[4];
        creg c[4];
        x q[3];
        h q[0];
        h q[1];
        h q[2];
        h q[3];
        cx q[0],q[3];
        cx q[1],q[3];
        cx q[2],q[3];
        h q[0];
        h q[1];
        h q[2];
        h q[3];
        measure q[0] -> c[0];
        measure q[1] -> c[1];
        measure q[2] -> c[2];
        measure q[3] -> c[3];
        "#;

        let program = parser::parse_program(&source).unwrap();

        let circuit = Circuit::new(program).unwrap();
        println!("{:?}", circuit);
    }
}
