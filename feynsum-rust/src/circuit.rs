mod gate;

use log::error;
use std::collections::HashMap;

use crate::parser::{Argument, Expression, OpCode, QasmStatement};
use crate::types::{QubitIndex, Real};
pub use gate::{Gate, GateDefn, PullApplicable, PullApplyOutput, PushApplicable, PushApplyOutput};

#[derive(Debug)]
pub enum CircuitBuildError {
    IndexOutOfBounds,
    UnknownQReg,
    UnsupportedExpression,
    UnsupportedGateArg,
    UnsupportedIdentifier,
    UnsupportedOpcode,
}

#[derive(Debug)]
pub struct Circuit {
    pub num_qubits: usize,
    pub gates: Vec<Gate>,
}

impl Circuit {
    pub fn new(statements: Vec<QasmStatement>) -> Result<Self, CircuitBuildError> {
        let mut num_qubits_so_far: usize = 0;
        let mut qregs = HashMap::<String, (QubitIndex, QubitIndex)>::new();
        let mut gates = Vec::<Gate>::new();

        for statement in statements {
            match statement {
                QasmStatement::QReg { name, size } => {
                    qregs.insert(name, (num_qubits_so_far, num_qubits_so_far + size));
                    num_qubits_so_far += size;
                }
                QasmStatement::GateCall { name, params, args } => {
                    let param_arity = params.len();
                    let arg_arity = args.len();

                    let get_index = |arg: Argument| -> Result<QubitIndex, CircuitBuildError> {
                        match arg {
                            Argument::Id(id) => {
                                error!("unsupported gate arg: identifier {}", id);
                                Err(CircuitBuildError::UnsupportedIdentifier)
                            }
                            Argument::Item(name, index) => {
                                match qregs.get(&name) {
                                    Some((start, stop)) => {
                                        if index >= (stop - start) {
                                            // NOTE: index is always nonzero as it is of type usize
                                            error!("index out of bounds: {}", index);
                                            Err(CircuitBuildError::IndexOutOfBounds)
                                        } else {
                                            Ok(start + index)
                                        }
                                    }
                                    None => {
                                        error!("unknown qreg: {}", name);
                                        Err(CircuitBuildError::UnknownQReg)
                                    }
                                }
                            }
                            arg => {
                                error!("unsupported gate arg: {:?}", arg);
                                Err(CircuitBuildError::UnsupportedGateArg)
                            }
                        }
                    };

                    let args: Vec<QubitIndex> = args
                        .into_iter()
                        .map(get_index)
                        .collect::<Result<Vec<_>, _>>()?;
                    let params: Vec<Real> = params
                        .into_iter()
                        .map(eval)
                        .collect::<Result<Vec<_>, _>>()?;

                    let gate_defn = match (name.as_str(), param_arity, arg_arity) {
                        ("ccx", 0, 3) => GateDefn::CCX {
                            control1: args[0],
                            control2: args[1],
                            target: args[2],
                        },
                        ("cphase", 1, 2) | ("cp", 1, 2) => GateDefn::CPhase {
                            control: args[0],
                            target: args[1],
                            rot: params[0],
                        },
                        ("cswap", 0, 3) => GateDefn::CSwap {
                            control: args[0],
                            target1: args[1],
                            target2: args[2],
                        },
                        ("cx", 0, 2) => GateDefn::CX {
                            control: args[0],
                            target: args[1],
                        },
                        ("cz", 0, 2) => GateDefn::CZ {
                            control: args[0],
                            target: args[1],
                        },
                        ("fsim", 2, 2) => GateDefn::FSim {
                            left: args[0],
                            right: args[1],
                            theta: params[0],
                            phi: params[1],
                        },
                        ("h", 0, 1) => GateDefn::Hadamard(args[0]),
                        ("phase", 1, 1) | ("p", 1, 1) => GateDefn::Phase {
                            target: args[0],
                            rot: params[0],
                        },
                        ("rx", 1, 1) => GateDefn::RX {
                            rot: params[0],
                            target: args[0],
                        },
                        ("ry", 1, 1) => GateDefn::RY {
                            rot: params[0],
                            target: args[0],
                        },
                        ("rz", 1, 1) => GateDefn::RZ {
                            rot: params[0],
                            target: args[0],
                        },
                        ("s", 0, 1) => GateDefn::S(args[0]),
                        ("sdg", 0, 1) => GateDefn::Sdg(args[0]),
                        ("sw", 0, 1) => GateDefn::SqrtW(args[0]),
                        ("swap", 0, 2) => GateDefn::Swap {
                            target1: args[0],
                            target2: args[1],
                        },
                        ("sx", 0, 1) => GateDefn::SqrtX(args[0]),
                        ("sxdg", 0, 1) => GateDefn::SqrtXdg(args[0]),
                        ("t", 0, 1) => GateDefn::T(args[0]),
                        ("tdg", 0, 1) => GateDefn::Tdg(args[0]),
                        // NOTE: U3 gate is deprecated
                        ("u", 3, 1) | ("u3", 3, 1) => GateDefn::U {
                            target: args[0],
                            theta: params[0],
                            phi: params[1],
                            lambda: params[2],
                        },
                        ("u1", 1, 1) => GateDefn::U {
                            target: args[0],
                            theta: 0.0,
                            phi: 0.0,
                            lambda: params[0],
                        },
                        ("u2", 2, 1) => GateDefn::U {
                            target: args[0],
                            theta: std::f32::consts::PI / 2.0,
                            phi: params[0],
                            lambda: params[1],
                        },
                        ("x", 0, 1) => GateDefn::X(args[0]),
                        ("y", 0, 1) => GateDefn::PauliY(args[0]),
                        ("z", 0, 1) => GateDefn::PauliZ(args[0]),
                        _ => {
                            log::warn!("unknown gate: {}", name);
                            GateDefn::Other { name, params, args }
                        }
                    };

                    gates.push(Gate::new(gate_defn));
                }
            }
        }

        Ok(Circuit {
            num_qubits: num_qubits_so_far,
            gates,
        })
    }

    pub fn num_gates(&self) -> usize {
        self.gates.len()
    }
}

fn eval(exp: Expression) -> Result<Real, CircuitBuildError> {
    match exp {
        Expression::Pi => Ok(std::f32::consts::PI),
        Expression::Real(x) => Ok(x as Real),
        Expression::Int(x) => Ok(x as Real),
        Expression::Op(opcode, e1, e2) => {
            let v1 = eval(*e1)?;
            let v2 = eval(*e2)?;
            match opcode {
                OpCode::Add => Ok(v1 + v2),
                OpCode::Sub => Ok(v1 - v2),
                OpCode::Mul => Ok(v1 * v2),
                OpCode::Div => Ok(v1 / v2),
                OpCode::Pow => Ok(v1.powf(v2)),
                _ => Err(CircuitBuildError::UnsupportedOpcode),
            }
        }
        Expression::Minus(exp) => Ok(-eval(*exp)?),
        Expression::Id(_) => Err(CircuitBuildError::UnsupportedIdentifier),
        exp => {
            error!("unsupported expression: {:?}", exp);
            Err(CircuitBuildError::UnsupportedExpression)
        }
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
        "#;

        let program = parser::parse_program(&source).unwrap();

        let circuit = Circuit::new(program).unwrap();
        assert_eq!(circuit.gates.len(), 12);
        assert_eq!(
            circuit
                .gates
                .iter()
                .map(|gate| gate.is_branching())
                .collect::<Vec<bool>>(),
            vec![false, true, true, true, true, false, false, false, true, true, true, true]
        )
    }
}
