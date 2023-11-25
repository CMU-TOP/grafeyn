use crate::circuit::{Gate, GateDefn};
use crate::types::{constants, BasisIdx, Complex, QubitIndex};
use nalgebra::{
    base::{Matrix, VecStorage},
    dmatrix, Dyn,
};

pub struct UnitaryMatrix {
    pub mat: Matrix<Complex, Dyn, Dyn, VecStorage<Complex, Dyn, Dyn>>,
    pub qubit_indices: Vec<QubitIndex>,
}
pub trait Unitary {
    fn unitary(&self) -> UnitaryMatrix;
}

impl<B: BasisIdx> Unitary for Gate<B> {
    // TODO: This should be cached
    fn unitary(&self) -> UnitaryMatrix {
        match &self.defn {
            GateDefn::CCX {
                control1,
                control2,
                target,
            } => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*control1, *control2, *target],
                }
            }
            GateDefn::CPhase {
                control,
                target,
                rot,
            } => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(rot.cos(), rot.sin())
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*control, *target],
                }
            }
            GateDefn::CSwap {
                control,
                target1,
                target2,
            } => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*control, *target1, *target2],
                }
            }
            GateDefn::CX { control, target } => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*control, *target],
                }
            }
            GateDefn::CZ { control, target } => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), -Complex::new(1.0, 0.0);
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*control, *target],
                }
            }
            GateDefn::FSim {
                left,
                right,
                theta,
                phi,
            } => {
                let a = Complex::new(theta.cos(), 0.0);
                let b = -Complex::new(0.0, theta.sin());
                let c = Complex::new(phi.cos(), phi.sin());
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(-phi.sin(), 0.0);
                    Complex::new(0.0, 0.0), a, b, Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), b, a, Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), c
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*left, *right],
                }
            }
            GateDefn::Hadamard(qi) => {
                let mat = dmatrix![
                        Complex::new(constants::RECP_SQRT_2, 0.0),
                        Complex::new(constants::RECP_SQRT_2, 0.0);
                        Complex::new(constants::RECP_SQRT_2, 0.0),
                        -Complex::new(constants::RECP_SQRT_2, 0.0)

                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::PauliY(qi) => {
                let mat = dmatrix![
                    Complex::new(0.0, 0.0), -Complex::new(0.0, 1.0);
                    Complex::new(0.0, 1.0), Complex::new(0.0, 0.0)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::PauliZ(qi) => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), -Complex::new(1.0, 0.0)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::Phase { target, rot } => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(rot.cos(), rot.sin())
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*target],
                }
            }
            GateDefn::RX { rot, target } => {
                let a = Complex::new((rot / 2.0).cos(), 0.0);
                let b = Complex::new(0.0, (rot / 2.0).sin());
                let mat = dmatrix![
                    a, -b;
                    -b, a
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*target],
                }
            }
            GateDefn::RY { rot, target } => {
                let a = Complex::new((rot / 2.0).cos(), 0.0);
                let b = Complex::new((rot / 2.0).sin(), 0.0);
                let mat = dmatrix![
                    a, -b;
                    b, a
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*target],
                }
            }
            GateDefn::RZ { rot, target } => {
                let a = Complex::new((rot / 2.0).cos(), (rot / 2.0).sin());
                let b = Complex::new((rot / 2.0).cos(), -(rot / 2.0).sin());
                let mat = dmatrix![
                    a, Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), b
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*target],
                }
            }
            GateDefn::S(qi) => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 1.0)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::Sdg(qi) => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), -Complex::new(0.0, 1.0)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::SqrtX(qi) => {
                let mat = dmatrix![
                    Complex::new(0.5, 0.5), Complex::new(0.5, -0.5);
                    Complex::new(0.5, -0.5), Complex::new(0.5, 0.5);
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::SqrtXdg(qi) => {
                let mat = dmatrix![
                    Complex::new(0.5, -0.5), Complex::new(0.5, 0.5);
                    Complex::new(0.5, 0.5), Complex::new(0.5, -0.5);
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::Swap { target1, target2 } => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(1.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(0.0, 0.0), Complex::new(1.0, 0.0);
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*target1, *target2],
                }
            }
            GateDefn::T(qi) => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(constants::RECP_SQRT_2, constants::RECP_SQRT_2)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::Tdg(qi) => {
                let mat = dmatrix![
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0);
                    Complex::new(0.0, 0.0), Complex::new(constants::RECP_SQRT_2, -constants::RECP_SQRT_2)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::U {
                target,
                theta,
                phi,
                lambda,
            } => {
                let cos = Complex::new((theta / 2.0).cos(), 0.0);
                let sin = Complex::new((theta / 2.0).sin(), 0.0);

                let a = cos;
                let b = -sin * Complex::new(lambda.cos(), lambda.sin());
                let c = sin * Complex::new(phi.cos(), phi.sin());
                let d = cos * Complex::new((phi + lambda).cos(), (phi + lambda).sin());

                let mat = dmatrix![
                    a, b;
                    c, d
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*target],
                }
            }
            GateDefn::X(qi) => {
                let mat = dmatrix![
                    Complex::new(0.0, 0.0), Complex::new(1.0, 0.0);
                    Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)
                ];
                UnitaryMatrix {
                    mat,
                    qubit_indices: vec![*qi],
                }
            }
            GateDefn::Other { .. } => panic!("unsupported gate {:?}", self),
        }
    }
}
