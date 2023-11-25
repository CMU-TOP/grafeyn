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
                todo!()
            }
            GateDefn::CPhase {
                control,
                target,
                rot,
            } => {
                todo!()
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
            _ => todo!(),
        }
    }
}
