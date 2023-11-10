use crate::circuit::{Gate, GateDefn};
use crate::types::{constants, BasisIdx, Complex};
pub trait Unitary {
    fn unitary(&self, num_qubits: usize) -> Vec<Vec<Complex>>;
}

impl<B: BasisIdx> Unitary for Gate<B> {
    fn unitary(&self, num_qubits: usize) -> Vec<Vec<Complex>> {
        let dim = 1 << num_qubits;
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
                let mat = vec![
                    vec![
                        Complex::new(constants::RECP_SQRT_2, 0.0),
                        Complex::new(constants::RECP_SQRT_2, 0.0),
                    ],
                    vec![
                        Complex::new(constants::RECP_SQRT_2, 0.0),
                        -Complex::new(constants::RECP_SQRT_2, 0.0),
                    ],
                ];
                tensor_product_with_identities(&mat, *qi, dim)
            }
            GateDefn::PauliY(qi) => {
                let mat = vec![
                    vec![Complex::new(0.0, 0.0), -Complex::new(0.0, 1.0)],
                    vec![Complex::new(0.0, 1.0), Complex::new(0.0, 0.0)],
                ];
                tensor_product_with_identities(&mat, *qi, dim)
            }
            GateDefn::PauliZ(qi) => {
                let mat = vec![
                    vec![Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
                    vec![Complex::new(0.0, 0.0), -Complex::new(1.0, 0.0)],
                ];
                tensor_product_with_identities(&mat, *qi, dim)
            }
            GateDefn::Phase { target, rot } => {
                let mat = vec![
                    vec![Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
                    vec![Complex::new(0.0, 0.0), Complex::new(rot.cos(), rot.sin())],
                ];
                tensor_product_with_identities(&mat, *target, dim)
            }
            GateDefn::S(qi) => {
                let mat = vec![
                    vec![Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
                    vec![Complex::new(0.0, 0.0), Complex::new(0.0, 1.0)],
                ];
                tensor_product_with_identities(&mat, *qi, dim)
            }
            GateDefn::Sdg(qi) => {
                let mat = vec![
                    vec![Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
                    vec![Complex::new(0.0, 0.0), -Complex::new(0.0, 1.0)],
                ];
                tensor_product_with_identities(&mat, *qi, dim)
            }
            _ => todo!(),
        }
    }
}

fn tensor_product_with_identities(
    mat: &[Vec<Complex>],
    qi: usize,
    dim: usize,
) -> Vec<Vec<Complex>> {
    let identity = vec![
        vec![Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
        vec![Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)],
    ];
    (0..dim).into_iter().fold(
        vec![vec![Complex::new(1.0, 0.0)]],
        |acc: Vec<Vec<Complex>>, idx| -> Vec<Vec<Complex>> {
            if idx == qi {
                tensor_product(&acc, &mat)
            } else {
                tensor_product(&acc, &identity)
            }
        },
    )
}

fn tensor_product(a: &[Vec<Complex>], b: &[Vec<Complex>]) -> Vec<Vec<Complex>> {
    a.iter()
        .flat_map(|a_row| {
            b.iter()
                .map(|b_row| {
                    a_row
                        .iter()
                        .flat_map(|&a_val| b_row.iter().map(move |&b_val| a_val * b_val))
                        .collect::<Vec<Complex>>()
                })
                .collect::<Vec<Vec<Complex>>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use num::complex::ComplexFloat;

    #[test]
    fn test_tensor_product_1() {
        let a = vec![
            vec![Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
            vec![Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)],
        ];
        let b = vec![
            vec![
                Complex::new(constants::RECP_SQRT_2, 0.0),
                Complex::new(constants::RECP_SQRT_2, 0.0),
            ],
            vec![
                Complex::new(constants::RECP_SQRT_2, 0.0),
                -Complex::new(constants::RECP_SQRT_2, 0.0),
            ],
        ];
        let result = tensor_product(&a, &b);

        let expected_result = vec![
            [
                Complex::new(constants::RECP_SQRT_2, 0.0),
                Complex::new(constants::RECP_SQRT_2, 0.0),
                Complex::new(0.0, 0.0),
                Complex::new(0.0, 0.0),
            ],
            [
                Complex::new(constants::RECP_SQRT_2, 0.0),
                -Complex::new(constants::RECP_SQRT_2, 0.0),
                Complex::new(0.0, 0.0),
                Complex::new(0.0, 0.0),
            ],
            [
                Complex::new(0.0, 0.0),
                Complex::new(0.0, 0.0),
                Complex::new(constants::RECP_SQRT_2, 0.0),
                Complex::new(constants::RECP_SQRT_2, 0.0),
            ],
            [
                Complex::new(0.0, 0.0),
                Complex::new(0.0, 0.0),
                Complex::new(constants::RECP_SQRT_2, 0.0),
                -Complex::new(constants::RECP_SQRT_2, 0.0),
            ],
        ];
        for row in 0..4 {
            for col in 0..4 {
                assert!(
                    (result[row][col] - expected_result[row][col]).abs()
                        < constants::ZERO_THRESHOLD
                )
            }
        }
    }

    #[test]
    fn test_tensor_product_2() {
        let a = vec![vec![Complex::new(1.0, 0.0)]];
        let b = vec![
            vec![
                Complex::new(constants::RECP_SQRT_2, 0.0),
                Complex::new(constants::RECP_SQRT_2, 0.0),
            ],
            vec![
                Complex::new(constants::RECP_SQRT_2, 0.0),
                -Complex::new(constants::RECP_SQRT_2, 0.0),
            ],
        ];
        let result = tensor_product(&a, &b);

        let expected_result = b;
        for row in 0..2 {
            for col in 0..2 {
                assert!(
                    (result[row][col] - expected_result[row][col]).abs()
                        < constants::ZERO_THRESHOLD
                )
            }
        }
    }
}
