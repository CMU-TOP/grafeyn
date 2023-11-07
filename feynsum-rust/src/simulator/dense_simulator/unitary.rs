use crate::circuit::Gate;
use crate::types::{BasisIdx, Complex};
pub trait Unitary {
    fn unitary(&self, num_qubits: usize) -> Vec<Vec<Complex>>;
}

impl<B: BasisIdx> Unitary for Vec<Gate<B>> {
    fn unitary(&self, num_qubits: usize) -> Vec<Vec<Complex>> {
        todo!()
    }
}
