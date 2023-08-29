mod gate;

pub use gate::Gate;

use crate::parser::Program;

pub struct Circuit {
    num_qubits: u32,
    gates: Vec<Gate>,
}

impl Circuit {
    pub fn new(program: &Program) -> Result<Self, ()> {
        unimplemented!()
    }
}
