use bit_vec::BitVec;

pub const MAX_QUBITS: usize = 63;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BasisIdx {
    bits: BitVec,
}

impl BasisIdx {
    // TODO: error handling
    pub fn get(&self, qi: usize) -> bool {
        self.bits.get(qi).expect("index out of bounds")
    }

    pub fn flip(&self, qi: usize) -> Self {
        let mut new_bits = self.bits.clone();
        new_bits.set(qi, !self.bits.get(qi).expect("index out of bounds"));
        BasisIdx { bits: new_bits }
    }

    pub fn zeros() -> Self {
        Self {
            bits: BitVec::from_elem(MAX_QUBITS, false),
        }
    }

    pub fn set(&self, qi: usize) -> Self {
        let mut new_bits = self.bits.clone();
        new_bits.set(qi, true);
        BasisIdx { bits: new_bits }
    }

    pub fn unset(&self, qi: usize) -> Self {
        let mut new_bits = self.bits.clone();
        new_bits.set(qi, false);
        BasisIdx { bits: new_bits }
    }

    pub fn swap(&self, qi1: usize, qi2: usize) -> Self {
        let mut new_bits = self.bits.clone();
        let tmp = new_bits.get(qi1).expect("index out of bounds");
        new_bits.set(qi1, new_bits.get(qi2).expect("index out of bounds"));
        new_bits.set(qi2, tmp);
        BasisIdx { bits: new_bits }
    }
}
