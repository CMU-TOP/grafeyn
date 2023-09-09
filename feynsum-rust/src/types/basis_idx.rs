use std::fmt::{self, Display, Formatter};

pub const MAX_QUBITS: usize = 63;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BasisIdx {
    bits: u64,
}

impl Display for BasisIdx {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format!("{:b}", self.bits).fmt(f)
    }
}

#[derive(Debug)]
pub enum BasisIdxErr {
    IndexOutOfBounds,
}

impl BasisIdx {
    pub fn get(&self, qi: usize) -> Result<bool, BasisIdxErr> {
        if qi >= MAX_QUBITS {
            Err(BasisIdxErr::IndexOutOfBounds)
        } else {
            Ok(self.bits & (1 << qi) != 0)
        }
    }

    pub fn flip(&self, qi: usize) -> Result<Self, BasisIdxErr> {
        if qi >= MAX_QUBITS {
            Err(BasisIdxErr::IndexOutOfBounds)
        } else {
            Ok(BasisIdx {
                bits: self.bits ^ (1 << qi),
            })
        }
    }

    pub fn zeros(num_qubits: usize) -> Self {
        assert!(num_qubits <= MAX_QUBITS);
        Self { bits: 0 }
    }

    pub fn set(&self, qi: usize) -> Self {
        Self {
            bits: self.bits | (1 << qi),
        }
    }

    pub fn unset(&self, qi: usize) -> Self {
        Self {
            bits: self.bits & !(1 << qi),
        }
    }

    pub fn swap(&self, qi1: usize, qi2: usize) -> Result<Self, BasisIdxErr> {
        if qi1 >= MAX_QUBITS || qi2 >= MAX_QUBITS {
            Err(BasisIdxErr::IndexOutOfBounds)
        } else {
            let tmp = ((self.bits >> qi1) ^ (self.bits >> qi2)) & 1;

            Ok(Self {
                bits: self.bits ^ (tmp << qi1) ^ (tmp << qi2),
            })
        }
    }

    #[cfg(test)]
    pub fn new(bits: &str) -> Self {
        Self {
            bits: u64::from_str_radix(bits, 2).unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get() {
        let bidx = BasisIdx { bits: 0b1010 };
        assert!(!bidx.get(0).unwrap());
        assert!(bidx.get(1).unwrap());
        assert!(!bidx.get(2).unwrap());
        assert!(bidx.get(3).unwrap());
    }

    #[test]
    fn test_flip() {
        let bidx = BasisIdx { bits: 0b1010 };
        let bidx = bidx.flip(0).unwrap();
        assert_eq!(bidx.bits, 0b1011);
    }

    #[test]
    fn test_zeros() {
        let bidx = BasisIdx::zeros(4);
        assert_eq!(bidx.bits, 0);
    }

    #[test]
    fn test_set() {
        let bidx = BasisIdx::zeros(4);
        let bidx1 = bidx.set(0);
        assert_eq!(bidx1.bits, 1);

        let bidx2 = bidx.set(2);
        assert_eq!(bidx2.bits, 4);
    }

    #[test]
    fn test_unset() {
        let bidx = BasisIdx { bits: 0b1010 };
        let bidx = bidx.unset(0);
        assert_eq!(bidx.bits, 0b1010);

        let bidx = bidx.unset(1);
        assert_eq!(bidx.bits, 0b1000);
    }

    #[test]
    fn test_swap() {
        let bidx = BasisIdx { bits: 0b1010 };
        let bidx = bidx.swap(0, 1).unwrap();
        assert_eq!(bidx.bits, 0b1001);
    }
}
