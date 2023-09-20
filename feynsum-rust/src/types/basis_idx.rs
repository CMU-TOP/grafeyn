use std::fmt::{self, Display, Formatter};

pub const MAX_QUBITS: usize = 63;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BasisIdx {
    bits: u64,
}

impl Display for BasisIdx {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format!("{:b}", self.bits).fmt(f)
    }
}

impl BasisIdx {
    pub fn get(&self, qi: usize) -> bool {
        self.bits & (1 << qi) != 0
    }

    pub const fn flip_unsafe(&self, qi: usize) -> Self {
        BasisIdx {
            bits: self.bits ^ (1 << qi),
        }
    }

    pub fn flip(&self, qi: usize) -> Self {
        self.flip_unsafe(qi)
    }

    pub fn zeros() -> Self {
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

    pub fn swap(&self, qi1: usize, qi2: usize) -> Self {
        let tmp = ((self.bits >> qi1) ^ (self.bits >> qi2)) & 1;

        Self {
            bits: self.bits ^ (tmp << qi1) ^ (tmp << qi2),
        }
    }

    // NOTE: DenseStateTable uses BasisIdx as an index into its array
    pub fn into_idx(self) -> usize {
        self.bits as usize
    }

    pub fn from_idx(idx: usize) -> Self {
        Self { bits: idx as u64 }
    }

    pub fn into_u64(self) -> u64 {
        self.bits
    }

    pub const fn from_u64(u: u64) -> Self {
        Self { bits: u }
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
        assert!(!bidx.get(0));
        assert!(bidx.get(1));
        assert!(!bidx.get(2));
        assert!(bidx.get(3));
    }

    #[test]
    fn test_flip() {
        let bidx = BasisIdx { bits: 0b1010 };
        let bidx = bidx.flip(0);
        assert_eq!(bidx.bits, 0b1011);
    }

    #[test]
    fn test_zeros() {
        let bidx = BasisIdx::zeros();
        assert_eq!(bidx.bits, 0);
    }

    #[test]
    fn test_set() {
        let bidx = BasisIdx::zeros();
        let bidx1 = bidx.set(0);
        assert_eq!(bidx1.bits, 1);

        let bidx2 = bidx.set(2);
        assert_eq!(bidx2.bits, 4);
    }

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
        let bidx = bidx.swap(0, 1);
        assert_eq!(bidx.bits, 0b1001);
    }
}
