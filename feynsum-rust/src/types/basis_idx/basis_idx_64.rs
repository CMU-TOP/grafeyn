use std::fmt::{self, Display, Formatter};
use std::hash::Hash;
use std::str::FromStr;
use std::sync::atomic::{AtomicU64, Ordering};

use super::{AtomicBasisIdx, BasisIdx};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BasisIdx64 {
    bits: u64,
}

impl Display for BasisIdx64 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match f.width() {
            Some(width) => format!("{:0width$b}", self.bits, width = width).fmt(f),
            None => format!("{:b}", self.bits).fmt(f),
        }
    }
}

impl FromStr for BasisIdx64 {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            bits: u64::from_str_radix(s, 2).map_err(|_| ())?,
        })
    }
}

impl BasisIdx64 {
    #[cfg(test)]
    pub fn new(bits: &str) -> Self {
        Self {
            bits: u64::from_str_radix(bits, 2).unwrap(),
        }
    }
}

impl BasisIdx for BasisIdx64 {
    fn get(&self, qi: usize) -> bool {
        self.bits & (1 << qi) != 0
    }

    fn flip(&self, qi: usize) -> Self {
        BasisIdx64 {
            bits: self.bits ^ (1 << qi),
        }
    }

    fn zeros() -> Self {
        Self { bits: 0 }
    }

    fn set(&self, qi: usize) -> Self {
        Self {
            bits: self.bits | (1 << qi),
        }
    }

    fn unset(&self, qi: usize) -> Self {
        Self {
            bits: self.bits & !(1 << qi),
        }
    }

    fn swap(&self, qi1: usize, qi2: usize) -> Self {
        let tmp = ((self.bits >> qi1) ^ (self.bits >> qi2)) & 1;

        Self {
            bits: self.bits ^ (tmp << qi1) ^ (tmp << qi2),
        }
    }

    fn from_idx(idx: usize) -> Self {
        Self { bits: idx as u64 }
    }

    fn into_idx(&self) -> usize {
        self.bits as usize
    }

    fn empty_key(_num_qubits: usize) -> Self {
        Self { bits: (1 << 63) }
    }

    fn into_bytes(&self) -> Vec<u8> {
        self.bits.to_be_bytes().to_vec()
    }
}

impl AtomicBasisIdx<BasisIdx64> for AtomicU64 {
    fn empty_key(num_qubits: usize) -> Self {
        Self::new(BasisIdx64::empty_key(num_qubits).into_u64())
    }

    fn load(&self) -> BasisIdx64 {
        BasisIdx64 {
            bits: self.load(Ordering::Relaxed),
        }
    }

    fn compare_exchange(&self, current: BasisIdx64, new: BasisIdx64) -> Result<BasisIdx64, ()> {
        match self.compare_exchange(
            current.into_u64(),
            new.into_u64(),
            Ordering::SeqCst,
            Ordering::Acquire,
        ) {
            Ok(v) => Ok(BasisIdx64 { bits: v }),
            Err(_) => Err(()),
        }
    }
}

impl BasisIdx64 {
    fn into_u64(self) -> u64 {
        self.bits
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get() {
        let bidx = BasisIdx64 { bits: 0b1010 };
        assert!(!bidx.get(0));
        assert!(bidx.get(1));
        assert!(!bidx.get(2));
        assert!(bidx.get(3));
    }

    #[test]
    fn test_flip() {
        let bidx = BasisIdx64 { bits: 0b1010 };
        let bidx = bidx.flip(0);
        assert_eq!(bidx.bits, 0b1011);
    }

    #[test]
    fn test_zeros() {
        let bidx = BasisIdx64::zeros();
        assert_eq!(bidx.bits, 0);
    }

    #[test]
    fn test_set() {
        let bidx = BasisIdx64::zeros();
        let bidx1 = bidx.set(0);
        assert_eq!(bidx1.bits, 1);

        let bidx2 = bidx.set(2);
        assert_eq!(bidx2.bits, 4);
    }

    #[test]
    fn test_unset() {
        let bidx = BasisIdx64 { bits: 0b1010 };
        let bidx = bidx.unset(0);
        assert_eq!(bidx.bits, 0b1010);

        let bidx = bidx.unset(1);
        assert_eq!(bidx.bits, 0b1000);
    }

    #[test]
    fn test_swap() {
        let bidx = BasisIdx64 { bits: 0b1010 };
        let bidx = bidx.swap(0, 1);
        assert_eq!(bidx.bits, 0b1001);
    }
}
