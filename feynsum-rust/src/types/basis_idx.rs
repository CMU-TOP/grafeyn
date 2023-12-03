use std::{fmt::Display, hash::Hash};

mod basis_idx_64;
mod basis_idx_unlimited;

pub use basis_idx_64::BasisIdx64;
pub use basis_idx_unlimited::BasisIdxUnlimited;

pub trait BasisIdx: Eq + Hash + Sync + Send + Clone + 'static + Display {
    fn get(&self, qi: usize) -> bool;
    fn flip(&self, qi: usize) -> Self;
    fn zeros() -> Self;
    fn set(&self, qi: usize) -> Self;
    fn unset(&self, qi: usize) -> Self;
    fn swap(&self, qi1: usize, qi2: usize) -> Self;
    fn from_idx(idx: usize) -> Self;
    fn as_idx(&self) -> usize;
    fn empty_key(num_qubits: usize) -> Self;
    fn as_bytes(&self) -> Vec<u8>;
}

// represents a type that is used to store a BasisIdx type in a concurrent data
// structure.
pub trait AtomicBasisIdx<B: BasisIdx>: Sync + Send {
    // TODO: Add methods
    fn empty_key(num_qubits: usize) -> Self;
    fn load(&self) -> B;
    fn compare_exchange(&self, current: B, new: B) -> Result<B, ()>;
}
