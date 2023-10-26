use std::{fmt::Display, hash::Hash};

pub const MAX_QUBITS: usize = 63;

mod basis_idx_64;

pub use basis_idx_64::BasisIdx64;

pub trait BasisIdx: Eq + Hash + Sync + Send + Clone + Copy + 'static + Display {
    fn get(&self, qi: usize) -> bool;
    fn flip(&self, qi: usize) -> Self;
    fn zeros() -> Self;
    fn set(&self, qi: usize) -> Self;
    fn unset(&self, qi: usize) -> Self;
    fn swap(&self, qi1: usize, qi2: usize) -> Self;
    fn from_idx(idx: usize) -> Self;
    fn into_idx(self) -> usize;
    fn into_u64(self) -> u64;
    fn from_u64(u: u64) -> Self;
    fn empty_key() -> Self;
}

// represents a type that is used to store a BasisIdx type in a concurrent data
// structure.
pub trait AtomicBasisIdx<B: BasisIdx>: Sync + Send {
    // TODO: Add methods
    fn empty_key() -> Self;
    fn load(&self) -> B;
    fn compare_exchange(&self, current: B, new: B) -> Result<B, ()>;
}
