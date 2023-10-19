use num::complex;
use atomic_float::AtomicF32;

pub type QubitIndex = usize;
pub type GateIndex = usize;
pub type Real = f32;
pub type Complex = complex::Complex<Real>;

// used to store weights in the concurrent hash table
pub type AtomicReal = AtomicF32;
pub type AtomicComplex = (AtomicReal, AtomicReal);

pub mod constants {
    pub const RECP_SQRT_2: super::Real = std::f32::consts::FRAC_1_SQRT_2;
    pub const ZERO_THRESHOLD: super::Real = 0.00000001;
}

pub mod basis_idx;

pub use basis_idx::BasisIdx;
