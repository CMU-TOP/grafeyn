use num::complex;

pub type QubitIndex = usize;
pub type Real = f64;
pub type Complex = complex::Complex<Real>;

pub mod constants {
    pub const RECP_SQRT_2: super::Real = 0.70710678118654752440;
}

pub mod basis_idx;

pub use basis_idx::{BasisIdx, BasisIdxErr};
