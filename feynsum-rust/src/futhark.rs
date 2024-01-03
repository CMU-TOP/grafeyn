// futhark_lib.rs file is dirty so we define a wrapper here
mod internal {
    #![allow(warnings)]
    include!(concat!(env!("OUT_DIR"), "/futhark_lib.rs"));
    use crate::circuit::UnitaryMatrix;
    use crate::types::Complex;

    pub fn create_context() -> Context {
        Context::new().unwrap()
    }

    pub struct FutharkVector<'a> {
        vec: ArrayF32D2<'a>,
    }

    impl<'a> FutharkVector<'a> {
        pub fn new(ctx: &'a Context, vec: Vec<Complex>) -> Self {
            let vec = ArrayF32D2::new(
                ctx,
                [vec.len() as i64, 2],
                vec.iter().flat_map(|c| [c.re, c.im]).collect::<Vec<f32>>(),
            )
            .unwrap();
            FutharkVector { vec }
        }

        pub fn dim(&self) -> usize {
            self.vec.shape[0] as usize
        }

        pub fn into_vec(self) -> Vec<Complex> {
            let linearized = self.vec.get().unwrap();
            assert!(linearized.len() == 2 * self.dim());

            linearized
                .chunks(2)
                .map(|c| Complex::new(c[0], c[1]))
                .collect()
        }
    }

    pub struct FutharkMatrix<'a> {
        mat: ArrayF32D3<'a>,
    }

    impl<'a> FutharkMatrix<'a> {
        pub fn new(ctx: &'a Context, mat: Vec<Vec<Complex>>) -> Self {
            let dim = mat.len();
            assert!(dim == mat[0].len());

            let mat = ArrayF32D3::new(
                ctx,
                [dim as i64, dim as i64, 2],
                mat.iter()
                    .flat_map(|row| row.iter().flat_map(|c| [c.re, c.im]).collect::<Vec<_>>())
                    .collect::<Vec<_>>(),
            )
            .unwrap();
            FutharkMatrix { mat }
        }

        pub fn dim(&self) -> usize {
            self.mat.shape[0] as usize
        }
    }

    pub fn apply_vec<'a>(
        ctx: &'a Context,
        state: FutharkVector,
        unitary: UnitaryMatrix,
    ) -> FutharkVector<'a> {
        let n = state.dim();
        let q = unitary.qubit_indices.len();
        assert!(unitary.mat.nrows() == 1 << q && unitary.mat.ncols() == 1 << q);

        let mat =
            ArrayF32D3::new(&ctx, [1 << q as i64, 1 << q as i64, 2], &flatten(&unitary)).unwrap();
        let qubit_indices = ArrayI64D1::new(
            &ctx,
            [q as i64],
            unitary
                .qubit_indices
                .iter()
                .map(|&i| i as i64)
                .collect::<Vec<i64>>(),
        )
        .unwrap();

        FutharkVector {
            vec: ctx.apply_vec(&state.vec, &mat, &qubit_indices).unwrap(),
        }
    }

    fn flatten(unitary: &UnitaryMatrix) -> Vec<f32> {
        unitary
            .mat
            .row_iter()
            .flat_map(|row| row.iter().flat_map(|c| [c.re, c.im]).collect::<Vec<_>>())
            .collect()
    }
}

pub use internal::{apply_vec, create_context, Context, FutharkVector};
