// futhark_lib.rs file is dirty so we define a wrapper here
mod internal {
    #![allow(warnings)]
    include!(concat!(env!("OUT_DIR"), "/futhark_lib.rs"));
    use crate::types::Complex;

    fn flatten(a: Vec<Vec<Complex>>) -> Vec<f32> {
        a.into_iter().flatten().flat_map(|c| [c.re, c.im]).collect()
    }

    pub fn matmul(a: Vec<Vec<Complex>>, b: Vec<Vec<Complex>>) -> Vec<Vec<Complex>> {
        let ctx = Context::new().unwrap();
        let n = a.len();
        let m = b.len();
        let p = b[0].len();
        log::debug!("n: {}, m: {}, p: {}", n, m, p);
        log::debug!("a[0].len(): {}", a[0].len());
        assert!(m == a[0].len());

        let a = ArrayF32D3::new(&ctx, [n as i64, m as i64, 2], &flatten(a)).unwrap();
        let b = ArrayF32D3::new(&ctx, [m as i64, p as i64, 2], &flatten(b)).unwrap();

        let linearized: Vec<f32> = ctx.matmul(&a, &b).unwrap().get().unwrap();
        assert!(linearized.len() == 2 * n * p);

        let result: Vec<Vec<Complex>> = linearized
            .chunks(2 * p)
            .map(|row| {
                row.chunks(2)
                    .map(|c| Complex::new(c[0], c[1]))
                    .collect::<Vec<Complex>>()
            })
            .collect();

        assert!(result.len() == n);
        result
    }

    pub fn apply_vec(
        s: Vec<Complex>,
        mat: Vec<Vec<Complex>>,
        qubit_indices: Vec<usize>,
    ) -> Vec<Complex> {
        let ctx = Context::new().unwrap();
        let n = s.len();
        let q = qubit_indices.len();
        log::debug!("n: {}, q: {}", n, q);
        assert!(mat.len() == 1 << q);
        assert!(mat[0].len() == 1 << q);

        let s = ArrayF32D2::new(
            &ctx,
            [n as i64, 2],
            s.iter().flat_map(|c| [c.re, c.im]).collect::<Vec<f32>>(),
        )
        .unwrap();
        let mat = ArrayF32D3::new(&ctx, [1 << q as i64, 1 << q as i64, 2], &flatten(mat)).unwrap();
        let qubit_indices = ArrayI64D1::new(
            &ctx,
            [q as i64],
            qubit_indices
                .iter()
                .map(|&i| i as i64)
                .collect::<Vec<i64>>(),
        )
        .unwrap();

        let linearized = ctx
            .apply_vec(&s, &mat, &qubit_indices)
            .unwrap()
            .get()
            .unwrap();
        assert!(linearized.len() == 2 * n);

        let result: Vec<Complex> = linearized
            .chunks(2)
            .map(|c| Complex::new(c[0], c[1]))
            .collect();

        assert!(result.len() == n);
        result
    }
}

pub use internal::{apply_vec, matmul};
