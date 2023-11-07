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
}

pub use internal::matmul;
