// futhark_lib.rs file is dirty so we define a wrapper here
mod internal {
    #![allow(warnings)]
    include!(concat!(env!("OUT_DIR"), "/futhark_lib.rs"));
    use crate::simulator::dense_simulator::UnitaryMatrix;
    use crate::types::Complex;

    pub fn apply_vec(s: Vec<Complex>, unitary: UnitaryMatrix) -> Vec<Complex> {
        let ctx = Context::new().unwrap();
        let n = s.len();
        let q = unitary.qubit_indices.len();
        log::debug!("n: {}, q: {}", n, q);
        assert!(unitary.mat.nrows() == 1 << q && unitary.mat.ncols() == 1 << q);

        let s = ArrayF32D2::new(
            &ctx,
            [n as i64, 2],
            s.iter().flat_map(|c| [c.re, c.im]).collect::<Vec<f32>>(),
        )
        .unwrap();
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

    fn flatten(unitary: &UnitaryMatrix) -> Vec<f32> {
        unitary
            .mat
            .row_iter()
            .flat_map(|row| row.iter().flat_map(|c| [c.re, c.im]).collect::<Vec<_>>())
            .collect()
    }
}

pub use internal::apply_vec;
