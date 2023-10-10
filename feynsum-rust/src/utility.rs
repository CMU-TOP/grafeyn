use crate::types::{constants, Complex, Real};

#[macro_export]
macro_rules! profile {
    ($($exp:expr)+) => {
        {
            let _instant = std::time::Instant::now();
            let _result = {
                $($exp)+
            };
            let _duration = _instant.elapsed();

            (_duration, _result)
        }
    }
}

pub fn is_real_zero(x: Real) -> bool {
    x.abs() < constants::ZERO_THRESHOLD
}

pub fn is_zero(c: Complex) -> bool {
    is_real_zero(c.re) && is_real_zero(c.im)
}

pub fn is_nonzero(c: Complex) -> bool {
    !is_real_zero(c.re) || !is_real_zero(c.im)
}

pub fn unpack_complex(num: u64) -> Complex {
    let [re, im]: [f32; 2] = bytemuck::cast(num);
    Complex::new(re, im)
}

pub fn pack_complex(c: Complex) -> u64 {
    let Complex { re, im } = c;
    bytemuck::cast([re, im])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_profile() {
        fn f(x: i32, y: i32) -> i32 {
            (0..y).into_iter().map(|_| x).sum()
        }

        let (duration, result) = profile!(f(2, 3));
        assert_eq!(result, 6);
        assert!(duration.as_nanos() > 0);
    }

    #[test]
    fn test_is_zero() {
        assert!(is_zero(Complex::new(0.0, 0.0)));
        assert!(is_zero(Complex::new(0.0, constants::ZERO_THRESHOLD / 2.0)));
        assert!(is_zero(Complex::new(constants::ZERO_THRESHOLD / 2.0, 0.0)));
        assert!(is_zero(Complex::new(
            constants::ZERO_THRESHOLD / 2.0,
            constants::ZERO_THRESHOLD / 2.0
        )));
    }
}
