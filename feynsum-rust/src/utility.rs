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

fn is_real_zero(x: Real) -> bool {
    x.abs() < constants::ZERO_THRESHOLD
}
pub fn is_zero(c: Complex) -> bool {
    is_real_zero(c.re) && is_real_zero(c.im)
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_profile() {
        fn f(x: i32, y: i32) -> i32 {
            (0..y).into_iter().map(|_| x).sum()
        }

        let (duration, result) = profile!(f(2, 3));
        assert_eq!(result, 6);
        assert!(duration.as_nanos() > 0);
    }
}
