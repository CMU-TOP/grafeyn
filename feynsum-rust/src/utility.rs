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
