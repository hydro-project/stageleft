#[cfg(test)]
mod tests {
    use stageleft_test::{crate_paths, using_once_cell, using_rand};

    #[test]
    fn test_quoted_using_dependency() {
        let _ = using_rand!();
    }

    #[test]
    fn test_quoted_using_crate() {
        let _ = crate_paths!();
    }

    #[test]
    fn test_quoted_using_optional_dep() {
        let result = using_once_cell!();
        assert_eq!(result, 42);
    }
}
