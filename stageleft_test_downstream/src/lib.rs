#[cfg(test)]
mod tests {
    use stageleft_test::{crate_paths, using_rand};

    #[test]
    fn test_quoted_using_dependency() {
        let _ = using_rand!();
    }

    #[test]
    fn test_quoted_using_crate() {
        let _ = crate_paths!();
    }
}
