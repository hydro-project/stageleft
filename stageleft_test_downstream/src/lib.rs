#[cfg(test)]
mod tests {
    use stageleft_test::using_rand;

    #[test]
    fn test_quoted_using_dependency() {
        let _ = using_rand!();
    }
}
