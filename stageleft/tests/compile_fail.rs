#[test]
fn test_all() {
    let t = trybuild::TestCases::new();
    let path = "tests/compile-fail/*.rs";
    t.compile_fail(path);
}
