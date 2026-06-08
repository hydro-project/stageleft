#[test]
fn test_all() {
    // Set STAGELEFT_FINAL_CRATE_MANIFEST_DIR so q!() generates portable macro names
    unsafe {
        std::env::set_var(
            "STAGELEFT_FINAL_CRATE_MANIFEST_DIR",
            env!("CARGO_MANIFEST_DIR"),
        );
    }
    let t = trybuild::TestCases::new();
    let path = "tests/compile-fail/*.rs";
    t.compile_fail(path);
}
