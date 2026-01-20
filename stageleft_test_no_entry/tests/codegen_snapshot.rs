#[test]
fn test_lib_pub() {
    insta::assert_snapshot!(include_str!(concat!(
        env!("OUT_DIR"),
        stageleft::PATH_SEPARATOR!(),
        "lib_pub.rs"
    )));
}

#[test]
fn test_staged_deps() {
    insta::assert_snapshot!(include_str!(concat!(
        env!("OUT_DIR"),
        stageleft::PATH_SEPARATOR!(),
        "staged_deps.rs"
    )));
}
