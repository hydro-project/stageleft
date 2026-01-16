fn main() {
    // Hack to make sure stageleft actually builds code.
    unsafe { std::env::set_var("STAGELEFT_TRYBUILD_BUILD_STAGED", "1") };
    stageleft_tool::gen_final!();
}
