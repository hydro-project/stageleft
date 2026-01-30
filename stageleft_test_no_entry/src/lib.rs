stageleft::stageleft_no_entry_crate!();

#[stageleft::export(MyKey)]
slotmap::new_key_type! {
    /// An item generated within a macro.
    pub struct MyKey;
}

/// Test that `stageleft::export` prevents splitbrain of `MyKey` type.
#[expect(dead_code, reason = "test code")]
fn splitbrain(st: SplitbrainStruct) {
    // This gets turned into `crate::__staged::MyKey`
    let _key: MyKey = st.my_key;
}

pub struct SplitbrainStruct {
    /// This stays as regular `MyKey` (equiv. to `crate::MyKey`).
    my_key: MyKey,
}

#[cfg(stageleft_runtime)]
#[expect(dead_code, reason = "test code")]
struct ThisShouldBeRemoved;

#[expect(dead_code, reason = "test code")]
pub mod public {
    #[cfg(stageleft_runtime)]
    struct ThisShouldAlsoBeRemoved;

    pub fn f() {}
    fn g() {}

    #[expect(clippy::module_inception, reason = "test code")]
    pub mod public {
        pub fn f() {}
        fn g() {}
    }

    mod private {
        pub fn f() {}
        fn g() {}
    }
}

#[expect(dead_code, reason = "test code")]
mod private {
    pub fn f() {}
    fn g() {}

    pub mod public {
        pub fn f() {}
        fn g() {}
    }

    #[expect(clippy::module_inception, reason = "test code")]
    mod private {
        pub fn f() {}
        fn g() {}
    }
}
