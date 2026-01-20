stageleft::stageleft_no_entry_crate!();

#[stageleft::export(MyKey)]
slotmap::new_key_type! {
    /// An item generated within a macro.
    pub struct MyKey;
}

/// Test that `stageleft::export` prevents splitbrain of `MyKey` type.
#[allow(dead_code)]
fn splitbrain(st: SplitbrainStruct) {
    // This gets turned into `crate::__staged::MyKey`
    let _key: MyKey = st.my_key;
}

pub struct SplitbrainStruct {
    /// This stays as regular `MyKey` (equiv. to `crate::MyKey`).
    my_key: MyKey,
}

#[cfg(stageleft_runtime)]
#[expect(dead_code)]
struct ThisShouldBeRemoved;

pub mod pub_mod {
    #[cfg(stageleft_runtime)]
    #[expect(dead_code)]
    struct ThisShouldAlsoBeRemoved;
}
