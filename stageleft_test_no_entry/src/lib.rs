stageleft::stageleft_no_entry_crate!();

stageleft::stageleft_export!(MyKey, OtherKey);

#[cfg(stageleft_runtime)]
slotmap::new_key_type! {
    /// An item generated within a macro.
    pub struct MyKey;

    /// Just test the macro expansion is delimiting properly.
    pub struct OtherKey;
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
