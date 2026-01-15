stageleft::stageleft_export!(PubKey, NotPubKey);

#[cfg(stageleft_runtime)]
slotmap::new_key_type! {
    pub struct PubKey;
    pub(crate) struct NotPubKey;
}

fn main() {}
