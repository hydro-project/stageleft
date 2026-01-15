#[stageleft::export(PubKey, NotPubKey)]
slotmap::new_key_type! {
    pub struct PubKey;
    pub(crate) struct NotPubKey;
}

fn main() {}
