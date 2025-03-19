use stageleft::{BorrowBounds, Quoted, q};

struct PrivateStruct {
    a: u32,
}

pub struct PublicStruct {
    // TODO(shadaj): right now, public structs must have public fields
    // because otherwise they may not be visible at splice time.
    // This should be documented and ideally have some tooling support.
    #[allow(clippy::allow_attributes, dead_code, reason = "// TODO(shadaj)")]
    pub a: u32,
}

#[stageleft::entry]
pub fn private_struct(_ctx: BorrowBounds<'_>) -> impl Quoted<u32> {
    q!({
        let my_struct = PrivateStruct { a: 1 };
        my_struct.a
    })
}

#[stageleft::entry]
pub fn public_struct(_ctx: BorrowBounds<'_>) -> impl Quoted<PublicStruct> {
    q!(PublicStruct { a: 1 })
}

fn my_local_function() -> bool {
    true
}

#[stageleft::entry]
pub fn self_path<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, bool> {
    q!(self::my_local_function())
}

pub mod subsubmodule {
    use stageleft::{BorrowBounds, Quoted, q};

    #[stageleft::entry]
    pub fn super_path<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, i32> {
        q!(super::super::GLOBAL_VAR)
    }

    #[stageleft::entry]
    pub fn self_super_path<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, i32> {
        q!(self::super::super::GLOBAL_VAR)
    }
}
