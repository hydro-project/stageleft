use std::marker::PhantomData;

use stageleft::{
    BorrowBounds, IntoQuotedMut, Quoted, QuotedWithContextWithProps, properties::Property, q,
};

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
pub fn private_struct(_ctx: BorrowBounds<'_>) -> impl Quoted<'_, u32> {
    q!({
        let my_struct = PrivateStruct { a: 1 };
        my_struct.a
    })
}

#[stageleft::entry]
pub fn public_struct(_ctx: BorrowBounds<'_>) -> impl Quoted<'_, PublicStruct> {
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

pub struct PropertiesType<Abc = ()>(PhantomData<Abc>);

impl PropertiesType<()> {
    pub fn lol(self, _x: bool) -> PropertiesType<String> {
        PropertiesType(PhantomData)
    }
}

impl<Abc> Property for PropertiesType<Abc> {
    type Root = PropertiesType<()>;

    fn make_root(_target: &mut Option<Self>) -> Self::Root {
        PropertiesType(PhantomData)
    }
}

fn needing_properties<'a>(f: impl IntoQuotedMut<'a, i32, (), PropertiesType<String>>) {
    let (_, _props) = f.splice_typed_ctx_props(&());
}

#[allow(unused, reason = "compilation test")]
fn test_properties() {
    needing_properties(q!(123, lol = true));
}
