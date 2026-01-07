use std::marker::PhantomData;

use stageleft::{IntoQuotedMut, QuotedWithContextWithProps, properties::Property, q};

pub struct PropertiesType<Abc = ()>(PhantomData<Abc>);

impl PropertiesType<()> {
    pub fn example_property(self, _x: bool) -> PropertiesType<String> {
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
    needing_properties(q!(123, example_property = true));
}
