/// A "property builder", which can accumulate annotations on a quoted snippet through chained
/// function calls that may change the builder's type.
pub trait Property: Sized {
    /// A version of this property with all the annotations removed, on which annotations will be applied.
    type Root;

    /// Constructs a root instance of the property with all annotations removed.
    fn make_root(target: &mut Option<Self>) -> Self::Root;
}

impl Property for () {
    type Root = ();
    fn make_root(_target: &mut Option<Self>) -> Self::Root {}
}
