pub trait Property: Sized {
    type Root;
    fn make_root(target: &mut Option<Self>) -> Self::Root;
}

impl Property for () {
    type Root = ();
    fn make_root(_target: &mut Option<Self>) -> Self::Root {}
}
