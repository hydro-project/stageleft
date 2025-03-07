#![cfg_attr(stageleft_macro, allow(dead_code))]
stageleft::stageleft_crate!(stageleft_test_macro);

use stageleft::{BorrowBounds, IntoQuotedOnce, Quoted, RuntimeData, q};

pub(crate) mod features;
pub(crate) mod submodule;

#[stageleft::entry]
fn raise_to_power(_ctx: BorrowBounds<'_>, value: RuntimeData<i32>, power: u32) -> impl Quoted<i32> {
    if power == 1 {
        q!(value).boxed()
    } else if power % 2 == 0 {
        let half_result = raise_to_power(_ctx, value, power / 2);
        q!({
            let v = half_result;
            v * v
        })
        .boxed()
    } else {
        let half_result = raise_to_power(_ctx, value, power / 2);
        q!({
            let v = half_result;
            (v * v) * value
        })
        .boxed()
    }
}

#[stageleft::entry(bool)]
fn closure_capture_lifetime<'a, I: Copy + Into<u32> + 'a>(
    _ctx: BorrowBounds<'a>,
    v: RuntimeData<I>,
) -> impl Quoted<'a, Box<dyn Fn() -> u32 + 'a>> {
    q!(Box::new(move || { v.into() }) as Box<dyn Fn() -> u32 + 'a>)
}

fn my_top_level_function() -> bool {
    true
}

#[stageleft::entry]
fn crate_paths<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, bool> {
    q!(crate::my_top_level_function())
}

#[stageleft::entry]
fn local_paths<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, bool> {
    q!(my_top_level_function())
}

#[stageleft::entry]
fn captured_closure<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, bool> {
    let closure = q!(|| true);
    q!({
        let closure = closure;
        closure()
    })
}

#[cfg(stageleft_runtime)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_raise_to_power_of_two() {
        let result = raise_to_power!(2, 10);
        assert_eq!(result, 1024);
    }

    #[test]
    fn test_raise_to_odd_power() {
        let result = raise_to_power!(2, 5);
        assert_eq!(result, 32);
    }

    #[test]
    fn test_closure_capture_lifetime() {
        let result = closure_capture_lifetime!(1u8);
        assert_eq!(result(), 1);
    }

    #[test]
    fn test_crate_paths() {
        assert_eq!(crate_paths!(), true);
    }

    #[test]
    fn test_local_paths() {
        assert_eq!(local_paths!(), true);
    }

    #[test]
    fn test_captured_closure() {
        assert_eq!(captured_closure!(), true);
    }

    #[test]
    fn test_submodule_private_struct() {
        let result = submodule::private_struct!();
        assert_eq!(result, 1);
    }

    #[test]
    fn test_submodule_public_struct() {
        let result: submodule::PublicStruct = submodule::public_struct!();
        assert_eq!(result.a, 1);
    }
}
