#![cfg_attr(stageleft_macro, allow(dead_code))]
stageleft::stageleft_crate!(stageleft_test_macro);

use stageleft::{BorrowBounds, IntoQuotedOnce, Quoted, RuntimeData, q};

pub(crate) mod features;
pub(crate) mod property_example;
pub(crate) mod submodule;

static GLOBAL_VAR: i32 = 42;

mod private {
    type SomeType = i32;

    #[allow(dead_code)]
    fn function_using_absolute_type_path(
        _xyz: Option<crate::private::SomeType>,
    ) -> crate::private::SomeType {
        123
    }
}

#[stageleft::entry]
pub fn using_global_var(_ctx: BorrowBounds<'_>) -> impl Quoted<'_, i32> {
    q!(GLOBAL_VAR)
}

#[stageleft::entry]
pub fn using_rand(_ctx: BorrowBounds<'_>) -> impl Quoted<'_, i32> {
    q!(rand_alias::random::<i32>())
}

#[stageleft::entry]
fn raise_to_power(
    _ctx: BorrowBounds<'_>,
    value: RuntimeData<i32>,
    power: u32,
) -> impl Quoted<'_, i32> {
    if power == 1 {
        q!(value).boxed()
    } else if power.is_multiple_of(2) {
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
pub fn crate_paths<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, bool> {
    q!(crate::my_top_level_function())
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
        assert!(crate_paths!());
    }

    #[test]
    fn test_local_paths() {
        assert!(submodule::self_path!());
    }

    #[test]
    fn test_super_paths() {
        assert!(submodule::subsubmodule::super_path!() == 42);
    }

    #[test]
    fn test_self_super_paths() {
        assert!(submodule::subsubmodule::self_super_path!() == 42);
    }

    #[test]
    fn test_captured_closure() {
        assert!(captured_closure!());
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
