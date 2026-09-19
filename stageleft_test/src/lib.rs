#![cfg_attr(stageleft_macro, allow(dead_code, reason = "test code"))]
stageleft::stageleft_crate!(stageleft_test_macro);

use stageleft::{BorrowBounds, IntoQuotedOnce, Quoted, RuntimeData, q};

pub(crate) mod features;
pub(crate) mod property_example;
pub(crate) mod submodule;

#[expect(unused, reason = "for testing ambiguous top-level crate imports")]
use submodule::PublicStruct;

static GLOBAL_VAR: i32 = 42;

mod private {
    type SomeType = i32;

    #[expect(dead_code, unused_qualifications, reason = "test code")]
    fn function_using_absolute_type_path(
        _xyz: Option<crate::private::SomeType>,
    ) -> crate::private::SomeType {
        123
    }

    mod extra_private {
        #[expect(dead_code, reason = "test code")]
        pub struct PubInsideExtraPrivate;
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
pub fn self_path_at_root<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, bool> {
    q!(self::my_top_level_function())
}

#[stageleft::entry]
pub fn use_rename<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, usize> {
    q!({
        use std::collections::HashSet as MySet;
        let mut set = MySet::new();
        set.insert(123);
        set.len()
    })
}

#[stageleft::entry]
pub fn use_crate_path<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, bool> {
    q!({
        use crate::my_top_level_function as renamed_function;
        renamed_function()
    })
}

#[stageleft::entry]
fn captured_closure<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, bool> {
    let closure = q!(|| true);
    q!({
        let closure = closure;
        closure()
    })
}

#[cfg(feature = "once_cell_feature")]
#[stageleft::entry]
pub fn using_once_cell(_ctx: BorrowBounds<'_>) -> impl Quoted<'_, i32> {
    q!(*once_cell::sync::Lazy::force(&once_cell::sync::Lazy::new(
        || 42
    )))
}

#[expect(dead_code, reason = "test code")]
fn ref_str<'a>(s: &str) -> impl Quoted<'a, &'static str> {
    q!(s)
}

#[stageleft::entry]
fn captured_primitives<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, (bool, char, f32, f64)> {
    let b = true;
    let c = '\u{1F600}';
    let f_32 = 2.5f32;
    let f_64 = -1.5f64;
    q!((b, c, f_32, f_64))
}

#[stageleft::entry]
fn captured_bool_in_closure<'a>(
    _ctx: BorrowBounds<'a>,
    x: RuntimeData<i32>,
) -> impl Quoted<'a, i32> {
    let debug_mode = true;
    q!((move |x: i32| if debug_mode { x } else { 0 })(x))
}

#[stageleft::entry]
fn captured_nonfinite_floats<'a>(_ctx: BorrowBounds<'a>) -> impl Quoted<'a, (f32, f64, f64)> {
    let nan = f32::NAN;
    let inf = f64::INFINITY;
    let neg_inf = f64::NEG_INFINITY;
    q!((nan, inf, neg_inf))
}

#[stageleft::entry]
fn captured_time<'a>(
    _ctx: BorrowBounds<'a>,
) -> impl Quoted<
    'a,
    (
        std::time::Duration,
        std::time::SystemTime,
        std::time::SystemTime,
    ),
> {
    let dur = std::time::Duration::new(123, 456);
    // Note windows only supports 100 nanosecond precision (2nd arg): https://doc.rust-lang.org/std/time/struct.SystemTime.html#platform-specific-behavior
    let after_epoch = std::time::UNIX_EPOCH + std::time::Duration::new(1_700_000_000, 200);
    let before_epoch = std::time::UNIX_EPOCH - std::time::Duration::new(5, 500);
    q!((dur, after_epoch, before_epoch))
}

#[stageleft::entry]
fn literal_args<'a>(
    _ctx: BorrowBounds<'a>,
    flag: bool,
    ch: char,
    factor: f64,
) -> impl Quoted<'a, (bool, char, f64)> {
    let doubled = factor * 2.0;
    q!((flag, ch, doubled))
}

pub(crate) mod backtrace_test;

#[cfg(stageleft_runtime)]
#[cfg(test)]
mod tests {
    use super::*;
    use stageleft::QuotedWithContext;
    use stageleft::internal::syn;

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
    fn test_self_path_at_root() {
        assert!(self_path_at_root!());
    }

    #[test]
    fn test_use_rename() {
        assert_eq!(use_rename!(), 1);
    }

    #[test]
    fn test_use_crate_path() {
        assert!(use_crate_path!());
    }

    #[test]
    fn test_use_crate_path_in_test_module() {
        // This tests the fallback path (test module) with a `use` statement that
        // has a relative path prefix
        let quoted = q!({
            use crate::my_top_level_function as renamed_function;
            renamed_function()
        });
        let expr = quoted.splice_untyped_ctx(&());
        let file: syn::File = syn::parse_quote!(fn main() { #expr });
        insta::assert_snapshot!(prettyplease::unparse(&file));
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
    fn test_captured_primitives() {
        assert_eq!(captured_primitives!(), (true, '\u{1F600}', 2.5f32, -1.5f64));
    }

    #[test]
    fn test_captured_bool_in_closure() {
        assert_eq!(captured_bool_in_closure!(7), 7);
    }

    #[test]
    fn test_captured_nonfinite_floats() {
        let (nan, inf, neg_inf) = captured_nonfinite_floats!();
        assert!(nan.is_nan());
        assert_eq!(inf, f64::INFINITY);
        assert_eq!(neg_inf, f64::NEG_INFINITY);
    }

    #[test]
    fn test_captured_time() {
        let (dur, after_epoch, before_epoch) = captured_time!();
        assert_eq!(dur, std::time::Duration::new(123, 456));
        // Note windows only supports 100 nanosecond precision (2nd arg): https://doc.rust-lang.org/std/time/struct.SystemTime.html#platform-specific-behavior
        assert_eq!(
            after_epoch,
            std::time::UNIX_EPOCH + std::time::Duration::new(1_700_000_000, 200)
        );
        assert_eq!(
            before_epoch,
            std::time::UNIX_EPOCH - std::time::Duration::new(5, 500)
        );
    }

    #[test]
    fn test_literal_args() {
        assert_eq!(literal_args!(true, 'x', 2.5), (true, 'x', 5.0f64));
    }

    #[test]
    fn test_literal_args_negative_and_parenthesized() {
        assert_eq!(literal_args!(false, 'x', -2.5), (false, 'x', -5.0f64));
        assert_eq!(literal_args!(true, 'x', (1.25)), (true, 'x', 2.5f64));
    }

    #[test]
    fn test_submodule_private_struct() {
        let result = submodule::private_struct!();
        assert_eq!(result, 1);
    }

    #[test]
    fn test_submodule_public_struct() {
        #[expect(unused_qualifications, reason = "don't want to use super import")]
        let result: super::submodule::PublicStruct = submodule::public_struct!();
        assert_eq!(result.a, 1);
    }

    #[cfg(feature = "once_cell_feature")]
    #[test]
    fn test_using_once_cell() {
        let result = using_once_cell!();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_quoting() {
        let quoted = q!(1 + 2);
        let _ = quoted.splice_typed_ctx(&());
    }

    #[test]
    #[ignore = "requires rustc --remap-path-prefix; exercised by CI"]
    fn test_quote_macro_name_is_stable_under_path_remapping() {
        assert_eq!(
            file!(),
            "cache-workspace/stageleft_test/src/lib.rs",
            "the test must be compiled with the expected source-path remapping"
        );

        let expr = QuotedWithContext::splice_untyped_ctx(
            crate_paths(stageleft::QuotedContext::create()),
            &(),
        );
        let file: syn::File = syn::parse_quote!(fn main() { #expr });
        let rendered = prettyplease::unparse(&file);

        assert!(
            rendered.contains("__stageleft_quote_src_lib_rs_"),
            "macro name should use the physical crate-relative path:\n{rendered}"
        );
        assert!(
            !rendered.contains("cache_workspace_stageleft_test"),
            "macro name must not use the remapped display path:\n{rendered}"
        );
    }

    #[test]
    fn test_splice_snapshot_simple() {
        let quoted = q!(1 + 2);
        let expr = quoted.splice_untyped_ctx(&());
        let file: syn::File = syn::parse_quote!(fn main() { #expr });
        insta::assert_snapshot!(prettyplease::unparse(&file));
    }

    #[test]
    fn test_splice_snapshot_free_var() {
        let x = 42i32;
        let quoted = q!(x + 1);
        let expr = quoted.splice_untyped_ctx(&());
        let file: syn::File = syn::parse_quote!(fn main() { #expr });
        insta::assert_snapshot!(prettyplease::unparse(&file));
    }

    #[test]
    fn test_splice_snapshot_free_var_captures() {
        let b = false;
        let c = 'q';
        let f = 1.5f64;
        // Use an explicit bit pattern (the canonical quiet NaN, which is what
        // `f32::NAN` is in practice) rather than `f32::NAN` itself, since the
        // exact bit pattern of `f32::NAN` is not guaranteed to be stable
        // across platforms/toolchains and would make this snapshot brittle.
        let nan = f32::from_bits(0x7FC00000);
        let dur = std::time::Duration::from_millis(1500);
        let before_epoch = std::time::UNIX_EPOCH - std::time::Duration::new(5, 500);
        // Note windows only supports 100 nanosecond precision (2nd arg): https://doc.rust-lang.org/std/time/struct.SystemTime.html#platform-specific-behavior
        let after_epoch = std::time::UNIX_EPOCH + std::time::Duration::new(1_700_000_000, 200);
        let quoted = q!((b, c, f, nan, dur, before_epoch, after_epoch));
        let expr = quoted.splice_untyped_ctx(&());
        let file: syn::File = syn::parse_quote!(fn main() { #expr });
        insta::assert_snapshot!(prettyplease::unparse(&file));
    }

    #[test]
    fn test_splice_snapshot_crate_path() {
        let expr = QuotedWithContext::splice_untyped_ctx(
            crate_paths(stageleft::QuotedContext::create()),
            &(),
        );
        let file: syn::File = syn::parse_quote!(fn main() { #expr });
        insta::assert_snapshot!(prettyplease::unparse(&file));
    }

    #[test]
    fn test_splice_snapshot_fnmut2_borrow_mut() {
        let quoted = q!(|state: &mut i32, item: i32| *state += item);
        let expr = quoted.splice_fnmut2_borrow_mut_ctx(&());
        let file: syn::File = syn::parse_quote!(fn main() { #expr });
        insta::assert_snapshot!(prettyplease::unparse(&file));
    }

    #[test]
    fn test_crate_path_in_macro() {
        // This tests the fallback path (test module) with a relative path inside a macro call
        let quoted = q!(assert!(crate::my_top_level_function()));
        let expr = quoted.splice_untyped_ctx(&());
        let file: syn::File = syn::parse_quote!(fn main() { #expr });
        insta::assert_snapshot!(prettyplease::unparse(&file));
    }
}
