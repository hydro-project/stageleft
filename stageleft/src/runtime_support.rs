use std::marker::PhantomData;

use proc_macro2::{Span, TokenStream};
use quote::quote;

/// A generic function that panics when called. Used by `uninitialized()` to
/// return a `fn() -> T` without constructing a `T`.
#[doc(hidden)]
pub fn panicking_uninit_value<T>() -> T {
    panic!("stageleft: tried to use uninitialized free variable")
}

pub struct QuoteTokens {
    pub prelude: Option<TokenStream>,
    pub expr: Option<TokenStream>,
}

pub fn get_final_crate_name(crate_name: &str) -> TokenStream {
    let final_crate = proc_macro_crate::crate_name(crate_name).unwrap_or_else(|_| {
        panic!("Expected consumer `{crate_name}` package to be present in `Cargo.toml`")
    });

    match final_crate {
        proc_macro_crate::FoundCrate::Itself => {
            if std::env::var("CARGO_BIN_NAME").is_ok() {
                let underscored = crate_name.replace('-', "_");
                let underscored_ident = syn::Ident::new(&underscored, Span::call_site());
                quote! { #underscored_ident }
            } else {
                quote! { crate }
            }
        }
        proc_macro_crate::FoundCrate::Name(name) => {
            let ident = syn::Ident::new(&name, Span::call_site());
            quote! { #ident }
        }
    }
}

thread_local! {
    pub(crate) static MACRO_TO_CRATE: std::cell::RefCell<Option<(String, String)>> = const { std::cell::RefCell::new(None) };
}

pub fn set_macro_to_crate(macro_name: impl Into<String>, crate_name: impl Into<String>) {
    MACRO_TO_CRATE.with(|cell| {
        *cell.borrow_mut() = Some((macro_name.into(), crate_name.into()));
    });
}

static TEST_MODULES: std::sync::RwLock<Vec<(&'static str, &'static [&'static str])>> =
    std::sync::RwLock::new(Vec::new());

/// Register test module paths for a crate. Called from ctor in generated code.
pub fn register_test_modules(crate_name: &'static str, modules: &'static [&'static str]) {
    TEST_MODULES.write().unwrap().push((crate_name, modules));
}

/// Check if a module path is inside a test module for the given crate.
pub(crate) fn is_test_module(crate_name: &str, module_path: &str) -> bool {
    let guard = TEST_MODULES.read().unwrap();
    for (name, modules) in guard.iter() {
        if *name == crate_name {
            for test_mod in modules.iter() {
                if module_path == *test_mod || module_path.starts_with(&format!("{test_mod}::")) {
                    return true;
                }
            }
        }
    }
    false
}

pub trait ParseFromLiteral {
    fn parse_from_literal(literal: &syn::Expr) -> Self;
}

/// Unwraps an expression that is expected to be a literal, looking through
/// parentheses, invisible groups, and unary negation. Returns the literal
/// along with whether an (odd number of) negation(s) was applied to it.
fn unwrap_literal(expr: &syn::Expr) -> (bool, &syn::Lit) {
    match expr {
        syn::Expr::Lit(syn::ExprLit { lit, .. }) => (false, lit),
        syn::Expr::Paren(syn::ExprParen { expr, .. })
        | syn::Expr::Group(syn::ExprGroup { expr, .. }) => unwrap_literal(expr),
        syn::Expr::Unary(syn::ExprUnary {
            op: syn::UnOp::Neg(_),
            expr,
            ..
        }) => {
            let (negated, lit) = unwrap_literal(expr);
            (!negated, lit)
        }
        _ => panic!(
            "Expected a literal, got `{}`",
            quote!(#expr).to_string().replace(char::is_whitespace, "")
        ),
    }
}

macro_rules! impl_parse_from_literal_numeric {
    ($($ty:ty),*) => {
        $(
            impl ParseFromLiteral for $ty {
                fn parse_from_literal(literal: &syn::Expr) -> Self {
                    let (negated, lit) = unwrap_literal(literal);
                    let digits = match lit {
                        syn::Lit::Int(lit_int) => lit_int.base10_digits(),
                        syn::Lit::Float(lit_float) => lit_float.base10_digits(),
                        _ => panic!(
                            "Expected `{}` literal, got `{}`",
                            stringify!($ty),
                            quote!(#lit)
                        ),
                    };
                    let repr = if negated {
                        format!("-{}", digits)
                    } else {
                        digits.to_string()
                    };
                    repr.parse().unwrap_or_else(|_| {
                        panic!("Literal `{}` cannot be parsed as `{}`", repr, stringify!($ty))
                    })
                }
            }
        )*
    };
}

impl_parse_from_literal_numeric!(i8, i16, i32, i64, i128, isize);
impl_parse_from_literal_numeric!(u8, u16, u32, u64, u128, usize);
impl_parse_from_literal_numeric!(f32, f64);

impl ParseFromLiteral for bool {
    fn parse_from_literal(literal: &syn::Expr) -> Self {
        let (false, syn::Lit::Bool(lit_bool)) = unwrap_literal(literal) else {
            panic!("Expected `bool` literal, got `{}`", quote!(#literal))
        };
        lit_bool.value()
    }
}

impl ParseFromLiteral for char {
    fn parse_from_literal(literal: &syn::Expr) -> Self {
        let (false, syn::Lit::Char(lit_char)) = unwrap_literal(literal) else {
            panic!("Expected `char` literal, got `{}`", quote!(#literal))
        };
        lit_char.value()
    }
}

/// A variant of `FreeVariableWithContext` that also has a properties type parameter.
/// When `Props = ()`, this is equivalent to `FreeVariableWithContext`.
pub trait FreeVariableWithContextWithProps<Ctx, Props> {
    type O;

    fn to_tokens(self, ctx: &Ctx) -> (QuoteTokens, Props)
    where
        Self: Sized;

    fn uninitialized(&self, _ctx: &Ctx) -> fn() -> Self::O {
        panicking_uninit_value::<Self::O>
    }
}

pub trait FreeVariableWithContext<Ctx>: FreeVariableWithContextWithProps<Ctx, ()> {
    fn to_tokens(self, ctx: &Ctx) -> QuoteTokens
    where
        Self: Sized,
    {
        FreeVariableWithContextWithProps::to_tokens(self, ctx).0
    }

    fn uninitialized(
        &self,
        ctx: &Ctx,
    ) -> fn() -> <Self as FreeVariableWithContextWithProps<Ctx, ()>>::O {
        FreeVariableWithContextWithProps::uninitialized(self, ctx)
    }
}

/// Blanket impl: anything implementing FreeVariableWithContextWithProps<Ctx, ()> also implements FreeVariableWithContext
impl<Ctx, T: FreeVariableWithContextWithProps<Ctx, ()>> FreeVariableWithContext<Ctx> for T {}

pub trait FreeVariable<O>: FreeVariableWithContext<(), O = O> {
    fn to_tokens(self) -> QuoteTokens
    where
        Self: Sized,
    {
        FreeVariableWithContextWithProps::to_tokens(self, &()).0
    }

    fn uninitialized(&self) -> fn() -> O {
        panicking_uninit_value::<O>
    }
}

impl<O, T: FreeVariableWithContext<(), O = O>> FreeVariable<O> for T {}

/// Implements free-variable capture for types whose values can be emitted
/// directly as literal tokens (via their [`quote::ToTokens`] implementation).
macro_rules! impl_free_variable_from_literal {
    ($($ty:ty),*) => {
        $(
            impl<Ctx> FreeVariableWithContextWithProps<Ctx, ()> for $ty {
                type O = $ty;

                fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
                    (QuoteTokens {
                        prelude: None,
                        expr: Some(quote!(#self))
                    }, ())
                }
            }

            impl<'a, Ctx> crate::QuotedWithContextWithProps<'a, $ty, Ctx, ()> for $ty {}
        )*
    };
}

impl_free_variable_from_literal!(i8, i16, i32, i64, i128, isize);
impl_free_variable_from_literal!(u8, u16, u32, u64, u128, usize);
impl_free_variable_from_literal!(bool, char);

/// Implements free-variable capture for floats. All values are emitted as a
/// `from_bits` call with the exact bit pattern, e.g.
/// `::core::primitive::f64::from_bits(4609434218613702656u64)` for `1.5f64`.
///
/// This is exact by construction for every value: it preserves the bit
/// pattern of infinities, NaNs (including payloads), and signed zeros, and
/// avoids relying on the round-trip fidelity of float formatting, which is an
/// implementation detail of the standard library rather than a documented
/// guarantee of `quote`/`proc-macro2`/`Display`. (`from_bits` is a stable
/// `const fn`, so the emitted expression is usable in const contexts too.)
macro_rules! impl_free_variable_float {
    ($($ty:ty),*) => {
        $(
            impl<Ctx> FreeVariableWithContextWithProps<Ctx, ()> for $ty {
                type O = $ty;

                fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
                    let bits = self.to_bits();
                    (QuoteTokens {
                        prelude: None,
                        expr: Some(quote!(::core::primitive::$ty::from_bits(#bits)))
                    }, ())
                }
            }

            impl<'a, Ctx> crate::QuotedWithContextWithProps<'a, $ty, Ctx, ()> for $ty {}
        )*
    };
}

impl_free_variable_float!(f32, f64);

impl<Ctx> FreeVariableWithContextWithProps<Ctx, ()> for std::time::Duration {
    type O = std::time::Duration;

    fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
        let secs = self.as_secs();
        let nanos = self.subsec_nanos();
        (
            QuoteTokens {
                prelude: None,
                expr: Some(quote!(::core::time::Duration::new(#secs, #nanos))),
            },
            (),
        )
    }
}

impl<'a, Ctx> crate::QuotedWithContextWithProps<'a, std::time::Duration, Ctx, ()>
    for std::time::Duration
{
}

impl<Ctx> FreeVariableWithContextWithProps<Ctx, ()> for std::time::SystemTime {
    type O = std::time::SystemTime;

    fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
        // A `SystemTime` is anchored to the unix epoch, so it can be quoted as an
        // exact offset (possibly negative) from `UNIX_EPOCH`.
        let (op, duration) = match self.duration_since(std::time::UNIX_EPOCH) {
            Ok(after_epoch) => (quote!(+), after_epoch),
            Err(err) => (quote!(-), err.duration()),
        };
        let secs = duration.as_secs();
        let nanos = duration.subsec_nanos();
        let expr = quote!((::std::time::UNIX_EPOCH #op ::core::time::Duration::new(#secs, #nanos)));
        (
            QuoteTokens {
                prelude: None,
                expr: Some(expr),
            },
            (),
        )
    }
}

impl<'a, Ctx> crate::QuotedWithContextWithProps<'a, std::time::SystemTime, Ctx, ()>
    for std::time::SystemTime
{
}

impl<Ctx> FreeVariableWithContextWithProps<Ctx, ()> for &str {
    type O = &'static str;

    fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
        (
            QuoteTokens {
                prelude: None,
                expr: Some(quote!(#self)),
            },
            (),
        )
    }
}

impl<Ctx> FreeVariableWithContextWithProps<Ctx, ()> for String {
    type O = &'static str;

    fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
        (
            QuoteTokens {
                prelude: None,
                expr: Some(quote!(#self)),
            },
            (),
        )
    }
}

/// Free-variable capture for `Option<T>` where the inner `T` is itself
/// capturable. `Some(v)` splices as `::core::option::Option::Some(<v>)`.
/// `None` splices as `::core::option::Option::<T>::None`, with the inner
/// type named explicitly via [`crate::quote_type`] so that the spliced code
/// does not depend on type inference at the splice site (which could fail,
/// or silently drift to another type via integer fallback). Types that
/// `quote_type` cannot name (e.g. closures) degrade to `_` and fall back to
/// inference.
impl<Ctx, T: FreeVariableWithContextWithProps<Ctx, ()>> FreeVariableWithContextWithProps<Ctx, ()>
    for Option<T>
{
    type O = Option<T::O>;

    fn to_tokens(self, ctx: &Ctx) -> (QuoteTokens, ()) {
        match self {
            Some(inner) => {
                let (tokens, ()) = FreeVariableWithContextWithProps::to_tokens(inner, ctx);
                let expr = tokens.expr.unwrap_or_else(|| {
                    panic!("cannot capture an `Option` whose inner value has no expression")
                });
                (
                    QuoteTokens {
                        prelude: tokens.prelude,
                        expr: Some(quote!(::core::option::Option::Some(#expr))),
                    },
                    (),
                )
            }
            None => {
                let inner_type = crate::quote_type::<T::O>();
                (
                    QuoteTokens {
                        prelude: None,
                        expr: Some(quote!(::core::option::Option::<#inner_type>::None)),
                    },
                    (),
                )
            }
        }
    }
}

impl<'a, Ctx, T: FreeVariableWithContextWithProps<Ctx, ()>>
    crate::QuotedWithContextWithProps<'a, Option<T::O>, Ctx, ()> for Option<T>
{
}

pub struct Import<T> {
    module_path: &'static str,
    crate_name: &'static str,
    path: &'static str,
    as_name: &'static str,
    _phantom: PhantomData<T>,
}

impl<T> Copy for Import<T> {}
impl<T> Clone for Import<T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub fn create_import<T>(
    module_path: &'static str,
    crate_name: &'static str,
    path: &'static str,
    as_name: &'static str,
    _unused_type_check: T,
) -> Import<T> {
    Import {
        module_path,
        crate_name,
        path,
        as_name,
        _phantom: PhantomData,
    }
}

impl<T, Ctx> FreeVariableWithContextWithProps<Ctx, ()> for Import<T> {
    type O = T;

    fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
        let final_crate_root = get_final_crate_name(self.crate_name);

        let module_path = syn::parse_str::<syn::Path>(self.module_path).unwrap();
        let parsed = syn::parse_str::<syn::Path>(self.path).unwrap();
        let as_ident = syn::Ident::new(self.as_name, Span::call_site());
        (
            QuoteTokens {
                prelude: Some(quote!(use #final_crate_root::#module_path::#parsed as #as_ident;)),
                expr: None,
            },
            (),
        )
    }
}

pub fn type_hint<O>(v: O) -> O {
    v
}

pub fn fn0_type_hint<'a, O>(f: impl Fn() -> O + 'a) -> impl Fn() -> O + 'a {
    f
}

pub fn fn1_type_hint<'a, I, O>(f: impl Fn(I) -> O + 'a) -> impl Fn(I) -> O + 'a {
    f
}

pub fn fn1_borrow_type_hint<'a, I, O>(f: impl Fn(&I) -> O + 'a) -> impl Fn(&I) -> O + 'a {
    f
}

pub fn fn2_type_hint<'a, I1, I2, O>(f: impl Fn(I1, I2) -> O + 'a) -> impl Fn(I1, I2) -> O + 'a {
    f
}

pub fn fn2_borrow_type_hint<'a, I1, I2, O>(
    f: impl Fn(&I1, &I2) -> O + 'a,
) -> impl Fn(&I1, &I2) -> O + 'a {
    f
}

pub fn fn2_borrow_mut_type_hint<'a, I1, I2, O>(
    f: impl Fn(&mut I1, I2) -> O + 'a,
) -> impl Fn(&mut I1, I2) -> O + 'a {
    f
}

pub fn fnmut0_type_hint<'a, O>(f: impl FnMut() -> O + 'a) -> impl FnMut() -> O + 'a {
    f
}

pub fn fnmut1_type_hint<'a, I, O>(f: impl FnMut(I) -> O + 'a) -> impl FnMut(I) -> O + 'a {
    f
}

pub fn fnmut1_borrow_type_hint<'a, I, O>(f: impl FnMut(&I) -> O + 'a) -> impl FnMut(&I) -> O + 'a {
    f
}

pub fn fnmut2_type_hint<'a, I1, I2, O>(
    f: impl FnMut(I1, I2) -> O + 'a,
) -> impl FnMut(I1, I2) -> O + 'a {
    f
}

pub fn fnmut2_borrow_type_hint<'a, I1, I2, O>(
    f: impl FnMut(&I1, &I2) -> O + 'a,
) -> impl FnMut(&I1, &I2) -> O + 'a {
    f
}

pub fn fnmut2_borrow_mut_type_hint<'a, I1, I2, O>(
    f: impl FnMut(&mut I1, I2) -> O + 'a,
) -> impl FnMut(&mut I1, I2) -> O + 'a {
    f
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Splices `value` as a float free variable, asserts the emitted tokens
    /// are exactly `::core::primitive::fNN::from_bits(<bits>uNN)`, and
    /// reconstructs the value from those tokens for bit-exact comparison.
    macro_rules! roundtrip_float {
        ($ty:ty, $bits_ty:ty, $value:expr) => {{
            let value: $ty = $value;
            let (tokens, ()) = FreeVariableWithContextWithProps::<(), ()>::to_tokens(value, &());
            let tokens = tokens.expr.unwrap().to_string();
            let expected = format!(
                "::core::primitive::{}::from_bits({}{})",
                stringify!($ty),
                value.to_bits(),
                stringify!($bits_ty),
            );
            assert_eq!(
                tokens.replace(char::is_whitespace, ""),
                expected,
                "unexpected tokens emitted for {} {value:?}",
                stringify!($ty)
            );
            let bits: $bits_ty = tokens
                .split('(')
                .nth(1)
                .unwrap()
                .trim_end_matches(')')
                .trim()
                .trim_end_matches(stringify!($bits_ty))
                .parse()
                .unwrap();
            <$ty>::from_bits(bits)
        }};
    }

    #[test]
    fn float_capture_roundtrip() {
        let f64_values = [
            0.0f64,
            -0.0,
            0.1,
            0.2,
            0.1 + 0.2, // 0.30000000000000004
            1.0 / 3.0,
            std::f64::consts::PI,
            std::f64::consts::E,
            f64::MAX,
            f64::MIN,
            f64::MIN_POSITIVE,
            f64::EPSILON,
            f64::from_bits(1), // smallest positive subnormal (5e-324)
            -1.5e-308,         // subnormal-adjacent
            123456789.12345679,
            2f64.powi(60) + 1.0,
            f64::NAN,
            -f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
        ];
        for value in f64_values {
            assert_eq!(
                roundtrip_float!(f64, u64, value).to_bits(),
                value.to_bits(),
                "f64 {value:?} did not round-trip"
            );
        }

        let f32_values = [
            0.0f32,
            -0.0,
            0.1,
            1.0 / 3.0,
            std::f32::consts::PI,
            f32::MAX,
            f32::MIN_POSITIVE,
            f32::EPSILON,
            f32::from_bits(1), // smallest positive subnormal
            f32::NAN,
            f32::INFINITY,
        ];
        for value in f32_values {
            assert_eq!(
                roundtrip_float!(f32, u32, value).to_bits(),
                value.to_bits(),
                "f32 {value:?} did not round-trip"
            );
        }
    }

    #[test]
    fn parse_from_literal_negative_and_parenthesized() {
        let parse = |s: &str| syn::parse_str::<syn::Expr>(s).unwrap();

        assert_eq!(f64::parse_from_literal(&parse("-1.5")), -1.5);
        assert_eq!(f64::parse_from_literal(&parse("(1.5)")), 1.5);
        assert_eq!(f64::parse_from_literal(&parse("-(1.5)")), -1.5);
        assert_eq!(f64::parse_from_literal(&parse("--1.5")), 1.5);
        assert_eq!(f32::parse_from_literal(&parse("-2")), -2.0f32);
        assert_eq!(i32::parse_from_literal(&parse("-42")), -42);
        assert_eq!(
            i64::parse_from_literal(&parse("-9223372036854775808")),
            i64::MIN
        );
        assert_eq!(u32::parse_from_literal(&parse("(42)")), 42);
        assert!(bool::parse_from_literal(&parse("(true)")));
        assert_eq!(char::parse_from_literal(&parse("('x')")), 'x');
    }

    #[test]
    fn option_capture_tokens() {
        fn capture_tokens<T: FreeVariableWithContextWithProps<(), ()>>(value: T) -> String {
            let (tokens, ()) = FreeVariableWithContextWithProps::<(), ()>::to_tokens(value, &());
            tokens
                .expr
                .unwrap()
                .to_string()
                .replace(char::is_whitespace, "")
        }

        assert_eq!(
            capture_tokens(Some(5i32)),
            "::core::option::Option::Some(5i32)"
        );
        assert_eq!(
            capture_tokens(None::<i32>),
            "::core::option::Option::<i32>::None"
        );
        assert_eq!(
            capture_tokens(Some("hi".to_owned())),
            "::core::option::Option::Some(\"hi\")"
        );
        assert_eq!(
            capture_tokens(None::<String>),
            "::core::option::Option::<&str>::None"
        );
        assert_eq!(
            capture_tokens(Some(Some(true))),
            "::core::option::Option::Some(::core::option::Option::Some(true))"
        );
        assert_eq!(
            capture_tokens(Some(None::<bool>)),
            "::core::option::Option::Some(::core::option::Option::<bool>::None)"
        );
        assert_eq!(
            capture_tokens(None::<Option<std::time::Duration>>),
            "::core::option::Option::<core::option::Option<core::time::Duration>>::None"
        );
    }

    #[test]
    #[should_panic(expected = "cannot be parsed as `u32`")]
    fn parse_from_literal_rejects_negative_unsigned() {
        u32::parse_from_literal(&syn::parse_str::<syn::Expr>("-42").unwrap());
    }

    #[test]
    #[should_panic(expected = "Expected a literal")]
    fn parse_from_literal_rejects_non_literal() {
        i32::parse_from_literal(&syn::parse_str::<syn::Expr>("1 + 2").unwrap());
    }
}
