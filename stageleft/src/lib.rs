use core::panic;
use std::marker::PhantomData;

use internal::QuotedOutput;
use proc_macro_crate::FoundCrate;
use proc_macro2::Span;
use quote::quote;

pub use stageleft_macro::{entry, export, q, quse_fn, top_level_mod};
pub use type_name::{add_private_reexport, quote_type};

#[doc(hidden)]
pub mod internal {
    use crate::runtime_support::QuoteTokens;
    pub use ctor;
    pub use proc_macro2::{Span, TokenStream};
    pub use quote::quote;

    pub use crate::type_name::{add_crate_with_staged, add_deps_reexport};
    pub use {proc_macro_crate, proc_macro2, syn};

    pub struct Capture {
        pub ident: &'static str,
        pub tokens: QuoteTokens,
    }

    pub struct QuotedOutput {
        pub module_path: &'static str,
        pub crate_name: &'static str,
        pub tokens: &'static str,
        pub captures: Vec<Capture>,
    }
}

pub(crate) mod attempt_transform_macro;
mod rewrite_paths;
pub mod runtime_support;
mod type_name;

pub mod properties;

use runtime_support::{FreeVariableWithContextWithProps, QuoteTokens, get_final_crate_name};
use syn::visit_mut::VisitMut;

#[cfg(windows)]
#[macro_export]
macro_rules! PATH_SEPARATOR {
    () => {
        r"\"
    };
}

#[cfg(not(windows))]
#[macro_export]
macro_rules! PATH_SEPARATOR {
    () => {
        r"/"
    };
}

#[macro_export]
macro_rules! stageleft_crate {
    ($macro_crate:ident) => {
        #[cfg(not(stageleft_macro))]
        #[doc(hidden)]
        pub use $macro_crate as __macro;

        #[cfg(stageleft_macro)]
        include!(concat!(
            env!("OUT_DIR"),
            $crate::PATH_SEPARATOR!(),
            "lib_macro.rs"
        ));

        #[cfg(not(stageleft_macro))]
        $crate::stageleft_no_entry_crate!();
    };
}

#[macro_export]
macro_rules! stageleft_no_entry_crate {
    () => {
        #[doc(hidden)]
        #[allow(
            unused,
            ambiguous_glob_reexports,
            unexpected_cfgs,
            mismatched_lifetime_syntaxes,
            clippy::suspicious_else_formatting,
            clippy::type_complexity,
            reason = "generated code"
        )]
        pub mod __staged {
            #[cfg(any(feature = "stageleft_macro_entrypoint", stageleft_trybuild))]
            include!(concat!(
                env!("OUT_DIR"),
                $crate::PATH_SEPARATOR!(),
                "lib_pub.rs"
            ));

            include!(concat!(
                env!("OUT_DIR"),
                $crate::PATH_SEPARATOR!(),
                "staged_deps.rs"
            ));
        }
    };
}

pub trait QuotedContext {
    fn create() -> Self;
}

pub struct BorrowBounds<'a> {
    _marker: PhantomData<fn(&'a ()) -> &'a ()>,
}

impl QuotedContext for BorrowBounds<'_> {
    fn create() -> Self {
        BorrowBounds {
            _marker: PhantomData,
        }
    }
}

pub trait QuotedWithContextWithProps<'a, T, Ctx, Props>:
    FreeVariableWithContextWithProps<Ctx, Props, O = T>
{
    fn splice_untyped_ctx_props(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
    {
        let (res, props) = FreeVariableWithContextWithProps::to_tokens(self, ctx);
        if res.prelude.is_some() {
            panic!("Quoted value should not have prelude");
        }

        (syn::parse2(res.expr.unwrap()).unwrap(), props)
    }

    fn splice_typed_ctx_props(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let out_type = quote_type::<T>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::type_hint::<#out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fn0_ctx_props<O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: Fn() -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fn0_type_hint::<#out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fn1_ctx_props<I, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: Fn(I) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in_type = quote_type::<I>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fn1_type_hint::<#in_type, #out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fn1_borrow_ctx_props<I, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: Fn(&I) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in_type = quote_type::<I>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fn1_borrow_type_hint::<#in_type, #out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fn2_ctx_props<I1, I2, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: Fn(I1, I2) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in1_type = quote_type::<I1>();
        let in2_type = quote_type::<I2>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fn2_type_hint::<#in1_type, #in2_type, #out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fn2_borrow_ctx_props<I1, I2, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: Fn(&I1, &I2) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in1_type = quote_type::<I1>();
        let in2_type = quote_type::<I2>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fn2_borrow_type_hint::<#in1_type, #in2_type, #out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fn2_borrow_mut_ctx_props<I1, I2, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: Fn(&mut I1, I2) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in1_type = quote_type::<I1>();
        let in2_type = quote_type::<I2>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fn2_borrow_mut_type_hint::<#in1_type, #in2_type, #out_type>(#inner_expr)
            },
            props,
        )
    }
}

pub trait QuotedWithContext<'a, T, Ctx>: QuotedWithContextWithProps<'a, T, Ctx, ()> {
    fn splice_untyped_ctx(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
    {
        QuotedWithContextWithProps::splice_untyped_ctx_props(self, ctx).0
    }

    fn splice_typed_ctx(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
    {
        QuotedWithContextWithProps::splice_typed_ctx_props(self, ctx).0
    }

    fn splice_fn0_ctx<O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: Fn() -> O,
    {
        QuotedWithContextWithProps::splice_fn0_ctx_props(self, ctx).0
    }

    fn splice_fn1_ctx<I, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: Fn(I) -> O,
    {
        QuotedWithContextWithProps::splice_fn1_ctx_props(self, ctx).0
    }

    fn splice_fn1_borrow_ctx<I, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: Fn(&I) -> O,
    {
        QuotedWithContextWithProps::splice_fn1_borrow_ctx_props(self, ctx).0
    }

    fn splice_fn2_ctx<I1, I2, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: Fn(I1, I2) -> O,
    {
        QuotedWithContextWithProps::splice_fn2_ctx_props(self, ctx).0
    }

    fn splice_fn2_borrow_ctx<I1, I2, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: Fn(&I1, &I2) -> O,
    {
        QuotedWithContextWithProps::splice_fn2_borrow_ctx_props(self, ctx).0
    }

    fn splice_fn2_borrow_mut_ctx<I1, I2, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: Fn(&mut I1, I2) -> O,
    {
        QuotedWithContextWithProps::splice_fn2_borrow_mut_ctx_props(self, ctx).0
    }

    fn splice_untyped(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
    {
        QuotedWithContext::splice_untyped_ctx(self, &Default::default())
    }

    fn splice_typed(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let out_type = quote_type::<T>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::type_hint::<#out_type>(#inner_expr)
        }
    }

    fn splice_fn0<O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: Fn() -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fn0_type_hint::<#out_type>(#inner_expr)
        }
    }

    fn splice_fn1<I, O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: Fn(I) -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let in_type = quote_type::<I>();
        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fn1_type_hint::<#in_type, #out_type>(#inner_expr)
        }
    }

    fn splice_fn1_borrow<I, O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: Fn(&I) -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let in_type = quote_type::<I>();
        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fn1_borrow_type_hint::<#in_type, #out_type>(#inner_expr)
        }
    }

    fn splice_fn2_borrow_mut<I1, I2, O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: Fn(&mut I1, I2) -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let in1_type = quote_type::<I1>();
        let in2_type = quote_type::<I2>();
        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fn2_borrow_mut_type_hint::<#in1_type, #in2_type, #out_type>(#inner_expr)
        }
    }
}

impl<'a, T, Ctx, F: QuotedWithContextWithProps<'a, T, Ctx, ()>> QuotedWithContext<'a, T, Ctx>
    for F
{
}

pub trait Quoted<'a, T>: QuotedWithContext<'a, T, ()> {}
impl<'a, T, F: QuotedWithContext<'a, T, ()>> Quoted<'a, T> for F {}

fn stageleft_root() -> syn::Ident {
    let pkg_name = env!("CARGO_PKG_NAME");
    let stageleft_crate = proc_macro_crate::crate_name(pkg_name).unwrap_or_else(|_| {
        panic!("Expected stageleft `{pkg_name}` package to be present in `Cargo.toml`")
    });

    match stageleft_crate {
        FoundCrate::Name(name) => syn::Ident::new(&name, Span::call_site()),
        FoundCrate::Itself => syn::Ident::new("crate", Span::call_site()),
    }
}

pub trait IntoQuotedOnce<'a, T, Ctx, Props = ()>:
    for<'b> FnOnce(&'b Ctx, &mut QuotedOutput, &mut Option<Props>) -> T
    + 'a
    + QuotedWithContextWithProps<'a, T, Ctx, Props>
{
    fn boxed(self) -> Box<dyn IntoQuotedOnce<'a, T, Ctx, Props>>
    where
        Self: Sized,
    {
        Box::new(self)
    }
}

impl<'a, T, Ctx, Props, F: for<'b> FnOnce(&'b Ctx, &mut QuotedOutput, &mut Option<Props>) -> T + 'a>
    QuotedWithContextWithProps<'a, T, Ctx, Props> for F
{
}

impl<'a, T, Ctx, Props, F: for<'b> FnOnce(&'b Ctx, &mut QuotedOutput, &mut Option<Props>) -> T + 'a>
    IntoQuotedOnce<'a, T, Ctx, Props> for F
{
}

impl<T, Ctx, Props, F: for<'b> FnOnce(&'b Ctx, &mut QuotedOutput, &mut Option<Props>) -> T>
    FreeVariableWithContextWithProps<Ctx, Props> for F
{
    type O = T;

    fn to_tokens(self, ctx: &Ctx) -> (QuoteTokens, Props) {
        let mut output = QuotedOutput {
            module_path: "",
            crate_name: "",
            tokens: "",
            captures: Vec::new(),
        };

        let mut props = None;

        // this is an uninit value so we can't drop it
        std::mem::forget(self(ctx, &mut output, &mut props));

        let instantiated_free_variables = output.captures.iter().flat_map(|capture| {
            let ident = syn::Ident::new(capture.ident, Span::call_site());
            capture
                .tokens
                .prelude
                .iter()
                .map(|prelude| quote!(#prelude))
                .chain(
                    capture
                        .tokens
                        .expr
                        .iter()
                        .map(move |value| quote!(let #ident = #value;)),
                )
        });

        let final_crate_root = get_final_crate_name(output.crate_name);

        let module_path: syn::Path = syn::parse_str(output.module_path).unwrap();
        let module_path_segments = module_path
            .segments
            .iter()
            .skip(1)
            .cloned()
            .collect::<Vec<_>>();
        let module_path = if module_path_segments.is_empty()
            || module_path_segments
                .iter()
                .any(|segment| segment.ident.to_string().starts_with("__doctest"))
        {
            None
        } else {
            Some(syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::from_iter(module_path_segments),
            })
        };

        let mut expr: syn::Expr = syn::parse_str(output.tokens).unwrap();
        rewrite_paths::RewritePaths {
            crate_root_path: syn::parse_quote!(#final_crate_root::__staged),
            module_path: module_path
                .clone()
                .map(|module_path| syn::parse_quote!(#final_crate_root::__staged::#module_path)),
        }
        .visit_expr_mut(&mut expr);

        let with_env = if let Some(module_path) = module_path {
            quote!({
                use #final_crate_root::__staged::__deps::*;
                use #final_crate_root::__staged::#module_path::*;
                #(#instantiated_free_variables)*
                #expr
            })
        } else {
            quote!({
                use #final_crate_root::__staged::__deps::*;
                use #final_crate_root::__staged::*;
                #(#instantiated_free_variables)*
                #expr
            })
        };

        (
            QuoteTokens {
                prelude: None,
                expr: Some(with_env),
            },
            props.unwrap(),
        )
    }
}

pub trait IntoQuotedMut<'a, T, Ctx, Props = ()>:
    FnMut(&Ctx, &mut QuotedOutput, &mut Option<Props>) -> T + 'a
{
}

impl<'a, T, Ctx, Props, F: FnMut(&Ctx, &mut QuotedOutput, &mut Option<Props>) -> T + 'a>
    IntoQuotedMut<'a, T, Ctx, Props> for F
{
}

/// Represents a piece of data that will be passed into the generated code
pub struct RuntimeData<T> {
    ident: &'static str,
    _phantom: PhantomData<T>,
}

impl<T: Copy> Copy for RuntimeData<T> {}

impl<T: Copy> Clone for RuntimeData<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> RuntimeData<T> {
    pub fn new(ident: &'static str) -> RuntimeData<T> {
        RuntimeData {
            ident,
            _phantom: PhantomData,
        }
    }
}

impl<T, Ctx> FreeVariableWithContextWithProps<Ctx, ()> for RuntimeData<T> {
    type O = T;

    fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
        let ident = syn::Ident::new(self.ident, Span::call_site());
        (
            QuoteTokens {
                prelude: None,
                expr: Some(quote!(#ident)),
            },
            (),
        )
    }
}
