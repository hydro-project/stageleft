use std::marker::PhantomData;

use internal::QuotedOutput;
use proc_macro_crate::FoundCrate;
use proc_macro2::Span;
use quote::quote;

pub use stageleft_macro::{entry, export, q, quse_fn};
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
        pub macro_name: Option<&'static str>,
        pub relative_paths: &'static [&'static str],
        pub pkg_name: &'static str,
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
            ambiguous_glob_reexports,
            mismatched_lifetime_syntaxes,
            unexpected_cfgs,
            unfulfilled_lint_expectations,
            unused,
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

        #[cfg(test)]
        $crate::internal::ctor::declarative::ctor! {
            #[ctor(unsafe)]
            fn __stageleft_register_test_modules() {
                let (crate_name, modules) = include!(concat!(
                    env!("OUT_DIR"),
                    $crate::PATH_SEPARATOR!(),
                    "test_modules.rs"
                ));
                $crate::runtime_support::register_test_modules(crate_name, modules);
            }
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

    fn splice_fnmut0_ctx_props<O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: FnMut() -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fnmut0_type_hint::<#out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fnmut1_ctx_props<I, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: FnMut(I) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in_type = quote_type::<I>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fnmut1_type_hint::<#in_type, #out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fnmut1_borrow_ctx_props<I, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: FnMut(&I) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in_type = quote_type::<I>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fnmut1_borrow_type_hint::<#in_type, #out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fnmut2_ctx_props<I1, I2, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: FnMut(I1, I2) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in1_type = quote_type::<I1>();
        let in2_type = quote_type::<I2>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fnmut2_type_hint::<#in1_type, #in2_type, #out_type>(#inner_expr)
            },
            props,
        )
    }

    fn splice_fnmut2_borrow_ctx_props<I1, I2, O>(self, ctx: &Ctx) -> (syn::Expr, Props)
    where
        Self: Sized,
        T: FnMut(&I1, &I2) -> O,
    {
        let (inner_expr, props) = self.splice_untyped_ctx_props(ctx);
        let stageleft_root = stageleft_root();

        let in1_type = quote_type::<I1>();
        let in2_type = quote_type::<I2>();
        let out_type = quote_type::<O>();

        (
            syn::parse_quote! {
                #stageleft_root::runtime_support::fnmut2_borrow_type_hint::<#in1_type, #in2_type, #out_type>(#inner_expr)
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

    fn splice_fnmut0_ctx<O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: FnMut() -> O,
    {
        QuotedWithContextWithProps::splice_fnmut0_ctx_props(self, ctx).0
    }

    fn splice_fnmut1_ctx<I, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: FnMut(I) -> O,
    {
        QuotedWithContextWithProps::splice_fnmut1_ctx_props(self, ctx).0
    }

    fn splice_fnmut1_borrow_ctx<I, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: FnMut(&I) -> O,
    {
        QuotedWithContextWithProps::splice_fnmut1_borrow_ctx_props(self, ctx).0
    }

    fn splice_fnmut2_ctx<I1, I2, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: FnMut(I1, I2) -> O,
    {
        QuotedWithContextWithProps::splice_fnmut2_ctx_props(self, ctx).0
    }

    fn splice_fnmut2_borrow_ctx<I1, I2, O>(self, ctx: &Ctx) -> syn::Expr
    where
        Self: Sized,
        T: FnMut(&I1, &I2) -> O,
    {
        QuotedWithContextWithProps::splice_fnmut2_borrow_ctx_props(self, ctx).0
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

    fn splice_fnmut0<O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: FnMut() -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fnmut0_type_hint::<#out_type>(#inner_expr)
        }
    }

    fn splice_fnmut1<I, O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: FnMut(I) -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let in_type = quote_type::<I>();
        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fnmut1_type_hint::<#in_type, #out_type>(#inner_expr)
        }
    }

    fn splice_fnmut1_borrow<I, O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: FnMut(&I) -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let in_type = quote_type::<I>();
        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fnmut1_borrow_type_hint::<#in_type, #out_type>(#inner_expr)
        }
    }

    fn splice_fnmut2<I1, I2, O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: FnMut(I1, I2) -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let in1_type = quote_type::<I1>();
        let in2_type = quote_type::<I2>();
        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fnmut2_type_hint::<#in1_type, #in2_type, #out_type>(#inner_expr)
        }
    }

    fn splice_fnmut2_borrow<I1, I2, O>(self) -> syn::Expr
    where
        Self: Sized,
        Ctx: Default,
        T: FnMut(&I1, &I2) -> O,
    {
        let inner_expr = QuotedWithContext::splice_untyped(self);
        let stageleft_root = stageleft_root();

        let in1_type = quote_type::<I1>();
        let in2_type = quote_type::<I2>();
        let out_type = quote_type::<O>();

        syn::parse_quote! {
            #stageleft_root::runtime_support::fnmut2_borrow_type_hint::<#in1_type, #in2_type, #out_type>(#inner_expr)
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

pub trait IntoQuotedOnce<'a, T: 'a, Ctx, Props = ()>:
    FnOnce(&Ctx, &mut QuotedOutput, &mut Option<Props>) -> T
    + QuotedWithContextWithProps<'a, T, Ctx, Props>
{
    fn boxed(self) -> Box<dyn 'a + IntoQuotedOnce<'a, T, Ctx, Props>>
    where
        Self: 'a + Sized,
    {
        Box::new(self)
    }
}

impl<'a, T: 'a, Ctx, Props, F: FnOnce(&Ctx, &mut QuotedOutput, &mut Option<Props>) -> T>
    QuotedWithContextWithProps<'a, T, Ctx, Props> for F
{
}

impl<'a, T: 'a, Ctx, Props, F: FnOnce(&Ctx, &mut QuotedOutput, &mut Option<Props>) -> T>
    IntoQuotedOnce<'a, T, Ctx, Props> for F
{
}

/// Resolves a relative path string (e.g. "crate :: Foo :: bar", "self :: func", "super :: X")
/// to the concrete rewritten path for the splice site.
fn resolve_relative_path(
    path_str: &str,
    final_crate_root: &proc_macro2::TokenStream,
    module_path: Option<&syn::Path>,
) -> proc_macro2::TokenStream {
    let mut path: syn::Path = syn::parse_str(path_str).unwrap();

    // Apply the same rewriting logic as RewritePaths
    let crate_root_path: syn::Path = syn::parse_quote!(#final_crate_root::__staged);
    let full_module_path: Option<syn::Path> =
        module_path.map(|mp| syn::parse_quote!(#final_crate_root::__staged::#mp));

    rewrite_paths::RewritePaths {
        crate_root_path,
        module_path: full_module_path,
    }
    .visit_path_mut(&mut path);

    quote!(#path)
}

/// Generates the spliced token stream from a `QuotedOutput`.
fn splice_quoted_output(output: &QuotedOutput) -> proc_macro2::TokenStream {
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

    let macro_name = output
        .macro_name
        .expect("macro_name must be set when splicing");
    let macro_ident = syn::Ident::new(macro_name, Span::call_site());

    // Resolve path arguments
    let path_args: Vec<proc_macro2::TokenStream> = output
        .relative_paths
        .iter()
        .map(|path_str| resolve_relative_path(path_str, &final_crate_root, module_path.as_ref()))
        .collect();

    // Free variable expression arguments: `name = value`
    let free_var_args: Vec<proc_macro2::TokenStream> = output
        .captures
        .iter()
        .filter_map(|capture| {
            let ident = syn::Ident::new(capture.ident, Span::call_site());
            capture
                .tokens
                .expr
                .as_ref()
                .map(|expr| quote!(#ident = #expr))
        })
        .collect();

    // Preludes (use statements) still go outside the macro invocation
    let preludes: Vec<_> = output
        .captures
        .iter()
        .filter_map(|capture| capture.tokens.prelude.clone())
        .collect();

    // Path arguments: `__sl_pN = resolved::path`
    let named_path_args: Vec<proc_macro2::TokenStream> = path_args
        .iter()
        .enumerate()
        .map(|(idx, p)| {
            let name = syn::Ident::new(&format!("__sl_p{idx}"), Span::call_site());
            quote!(#name = #p)
        })
        .collect();

    let all_args: Vec<_> = named_path_args
        .iter()
        .map(|p| quote!(#p))
        .chain(free_var_args.iter().map(|v| quote!(#v)))
        .collect();

    // Build the macro invocation path.
    let macro_invocation = {
        let literal_body: proc_macro2::TokenStream = output.tokens.parse().unwrap();
        let final_crate = proc_macro_crate::crate_name(output.crate_name)
            .unwrap_or_else(|_| panic!("Expected `{}` package in Cargo.toml", output.crate_name));
        match final_crate {
            FoundCrate::Itself => {
                let pkg_name_underscore = output.pkg_name.replace('-', "_");

                let module_path_without_crate = output
                    .module_path
                    .strip_prefix(&pkg_name_underscore)
                    .and_then(|s| s.strip_prefix("::"))
                    .unwrap_or("");

                let is_inside_crate = output.module_path == pkg_name_underscore
                    || output
                        .module_path
                        .starts_with(&format!("{pkg_name_underscore}::"));

                if !is_inside_crate
                    || runtime_support::is_test_module(
                        &pkg_name_underscore,
                        module_path_without_crate,
                    )
                {
                    // Build fallback body: replace __sl_pN idents with resolved paths
                    let free_var_let_bindings: Vec<proc_macro2::TokenStream> = output
                        .captures
                        .iter()
                        .filter_map(|capture| {
                            let ident = syn::Ident::new(capture.ident, Span::call_site());
                            capture
                                .tokens
                                .expr
                                .as_ref()
                                .map(|expr| quote!(let #ident = #expr;))
                        })
                        .collect();
                    let mut fallback_expr: syn::Expr = syn::parse_str(output.tokens).unwrap();
                    // Replace __sl_pN path prefixes with resolved paths
                    struct MetavarRewriter<'a> {
                        path_args: &'a [proc_macro2::TokenStream],
                    }
                    impl VisitMut for MetavarRewriter<'_> {
                        fn visit_path_mut(&mut self, path: &mut syn::Path) {
                            if let Some(first) = path.segments.first()
                                && let Some(idx_str) =
                                    first.ident.to_string().strip_prefix("__sl_p")
                                && let Ok(idx) = idx_str.parse::<usize>()
                                && let Some(resolved) = self.path_args.get(idx)
                            {
                                let resolved_path: syn::Path =
                                    syn::parse2(resolved.clone()).unwrap();
                                let remaining: Vec<_> =
                                    path.segments.iter().skip(1).cloned().collect();
                                *path = resolved_path;
                                for seg in remaining {
                                    path.segments.push(seg);
                                }
                            }
                            syn::visit_mut::visit_path_mut(self, path);
                        }

                        fn visit_macro_mut(&mut self, i: &mut syn::Macro) {
                            attempt_transform_macro::attempt_transform_macro(self, i);
                        }
                    }
                    MetavarRewriter {
                        path_args: &path_args,
                    }
                    .visit_expr_mut(&mut fallback_expr);
                    quote!({
                        #(#free_var_let_bindings)*
                        #fallback_expr
                    })
                } else {
                    quote!(
                        crate::__staged::#macro_ident!([#(#all_args,)*] [#literal_body])
                    )
                }
            }
            FoundCrate::Name(name) => {
                let crate_ident = syn::Ident::new(&name, Span::call_site());
                quote!(#crate_ident::#macro_ident!([#(#all_args,)*] [#literal_body]))
            }
        }
    };

    if let Some(module_path) = &module_path {
        quote!({
            use #final_crate_root::__staged::__deps::*;
            use #final_crate_root::__staged::#module_path::*;
            #(#preludes)*
            #macro_invocation
        })
    } else {
        quote!({
            use #final_crate_root::__staged::__deps::*;
            use #final_crate_root::__staged::*;
            #(#preludes)*
            #macro_invocation
        })
    }
}

const EXPECTED_PANIC_MESSAGE: &str = "stageleft: q!() closure completed";

/// Invokes a quoted closure, capturing its output via catch_unwind.
fn invoke_quoted<T, Ctx, Props>(
    f: impl FnOnce(&Ctx, &mut QuotedOutput, &mut Option<Props>) -> T,
    ctx: &Ctx,
) -> (QuotedOutput, Props) {
    static GLOBAL_HOOK_MUTEX: parking_lot::ReentrantMutex<()> =
        parking_lot::ReentrantMutex::new(());

    let mut output = QuotedOutput {
        module_path: "",
        crate_name: "",
        tokens: "",
        captures: Vec::new(),
        macro_name: None,
        relative_paths: &[],
        pkg_name: "",
    };

    let mut props = None;

    let output_ref = std::panic::AssertUnwindSafe(&mut output);
    let props_ref = std::panic::AssertUnwindSafe(&mut props);
    let _guard = GLOBAL_HOOK_MUTEX.lock();
    let prev_hook = std::sync::Arc::new(std::panic::take_hook());
    let prev_hook_clone = prev_hook.clone();
    std::panic::set_hook(Box::new(move |info| {
        let is_expected = info
            .payload()
            .downcast_ref::<&str>()
            .is_some_and(|s| *s == EXPECTED_PANIC_MESSAGE);
        if !is_expected {
            prev_hook_clone(info);
        }
    }));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
        let output_ref = output_ref;
        let props_ref = props_ref;
        std::mem::forget(f(ctx, output_ref.0, props_ref.0));
    }));
    let _ = std::panic::take_hook();
    std::panic::set_hook(
        std::sync::Arc::try_unwrap(prev_hook)
            .unwrap_or_else(|_| panic!("prev_hook Arc should have no other references")),
    );
    drop(_guard);

    if let Err(payload) = result {
        let is_expected = payload
            .downcast_ref::<&str>()
            .is_some_and(|s| *s == EXPECTED_PANIC_MESSAGE);

        if !is_expected {
            std::panic::resume_unwind(payload);
        }
    }

    (output, props.unwrap())
}

impl<T, Ctx, Props, F: for<'b> FnOnce(&'b Ctx, &mut QuotedOutput, &mut Option<Props>) -> T>
    FreeVariableWithContextWithProps<Ctx, Props> for F
{
    type O = T;

    fn to_tokens(self, ctx: &Ctx) -> (QuoteTokens, Props) {
        let (output, props) = invoke_quoted(self, ctx);
        let with_env = splice_quoted_output(&output);

        (
            QuoteTokens {
                prelude: None,
                expr: Some(with_env),
            },
            props,
        )
    }
}

pub trait IntoQuotedMut<'a, T: 'a, Ctx, Props = ()>:
    FnMut(&Ctx, &mut QuotedOutput, &mut Option<Props>) -> T
{
}

impl<'a, T: 'a, Ctx, Props, F: FnMut(&Ctx, &mut QuotedOutput, &mut Option<Props>) -> T>
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

#[cfg(test)]
mod tests {
    use super::*;

    /// A type with a buggy FreeVariable impl that panics with a non-stageleft message.
    struct PanickingFreeVar;

    impl<Ctx> FreeVariableWithContextWithProps<Ctx, ()> for PanickingFreeVar {
        type O = i32;

        fn to_tokens(self, _ctx: &Ctx) -> (QuoteTokens, ()) {
            panic!("buggy free variable implementation")
        }
    }

    #[test]
    #[should_panic(expected = "buggy free variable implementation")]
    fn panicking_free_variable_propagates() {
        let f = |_ctx: &(), output: &mut QuotedOutput, props: &mut Option<()>| -> i32 {
            output.captures.push(internal::Capture {
                ident: "x__free",
                tokens: FreeVariableWithContextWithProps::to_tokens(PanickingFreeVar, _ctx).0,
            });

            output.module_path = "test";
            output.crate_name = "test";
            output.tokens = "";
            *props = Some(());

            panic!("stageleft: q!() closure completed");
        };

        invoke_quoted(f, &());
    }
}
