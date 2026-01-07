use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::parse::{Parse, ParseStream};
use syn::visit_mut::VisitMut;
use syn::{Expr, Ident, Token};

use self::free_variable::FreeVariableVisitor;

mod attempt_transform_macro;
mod free_variable;

/// A property annotation like `commutative = Kani` or `idempotent = ManualProof(/** something */)`
struct PropertyAnnotation {
    name: Ident,
    _eq: Token![=],
    value: Expr,
}

impl Parse for PropertyAnnotation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(PropertyAnnotation {
            name: input.parse()?,
            _eq: input.parse()?,
            value: input.parse()?,
        })
    }
}

/// Input to q! macro: expression followed by optional property annotations
struct QInput {
    expr: Expr,
    properties: Vec<PropertyAnnotation>,
}

impl Parse for QInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let expr: Expr = input.parse()?;
        let mut properties = Vec::new();

        while input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;
            if input.is_empty() {
                break;
            }
            properties.push(input.parse()?);
        }

        Ok(QInput { expr, properties })
    }
}

pub fn q_impl(root: TokenStream, toks: proc_macro2::TokenStream) -> TokenStream {
    let parsed = syn::parse2::<QInput>(toks.clone());

    let (expr_toks, properties) = match parsed {
        Ok(input) => (input.expr.into_token_stream(), input.properties),
        Err(_) => (toks, Vec::new()),
    };

    let mut visitor = FreeVariableVisitor::default();
    let rewritten_toks = if let Ok(mut expr) = syn::parse2::<syn::Expr>(expr_toks.clone()) {
        visitor.visit_expr_mut(&mut expr);
        expr.into_token_stream()
    } else {
        expr_toks
    };

    let unitialized_free_variables = visitor.free_variables.iter().map(|i| {
        let ident_shadow_str = format!("{i}__free");
        let i_shadow_ident = syn::Ident::new(&ident_shadow_str, Span::call_site());

        quote!(
            #[allow(unused, non_upper_case_globals, non_snake_case)]
            let #i_shadow_ident = {
                let _out = ::#root::runtime_support::FreeVariableWithContext::uninitialized(&#i, __stageleft_ctx);
                __output.captures.push(::#root::internal::Capture {
                    ident: #ident_shadow_str,
                    tokens: ::#root::runtime_support::FreeVariableWithContext::to_tokens(#i, __stageleft_ctx),
                });
                _out
            };
        )
    });

    let uninit_forgets = visitor.free_variables.iter().map(|i| {
        let i_shadow_ident = syn::Ident::new(&format!("{i}__free"), Span::call_site());

        quote!(
            #[allow(unused, non_upper_case_globals, non_snake_case, forget_non_drop)]
            ::std::mem::forget(#i_shadow_ident);
        )
    });

    // Generate property builder chain: Default::default().a(b).c(d)
    let props_builder = if properties.is_empty() {
        quote!(#root::properties::Property::make_root(__props))
    } else {
        let builder_calls = properties.iter().map(|prop| {
            let name = &prop.name;
            let value = &prop.value;
            quote!(.#name(#value))
        });
        quote!(#root::properties::Property::make_root(__props)#(#builder_calls)*)
    };

    // necessary to ensure proper hover in Rust Analyzer
    let expr_without_spans = if std::env::var("RUST_ANALYZER_INTERNALS_DO_NOT_USE").is_ok() {
        // for unknown reasons, Rust Analyzer completions break if we emit the input as a string as well :(
        "".to_string()
    } else {
        rewritten_toks.to_string()
    };

    quote!({
        move |__stageleft_ctx: &_, __output: &mut ::#root::internal::QuotedOutput, __props: &mut _| {
            #(#unitialized_free_variables;)*

            if true {
                __output.module_path = module_path!();
                __output.crate_name = option_env!("STAGELEFT_FINAL_CRATE_NAME").unwrap_or(env!("CARGO_PKG_NAME"));
                __output.tokens = #expr_without_spans;

                *__props = Some(#props_builder);

                #(#uninit_forgets;)*
                unsafe {
                    return ::std::mem::MaybeUninit::uninit().assume_init();
                }
            }

            #[allow(unreachable_code, unused_qualifications)]
            {
                #rewritten_toks
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use quote::quote;
    use syn::parse_quote;

    use super::q_impl;

    macro_rules! test_quote {
        ($program:expr) => {
            let quoted_tokens = q_impl(quote!(stageleft), parse_quote!($program));
            let wrapped: syn::File = parse_quote! {
                fn main() {
                    #quoted_tokens
                }
            };

            insta::with_settings!({
                snapshot_suffix => "macro_tokens",
                prepend_module_to_snapshot => false,
            }, {
                insta::assert_snapshot!(
                    prettyplease::unparse(&wrapped)
                );
            });
        };
    }

    #[test]
    fn test_simple() {
        test_quote! {
            1 + 2
        }
    }

    #[test]
    fn test_capture_local() {
        test_quote! {
            x + 2
        }
    }

    #[test]
    fn test_capture_copy_local() {
        test_quote! {
            (x + 2) + (x + 2)
        }
    }

    #[test]
    fn test_capture_copy_local_block() {
        test_quote! {{
            let _ = x + 2;
            let _ = x + 2;
        }}
    }

    #[test]
    fn test_capture_copy_local_block_let() {
        test_quote! {{
            let x = x + 2;
            let _ = x + 2;
        }}
    }

    #[test]
    fn test_capture_local_mut() {
        test_quote! {
            x += 2
        }
    }

    #[test]
    fn test_non_capture_local() {
        test_quote! {
            {
                let x = 1;
                x + 2
            }
        }
    }

    #[test]
    fn test_capture_in_macro() {
        test_quote! {
            dbg!(x)
        }
    }

    #[test]
    fn test_non_capture_struct_creation() {
        test_quote! {
            Foo { x: 1 }
        }
    }

    #[test]
    fn test_non_capture_enum_creation() {
        test_quote! {
            Foo::Bar { x: 1 }
        }
    }

    #[test]
    fn test_prelude_enum_variants() {
        test_quote! {
            Some(1)
        }

        test_quote! {
            None
        }

        test_quote! {
            Ok(1)
        }

        test_quote! {
            Err(1)
        }
    }
}
