use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::visit::Visit;

use self::free_variable::FreeVariableVisitor;

mod free_variable;

pub fn q_impl(root: TokenStream, toks: proc_macro2::TokenStream) -> TokenStream {
    let mut visitor = FreeVariableVisitor::default();
    if let Ok(expr) = syn::parse2::<syn::Expr>(toks.clone()) {
        visitor.visit_expr(&expr);
    }

    let unitialized_free_variables = visitor.free_variables.iter().map(|i| {
        let ident_str = i.to_string();
        let i_without_span = syn::Ident::new(&ident_str, Span::call_site());

        quote!(
            #[allow(unused, non_upper_case_globals, non_snake_case)]
            let #i_without_span = {
                let _out = ::#root::runtime_support::FreeVariableWithContext::uninitialized(&#i_without_span, __stageleft_ctx);
                __output.captures.push(::#root::internal::Capture {
                    ident: #ident_str,
                    tokens: ::#root::runtime_support::FreeVariableWithContext::to_tokens(#i_without_span, __stageleft_ctx),
                });
                _out
            };
        )
    });

    let uninit_forgets = visitor.free_variables.iter().map(|i| {
        let i_without_span = syn::Ident::new(&i.to_string(), Span::call_site());
        quote!(
            #[allow(unused, non_upper_case_globals, non_snake_case)]
            ::std::mem::forget(#i_without_span);
        )
    });

    // necessary to ensure proper hover in Rust Analyzer
    let expr_without_spans = if std::env::var("RUST_ANALYZER_INTERNALS_DO_NOT_USE").is_ok() {
        // for unknown reasons, Rust Analyzer completions break if we emit the input as a string as well :(
        "".to_string()
    } else {
        toks.to_string()
    };

    quote!({
        move |__stageleft_ctx: &_, __output: &mut ::#root::internal::QuotedOutput| {
            #(#unitialized_free_variables;)*

            if true {
                __output.module_path = module_path!();
                __output.crate_name = option_env!("STAGELEFT_FINAL_CRATE_NAME").unwrap_or(env!("CARGO_PKG_NAME"));
                __output.tokens = #expr_without_spans;

                #(#uninit_forgets;)*
                unsafe {
                    return ::std::mem::MaybeUninit::uninit().assume_init();
                }
            }

            #[allow(unreachable_code, unused_qualifications)]
            {
                #toks
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

            insta::with_settings!({snapshot_suffix => "macro_tokens"}, {
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
