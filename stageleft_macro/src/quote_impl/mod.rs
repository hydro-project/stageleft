use std::collections::BTreeMap;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::Token;
use syn::parse::{Parse, ParseStream};
use syn::visit_mut::VisitMut;

use self::free_variable::FreeVariableVisitor;

mod attempt_transform_macro;
mod free_variable;

/// A property annotation like `commutative = Kani` or `idempotent = ManualProof(/** something */)`
struct PropertyAnnotation {
    name: syn::Ident,
    _eq: Token![=],
    value: syn::Expr,
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
    expr: syn::Expr,
    properties: Vec<PropertyAnnotation>,
}

impl Parse for QInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let expr: syn::Expr = input.parse()?;
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


/// Rewrites a token stream to replace path metavar idents with `$ident` references
/// for use inside a macro_rules! body.
fn rewrite_body_for_macro(
    body: &TokenStream,
    relative_paths: &BTreeMap<String, usize>,
) -> TokenStream {
    let metavar_idents: std::collections::HashSet<String> = relative_paths
        .values()
        .map(|idx| format!("__sl_p{idx}"))
        .collect();

    fn rewrite_stream(
        stream: TokenStream,
        metavar_idents: &std::collections::HashSet<String>,
    ) -> TokenStream {
        use proc_macro2::{Delimiter, Group, Punct, Spacing, TokenTree};

        let mut result = Vec::new();
        for tt in stream {
            match tt {
                TokenTree::Ident(ref ident) if metavar_idents.contains(&ident.to_string()) => {
                    // Emit $($__sl_pN)::* for path prefix metavars
                    // Inner group: ($__sl_pN)
                    let inner: TokenStream = vec![
                        TokenTree::Punct(Punct::new('$', Spacing::Joint)),
                        tt.clone(),
                    ]
                    .into_iter()
                    .collect();
                    let inner_group = Group::new(Delimiter::Parenthesis, inner);
                    // Outer: $(...) :: *
                    result.push(TokenTree::Punct(Punct::new('$', Spacing::Alone)));
                    result.push(TokenTree::Group(inner_group));
                    result.push(TokenTree::Punct(Punct::new(':', Spacing::Joint)));
                    result.push(TokenTree::Punct(Punct::new(':', Spacing::Alone)));
                    result.push(TokenTree::Punct(Punct::new('*', Spacing::Alone)));
                }
                TokenTree::Group(group) => {
                    let rewritten = rewrite_stream(group.stream(), metavar_idents);
                    let mut new_group = Group::new(group.delimiter(), rewritten);
                    new_group.set_span(group.span());
                    result.push(TokenTree::Group(new_group));
                }
                other => result.push(other),
            }
        }
        result.into_iter().collect()
    }

    rewrite_stream(body.clone(), &metavar_idents)
}

pub fn q_impl(root: TokenStream, toks: TokenStream) -> TokenStream {
    let parsed = syn::parse2::<QInput>(toks.clone());

    let (expr_toks, properties) = match parsed {
        Ok(input) => (input.expr.into_token_stream(), input.properties),
        Err(_) => (toks.clone(), Vec::new()),
    };

    let is_rust_analyzer = std::env::var("RUST_ANALYZER_INTERNALS_DO_NOT_USE").is_ok();

    let mut visitor = FreeVariableVisitor {
        rewrite_paths: !is_rust_analyzer,
        ..Default::default()
    };
    let rewritten_toks = if let Ok(mut expr) = syn::parse2::<syn::Expr>(expr_toks.clone()) {
        visitor.visit_expr_mut(&mut expr);
        expr.into_token_stream()
    } else {
        expr_toks
    };

    let relative_paths = visitor.relative_paths;

    // Skip macro generation in Rust Analyzer mode for performance
    let (hidden_macro, macro_name, relative_path_strs, tokens_str): (TokenStream, Option<String>, Vec<String>, String) =
        if is_rust_analyzer {
            (quote!(), None, vec![], String::new())
        } else {
            // Generate a hash for the hidden macro name using source file + span location + content
            let span = toks
                .into_iter()
                .next()
                .map(|t| t.span())
                .unwrap_or_else(Span::call_site);
            let start = span.start();
            // Make the file path relative to the final crate's manifest dir for portability.
            let file_path = std::path::PathBuf::from(span.file());
            let canonical_file = file_path.canonicalize().unwrap_or(file_path);
            let manifest_dir = std::path::PathBuf::from(
                std::env::var("STAGELEFT_FINAL_CRATE_MANIFEST_DIR")
                    .or_else(|_| std::env::var("CARGO_MANIFEST_DIR"))
                    .expect("STAGELEFT_FINAL_CRATE_MANIFEST_DIR or CARGO_MANIFEST_DIR must be set"),
            );
            let canonical_manifest_dir = manifest_dir.canonicalize().unwrap_or(manifest_dir);
            let relative_file = canonical_file
                .strip_prefix(&canonical_manifest_dir)
                .unwrap_or(&canonical_file)
                .display()
                .to_string();
            // Build a deterministic macro name from the relative file path + location.
            // Normalize to valid Rust identifier characters.
            let normalized_file: String = relative_file
                .chars()
                .map(|c| if c.is_alphanumeric() { c } else { '_' })
                .collect();
            let macro_name = format!(
                "__stageleft_quote_{normalized_file}_{}_{}",
                start.line, start.column
            );
            let macro_name_ident = syn::Ident::new(&macro_name, Span::call_site());

            // Build the hidden macro body: add $ prefixes to path metavar idents
            let macro_body_toks = if !relative_paths.is_empty() {
                rewrite_body_for_macro(&rewritten_toks, &relative_paths)
            } else {
                rewritten_toks.clone()
            };

            // Generate macro parameter patterns for paths: `__sl_p0 = $($__sl_p0:ident)::*`
            let path_param_patterns: Vec<TokenStream> = {
                let mut sorted: Vec<_> = relative_paths.iter().collect();
                sorted.sort_by_key(|(_, idx)| *idx);
                sorted
                    .iter()
                    .map(|(_, idx)| {
                        let metavar = syn::Ident::new(&format!("__sl_p{idx}"), Span::call_site());
                        quote!(#metavar = $($#metavar:ident)::*)
                    })
                    .collect()
            };

            // Generate macro parameter patterns for free variables: `x__free = $x__free:expr`
            let free_var_param_patterns: Vec<TokenStream> = visitor
                .free_variables
                .iter()
                .map(|i| {
                    let i_free = syn::Ident::new(&format!("{i}__free"), Span::call_site());
                    quote!(#i_free = $#i_free:expr)
                })
                .collect();

            // All macro params: paths first, then free variables, then literal body match
            let all_macro_params: Vec<&TokenStream> = path_param_patterns
                .iter()
                .chain(free_var_param_patterns.iter())
                .collect();

            // Generate let bindings for free variables inside the macro body
            let free_var_let_bindings: Vec<TokenStream> = visitor
                .free_variables
                .iter()
                .map(|i| {
                    let i_free = syn::Ident::new(&format!("{i}__free"), Span::call_site());
                    quote!(let #i_free = $#i_free;)
                })
                .collect();

            // The literal body match is the path-rewritten tokens
            let literal_body = &rewritten_toks;

            let hidden_macro = quote! {
                #[allow(unexpected_cfgs, non_local_definitions, clippy::crate_in_macro_def)]
                #[cfg_attr(not(stageleft_macro), macro_export)]
                #[doc(hidden)]
                macro_rules! #macro_name_ident {
                    ([#(#all_macro_params,)*] [#literal_body]) => {
                        {
                            #[allow(non_snake_case)]
                            {
                                #(#free_var_let_bindings)*
                                #macro_body_toks
                            }
                        }
                    };
                }
            };

            // Serialize relative paths for the splice site (sorted by index)
            let relative_path_strs: Vec<&String> = {
                let mut sorted: Vec<_> = relative_paths.iter().collect();
                sorted.sort_by_key(|(_, idx)| *idx);
                sorted.iter().map(|(path_str, _)| *path_str).collect()
            };

            (
                hidden_macro,
                Some(macro_name),
                relative_path_strs.into_iter().cloned().collect(),
                rewritten_toks.to_string(),
            )
        };

    // Get fn() -> T pointers BEFORE the if block so they're in scope for both branches.
    let uninit_fn_bindings = visitor.free_variables.iter().map(|i| {
        let i_shadow_ident = syn::Ident::new(&format!("{i}__free"), Span::call_site());

        quote!(
            #[allow(unused, non_upper_case_globals, non_snake_case)]
            let #i_shadow_ident = ::#root::runtime_support::FreeVariableWithContext::uninitialized(&#i, __stageleft_ctx);
        )
    });

    // Push captures inside the if block (consumes the original variables via to_tokens).
    let capture_pushes = visitor.free_variables.iter().map(|i| {
        let ident_shadow_str = format!("{i}__free");

        quote!(
            __output.captures.push(::#root::internal::Capture {
                ident: #ident_shadow_str,
                tokens: ::#root::runtime_support::FreeVariableWithContext::to_tokens(#i, __stageleft_ctx),
            });
        )
    });

    // In the unreachable block, invoke the fn() -> T to shadow with the typed value.
    let uninit_invocations = visitor.free_variables.iter().map(|i| {
        let i_shadow_ident = syn::Ident::new(&format!("{i}__free"), Span::call_site());

        quote!(
            #[allow(unused, non_upper_case_globals, non_snake_case)]
            let #i_shadow_ident = #i_shadow_ident();
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
    let expr_without_spans = if is_rust_analyzer {
        // for unknown reasons, Rust Analyzer completions break if we emit the input as a string as well :(
        "".to_owned()
    } else {
        tokens_str
    };

    let macro_name_tokens = match &macro_name {
        None => quote!(None),
        Some(name) => quote!(Some(#name)),
    };

    // In non-RA mode, the unreachable block invokes the macro for type checking.
    // In RA mode, paste the original tokens directly (paths resolve locally).
    let unreachable_expr = if let Some(ref name) = macro_name {
        let macro_ident = syn::Ident::new(name, Span::call_site());
        // Pass original path prefixes (crate, self, super::super, etc.)
        let path_prefix_args: Vec<TokenStream> = {
            let mut sorted: Vec<_> = relative_paths.iter().collect();
            sorted.sort_by_key(|(_, idx)| *idx);
            sorted
                .iter()
                .map(|(prefix_str, idx)| {
                    let prefix: syn::Path = syn::parse_str(prefix_str).unwrap();
                    let name = syn::Ident::new(&format!("__sl_p{idx}"), Span::call_site());
                    quote!(#name = #prefix)
                })
                .collect()
        };
        // Pass uninit values as free var args (already bound by uninit_invocations above)
        let free_var_uninit_args: Vec<TokenStream> = visitor
            .free_variables
            .iter()
            .map(|i| {
                let i_free = syn::Ident::new(&format!("{i}__free"), Span::call_site());
                quote!(#i_free = #i_free)
            })
            .collect();
        quote!(#macro_ident!([#(#path_prefix_args,)* #(#free_var_uninit_args,)*] [#rewritten_toks]))
    } else {
        rewritten_toks
    };

    quote!({
        #hidden_macro

        move |__stageleft_ctx: &_, __output: &mut ::#root::internal::QuotedOutput, __props: &mut _| {
            // Bind fn() -> T pointers before the if block so they're in scope everywhere.
            #(#uninit_fn_bindings)*

            if true {
                // Push captures (consumes the original variables via to_tokens)
                #(#capture_pushes)*

                __output.module_path = module_path!();
                __output.crate_name = option_env!("STAGELEFT_FINAL_CRATE_NAME").unwrap_or(env!("CARGO_PKG_NAME"));
                __output.pkg_name = env!("CARGO_PKG_NAME");
                __output.tokens = #expr_without_spans;
                __output.macro_name = #macro_name_tokens;
                __output.relative_paths = &[#(#relative_path_strs),*];

                *__props = Some(#props_builder);

                // All side effects are done. Panic instead of returning T.
                // The caller catches this with catch_unwind.
                ::core::panic!("stageleft: q!() closure completed");
            }

            // Unreachable: provides return type T for type inference.
            #[allow(unreachable_code, unused_qualifications)]
            {
                #(#uninit_invocations)*
                #unreachable_expr
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
    fn test_let_else_structural_binding() {
        test_quote! {
            || {
                let Some(x) = Some(1) else { return };
                x
            }
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
