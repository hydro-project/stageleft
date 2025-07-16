// synced with stageleft

use quote::ToTokens;
use syn::visit_mut::VisitMut;

pub fn attempt_transform_macro(rewriter: &mut impl VisitMut, i: &mut syn::Macro) {
    // TODO(shadaj): emit a warning if our guess at parsing fails
    match i.delimiter {
        syn::MacroDelimiter::Paren(_) | syn::MacroDelimiter::Bracket(_) => {
            i.tokens = i
                .parse_body_with(
                    syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated,
                )
                .ok()
                .map(|mut exprs| {
                    for arg in &mut exprs {
                        rewriter.visit_expr_mut(arg);
                    }
                    exprs.to_token_stream()
                })
                .unwrap_or(i.tokens.clone());
        }
        syn::MacroDelimiter::Brace(_binding_0) => {
            i.tokens = i
                .parse_body_with(syn::Block::parse_within)
                .ok()
                .map(|mut stmts| {
                    for stmt in &mut stmts {
                        rewriter.visit_stmt_mut(stmt);
                    }
                    syn::punctuated::Punctuated::<syn::Stmt, syn::Token![;]>::from_iter(stmts)
                        .to_token_stream()
                })
                .unwrap_or(i.tokens.clone());
        }
    }
}
