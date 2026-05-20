use std::collections::{BTreeMap, BTreeSet, HashSet};

mod prelude;
use prelude::is_prelude;

#[derive(Debug)]
pub struct ScopeStack {
    scopes: Vec<(HashSet<String>, HashSet<String>)>,
}

impl Default for ScopeStack {
    fn default() -> Self {
        ScopeStack {
            scopes: vec![(HashSet::new(), HashSet::new())],
        }
    }
}

impl ScopeStack {
    pub fn push(&mut self) {
        self.scopes.push((HashSet::new(), HashSet::new()));
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn insert_term(&mut self, ident: syn::Ident) {
        self.scopes
            .last_mut()
            .expect("Scope stack should not be empty")
            .0
            .insert(ident.to_string());
    }

    pub fn insert_type(&mut self, ident: syn::Ident) {
        self.scopes
            .last_mut()
            .expect("Scope stack should not be empty")
            .1
            .insert(ident.to_string());
    }

    pub fn contains_term(&self, ident: &syn::Ident) -> bool {
        let ident = ident.to_string();
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.0.contains(&ident))
    }

    pub fn contains_type(&self, ident: &syn::Ident) -> bool {
        let ident = ident.to_string();
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.1.contains(&ident))
    }
}

#[derive(Default)]
pub struct FreeVariableVisitor {
    pub free_variables: BTreeSet<syn::Ident>,
    pub relative_paths: BTreeMap<String, usize>,
    pub current_scope: ScopeStack,
    /// When true, rewrite relative path prefixes to __sl_pN idents in-place.
    pub rewrite_paths: bool,
}

/// Visitor that only extracts bound identifiers from patterns,
/// without triggering free variable detection on paths like `Some`.
struct PatBindingVisitor<'a> {
    scope: &'a mut ScopeStack,
}

impl syn::visit::Visit<'_> for PatBindingVisitor<'_> {
    fn visit_pat_ident(&mut self, i: &syn::PatIdent) {
        self.scope.insert_term(i.ident.clone());
        syn::visit::visit_pat_ident(self, i);
    }
}

impl syn::visit_mut::VisitMut for FreeVariableVisitor {
    fn visit_expr_closure_mut(&mut self, i: &mut syn::ExprClosure) {
        self.current_scope.push();
        i.inputs.iter_mut().for_each(|input| {
            self.visit_pat_mut(input);
        });

        syn::visit_mut::visit_expr_closure_mut(self, i);

        self.current_scope.pop();
    }

    fn visit_item_fn_mut(&mut self, i: &mut syn::ItemFn) {
        self.current_scope.push();
        syn::visit_mut::visit_item_fn_mut(self, i);
        self.current_scope.pop();
    }

    fn visit_generic_param_mut(&mut self, i: &mut syn::GenericParam) {
        match i {
            syn::GenericParam::Type(type_param) => {
                self.current_scope.insert_type(type_param.ident.clone());
            }
            syn::GenericParam::Lifetime(lifetime_param) => {
                self.current_scope
                    .insert_type(lifetime_param.lifetime.ident.clone());
            }
            syn::GenericParam::Const(const_param) => {
                self.current_scope.insert_type(const_param.ident.clone());
            }
        }
    }

    fn visit_block_mut(&mut self, i: &mut syn::Block) {
        self.current_scope.push();
        syn::visit_mut::visit_block_mut(self, i);
        self.current_scope.pop();
    }

    fn visit_local_mut(&mut self, i: &mut syn::Local) {
        i.init.iter_mut().for_each(|init| {
            syn::visit_mut::visit_local_init_mut(self, init);
        });

        let mut binding_visitor = PatBindingVisitor {
            scope: &mut self.current_scope,
        };
        syn::visit::Visit::visit_pat(&mut binding_visitor, &i.pat);
    }

    fn visit_ident_mut(&mut self, i: &mut proc_macro2::Ident) {
        if !self.current_scope.contains_term(i) {
            self.free_variables.insert(i.clone());
            *i = syn::Ident::new(&format!("{i}__free"), i.span());
        }
    }

    fn visit_lifetime_mut(&mut self, i: &mut syn::Lifetime) {
        if !self.current_scope.contains_type(&i.ident) {
            self.free_variables.insert(i.ident.clone());
            i.ident = syn::Ident::new(&format!("{}__free", i.ident), i.ident.span());
        }
    }

    fn visit_path_mut(&mut self, i: &mut syn::Path) {
        // Collect and rewrite relative path prefixes (crate::, self::, super::)
        if self.rewrite_paths
            && i.leading_colon.is_none()
            && i.segments.len() > 1
            && let Some(first) = i.segments.first()
            && (first.ident == "crate" || first.ident == "self" || first.ident == "super")
        {
            let prefix_len = if first.ident == "crate" {
                1
            } else {
                i.segments
                    .iter()
                    .take_while(|s| s.ident == "self" || s.ident == "super")
                    .count()
            };
            let prefix_segments: Vec<_> = i.segments.iter().take(prefix_len).collect();
            let key = quote::quote!(#(#prefix_segments)::*).to_string();
            let next_idx = self.relative_paths.len();
            let idx = *self.relative_paths.entry(key).or_insert(next_idx);

            let metavar = syn::Ident::new(&format!("__sl_p{idx}"), proc_macro2::Span::call_site());
            let remaining: Vec<_> = i.segments.iter().skip(prefix_len).cloned().collect();
            i.segments.clear();
            i.segments.push(syn::PathSegment::from(metavar));
            for seg in remaining {
                i.segments.push(seg);
            }
            // Visit path arguments in remaining segments
            for node in i.segments.iter_mut().skip(1) {
                self.visit_path_arguments_mut(&mut node.arguments);
            }
            return;
        }

        if i.leading_colon.is_none() && !is_prelude(&i.segments.first().unwrap().ident) {
            let one_segment = i.segments.len() == 1;
            let node = i.segments.first_mut().unwrap();
            if one_segment && !self.current_scope.contains_term(&node.ident) {
                self.free_variables.insert(node.ident.clone());
                node.ident = syn::Ident::new(&format!("{}__free", node.ident), node.ident.span());
            }
        }

        for node in i.segments.iter_mut() {
            self.visit_path_arguments_mut(&mut node.arguments);
        }
    }

    fn visit_arm_mut(&mut self, i: &mut syn::Arm) {
        self.current_scope.push();
        syn::visit_mut::visit_arm_mut(self, i);
        self.current_scope.pop();
    }

    fn visit_field_pat_mut(&mut self, i: &mut syn::FieldPat) {
        for it in &mut i.attrs {
            self.visit_attribute_mut(it);
        }
        self.visit_pat_mut(&mut i.pat);
    }

    fn visit_pat_ident_mut(&mut self, i: &mut syn::PatIdent) {
        self.current_scope.insert_term(i.ident.clone());
    }

    fn visit_expr_method_call_mut(&mut self, i: &mut syn::ExprMethodCall) {
        syn::visit_mut::visit_expr_mut(self, &mut i.receiver);
        for arg in &mut i.args {
            self.visit_expr_mut(arg);
        }
    }

    fn visit_type_mut(&mut self, _: &mut syn::Type) {}

    fn visit_expr_struct_mut(&mut self, node: &mut syn::ExprStruct) {
        for it in &mut node.attrs {
            self.visit_attribute_mut(it);
        }
        if let Some(it) = &mut node.qself {
            self.visit_qself_mut(it);
        }
        // No need to capture the struct path
        // self.visit_path(&node.path);
        for el in syn::punctuated::Punctuated::pairs_mut(&mut node.fields) {
            let it = el.into_value();
            self.visit_expr_mut(&mut it.expr);
        }
        if let Some(it) = &mut node.rest {
            self.visit_expr_mut(it);
        }
    }

    fn visit_expr_field_mut(&mut self, i: &mut syn::ExprField) {
        self.visit_expr_mut(&mut i.base);
    }

    fn visit_macro_mut(&mut self, i: &mut syn::Macro) {
        super::attempt_transform_macro::attempt_transform_macro(self, i);
    }
}
