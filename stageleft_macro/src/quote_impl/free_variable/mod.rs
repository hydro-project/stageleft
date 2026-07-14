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

impl FreeVariableVisitor {
    /// If `idents` (the leading idents of a path with no leading `::`) starts with a
    /// relative prefix (`crate`, or a run of `self` / `super`), registers the prefix
    /// in `relative_paths` and returns the number of prefix idents together with the
    /// `__sl_pN` metavar ident that should replace them. Returns `None` if the path
    /// is not relative.
    fn relative_prefix_metavar<'a>(
        &mut self,
        idents: impl IntoIterator<Item = &'a syn::Ident>,
    ) -> Option<(usize, syn::Ident)> {
        let mut idents = idents.into_iter();
        let first = idents.next()?;
        if *first != "crate" && *first != "self" && *first != "super" {
            return None;
        }

        let mut prefix = vec![first.clone()];
        if *first != "crate" {
            prefix.extend(
                idents
                    .take_while(|i| **i == "self" || **i == "super")
                    .cloned(),
            );
        }

        let key = quote::quote!(#(#prefix)::*).to_string();
        let next_idx = self.relative_paths.len();
        let idx = *self.relative_paths.entry(key).or_insert(next_idx);
        Some((prefix.len(), super::path_metavar(idx)))
    }
}

/// Registers the names bound by a `use` tree (e.g. `y` in `use x as y;`) into
/// the given scope so that later references to them are not treated as free
/// variables. `parent` is the ident of the enclosing path segment, used to
/// resolve `use foo::bar::{self}` (which binds `bar`).
fn register_use_tree_bindings(
    tree: &syn::UseTree,
    parent: Option<&syn::Ident>,
    scope: &mut ScopeStack,
) {
    match tree {
        syn::UseTree::Path(path) => {
            register_use_tree_bindings(&path.tree, Some(&path.ident), scope);
        }
        syn::UseTree::Name(name) => {
            let bound = if name.ident == "self" {
                parent
            } else {
                Some(&name.ident)
            };
            if let Some(bound) = bound {
                scope.insert_term(bound.clone());
                scope.insert_type(bound.clone());
            }
        }
        syn::UseTree::Rename(rename) => {
            scope.insert_term(rename.rename.clone());
            scope.insert_type(rename.rename.clone());
        }
        syn::UseTree::Glob(_) => {}
        syn::UseTree::Group(group) => {
            for tree in &group.items {
                register_use_tree_bindings(tree, parent, scope);
            }
        }
    }
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

    fn visit_item_use_mut(&mut self, i: &mut syn::ItemUse) {
        // Rewrite relative path prefixes (crate::, self::, super::) in the use
        // tree, mirroring the logic in `visit_path_mut`.
        if self.rewrite_paths && i.leading_colon.is_none() {
            // Collect the idents of the leading `UseTree::Path` chain.
            let mut leading = Vec::new();
            let mut cursor = &i.tree;
            while let syn::UseTree::Path(path) = cursor {
                leading.push(path.ident.clone());
                cursor = &path.tree;
            }

            if let Some((prefix_len, metavar)) = self.relative_prefix_metavar(&leading) {
                // Peel off the prefix segments and reattach the remainder under the metavar.
                let mut remaining = std::mem::replace(
                    &mut i.tree,
                    syn::UseTree::Glob(syn::UseGlob {
                        star_token: Default::default(),
                    }),
                );
                for _ in 0..prefix_len {
                    remaining = match remaining {
                        syn::UseTree::Path(path) => *path.tree,
                        _ => unreachable!("prefix segments are always `UseTree::Path`"),
                    };
                }
                i.tree = syn::UseTree::Path(syn::UsePath {
                    ident: metavar,
                    colon2_token: Default::default(),
                    tree: Box::new(remaining),
                });
            }
        }

        // The idents in a `use` path refer to crates / modules / items, never
        // to local variables, so they must not be captured as free variables
        // (and must not be renamed to `x__free`). Instead, register the names
        // the `use` binds so later references to them are not captured either.
        register_use_tree_bindings(&i.tree, None, &mut self.current_scope);
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
            && let Some((prefix_len, metavar)) =
                self.relative_prefix_metavar(i.segments.iter().map(|s| &s.ident))
        {
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
