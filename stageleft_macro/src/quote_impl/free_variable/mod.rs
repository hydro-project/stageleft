use std::collections::{BTreeSet, HashSet};

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
    pub current_scope: ScopeStack,
}

impl<'a> syn::visit::Visit<'a> for FreeVariableVisitor {
    fn visit_expr_closure(&mut self, i: &syn::ExprClosure) {
        self.current_scope.push();
        i.inputs.iter().for_each(|input| {
            self.visit_pat(input);
        });

        syn::visit::visit_expr_closure(self, i);

        self.current_scope.pop();
    }

    fn visit_item_fn(&mut self, i: &syn::ItemFn) {
        self.current_scope.push();
        syn::visit::visit_item_fn(self, i);
        self.current_scope.pop();
    }

    fn visit_generic_param(&mut self, i: &syn::GenericParam) {
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

    fn visit_block(&mut self, i: &syn::Block) {
        self.current_scope.push();
        syn::visit::visit_block(self, i);
        self.current_scope.pop();
    }

    fn visit_local(&mut self, i: &syn::Local) {
        i.init.iter().for_each(|init| {
            syn::visit::visit_local_init(self, init);
        });
        match &i.pat {
            syn::Pat::Ident(pat_ident) => {
                self.current_scope.insert_term(pat_ident.ident.clone());
            }
            syn::Pat::Type(pat_type) => {
                self.visit_pat(&pat_type.pat);
            }
            syn::Pat::Wild(_) => {
                // Do nothing
            }
            syn::Pat::Tuple(pat_tuple) => {
                for el in &pat_tuple.elems {
                    self.visit_pat(el);
                }
            }
            _ => panic!("Local variables must be identifiers, got {:?}", i.pat),
        }
    }

    fn visit_ident(&mut self, i: &proc_macro2::Ident) {
        if !self.current_scope.contains_term(i) {
            self.free_variables.insert(i.clone());
        }
    }

    fn visit_lifetime(&mut self, i: &syn::Lifetime) {
        if !self.current_scope.contains_type(&i.ident) {
            self.free_variables.insert(i.ident.clone());
        }
    }

    fn visit_path(&mut self, i: &syn::Path) {
        if i.leading_colon.is_none() && !is_prelude(&i.segments.first().unwrap().ident) {
            let one_segment = i.segments.len() == 1;
            let node = i.segments.first().unwrap();
            if one_segment && !self.current_scope.contains_term(&node.ident) {
                self.free_variables.insert(node.ident.clone());
            }
        }

        for node in i.segments.iter() {
            self.visit_path_arguments(&node.arguments);
        }
    }

    fn visit_arm(&mut self, i: &syn::Arm) {
        self.current_scope.push();
        syn::visit::visit_arm(self, i);
        self.current_scope.pop();
    }

    fn visit_field_pat(&mut self, i: &syn::FieldPat) {
        for it in &i.attrs {
            self.visit_attribute(it);
        }
        self.visit_pat(&i.pat);
    }

    fn visit_pat_ident(&mut self, i: &syn::PatIdent) {
        self.current_scope.insert_term(i.ident.clone());
    }

    fn visit_expr_call(&mut self, i: &syn::ExprCall) {
        if let syn::Expr::Path(path) = &*i.func {
            if path.path.segments.len() == 1 {
                // skip, don't emit a free variable, assume that it's a
                // fn call and not a closure call; for closure calls, we
                // require the user to manually capture it with a `let` binding
            } else {
                syn::visit::visit_expr(self, &i.func);
            }
        } else {
            syn::visit::visit_expr(self, &i.func);
        }

        for arg in &i.args {
            self.visit_expr(arg);
        }
    }

    fn visit_expr_method_call(&mut self, i: &syn::ExprMethodCall) {
        syn::visit::visit_expr(self, &i.receiver);
        for arg in &i.args {
            self.visit_expr(arg);
        }
    }

    fn visit_type(&mut self, _: &syn::Type) {}

    fn visit_expr_struct(&mut self, node: &syn::ExprStruct) {
        for it in &node.attrs {
            self.visit_attribute(it);
        }
        if let Some(it) = &node.qself {
            self.visit_qself(it);
        }
        // No need to capture the struct path
        // self.visit_path(&node.path);
        for el in syn::punctuated::Punctuated::pairs(&node.fields) {
            let it = el.into_value();
            self.visit_expr(&it.expr);
        }
        if let Some(it) = &node.rest {
            self.visit_expr(it);
        }
    }

    fn visit_expr_field(&mut self, i: &syn::ExprField) {
        self.visit_expr(&i.base);
    }

    fn visit_macro(&mut self, i: &syn::Macro) {
        // TODO(shadaj): emit a warning if our guess at parsing fails
        match i.delimiter {
            syn::MacroDelimiter::Paren(_binding_0) => {
                i.parse_body_with(
                    syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated,
                )
                .ok()
                .into_iter()
                .for_each(|exprs| {
                    for arg in &exprs {
                        self.visit_expr(arg);
                    }
                });
            }
            syn::MacroDelimiter::Brace(_binding_0) => {
                i.parse_body_with(syn::Block::parse_within)
                    .ok()
                    .into_iter()
                    .for_each(|stmts| {
                        for stmt in &stmts {
                            self.visit_stmt(stmt);
                        }
                    });
            }
            syn::MacroDelimiter::Bracket(_binding_0) => {
                i.parse_body_with(
                    syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated,
                )
                .ok()
                .into_iter()
                .for_each(|exprs| {
                    for arg in &exprs {
                        self.visit_expr(arg);
                    }
                });
            }
        }
    }
}
