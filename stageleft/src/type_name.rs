use std::sync::{LazyLock, RwLock};

use proc_macro2::Span;
use syn::visit_mut::VisitMut;
use syn::{TypeInfer, parse_quote};

use crate::runtime_support::get_final_crate_name;

type ReexportsSet = LazyLock<RwLock<Vec<(Vec<String>, Vec<String>)>>>;
static PRIVATE_REEXPORTS: ReexportsSet = LazyLock::new(|| {
    RwLock::new(vec![
        (vec!["alloc".into()], vec!["std".into()]),
        (
            vec!["core".into(), "ops".into(), "range".into()],
            vec!["std".into(), "ops".into()],
        ),
        (
            vec!["core".into(), "slice".into(), "iter".into()],
            vec!["std".into(), "slice".into()],
        ),
        (
            vec!["core".into(), "iter".into(), "adapters".into(), "*".into()],
            vec!["std".into(), "iter".into()],
        ),
        (
            vec![
                "std".into(),
                "collections".into(),
                "hash".into(),
                "map".into(),
            ],
            vec!["std".into(), "collections".into(), "hash_map".into()],
        ),
        (
            vec![
                "std".into(),
                "collections".into(),
                "hash".into(),
                "set".into(),
            ],
            vec!["std".into(), "collections".into(), "hash_set".into()],
        ),
        (
            vec!["std".into(), "vec".into(), "into_iter".into()],
            vec!["std".into(), "vec".into()],
        ),
        (
            vec!["std".into(), "io".into(), "error".into()],
            vec!["std".into(), "io".into()],
        ),
        (
            vec!["tokio".into(), "time".into(), "instant".into()],
            vec!["tokio".into(), "time".into()],
        ),
        (vec!["bytes".into(), "bytes".into()], vec!["bytes".into()]),
        (
            vec!["bytes".into(), "bytes_mut".into()],
            vec!["bytes".into()],
        ),
    ])
});

static DEPS_REEXPORTS: RwLock<Vec<(Vec<String>, Vec<String>)>> = RwLock::new(vec![]);

static CRATES_WITH_STAGED: RwLock<Vec<String>> = RwLock::new(Vec::new());

/// Adds a private module re-export transformation to the type quoting system.
///
/// Sometimes, the [`quote_type`] function may produce an uncompilable reference to a
/// type inside a private module if the type is re-exported from a public module
/// (because Rust's `type_name` only gives the path to the original definition).
///
/// This function adds a rewrite rule for such cases, where the `from` path is
/// replaced with the `to` path. The paths are given as a list of strings, where
/// each string is a segment of the path. The `from` path is matched against the
/// beginning of the type path, and if it matches, the `to` path is substituted
/// in its place. The `from` path may contain a wildcard `*` to glob a segment.
///
/// # Example
/// ```rust
/// stageleft::add_private_reexport(
///     vec!["std", "collections", "hash", "map"],
///     vec!["std", "collections", "hash_map"],
/// );
/// ```
pub fn add_private_reexport(from: Vec<impl Into<String>>, to: Vec<impl Into<String>>) {
    let mut transformations = PRIVATE_REEXPORTS.write().unwrap();
    transformations.push((
        from.into_iter().map(Into::into).collect(),
        to.into_iter().map(Into::into).collect(),
    ));
}

pub fn add_deps_reexport(from: Vec<impl Into<String>>, to: Vec<impl Into<String>>) {
    let mut transformations = DEPS_REEXPORTS.write().unwrap();
    transformations.push((
        from.into_iter().map(Into::into).collect(),
        to.into_iter().map(Into::into).collect(),
    ));
}

#[doc(hidden)]
/// Internal API that marks a crate which has an `__staged` companion module to resolve
/// symbols in quoted code.
pub fn add_crate_with_staged(name: impl Into<String>) {
    let mut crates = CRATES_WITH_STAGED.write().unwrap();
    crates.push(name.into());
}

struct RewritePrivateReexports {
    mapping: Option<(String, String)>,
}

impl VisitMut for RewritePrivateReexports {
    fn visit_path_mut(&mut self, i: &mut syn::Path) {
        let transformations = PRIVATE_REEXPORTS.read().unwrap();
        let deps_transformations = DEPS_REEXPORTS.read().unwrap();
        for (from, to) in transformations.iter().chain(deps_transformations.iter()) {
            #[expect(clippy::cmp_owned, reason = "buggy lint for syn::Ident::to_string")]
            if i.segments.len() >= from.len()
                && from
                    .iter()
                    .zip(i.segments.iter())
                    .all(|(f, s)| *f == "*" || *f == s.ident.to_string())
            {
                *i = syn::Path {
                    leading_colon: i.leading_colon,
                    segments: syn::punctuated::Punctuated::from_iter(
                        to.iter()
                            .map(|s| syn::PathSegment::from(syn::Ident::new(s, Span::call_site())))
                            .chain(i.segments.iter().skip(from.len()).cloned()),
                    ),
                };

                drop(transformations);
                self.visit_path_mut(i);
                return;
            }
        }
        drop(transformations);

        if let Some((macro_name, final_name)) = &self.mapping {
            if i.segments.first().unwrap().ident == macro_name {
                *i.segments.first_mut().unwrap() =
                    syn::parse2(get_final_crate_name(final_name)).unwrap();

                i.segments.insert(1, parse_quote!(__staged));
            } else {
                syn::visit_mut::visit_path_mut(self, i);
            }
        } else {
            syn::visit_mut::visit_path_mut(self, i);
        }
    }
}

struct RewriteCrateWithStaged;

impl VisitMut for RewriteCrateWithStaged {
    fn visit_path_mut(&mut self, i: &mut syn::Path) {
        if let Some(first_segment) = i.segments.first_mut() {
            let crates = CRATES_WITH_STAGED.read().unwrap();

            if crates.contains(&first_segment.ident.to_string())
                && i.segments.get(1).is_none_or(|s| s.ident != "__staged")
            {
                i.segments.insert(1, parse_quote!(__staged));
            }
        }

        syn::visit_mut::visit_path_mut(self, i);
    }
}

struct ElimClosureToInfer;

impl VisitMut for ElimClosureToInfer {
    fn visit_type_mut(&mut self, i: &mut syn::Type) {
        if let syn::Type::Path(p) = i
            && p.path
                .segments
                .iter()
                .any(|s| s.ident == "CLOSURE_TO_INFER")
        {
            *i = syn::Type::Infer(TypeInfer {
                underscore_token: Default::default(),
            });
            return;
        }

        syn::visit_mut::visit_type_mut(self, i);
    }
}

/// Captures a fully qualified path to a given type, which is useful when
/// the generated code needs to explicitly refer to a type known at staging time.
///
/// This API is fairly experimental, and comes with caveats. For example, it cannot
/// handle closure types. In addition, when a user refers to a re-exported type,
/// the original type path may be returned here, which could involve private modules.
///
/// Also, users must be careful to ensure that any crates referred in the type are
/// available where it is spliced.
pub fn quote_type<T>() -> syn::Type {
    let name = std::any::type_name::<T>().replace("{{closure}}", "CLOSURE_TO_INFER");
    let mut t_type: syn::Type = syn::parse_str(&name).unwrap_or_else(|_| {
        panic!("Could not parse type name: {name}");
    });
    let mapping = super::runtime_support::MACRO_TO_CRATE.with(|m| m.borrow().clone());
    ElimClosureToInfer.visit_type_mut(&mut t_type);
    RewritePrivateReexports { mapping }.visit_type_mut(&mut t_type);
    RewriteCrateWithStaged.visit_type_mut(&mut t_type);

    t_type
}
