use std::collections::BTreeMap;
use std::path::Path;
use std::{env, fs};

use proc_macro2::Span;
use quote::ToTokens;
use sha2::{Digest, Sha256};
use syn::parse_quote;
use syn::visit::Visit;
use syn::visit_mut::VisitMut;
use toml_edit::DocumentMut;

struct GenMacroVistor {
    exported_macros: BTreeMap<(String, String), Vec<syn::Attribute>>,
    current_mod: syn::Path,
}

// marks everything as pub(crate) because proc-macros cannot actually export anything
impl<'a> Visit<'a> for GenMacroVistor {
    fn visit_item_mod(&mut self, i: &'a syn::ItemMod) {
        // Push
        self.current_mod.segments.push(i.ident.clone().into());

        syn::visit::visit_item_mod(self, i);

        // Pop
        self.current_mod.segments.pop().unwrap();
        self.current_mod.segments.pop_punct().unwrap(); // Remove trailing `::`.
    }

    fn visit_item_fn(&mut self, i: &'a syn::ItemFn) {
        let is_entry = i
            .attrs
            .iter()
            .any(|a| a.path().to_token_stream().to_string() == "stageleft :: entry"); // TODO(mingwei): use #root?

        if is_entry {
            let cur_path = &self.current_mod;
            let mut i_cloned = i.clone();
            i_cloned.attrs = vec![];
            i_cloned.vis = syn::Visibility::Inherited; // normalize pub
            let non_entry_attrs = i
                .attrs
                .iter()
                .filter(|a| {
                    a.path().to_token_stream().to_string() != "stageleft :: entry" // TODO(mingwei): use #root?
                })
                .cloned()
                .collect::<Vec<_>>();

            let contents = i_cloned
                .to_token_stream()
                .to_string()
                .chars()
                .filter(|c| c.is_alphanumeric())
                .collect::<String>();
            let contents_hash = format!("{:X}", Sha256::digest(contents));
            self.exported_macros.insert(
                (contents_hash, cur_path.to_token_stream().to_string()),
                non_entry_attrs,
            );
        }
    }
}

pub fn gen_macro(staged_path: &Path, crate_name: &str) {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("lib_macro.rs");

    let flow_lib =
        syn_inline_mod::parse_and_inline_modules(&staged_path.join("src").join("lib.rs"));
    let mut visitor = GenMacroVistor {
        exported_macros: Default::default(),
        current_mod: parse_quote!(crate),
    };
    visitor.visit_file(&flow_lib);

    let staged_path_absolute = fs::canonicalize(staged_path).unwrap();

    let mut out_file: syn::File = parse_quote!();

    for ((hash, exported_from), attrs) in visitor.exported_macros {
        let underscored_path = syn::Ident::new(&("macro_".to_owned() + &hash), Span::call_site());
        let underscored_path_impl =
            syn::Ident::new(&("macro_".to_owned() + &hash + "_impl"), Span::call_site());
        let exported_from_parsed: syn::Path = syn::parse_str(&exported_from).unwrap();

        let proc_macro_wrapper: syn::ItemFn = parse_quote!(
            #(#attrs)*
            #[proc_macro]
            #[expect(unused_qualifications, non_snake_case, reason = "generated code")]
            pub fn #underscored_path(input: ::proc_macro::TokenStream) -> ::proc_macro::TokenStream {
                let input = ::stageleft::internal::TokenStream::from(input);
                let out = #exported_from_parsed::#underscored_path_impl(input);
                ::proc_macro::TokenStream::from(out)
            }
        );

        out_file.items.push(syn::Item::Fn(proc_macro_wrapper));
    }

    fs::write(dest_path, prettyplease::unparse(&out_file)).unwrap();

    println!("cargo::rustc-check-cfg=cfg(stageleft_macro)");
    println!("cargo::rustc-check-cfg=cfg(stageleft_runtime)");
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rustc-env=STAGELEFT_FINAL_CRATE_NAME={crate_name}");
    println!(
        "cargo::rustc-env=STAGELEFT_FINAL_CRATE_MANIFEST_DIR={}",
        staged_path_absolute.display()
    );
    println!("cargo::rustc-cfg=stageleft_macro");

    println!(
        "cargo::rerun-if-changed={}",
        staged_path_absolute.to_string_lossy()
    );
}

struct GenFinalPubVisitor {
    /// The current module path, starting with `crate`.
    current_mod: syn::Path,
    /// Stack of if each segment of `current_mod` is pub.
    stack_is_pub: Vec<bool>,

    /// If `Some("FEATURE")`, `#[cfg(test)]` modules will be gated with `#[cfg(feature = "FEATURE")]` instead of being
    /// fully removed.
    test_mode_feature: Option<String>,

    /// Whether the staged crate will be included in a separate crate (instead of the original crate as is usual). If
    /// true, then disables `pub use` [re-exporting from non-`pub` ancestor modules](https://doc.rust-lang.org/reference/visibility-and-privacy.html#r-vis.access).
    is_staged_separate: bool,

    crate_root_modules: Vec<String>,
}
impl GenFinalPubVisitor {
    pub fn new(
        orig_crate_ident: syn::Path,
        test_mode_feature: Option<String>,
        is_staged_separate: bool,
        crate_root_modules: Vec<String>,
    ) -> Self {
        Self {
            current_mod: orig_crate_ident,
            stack_is_pub: Vec::new(),
            test_mode_feature,
            is_staged_separate,
            crate_root_modules,
        }
    }

    /// If items in the current module path ([`Self::current_mod`]) are accessible, for `pub use` re-exporting.
    fn can_access_current(&self) -> bool {
        self.stack_is_pub
            .iter()
            // If the staged crate is included in the original crate, the innermost module may be private due to the
            // ancestor rule: https://doc.rust-lang.org/reference/visibility-and-privacy.html#r-vis.access
            .skip(if self.is_staged_separate { 0 } else { 1 })
            .all(|&x| x)
    }
}

fn get_cfg_attrs(attrs: &[syn::Attribute]) -> impl Iterator<Item = &syn::Attribute> + '_ {
    attrs.iter().filter(|attr| attr.path().is_ident("cfg"))
}

fn is_runtime(attrs: &[syn::Attribute]) -> bool {
    get_cfg_attrs(attrs)
        .any(|attr| attr.to_token_stream().to_string() == "# [cfg (stageleft_runtime)]")
}

fn get_stageleft_export_items(attrs: &[syn::Attribute]) -> Option<Vec<syn::Ident>> {
    attrs
        .iter()
        .filter(|a| a.path().to_token_stream().to_string() == "stageleft :: export") // TODO(mingwei): use #root?
        .filter_map(|a| {
            a.parse_args_with(
                syn::punctuated::Punctuated::<syn::Ident, syn::Token![,]>::parse_terminated,
            )
            .ok()
        })
        .fold(None, |mut acc, curr| {
            acc.get_or_insert_default().extend(curr.iter().cloned());
            acc
        })
}

fn item_attributes(item: &syn::Item) -> &[syn::Attribute] {
    match item {
        syn::Item::Const(i) => &i.attrs,
        syn::Item::Enum(i) => &i.attrs,
        syn::Item::ExternCrate(i) => &i.attrs,
        syn::Item::Fn(i) => &i.attrs,
        syn::Item::ForeignMod(i) => &i.attrs,
        syn::Item::Impl(i) => &i.attrs,
        syn::Item::Macro(i) => &i.attrs,
        syn::Item::Mod(i) => &i.attrs,
        syn::Item::Struct(i) => &i.attrs,
        syn::Item::Trait(i) => &i.attrs,
        syn::Item::Type(i) => &i.attrs,
        syn::Item::Union(i) => &i.attrs,
        syn::Item::Use(i) => &i.attrs,
        syn::Item::Static(i) => &i.attrs,
        syn::Item::TraitAlias(i) => &i.attrs,
        syn::Item::Verbatim(_) => &[],
        x => panic!("Unknown item type: {:?}", x),
    }
}

fn item_visibility_ident(item: &syn::Item) -> Option<(&syn::Visibility, &syn::Ident)> {
    match item {
        syn::Item::Const(i) => Some((&i.vis, &i.ident)),
        syn::Item::Enum(i) => Some((&i.vis, &i.ident)),
        syn::Item::Fn(i) => Some((&i.vis, &i.sig.ident)),
        syn::Item::Static(i) => Some((&i.vis, &i.ident)),
        syn::Item::Struct(i) => Some((&i.vis, &i.ident)),
        syn::Item::Trait(i) => Some((&i.vis, &i.ident)),
        syn::Item::TraitAlias(i) => Some((&i.vis, &i.ident)),
        syn::Item::Type(i) => Some((&i.vis, &i.ident)),
        syn::Item::Union(i) => Some((&i.vis, &i.ident)),
        _ => None,
    }
}

impl VisitMut for GenFinalPubVisitor {
    fn visit_item_enum_mut(&mut self, i: &mut syn::ItemEnum) {
        i.vis = parse_quote!(pub);
        syn::visit_mut::visit_item_enum_mut(self, i);
    }

    fn visit_variant_mut(&mut self, _i: &mut syn::Variant) {
        // variant fields do not have visibility modifiers
    }

    fn visit_item_static_mut(&mut self, i: &mut syn::ItemStatic) {
        i.vis = parse_quote!(pub);
        syn::visit_mut::visit_item_static_mut(self, i);
    }

    fn visit_item_const_mut(&mut self, i: &mut syn::ItemConst) {
        i.vis = parse_quote!(pub);
        syn::visit_mut::visit_item_const_mut(self, i);
    }

    fn visit_item_struct_mut(&mut self, i: &mut syn::ItemStruct) {
        i.vis = parse_quote!(pub);
        syn::visit_mut::visit_item_struct_mut(self, i);
    }

    fn visit_item_type_mut(&mut self, i: &mut syn::ItemType) {
        i.vis = parse_quote!(pub);
        syn::visit_mut::visit_item_type_mut(self, i);
    }

    fn visit_field_mut(&mut self, i: &mut syn::Field) {
        i.vis = parse_quote!(pub);
        syn::visit_mut::visit_field_mut(self, i);
    }

    fn visit_item_use_mut(&mut self, i: &mut syn::ItemUse) {
        i.vis = parse_quote!(pub);

        if self.current_mod.get_ident().is_some_and(|i| i == "crate")
            && let syn::UseTree::Path(p) = &mut i.tree
            && self.crate_root_modules.contains(&p.ident.to_string())
        {
            let orig_path = p.clone();
            *p = syn::UsePath {
                ident: syn::Ident::new("self", Span::call_site()),
                colon2_token: Default::default(),
                tree: Box::new(syn::UseTree::Path(orig_path)),
            };
        }

        syn::visit_mut::visit_item_use_mut(self, i);
    }

    fn visit_use_path_mut(&mut self, i: &mut syn::UsePath) {
        if i.ident == "crate" {
            *i.tree = syn::UseTree::Path(syn::UsePath {
                ident: parse_quote!(__staged),
                colon2_token: Default::default(),
                tree: i.tree.clone(),
            });
        }

        syn::visit_mut::visit_use_path_mut(self, i);
    }

    fn visit_vis_restricted_mut(&mut self, _i: &mut syn::VisRestricted) {
        // don't treat the restriction as a path, we don't want to rewrite that to `__staged`
    }

    fn visit_path_mut(&mut self, i: &mut syn::Path) {
        if !i.segments.is_empty() && i.segments[0].ident == "crate" {
            i.segments.insert(
                1,
                syn::PathSegment {
                    ident: parse_quote!(__staged),
                    arguments: Default::default(),
                },
            );
        }

        syn::visit_mut::visit_path_mut(self, i);
    }

    fn visit_item_mod_mut(&mut self, i: &mut syn::ItemMod) {
        // Push
        self.current_mod.segments.push(i.ident.clone().into());
        self.stack_is_pub
            .push(matches!(i.vis, syn::Visibility::Public(_)));

        syn::visit_mut::visit_item_mod_mut(self, i);

        // Pop
        self.current_mod.segments.pop().unwrap();
        self.current_mod.segments.pop_punct().unwrap(); // Remove trailing `::`.
        self.stack_is_pub.pop().unwrap();

        // Make module pub.
        i.vis = parse_quote!(pub);
    }

    fn visit_item_fn_mut(&mut self, i: &mut syn::ItemFn) {
        i.vis = parse_quote!(pub);
        syn::visit_mut::visit_item_fn_mut(self, i);
    }

    fn visit_item_mut(&mut self, i: &mut syn::Item) {
        // TODO(shadaj): warn if a pub struct or enum has private fields
        // and is not marked for runtime
        let cur_path = &self.current_mod;

        // Remove if marked with `#[cfg(stageleft_runtime)]`
        if is_runtime(item_attributes(i)) {
            *i = syn::Item::Verbatim(Default::default());
            return;
        }

        match i {
            syn::Item::Macro(m) => {
                // TODO(mingwei): Handle if `can_access_current()` is false
                if let Some(exported_items) = get_stageleft_export_items(&m.attrs) {
                    *i = parse_quote! {
                        pub use #cur_path::{ #( #exported_items ),* };
                    };
                    return;
                }

                if m.attrs
                    .iter()
                    .any(|a| a.to_token_stream().to_string() == "# [macro_export]")
                {
                    // Macro is already exported by the original
                    *i = syn::Item::Verbatim(Default::default());
                    return;
                }

                let is_ctor = m
                    .mac
                    .path
                    .to_token_stream()
                    .to_string()
                    .ends_with("ctor :: declarative :: ctor");

                if is_ctor {
                    // no quoted code depends on this module, so we do not need to copy it
                    *i = syn::Item::Verbatim(Default::default());
                    return;
                }
            }
            syn::Item::Impl(_e) => {
                // TODO(shadaj): emit impls if the **struct** is private
                // currently, we just skip all impls
                *i = syn::Item::Verbatim(Default::default());
                return;
            }
            syn::Item::Mod(m) => {
                let is_test_mod = m
                    .attrs
                    .iter()
                    .any(|a| a.to_token_stream().to_string() == "# [cfg (test)]");

                if is_test_mod {
                    m.attrs
                        .retain(|a| a.to_token_stream().to_string() != "# [cfg (test)]");

                    if let Some(feature) = &self.test_mode_feature {
                        m.attrs.insert(0, parse_quote!(#[cfg(feature = #feature)]));
                    } else {
                        // if test mode is not enabled, there are no quoted snippets behind #[cfg(test)],
                        // so no #[cfg(test)] modules will ever be reachable
                        *i = syn::Item::Verbatim(Default::default());
                        return;
                    }
                }
            }
            syn::Item::Fn(f) => {
                let is_ctor = f
                    .attrs
                    .iter()
                    .any(|a| a.path().to_token_stream().to_string() == "ctor :: ctor");

                let is_test = f.attrs.iter().any(|a| {
                    a.path().to_token_stream().to_string() == "test"
                        || a.path().to_token_stream().to_string() == "tokio :: test"
                });

                if is_ctor || is_test {
                    // no quoted code depends on this module, so we do not need to copy it
                    *i = syn::Item::Verbatim(Default::default());
                    return;
                }
            }
            _ => {}
        }

        // If a named item can be accessed (mod can be accessed and item is pub), simply re-export from original crate.
        if self.can_access_current()
            && let Some((syn::Visibility::Public(_), name_ident)) = item_visibility_ident(i)
        {
            let cfg_attrs = get_cfg_attrs(item_attributes(i));
            *i = parse_quote!(#(#cfg_attrs)* pub use #cur_path::#name_ident;);
            return;
        }

        syn::visit_mut::visit_item_mut(self, i);
    }

    fn visit_file_mut(&mut self, i: &mut syn::File) {
        i.attrs = vec![];
        i.items.retain(|i| match i {
            syn::Item::Macro(m) => {
                m.mac.path.to_token_stream().to_string() != "stageleft :: stageleft_crate" // TODO(mingwei): use #root?
                    && m.mac.path.to_token_stream().to_string()
                        != "stageleft :: stageleft_no_entry_crate" // TODO(mingwei): use #root?
            }
            _ => true,
        });

        syn::visit_mut::visit_file_mut(self, i);
    }
}

/// For an optional dependency, compute the list of features that enable it.
///
/// A feature enables the optional dependency if its list contains `dep:<name>` or a "strong"
/// dependency feature reference `<name>/<feat>` (but not a "weak" reference `<name>?/<feat>`,
/// which only applies if the dependency is enabled by something else).
///
/// Additionally, if no feature references `dep:<name>`, Cargo creates an implicit feature with
/// the dep's name, which is included in the returned list.
fn optional_dep_features(dep_name: &str, features_table: Option<&toml_edit::Table>) -> Vec<String> {
    let dep_ref = format!("dep:{dep_name}");
    let strong_ref_prefix = format!("{dep_name}/");
    // Whether any feature references `dep:<name>`, which suppresses the implicit feature.
    let mut any_dep_ref = false;
    let mut gating: Vec<String> = Vec::new();
    for (feat, vals) in features_table.into_iter().flat_map(|t| t.iter()) {
        let enables = vals.as_array().is_some_and(|arr| {
            arr.iter().filter_map(|v| v.as_str()).fold(false, |acc, s| {
                if s == dep_ref {
                    any_dep_ref = true;
                    true
                } else {
                    // A strong `<name>/<feat>` reference also enables the optional dep.
                    // (A weak `<name>?/<feat>` reference does not.)
                    acc || s.starts_with(&strong_ref_prefix)
                }
            })
        });
        if enables {
            gating.push(feat.to_owned());
        }
    }
    if !any_dep_ref {
        // No `dep:<name>` references, so Cargo creates an implicit feature named after the dep.
        let implicit = dep_name.to_owned();
        if !gating.contains(&implicit) {
            gating.push(implicit);
        }
    }
    gating
}

/// A single declaration of a dependency, from `[dependencies]` or a
/// `[target.'cfg(...)'.dependencies]` table.
struct DepDeclaration {
    /// The `cfg(...)` predicate from the `[target.'cfg(...)']` key, or `None` for the plain
    /// `[dependencies]` table.
    target_cfg: Option<syn::Meta>,
    /// The features which enable the dependency, empty if the dependency is not optional.
    gating_features: Vec<String>,
}

impl DepDeclaration {
    /// The `cfg` predicate under which this declaration makes the dependency available, or
    /// `None` if it is unconditionally available.
    fn cfg_predicate(&self) -> Option<syn::Meta> {
        let feature_pred: Option<syn::Meta> = if self.gating_features.is_empty() {
            None
        } else {
            let preds = self
                .gating_features
                .iter()
                .map(|f| -> syn::Meta { parse_quote!(feature = #f) });
            Some(parse_quote!(any(#(#preds),*)))
        };
        match (&self.target_cfg, feature_pred) {
            (Some(target), Some(features)) => Some(parse_quote!(all(#target, #features))),
            (Some(target), None) => Some(target.clone()),
            (None, features) => features,
        }
    }
}

/// Build a `#[cfg(...)]` attribute gating a dependency declared by `declarations`, or `None` if
/// the dependency is unconditionally available.
fn cfg_attr_for_declarations(declarations: &[DepDeclaration]) -> Option<syn::Attribute> {
    let mut preds = Vec::with_capacity(declarations.len());
    for declaration in declarations {
        // If any declaration is unconditional, the dependency is always available.
        let pred = declaration.cfg_predicate()?;
        preds.push(pred);
    }
    if let [pred] = &*preds {
        Some(parse_quote!(#[cfg(#pred)]))
    } else {
        Some(parse_quote!(#[cfg(any(#(#preds),*))]))
    }
}

/// Parse a `[target.'<key>']` key such as `cfg(target_os = "linux")` into a `cfg` predicate.
/// Returns `None` for keys which are not `cfg(...)` expressions (i.e. plain target triples).
fn parse_target_cfg_key(target_key: &str) -> Option<syn::Meta> {
    let inner = target_key.trim().strip_prefix("cfg(")?.strip_suffix(')')?;
    syn::parse_str::<syn::Meta>(inner).ok()
}

/// A dependency of the staged crate, possibly declared in multiple tables
/// (e.g. `[dependencies]` and/or one or more `[target.'cfg(...)'.dependencies]` tables).
struct Dep {
    /// Dependency name with `-` replaced by `_`.
    name: String,
    /// `package = "..."` renamed crate name (with `-` replaced by `_`), if any.
    original_crate_name: Option<String>,
    declarations: Vec<DepDeclaration>,
}

/// Collect dependencies from a `[dependencies]`-shaped table into `deps`, merging entries for
/// dependencies which are declared in multiple tables.
fn collect_deps_from_table(
    deps_table: &dyn toml_edit::TableLike,
    target_cfg: Option<&syn::Meta>,
    features_table: Option<&toml_edit::Table>,
    deps: &mut Vec<Dep>,
) {
    for (name, v) in deps_table.iter() {
        let is_optional = v.get("optional").and_then(|o| o.as_bool()).unwrap_or(false);
        let gating_features = if is_optional {
            optional_dep_features(name, features_table)
        } else {
            vec![]
        };
        let declaration = DepDeclaration {
            target_cfg: target_cfg.cloned(),
            gating_features,
        };

        let name_underscored = name.replace('-', "_");
        if let Some(existing) = deps.iter_mut().find(|d| d.name == name_underscored) {
            existing.declarations.push(declaration);
        } else {
            deps.push(Dep {
                name: name_underscored,
                original_crate_name: v
                    .get("package")
                    .map(|v| v.as_str().unwrap().replace("-", "_")),
                declarations: vec![declaration],
            });
        }
    }
}

fn gen_deps_module(stageleft_name: syn::Ident, manifest_path: &Path) -> syn::ItemMod {
    // based on proc-macro-crate
    let toml_parsed = fs::read_to_string(manifest_path)
        .unwrap()
        .parse::<DocumentMut>()
        .unwrap();
    let features_table = toml_parsed.get("features").and_then(|v| v.as_table());

    let mut all_deps = Vec::new();
    if let Some(deps_table) = toml_parsed
        .get("dependencies")
        .and_then(|v| v.as_table_like())
    {
        collect_deps_from_table(deps_table, None, features_table, &mut all_deps);
    }
    // Also collect target-specific dependencies from `[target.'cfg(...)'.dependencies]` tables.
    for (target_key, target_val) in toml_parsed
        .get("target")
        .and_then(|v| v.as_table_like())
        .into_iter()
        .flat_map(|t| t.iter())
    {
        let Some(deps_table) = target_val
            .get("dependencies")
            .and_then(|v| v.as_table_like())
        else {
            continue;
        };
        if let Some(target_cfg) = parse_target_cfg_key(target_key) {
            collect_deps_from_table(deps_table, Some(&target_cfg), features_table, &mut all_deps);
        } else {
            println!(
                "cargo::warning=stageleft: skipping `[target.'{target_key}'.dependencies]`: only `cfg(...)` target keys are supported in `__deps`"
            );
        }
    }

    let deps_reexported = all_deps
        .iter()
        .map(|dep| {
            let name_ident = syn::Ident::new(&dep.name, Span::call_site());
            let cfg = cfg_attr_for_declarations(&dep.declarations);
            parse_quote! {
                #cfg
                pub use #name_ident;
            }
        })
        .collect::<Vec<syn::Item>>();

    let deps_reexported_runtime = all_deps
        .iter()
        .map(|dep| {
            let name = &dep.name;
            let original_crate_name_or_alias = dep.original_crate_name.as_deref().unwrap_or(name);
            let cfg = cfg_attr_for_declarations(&dep.declarations);
            parse_quote! {
                #cfg
                {
                    #stageleft_name::internal::add_deps_reexport(
                        vec![#original_crate_name_or_alias],
                        vec![
                            option_env!("STAGELEFT_FINAL_CRATE_NAME")
                                .unwrap_or(env!("CARGO_PKG_NAME"))
                                .replace("-", "_"),
                            ::std::borrow::ToOwned::to_owned("__staged"),
                            ::std::borrow::ToOwned::to_owned("__deps"),
                            ::std::borrow::ToOwned::to_owned(#name),
                        ]
                    );
                }
            }
        })
        .collect::<Vec<syn::Stmt>>();

    syn::parse_quote! {
        pub mod __deps {
            #(#deps_reexported)*

            #stageleft_name::internal::ctor::declarative::ctor! {
                #[ctor(unsafe)]
                fn __init() {
                    #(#deps_reexported_runtime)*
                    #stageleft_name::internal::add_crate_with_staged(env!("CARGO_PKG_NAME").replace("-", "_"));
                }
            }
        }
    }
}

/// Generates the contents of `mod __staged`, which contains a copy of the crate's code but with
/// all APIs made public so they can be resolved when quoted code is spliced.
///
/// # Arguments
/// * `lib_path` - path to the root Rust file, usually to `lib.rs`.
/// * `orig_crate_path` - Rust module path to the staged crate. Usually `crate`, but may be the staged crate name if
///   the entry and staged crate/target are different.
/// * `is_staged_separate` - Whether the staged crate will be included in a separate crate (instead of the original
///   crate as is usual). If true, then disables `pub use` [re-exporting from non-`pub` ancestor modules](https://doc.rust-lang.org/reference/visibility-and-privacy.html#r-vis.access).
/// * `test_mode_feature` - If `Some("FEATURE")`, `#[cfg(test)]` modules will be gated with
///   `#[cfg(feature = "FEATURE")]` instead of being fully removed.
fn gen_staged_mod(
    lib_path: &Path,
    orig_crate_path: syn::Path,
    test_mode_feature: Option<String>,
    is_staged_separate: bool,
) -> syn::File {
    assert!(
        !orig_crate_path.segments.trailing_punct(),
        "`orig_crate_path` may not have trailing `::`"
    );

    let mut flow_lib_pub = syn_inline_mod::parse_and_inline_modules(lib_path);
    let crate_root_modules = flow_lib_pub
        .items
        .iter()
        .filter_map(|i| match i {
            syn::Item::Mod(m) => Some(m.ident.to_string()),
            _ => None,
        })
        .collect();

    let mut final_pub_visitor = GenFinalPubVisitor::new(
        orig_crate_path.clone(),
        test_mode_feature,
        is_staged_separate,
        crate_root_modules,
    );
    final_pub_visitor.visit_file_mut(&mut flow_lib_pub);

    // macros exported with `#[macro_export]` are placed at the top-level of the crate,
    // so we need to pull them into the `mod __staged` so that relative imports resolve
    // correctly
    flow_lib_pub
        .items
        .insert(0, parse_quote!(pub use #orig_crate_path::*;));

    flow_lib_pub
}

/// Generates the contents for `__staged` when it will be emitted in "trybuild mode", which means that
/// it is included inline next to the spliced code that uses it, with the original crate available as
/// a dependency.
///
/// # Arguments
/// * `lib_path` - path to the root Rust file, usually to `lib.rs`.
/// * `manifest_path` - path to the package `Cargo.toml`.
/// * `orig_crate_path` - Rust module path to the staged crate. Usually `crate`, but may be the staged crate name if
///   the entry and staged crate/target are different.
/// * `test_mode_feature` - If `Some("FEATURE")`, `#[cfg(test)]` modules will be gated with
///   `#[cfg(feature = "FEATURE")]` instead of being fully removed.
pub fn gen_staged_trybuild(
    lib_path: &Path,
    manifest_path: &Path,
    orig_crate_path: &str,
    test_mode_feature: Option<String>,
) -> syn::File {
    let orig_crate_path = syn::parse_str(orig_crate_path)
        .expect("Failed to parse `orig_crate_path` as `crate`, crate name, or module path.");
    let mut flow_lib_pub = gen_staged_mod(lib_path, orig_crate_path, test_mode_feature, true);

    let deps_mod = gen_deps_module(parse_quote!(stageleft), manifest_path);

    flow_lib_pub.items.push(syn::Item::Mod(deps_mod));
    flow_lib_pub
}

/// Combined function that generates staged deps, test modules, and optionally lib_pub.
/// Parses the source tree only once.
#[doc(hidden)]
pub fn gen_staged(gen_pub: bool) {
    let out_dir = env::var_os("OUT_DIR").unwrap();

    let raw_toml_manifest = fs::read_to_string(Path::new("Cargo.toml"))
        .unwrap()
        .parse::<DocumentMut>()
        .unwrap();

    let maybe_custom_lib_path = raw_toml_manifest
        .get("lib")
        .and_then(|lib| lib.get("path"))
        .and_then(|path| path.as_str());

    let lib_path = maybe_custom_lib_path
        .map(Path::new)
        .unwrap_or_else(|| Path::new("src/lib.rs"));

    // Watch the directory containing the library root (usually `src`) so that changes to any
    // module file trigger a rebuild. Blindly emitting `rerun-if-changed=src` would cause
    // spurious rebuilds when a custom lib root is used and `src` does not exist, since Cargo
    // treats missing paths as always-dirty.
    let watch_path = lib_path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty() && parent.is_dir())
        .unwrap_or(lib_path);
    println!("cargo::rerun-if-changed={}", watch_path.display());

    // Parse source once
    let flow_lib = syn_inline_mod::parse_and_inline_modules(lib_path);

    // Generate lib_pub.rs if requested
    if gen_pub {
        let flow_lib_pub = gen_staged_mod(lib_path, parse_quote!(crate), None, false);

        fs::write(
            Path::new(&out_dir).join("lib_pub.rs"),
            prettyplease::unparse(&flow_lib_pub),
        )
        .unwrap();
    }

    // Generate staged_deps.rs
    // Remove `_tool` suffix.
    let main_pkg_name = env!("CARGO_PKG_NAME").rsplit_once(['-', '_']).unwrap().0;
    let stageleft_crate = proc_macro_crate::crate_name(main_pkg_name).unwrap_or_else(|_| {
        panic!("Expected stageleft {main_pkg_name} package to be present in `Cargo.toml`")
    });
    let stageleft_name = match stageleft_crate {
        proc_macro_crate::FoundCrate::Itself => syn::Ident::new(main_pkg_name, Span::call_site()),
        proc_macro_crate::FoundCrate::Name(name) => syn::Ident::new(&name, Span::call_site()),
    };

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_path = Path::new(&manifest_dir).join("Cargo.toml");

    let deps_file = gen_deps_module(stageleft_name, &manifest_path);

    fs::write(
        Path::new(&out_dir).join("staged_deps.rs"),
        prettyplease::unparse(&parse_quote!(#deps_file)),
    )
    .unwrap();

    // Collect test modules for fallback detection at splice time
    struct TestModCollector {
        current_mod: Vec<String>,
        test_modules: Vec<String>,
    }

    impl<'a> Visit<'a> for TestModCollector {
        fn visit_item_mod(&mut self, i: &'a syn::ItemMod) {
            let is_test_mod = i
                .attrs
                .iter()
                .any(|a| a.to_token_stream().to_string() == "# [cfg (test)]");

            self.current_mod.push(i.ident.to_string());

            if is_test_mod {
                self.test_modules.push(self.current_mod.join("::"));
            }

            syn::visit::visit_item_mod(self, i);
            self.current_mod.pop();
        }
    }

    let mut collector = TestModCollector {
        current_mod: Vec::new(),
        test_modules: Vec::new(),
    };
    Visit::visit_file(&mut collector, &flow_lib);

    let test_mod_strs = &collector.test_modules;
    let crate_name_str = env::var("CARGO_PKG_NAME").unwrap().replace('-', "_");

    let registration_body = quote::quote! {
        (#crate_name_str, &[#(#test_mod_strs),*])
    };

    fs::write(
        Path::new(&out_dir).join("test_modules.rs"),
        registration_body.to_string(),
    )
    .unwrap();
}

#[macro_export]
macro_rules! gen_final {
    () => {
        println!("cargo::rustc-check-cfg=cfg(stageleft_macro)");
        println!("cargo::rustc-check-cfg=cfg(stageleft_runtime)");
        println!("cargo::rustc-check-cfg=cfg(stageleft_trybuild)");
        println!("cargo::rustc-check-cfg=cfg(feature, values(\"stageleft_macro_entrypoint\"))");
        println!("cargo::rustc-cfg=stageleft_runtime");
        println!(
            "cargo::rustc-env=STAGELEFT_FINAL_CRATE_MANIFEST_DIR={}",
            std::env::var("CARGO_MANIFEST_DIR").unwrap()
        );

        println!("cargo::rerun-if-changed=Cargo.toml");
        println!("cargo::rerun-if-changed=build.rs");
        println!("cargo::rerun-if-env-changed=STAGELEFT_TRYBUILD_BUILD_STAGED");

        #[allow(
            unexpected_cfgs,
            reason = "Macro entrypoints must define the stageleft_macro_entrypoint feature"
        )]
        let gen_pub = if cfg!(feature = "stageleft_macro_entrypoint") {
            true
        } else if std::env::var("STAGELEFT_TRYBUILD_BUILD_STAGED").is_ok() {
            println!("cargo::rustc-cfg=stageleft_trybuild");
            true
        } else {
            false
        };

        $crate::gen_staged(gen_pub);
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_gen_deps_module_uses_crate_name_or_alias() {
        // Create a temporary Cargo.toml with a dependency that has a package alias
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(
            temp_file,
            r#"my_alias = {{ package = "actual_crate", version = "1.0" }}"#
        )
        .unwrap();
        writeln!(temp_file, r#"regular_crate = "2.0""#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());

        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains(r#""actual_crate""#),
            "Generated code should use actual crate name for aliased dependency: {}",
            generated_code
        );

        assert!(
            generated_code.contains(r#""regular_crate""#),
            "Generated code should use dependency name for regular dependency: {}",
            generated_code
        );

        assert!(
            generated_code.contains("add_deps_reexport"),
            "Generated code should contain add_deps_reexport calls: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_optional_implicit_feature() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(temp_file, r#"required_dep = "1.0""#).unwrap();
        writeln!(
            temp_file,
            r#"optional_dep = {{ version = "2.0", optional = true }}"#
        )
        .unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        // required dep should not be gated
        assert!(
            !generated_code.contains(r#"feature = "required_dep""#),
            "Required dep should not have cfg gate: {}",
            generated_code
        );
        // optional dep with no explicit dep: reference gets implicit feature
        assert!(
            generated_code.contains(r#"any (feature = "optional_dep")"#),
            "Optional dep should be gated by implicit feature: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_optional_explicit_dep_ref() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(
            temp_file,
            r#"optional_dep = {{ version = "1.0", optional = true }}"#
        )
        .unwrap();
        writeln!(temp_file, "\n[features]").unwrap();
        writeln!(temp_file, r#"my_feature = ["dep:optional_dep"]"#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains(r#"any (feature = "my_feature")"#),
            "Optional dep should be gated by explicit feature: {}",
            generated_code
        );
        assert!(
            !generated_code.contains(r#"any (feature = "optional_dep")"#),
            "Implicit feature should be suppressed when dep: is used: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_optional_multiple_features() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(
            temp_file,
            r#"optional_dep = {{ version = "1.0", optional = true }}"#
        )
        .unwrap();
        writeln!(temp_file, "\n[features]").unwrap();
        writeln!(temp_file, r#"feat_a = ["dep:optional_dep"]"#).unwrap();
        writeln!(temp_file, r#"feat_b = ["dep:optional_dep"]"#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains("any"),
            "Multiple features should use cfg(any(...)): {}",
            generated_code
        );
        assert!(
            generated_code.contains(r#"feature = "feat_a""#),
            "Should contain feat_a: {}",
            generated_code
        );
        assert!(
            generated_code.contains(r#"feature = "feat_b""#),
            "Should contain feat_b: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_optional_implicit_feature_transitive() {
        // When bar = ["foo"] and foo is optional with no dep:foo references,
        // the implicit feature "foo" is used. Enabling bar enables foo transitively.
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(temp_file, r#"foo = {{ version = "1.0", optional = true }}"#).unwrap();
        writeln!(temp_file, "\n[features]").unwrap();
        writeln!(temp_file, r#"bar = ["foo"]"#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        // Should gate on the implicit feature "foo", not "bar"
        assert!(
            generated_code.contains(r#"any (feature = "foo")"#),
            "Should gate on implicit feature foo: {}",
            generated_code
        );
        assert!(
            !generated_code.contains(r#"feature = "bar""#),
            "Should not gate on bar (it enables foo transitively): {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_optional_dep_in_default_feature() {
        // Cargo sets cfg(feature = "default") when default features are enabled,
        // so gating on "default" is correct behavior.
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(
            temp_file,
            r#"optional_dep = {{ version = "1.0", optional = true }}"#
        )
        .unwrap();
        writeln!(temp_file, "\n[features]").unwrap();
        writeln!(temp_file, r#"default = ["dep:optional_dep"]"#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains(r#"any (feature = "default")"#),
            "Should gate on default feature: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_optional_strong_dep_feature_ref() {
        // A strong `dep/feat` reference enables the optional dependency, so a feature
        // containing one must be included as a gating feature.
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(
            temp_file,
            r#"optional_dep = {{ version = "1.0", optional = true }}"#
        )
        .unwrap();
        writeln!(temp_file, "\n[features]").unwrap();
        writeln!(temp_file, r#"feat_a = ["dep:optional_dep"]"#).unwrap();
        writeln!(temp_file, r#"feat_b = ["optional_dep/some_feat"]"#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains(r#"any (feature = "feat_a" , feature = "feat_b")"#),
            "Both the dep: feature and the strong dep/feat feature should gate: {}",
            generated_code
        );
        assert!(
            !generated_code.contains(r#"feature = "optional_dep""#),
            "Implicit feature should be suppressed when dep: is used: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_optional_weak_dep_feature_ref() {
        // A weak `dep?/feat` reference does not enable the optional dependency, so a feature
        // containing one must not be included as a gating feature.
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(
            temp_file,
            r#"optional_dep = {{ version = "1.0", optional = true }}"#
        )
        .unwrap();
        writeln!(temp_file, "\n[features]").unwrap();
        writeln!(temp_file, r#"feat_a = ["dep:optional_dep"]"#).unwrap();
        writeln!(temp_file, r#"feat_w = ["optional_dep?/some_feat"]"#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains(r#"any (feature = "feat_a")"#),
            "Should gate on the dep: feature: {}",
            generated_code
        );
        assert!(
            !generated_code.contains(r#"feature = "feat_w""#),
            "Weak dep?/feat reference should not gate: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_target_specific_dep() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(temp_file, r#"regular_dep = "1.0""#).unwrap();
        writeln!(
            temp_file,
            "\n[target.'cfg(target_os = \"linux\")'.dependencies]"
        )
        .unwrap();
        writeln!(temp_file, r#"linux_dep = "1.0""#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains("pub use linux_dep"),
            "Target-specific dep should be re-exported: {}",
            generated_code
        );
        assert!(
            generated_code.contains(r#"# [cfg (target_os = "linux")] pub use linux_dep"#),
            "Target-specific dep should be gated by the target cfg: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_target_specific_optional_dep() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(
            temp_file,
            "[target.'cfg(target_os = \"linux\")'.dependencies]"
        )
        .unwrap();
        writeln!(
            temp_file,
            r#"procfs = {{ version = "0.17", optional = true }}"#
        )
        .unwrap();
        writeln!(temp_file, "\n[features]").unwrap();
        writeln!(temp_file, r#"runtime_measure = ["dep:procfs"]"#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains(
                r#"# [cfg (all (target_os = "linux" , any (feature = "runtime_measure")))] pub use procfs"#
            ),
            "Optional target-specific dep should be gated by both the target cfg and feature: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_dep_in_main_and_target_table() {
        // A dep declared unconditionally in [dependencies] and also in a target table
        // should be re-exported unconditionally.
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[dependencies]").unwrap();
        writeln!(temp_file, r#"tokio = "1.0""#).unwrap();
        writeln!(temp_file, "\n[target.'cfg(unix)'.dependencies]").unwrap();
        writeln!(
            temp_file,
            r#"tokio = {{ version = "1.0", features = ["fs"] }}"#
        )
        .unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains("pub use tokio"),
            "Dep should be re-exported: {}",
            generated_code
        );
        assert!(
            !generated_code.contains("cfg"),
            "Dep available unconditionally should not be cfg-gated: {}",
            generated_code
        );
        assert_eq!(
            generated_code.matches("pub use tokio").count(),
            1,
            "Dep should only be re-exported once: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_dep_in_multiple_target_tables() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[target.'cfg(unix)'.dependencies]").unwrap();
        writeln!(temp_file, r#"platform_dep = "1.0""#).unwrap();
        writeln!(temp_file, "\n[target.'cfg(windows)'.dependencies]").unwrap();
        writeln!(temp_file, r#"platform_dep = "1.0""#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            generated_code.contains(r#"# [cfg (any (unix , windows))] pub use platform_dep"#),
            "Dep in multiple target tables should be gated by any() of the target cfgs: {}",
            generated_code
        );
        assert_eq!(
            generated_code.matches("pub use platform_dep").count(),
            1,
            "Dep should only be re-exported once: {}",
            generated_code
        );
    }

    #[test]
    fn test_gen_deps_module_no_dependencies_table() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "[package]").unwrap();
        writeln!(temp_file, r#"name = "empty_crate""#).unwrap();
        temp_file.flush().unwrap();

        let stageleft_name = syn::Ident::new("stageleft", Span::call_site());
        let deps_module = gen_deps_module(stageleft_name, temp_file.path());
        let generated_code = quote::quote!(#deps_module).to_string();

        assert!(
            !generated_code.contains("pub use"),
            "No deps should be re-exported: {}",
            generated_code
        );
    }
}
