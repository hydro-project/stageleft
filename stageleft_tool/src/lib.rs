use std::collections::BTreeSet;
use std::path::Path;
use std::{env, fs};

use proc_macro2::Span;
use quote::ToTokens;
use sha2::{Digest, Sha256};
use syn::visit::Visit;
use syn::visit_mut::VisitMut;
use syn::{UsePath, Visibility, parse_quote};
use toml_edit::DocumentMut;

struct GenMacroVistor {
    exported_macros: BTreeSet<(String, String)>,
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
            i_cloned.vis = Visibility::Inherited; // normalize pub

            let contents = i_cloned
                .to_token_stream()
                .to_string()
                .chars()
                .filter(|c| c.is_alphanumeric())
                .collect::<String>();
            let contents_hash = format!("{:X}", Sha256::digest(contents));
            self.exported_macros
                .insert((contents_hash, cur_path.to_token_stream().to_string()));
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

    for (hash, exported_from) in visitor.exported_macros {
        let underscored_path = syn::Ident::new(&("macro_".to_string() + &hash), Span::call_site());
        let underscored_path_impl =
            syn::Ident::new(&("macro_".to_string() + &hash + "_impl"), Span::call_site());
        let exported_from_parsed: syn::Path = syn::parse_str(&exported_from).unwrap();

        let proc_macro_wrapper: syn::ItemFn = parse_quote!(
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

    /// All `#[macro_export]` declarative macros encountered, to be re-exported at the top `__staged` module due to the
    /// strange way `#[macro_export]` works.
    all_macros: Vec<syn::Ident>,
}
impl GenFinalPubVisitor {
    pub fn new(
        orig_crate_ident: syn::Path,
        test_mode_feature: Option<String>,
        is_staged_separate: bool,
    ) -> Self {
        Self {
            current_mod: orig_crate_ident,
            stack_is_pub: Vec::new(),
            test_mode_feature,
            is_staged_separate,
            all_macros: Vec::new(),
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
        syn::visit_mut::visit_item_use_mut(self, i);
    }

    fn visit_use_path_mut(&mut self, i: &mut UsePath) {
        if i.ident == "crate" {
            *i.tree = syn::UseTree::Path(UsePath {
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
            .push(matches!(i.vis, Visibility::Public(_)));

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
                    // Re-export macro at top-level later.
                    self.all_macros.push(m.ident.as_ref().unwrap().clone());
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
            && let Some((Visibility::Public(_), name_ident)) = item_visibility_ident(i)
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

fn gen_deps_module(stageleft_name: syn::Ident, manifest_path: &Path) -> syn::ItemMod {
    // based on proc-macro-crate
    let toml_parsed = fs::read_to_string(manifest_path)
        .unwrap()
        .parse::<DocumentMut>()
        .unwrap();
    let all_crate_names = toml_parsed["dependencies"]
        .as_table()
        .unwrap()
        .iter()
        .filter(|(_, v)| !v.get("optional").and_then(|o| o.as_bool()).unwrap_or(false))
        .map(|(name, v)| {
            (
                name.replace('-', "_"),
                v.get("package")
                    .map(|v| v.as_str().unwrap().replace("-", "_")),
            )
        })
        .collect::<Vec<_>>();

    let deps_reexported = all_crate_names
        .iter()
        .map(|(name, _)| {
            let name_ident = syn::Ident::new(name, Span::call_site());
            parse_quote! {
                pub use #name_ident;
            }
        })
        .collect::<Vec<syn::Item>>();

    let deps_reexported_runtime = all_crate_names
        .iter()
        .map(|(name, original_crate_name)| {
            let original_crate_name_or_alias = original_crate_name.as_ref().unwrap_or(name);
            parse_quote! {
                #stageleft_name::internal::add_deps_reexport(
                    vec![#original_crate_name_or_alias],
                    vec![
                        option_env!("STAGELEFT_FINAL_CRATE_NAME").unwrap_or(env!("CARGO_PKG_NAME"))
                            .replace("-", "_"),
                        "__staged".to_string(),
                        "__deps".to_string(),
                        #name.to_string()
                    ]
                );
            }
        })
        .collect::<Vec<syn::Stmt>>();

    syn::parse_quote! {
        pub mod __deps {
            #(#deps_reexported)*

            #[#stageleft_name::internal::ctor::ctor(crate_path = #stageleft_name::internal::ctor)]
            fn __init() {
                #(#deps_reexported_runtime)*
                #stageleft_name::internal::add_crate_with_staged(env!("CARGO_PKG_NAME").replace("-", "_"));
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

    let mut final_pub_visitor = GenFinalPubVisitor::new(
        orig_crate_path.clone(),
        test_mode_feature,
        is_staged_separate,
    );
    final_pub_visitor.visit_file_mut(&mut flow_lib_pub);

    // macros exported with `#[macro_export]` are placed at the top-level of the crate,
    // so we need to pull them into the `mod __staged` so that relative imports resolve
    // correctly
    for exported_macro in final_pub_visitor.all_macros {
        flow_lib_pub
            .items
            .push(parse_quote!(pub use #orig_crate_path::#exported_macro;));
    }

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

#[doc(hidden)]
pub fn gen_staged_pub() {
    let out_dir = env::var_os("OUT_DIR").unwrap();

    let raw_toml_manifest = fs::read_to_string(Path::new("Cargo.toml"))
        .unwrap()
        .parse::<DocumentMut>()
        .unwrap();

    let maybe_custom_lib_path = raw_toml_manifest
        .get("lib")
        .and_then(|lib| lib.get("path"))
        .and_then(|path| path.as_str());

    let flow_lib_pub = gen_staged_mod(
        maybe_custom_lib_path
            .map(Path::new)
            .unwrap_or_else(|| Path::new("src/lib.rs")),
        parse_quote!(crate),
        None,
        false,
    );

    fs::write(
        Path::new(&out_dir).join("lib_pub.rs"),
        prettyplease::unparse(&flow_lib_pub),
    )
    .unwrap();
    println!("cargo::rerun-if-changed=src");
}

#[doc(hidden)]
pub fn gen_staged_deps() {
    let out_dir = env::var_os("OUT_DIR").unwrap();

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
}

#[macro_export]
macro_rules! gen_final {
    () => {
        println!("cargo::rustc-check-cfg=cfg(stageleft_macro)");
        println!("cargo::rustc-check-cfg=cfg(stageleft_runtime)");
        println!("cargo::rustc-check-cfg=cfg(stageleft_trybuild)");
        println!("cargo::rustc-check-cfg=cfg(feature, values(\"stageleft_macro_entrypoint\"))");
        println!("cargo::rustc-cfg=stageleft_runtime");

        println!("cargo::rerun-if-changed=Cargo.toml");
        println!("cargo::rerun-if-changed=build.rs");
        println!("cargo::rerun-if-env-changed=STAGELEFT_TRYBUILD_BUILD_STAGED");

        #[allow(
            unexpected_cfgs,
            reason = "Macro entrypoints must define the stageleft_macro_entrypoint feature"
        )]
        {
            if cfg!(feature = "stageleft_macro_entrypoint") {
                $crate::gen_staged_pub()
            } else if std::env::var("STAGELEFT_TRYBUILD_BUILD_STAGED").is_ok() {
                println!("cargo::rustc-cfg=stageleft_trybuild");
                $crate::gen_staged_pub()
            }
        }

        $crate::gen_staged_deps()
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
}
