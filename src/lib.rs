#![forbid(
    rust_2018_idioms,
    future_incompatible,
    elided_lifetimes_in_paths,
    unsafe_code
)]
#![warn(
    missing_debug_implementations,
    missing_docs,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unused_extern_crates,
    unused_import_braces,
    unused_qualifications
)]

//! An attribute macro that generates methods for retrieving supertraits from
//! trait-objects (upcasting).
//!
//! If you have a trait with a supertrait, you sometimes want to upcast a trait object.
//! Rust currently does not support this.
//!
//! ```compile_fail
//! trait Super {}
//!
//! trait Sub: Super {}
//!
//! fn wants_upcast(sub: Box<dyn Sub>) {
//!     let s: Box<dyn Super> = sub;
//!     // do something with super.
//! }
//! ```
//!
//! This results in the following error:
//!
//! ```text
//! error[E0308]: mismatched types
//!  --> src/lib.rs:27:29
//!   |
//! 8 |     let s: Box<dyn Super> = sub;
//!   |            --------------   ^^^ expected trait `Super`, found trait `Sub`
//!   |            |
//!   |            expected due to this
//!   |
//!   = note: expected struct `std::boxed::Box<dyn Super>`
//!              found struct `std::boxed::Box<(dyn Sub + 'static)>`
//!
//! error: aborting due to previous error
//! ```
//!
//! The `as_dyn_trait` attribute solves this problem:
//!
//! ```no_run
//! # use as_dyn_trait::as_dyn_trait;
//! #[as_dyn_trait]
//! trait Super {}
//!
//! trait Sub: Super {}
//!
//! fn wants_upcast(sub: Box<dyn Sub>) {
//!     // s has type Box<dyn Super>.
//!     let s = sub.as_dyn_super();
//!     // do something with super.
//! }
//! ```
//!
//! To achieve this, the macro generates several traits. For a trait `MyTrait`, the names of these
//! traits and their methods are:
//! * `AsDynMyTraitRef`:
//!     * `fn as_dyn_my_trait(&self) -> &dyn MyTrait;`
//!     * `fn as_dyn_my_trait_mut(&mut self) -> &mut dyn MyTrait;`
//! * `AsDynMyTraitBox`:
//!     * `fn as_dyn_my_trait(self: Box<Self>) -> Box<dyn MyTrait>;`
//! * `AsDynMyTraitRc`:
//!     * `fn as_dyn_my_trait(self: Rc<Self>) -> Rc<dyn MyTrait>;`
//! * `AsDynMyTraitArc`:
//!     * `fn as_dyn_my_trait(self: Arc<Self>) -> Arc<dyn MyTrait>;`
//! * `AsDynMyTraitPinRef`:
//!     * `fn as_dyn_my_trait(self: Pin<&Self>) -> Pin<&dyn MyTrait>;`
//!     * `fn as_dyn_my_trait_mut(self: Pin<&mut Self>) -> Pin<&mut dyn MyTrait>;`
//! * `AsDynMyTraitPinBox`:
//!     * `fn as_dyn_my_trait(self: Pin<Box<Self>>) -> Pin<Box<dyn MyTrait>>;`
//! * `AsDynMyTraitPinRc`:
//!     * `fn as_dyn_my_trait(self: Pin<Rc<Self>>) -> Pin<Rc<dyn MyTrait>>;`
//! * `AsDynMyTraitPinArc`:
//!     * `fn as_dyn_my_trait(self: Pin<Arc<Self>>) -> Pin<Arc<dyn MyTrait>>;`
//!
//! These traits are automatically implemented for all `Sized` types that implement `MyTrait`. If you
//! want to implement `MyTrait` for dynamically sized types, you need to do so manually.
//!
//! In order for those traits to work on trait objects, all of them are automatically supertraits of `MyTrait`.
//!
//! The attribute supports several options. The options are passed to the attribute as a comma-separated list:
//!
//! ```no_run
//! # use as_dyn_trait::as_dyn_trait;
//! #[as_dyn_trait(enable_pin = true, trait_name_prefix = DifferentName)]
//! trait Super {}
//! ```
//!
//! * `trait_name_prefix`: The prefix to use for the generated traits. Default is `AsDyn` followed by
//!   the trait name.
//! * `ref_trait_name`: The name of the trait for references. Default is the trait name prefix with `Ref` appended.
//! * `box_trait_name`: The name of the trait for `Box<_>`. Default is the trait name prefix with `Box` appended.
//! * `rc_trait_name`: The name of the trait for `Rc<_>`. Default is the trait name prefix with `Rc` appended.
//! * `arc_trait_name`: The name of the trait for `Arc<_>`. Default is the trait name prefix with `Arc` appended.
//! * `pin_ref_trait_name`: The name of the trait for Pin<_> references. Default is the trait name prefix with `PinRef` appended.
//! * `pin_box_trait_name`: The name of the trait for `Pin<Box<_>>`. Default is the trait name prefix with `PinBox`
//!    appended.
//! * `pin_rc_trait_name`: The name of the trait for `Pin<Rc<_>>`. Default is the trait name prefix with `PinRc`
//!    appended.
//! * `pin_arc_trait_name`: The name of the trait for `Pin<Arc<_>>`. Default is the trait name prefix with `PinArc`
//!    appended.
//! * `method_name`: The name of the conversion method. Default is the `as_dyn_` followed by the trait name
//!    (converted to lower-snake-case).
//! * `mut_method_name`: The name of the conversion method for mutable references. Default is the `as_dyn_`
//!    followed by the trait name (converted to lower-snake-case) and `_mut`.
//! * `enable_ref`: Enables conversion of references. Default is true.
//! * `enable_box`: Enables conversion of `Box<_>`. Default is true.
//! * `enable_rc`: Enables conversion of `Rc<_>`. Default is true.
//! * `enable_arc`: Enables conversion of `Arc<_>`. Default is true.
//! * `enable_pin`: Enables conversion of the `Pin<_>` types. These are only generated conversion of the
//!    corresponding base pointer is also enabled. Default is false.
//!
//! This attribute does not use or generate any unsafe code.

extern crate proc_macro;

mod generics;
mod settings;

use self::{
    generics::GenericsExt,
    settings::{MacroOptions, Settings},
};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::collections::HashSet;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, FnArg, GenericParam, Generics, Ident,
    ItemTrait, Lifetime, LifetimeDef, Path, Token, TraitBound, TraitBoundModifier, Type,
    TypeParamBound, Visibility, WhereClause,
};

fn make_trait_bound(path: Path) -> TypeParamBound {
    TypeParamBound::Trait(TraitBound {
        paren_token: None,
        modifier: TraitBoundModifier::None,
        lifetimes: None,
        path,
    })
}

fn get_trait_doc(from: &str, to: &str) -> String {
    format!(
        "Trait for converting {} to {}. This is mainly useful for upcasting trait objects.",
        from, to,
    )
}

fn get_method_doc(from: &str, to: &str) -> String {
    format!(
        "Converts {} to {}. Internally, this only uses a type coercion.",
        from, to,
    )
}

struct MethodInfo<'a> {
    name: &'a Ident,
    doc_from: &'a str,
    doc_to: &'a str,
    lifetime: Option<&'a Lifetime>,
    self_param: FnArg,
    return_type: Type,
    where_clause: Option<WhereClause>,
}

impl<'a> MethodInfo<'a> {
    fn build(self) -> (TokenStream, TokenStream) {
        let MethodInfo {
            name,
            doc_from,
            doc_to,
            lifetime,
            self_param,
            return_type,
            where_clause,
        } = self;

        let doc = get_method_doc(doc_from, doc_to);
        let lifetime_bound: Option<Generics> = lifetime.map(|lifetime| {
            let lifetime = GenericParam::Lifetime(LifetimeDef {
                attrs: Vec::new(),
                lifetime: lifetime.clone(),
                colon_token: None,
                bounds: Default::default(),
            });
            parse_quote!(<#lifetime>)
        });

        (
            quote! {
                #[doc = #doc]
                fn #name #lifetime_bound (#self_param) -> #return_type
                    #where_clause;
            },
            quote! {
                fn #name #lifetime_bound (#self_param) -> #return_type
                    #where_clause
                {
                    self
                }
            },
        )
    }
}

fn split_option<A, B>(option: Option<(A, B)>) -> (Option<A>, Option<B>) {
    match option {
        Some((a, b)) => (Some(a), Some(b)),
        None => (None, None),
    }
}

fn make_trait(
    generics: &Generics,
    generics_extended: &Generics,
    generics_without_bounds: &Generics,
    new_trait_name: &Ident,
    type_param: &Ident,
    vis: &Visibility,
    supertraits: &mut Punctuated<TypeParamBound, Token![+]>,
    trait_doc_from: &str,
    trait_doc_to: &str,
    method_info: MethodInfo<'_>,
    mut_method_info: Option<MethodInfo<'_>>,
) -> TokenStream {
    supertraits.push(make_trait_bound(
        parse_quote!(#new_trait_name #generics_without_bounds),
    ));

    let trait_doc = get_trait_doc(trait_doc_from, trait_doc_to);

    let (method, method_impl) = method_info.build();
    let (mut_method, mut_method_impl) = split_option(mut_method_info.map(|m| m.build()));
    let where_clause = &generics.where_clause;

    quote! {
        #[doc = #trait_doc]
        #vis trait #new_trait_name #generics
            #where_clause
        {
            #method
            #mut_method
        }

        impl #generics_extended #new_trait_name #generics_without_bounds for #type_param
            #where_clause
        {
            #method_impl
            #mut_method_impl
        }
    }
}

struct MakeTraitOptions<'a> {
    trait_name: &'a Ident,
    generics: Generics,
    generics_extended: Generics,
    generics_without_bounds: Generics,
    type_param: Ident,
    vis: &'a Visibility,
    supertraits: &'a mut Punctuated<TypeParamBound, Token![+]>,
    method_name: &'a Ident,
    mut_method_name: &'a Ident,
    method_lifetime: Lifetime,
}

fn make_ref_trait(
    options: &mut MakeTraitOptions<'_>,
    new_trait_name: &Ident,
    pinned: bool,
) -> TokenStream {
    let &mut MakeTraitOptions {
        trait_name,
        ref generics,
        ref generics_extended,
        ref generics_without_bounds,
        ref type_param,
        vis,
        ref mut supertraits,
        method_name,
        mut_method_name,
        method_lifetime: _,
    } = options;

    let (
        doc_from,
        doc_to,
        self_param,
        return_type,
        doc_from_mut,
        doc_to_mut,
        self_param_mut,
        return_type_mut,
    ) = if pinned {
        (
            "`Pin<&Self>`",
            format!("`Pin<&dyn {0}>`", trait_name),
            parse_quote!(self: std::pin::Pin<&Self>),
            parse_quote!(std::pin::Pin<&dyn #trait_name #generics_without_bounds>),
            "`Pin<&mut Self>`",
            format!("`Pin<&mut dyn {0}>`", trait_name),
            parse_quote!(self: std::pin::Pin<&mut Self>),
            parse_quote!(std::pin::Pin<&mut dyn #trait_name #generics_without_bounds>),
        )
    } else {
        (
            "`&Self`",
            format!("`&dyn {0}`", trait_name),
            parse_quote!(&self),
            parse_quote!(&dyn #trait_name #generics_without_bounds),
            "`&mut Self`",
            format!("`&mut dyn {0}`", trait_name),
            parse_quote!(&mut self),
            parse_quote!(&mut dyn #trait_name #generics_without_bounds),
        )
    };
    let trait_doc_from = format!("{} and {}", doc_from, doc_from_mut);
    let trait_doc_to = format!("{} and {}", doc_to, doc_to_mut);

    make_trait(
        generics,
        generics_extended,
        generics_without_bounds,
        new_trait_name,
        type_param,
        &vis,
        supertraits,
        &trait_doc_from,
        &trait_doc_to,
        MethodInfo {
            name: method_name,
            doc_from,
            doc_to: &doc_to,
            lifetime: None,
            self_param,
            return_type,
            where_clause: None,
        },
        Some(MethodInfo {
            name: mut_method_name,
            doc_from: doc_from_mut,
            doc_to: &doc_to_mut,
            lifetime: None,
            self_param: self_param_mut,
            return_type: return_type_mut,
            where_clause: None,
        }),
    )
}

fn make_smart_ptr_trait(
    options: &mut MakeTraitOptions<'_>,
    new_trait_name: &Ident,
    smart_ptr: Path,
    pinned: bool,
) -> TokenStream {
    let &mut MakeTraitOptions {
        trait_name,
        ref generics,
        ref generics_extended,
        ref generics_without_bounds,
        ref type_param,
        vis,
        ref mut supertraits,
        method_name,
        mut_method_name: _,
        ref method_lifetime,
    } = options;

    let smart_ptr_name = &smart_ptr.segments.last().as_ref().unwrap().ident;
    let (doc_from, doc_to, self_param, return_type) = if pinned {
        (
            format!("`Pin<{}<Self>>`", smart_ptr_name),
            format!("`Pin<{}<dyn {}>>`", smart_ptr_name, trait_name),
            parse_quote!(self: std::pin::Pin<#smart_ptr<Self>>),
            parse_quote!(std::pin::Pin<#smart_ptr<dyn #trait_name #generics_without_bounds + #method_lifetime>>),
        )
    } else {
        (
            format!("`{}<Self>`", smart_ptr_name),
            format!("`{}<dyn {}>`", smart_ptr_name, trait_name),
            parse_quote!(self: #smart_ptr<Self>),
            parse_quote!(#smart_ptr<dyn #trait_name #generics_without_bounds + #method_lifetime>),
        )
    };

    make_trait(
        generics,
        generics_extended,
        generics_without_bounds,
        new_trait_name,
        type_param,
        vis,
        supertraits,
        &doc_from,
        &doc_to,
        MethodInfo {
            name: method_name,
            doc_from: &doc_from,
            doc_to: &doc_to,
            lifetime: Some(method_lifetime),
            self_param,
            return_type,
            where_clause: parse_quote!(where Self: #method_lifetime),
        },
        None,
    )
}

fn extend_generics(
    generics: &Generics,
    generics_without_bounds: &Generics,
    trait_name: &Ident,
    type_param: &Ident,
) -> Generics {
    let lt_token = Some(generics.lt_token.clone().unwrap_or_default());
    let mut params = generics.params.clone();
    let gt_token = Some(generics.gt_token.clone().unwrap_or_default());
    // We only use the where clause from the original generics object, no need to clone it here.
    let where_clause = None;

    params.push(GenericParam::Type(
        parse_quote!(#type_param: #trait_name #generics_without_bounds + std::marker::Sized),
    ));

    Generics {
        lt_token,
        params,
        gt_token,
        where_clause,
    }
}

fn find_unused_type_param(generics: &Generics) -> Ident {
    let params: HashSet<_> = generics
        .type_params()
        .map(|p| p.ident.to_string())
        .collect();

    for candidate in (b'T'..=b'Z').chain(b'A'..b'T') {
        let candidate_slice = &[candidate];
        let candidate = std::str::from_utf8(candidate_slice).unwrap();

        if !params.contains(candidate) {
            return Ident::new(candidate, Span::call_site());
        }
    }

    panic!("Unable to find an unused type parameter. Please report a bug.");
}

fn find_unused_lifetime(generics: &Generics) -> Lifetime {
    let lifetimes: HashSet<_> = generics
        .lifetimes()
        .map(|l| l.lifetime.ident.to_string())
        .collect();

    for candidate in b'a'..=b'z' {
        let candidate_slice = &[candidate];
        let candidate = std::str::from_utf8(candidate_slice).unwrap();

        if !lifetimes.contains(candidate) {
            return Lifetime {
                apostrophe: Span::call_site(),
                ident: Ident::new(candidate, Span::call_site()),
            };
        }
    }

    panic!("Unable to find an unused lifetime. Please report a bug.");
}

/// Generates methods for retrieving trait objects of supertraits from
/// trait objects of subtraits.
///
/// See the [module-level documentation](index.html) for more details.
#[proc_macro_attribute]
pub fn as_dyn_trait(
    args: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let trait_item = parse_macro_input!(item as ItemTrait);
    let settings = parse_macro_input!(args as MacroOptions);
    as_dyn_trait_impl(trait_item, settings).into()
}

fn as_dyn_trait_impl(mut trait_item: ItemTrait, settings: MacroOptions) -> TokenStream {
    let trait_name = &trait_item.ident;
    let generics = trait_item.generics.clone().remove_defaults();
    let generics_without_bounds = generics.clone().remove_bounds();

    let type_param = find_unused_type_param(&generics);
    let method_lifetime = find_unused_lifetime(&generics);

    let generics_extended =
        extend_generics(&generics, &generics_without_bounds, trait_name, &type_param);

    let settings = match Settings::read(trait_name, settings) {
        Ok(settings) => settings,
        Err(err) => return err.to_compile_error(),
    };

    let mut options = MakeTraitOptions {
        trait_name,
        generics,
        generics_extended,
        generics_without_bounds,
        type_param,
        vis: &trait_item.vis,
        supertraits: &mut trait_item.supertraits,
        method_name: settings.method_name(),
        mut_method_name: settings.mut_method_name(),
        method_lifetime,
    };

    let ref_trait = if settings.enable_ref() {
        Some(make_ref_trait(
            &mut options,
            &settings.ref_trait_name(),
            false,
        ))
    } else {
        None
    };

    let box_trait = if settings.enable_box() {
        Some(make_smart_ptr_trait(
            &mut options,
            &settings.box_trait_name(),
            parse_quote!(std::boxed::Box),
            false,
        ))
    } else {
        None
    };

    let rc_trait = if settings.enable_rc() {
        Some(make_smart_ptr_trait(
            &mut options,
            &settings.rc_trait_name(),
            parse_quote!(std::rc::Rc),
            false,
        ))
    } else {
        None
    };

    let arc_trait = if settings.enable_arc() {
        Some(make_smart_ptr_trait(
            &mut options,
            &settings.arc_trait_name(),
            parse_quote!(std::sync::Arc),
            false,
        ))
    } else {
        None
    };

    let pin_ref_trait = if settings.enable_pin() && settings.enable_ref() {
        Some(make_ref_trait(
            &mut options,
            &settings.pin_ref_trait_name(),
            true,
        ))
    } else {
        None
    };

    let pin_box_trait = if settings.enable_pin() && settings.enable_box() {
        Some(make_smart_ptr_trait(
            &mut options,
            &settings.pin_box_trait_name(),
            parse_quote!(std::boxed::Box),
            true,
        ))
    } else {
        None
    };

    let pin_rc_trait = if settings.enable_pin() && settings.enable_rc() {
        Some(make_smart_ptr_trait(
            &mut options,
            &settings.pin_rc_trait_name(),
            parse_quote!(std::rc::Rc),
            true,
        ))
    } else {
        None
    };

    let pin_arc_trait = if settings.enable_pin() && settings.enable_arc() {
        Some(make_smart_ptr_trait(
            &mut options,
            &settings.pin_arc_trait_name(),
            parse_quote!(std::sync::Arc),
            true,
        ))
    } else {
        None
    };

    quote! {
        #trait_item
        #ref_trait
        #box_trait
        #rc_trait
        #arc_trait
        #pin_ref_trait
        #pin_box_trait
        #pin_rc_trait
        #pin_arc_trait
    }
}
