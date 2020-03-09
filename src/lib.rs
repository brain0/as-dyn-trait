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

mod settings;

use self::settings::{MacroOptions, Settings};
use quote::quote;
use syn::{parse_macro_input, Ident, ItemTrait, TraitBound, TraitBoundModifier, TypeParamBound};

fn make_trait_bound(ident: &Ident) -> TypeParamBound {
    TypeParamBound::Trait(TraitBound {
        paren_token: None,
        modifier: TraitBoundModifier::None,
        lifetimes: None,
        path: ident.clone().into(),
    })
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

fn get_trait_doc(from: impl AsRef<str>, to: impl AsRef<str>) -> String {
    format!(
        "Trait for converting {} to {}. This is mainly useful for upcasting trait objects.",
        from.as_ref(),
        to.as_ref()
    )
}

fn get_method_doc(from: impl AsRef<str>, to: impl AsRef<str>) -> String {
    format!(
        "Converts {} to {}. Internally, this only uses a type coercion.",
        from.as_ref(),
        to.as_ref()
    )
}

fn as_dyn_trait_impl(
    mut trait_item: ItemTrait,
    settings: MacroOptions,
) -> proc_macro2::TokenStream {
    let trait_name = &trait_item.ident;

    let settings = match Settings::read(trait_name, settings) {
        Ok(settings) => settings,
        Err(err) => return err.to_compile_error(),
    };

    let vis = &trait_item.vis;
    let method_name = settings.method_name();
    let mut_method_name = settings.mut_method_name();

    let supertraits = &mut trait_item.supertraits;

    let ref_trait = if settings.enable_ref() {
        let ref_trait_name = settings.ref_trait_name();
        supertraits.push(make_trait_bound(&ref_trait_name));

        let trait_doc = get_trait_doc(
            "`&Self` and `&mut Self`",
            format!("`&dyn {0}` and `&mut dyn {0}`", trait_name),
        );
        let ref_method_doc = get_method_doc("`&Self`", format!("`&dyn {}`", trait_name));
        let ref_mut_method_doc =
            get_method_doc("`&mut Self`", format!("`&mut dyn {}`", trait_name));

        Some(quote! {
            #[doc = #trait_doc]
            #vis trait #ref_trait_name {
                #[doc = #ref_method_doc]
                fn #method_name(&self) -> &dyn #trait_name;
                #[doc = #ref_mut_method_doc]
                fn #mut_method_name(&mut self) -> &mut dyn #trait_name;
            }

            impl<T: #trait_name + Sized> #ref_trait_name for T {
                fn #method_name(&self) -> &dyn #trait_name {
                    self
                }

                fn #mut_method_name(&mut self) -> &mut dyn #trait_name {
                    self
                }
            }
        })
    } else {
        None
    };

    let box_trait = if settings.enable_box() {
        let box_trait_name = settings.box_trait_name();
        supertraits.push(make_trait_bound(&box_trait_name));

        let trait_doc = get_trait_doc("`Box<Self>`", format!("`Box<dyn {}>`", trait_name));
        let method_doc = get_method_doc("`Box<Self>`", format!("`Box<dyn {}>`", trait_name));

        Some(quote! {
            #[doc = #trait_doc]
            #vis trait #box_trait_name {
                #[doc = #method_doc]
                fn #method_name<'a>(self: std::boxed::Box<Self>) -> std::boxed::Box<dyn #trait_name + 'a>
                where
                    Self: 'a;
            }

            impl<T: #trait_name + Sized> #box_trait_name for T {
                fn #method_name<'a>(self: std::boxed::Box<Self>) -> std::boxed::Box<dyn #trait_name + 'a>
                where
                    Self: 'a
                {
                    self
                }
            }
        })
    } else {
        None
    };

    let rc_trait = if settings.enable_rc() {
        let rc_trait_name = settings.rc_trait_name();
        supertraits.push(make_trait_bound(&rc_trait_name));

        let trait_doc = get_trait_doc("`Rc<Self>`", format!("`Rc<dyn {}>`", trait_name));
        let method_doc = get_method_doc("`Rc<Self>`", format!("`Rc<dyn {}>`", trait_name));

        Some(quote! {
            #[doc = #trait_doc]
            #vis trait #rc_trait_name {
                #[doc = #method_doc]
                fn #method_name<'a>(self: std::rc::Rc<Self>) -> std::rc::Rc<dyn #trait_name + 'a>
                where
                    Self: 'a;
            }

            impl<T: #trait_name + Sized> #rc_trait_name for T {
                fn #method_name<'a>(self: std::rc::Rc<Self>) -> std::rc::Rc<dyn #trait_name + 'a>
                where
                    Self: 'a
                {
                    self
                }
            }
        })
    } else {
        None
    };

    let arc_trait = if settings.enable_arc() {
        let arc_trait_name = settings.arc_trait_name();
        supertraits.push(make_trait_bound(&arc_trait_name));

        let trait_doc = get_trait_doc("`Arc<Self>`", format!("`Arc<dyn {}>`", trait_name));
        let method_doc = get_method_doc("`Arc<Self>`", format!("`Arc<dyn {}>`", trait_name));

        Some(quote! {
            #[doc = #trait_doc]
            #vis trait #arc_trait_name {
                #[doc = #method_doc]
                fn #method_name<'a>(self: std::sync::Arc<Self>) -> std::sync::Arc<dyn #trait_name + 'a>
                where
                    Self: 'a;
            }

            impl<T: #trait_name + Sized> #arc_trait_name for T {
                fn #method_name<'a>(self: std::sync::Arc<Self>) -> std::sync::Arc<dyn #trait_name + 'a>
                where
                    Self: 'a
                {
                    self
                }
            }
        })
    } else {
        None
    };

    let pin_ref_trait = if settings.enable_pin() && settings.enable_ref() {
        let pin_ref_trait_name = settings.pin_ref_trait_name();
        supertraits.push(make_trait_bound(&pin_ref_trait_name));

        let trait_doc = get_trait_doc(
            "`Pin<&Self>` and `Pin<&mut Self>`",
            format!("`Pin<&dyn {0}>` and `Pin<&mut dyn {0}>`", trait_name),
        );
        let ref_method_doc = get_method_doc("`Pin<&Self>`", format!("`Pin<&dyn {}>`", trait_name));
        let ref_mut_method_doc = get_method_doc(
            "`Pin<&mut Self>`",
            format!("`Pin<&mut dyn {}>`", trait_name),
        );

        Some(quote! {
            #[doc = #trait_doc]
            #vis trait #pin_ref_trait_name {
                #[doc = #ref_method_doc]
                fn #method_name(self: core::pin::Pin<&Self>) -> core::pin::Pin<&dyn #trait_name>;
                #[doc = #ref_mut_method_doc]
                fn #mut_method_name(self: core::pin::Pin<&mut Self>) -> core::pin::Pin<&mut dyn #trait_name>;
            }

            impl<T: #trait_name + Sized> #pin_ref_trait_name for T {
                fn #method_name(self: core::pin::Pin<&Self>) -> core::pin::Pin<&dyn #trait_name> {
                    self
                }

                fn #mut_method_name(self: core::pin::Pin<&mut Self>) -> core::pin::Pin<&mut dyn #trait_name> {
                    self
                }
            }
        })
    } else {
        None
    };

    let pin_box_trait = if settings.enable_pin() && settings.enable_box() {
        let pin_box_trait_name = settings.pin_box_trait_name();
        supertraits.push(make_trait_bound(&pin_box_trait_name));

        let trait_doc = get_trait_doc(
            "`Pin<Box<Self>>`",
            format!("`Pin<Box<dyn {}>>`", trait_name),
        );
        let method_doc = get_method_doc(
            "`Pin<Box<Self>>`",
            format!("`Pin<Box<dyn {}>>`", trait_name),
        );

        Some(quote! {
            #[doc = #trait_doc]
            #vis trait #pin_box_trait_name {
                #[doc = #method_doc]
                fn #method_name<'a>(self: core::pin::Pin<std::boxed::Box<Self>>) -> core::pin::Pin<std::boxed::Box<dyn #trait_name + 'a>>
                where
                    Self: 'a;
            }

            impl<T: #trait_name + Sized> #pin_box_trait_name for T {
                fn #method_name<'a>(self: core::pin::Pin<std::boxed::Box<Self>>) -> core::pin::Pin<std::boxed::Box<dyn #trait_name + 'a>>
                where
                    Self: 'a
                {
                    self
                }
            }
        })
    } else {
        None
    };

    let pin_rc_trait = if settings.enable_pin() && settings.enable_rc() {
        let pin_rc_trait_name = settings.pin_rc_trait_name();
        supertraits.push(make_trait_bound(&pin_rc_trait_name));

        let trait_doc = get_trait_doc("`Pin<Rc<Self>>`", format!("`Pin<Rc<dyn {}>>`", trait_name));
        let method_doc =
            get_method_doc("`Pin<Rc<Self>>`", format!("`Pin<Rc<dyn {}>>`", trait_name));

        Some(quote! {
            #[doc = #trait_doc]
            #vis trait #pin_rc_trait_name {
                #[doc = #method_doc]
                fn #method_name<'a>(self: core::pin::Pin<std::rc::Rc<Self>>) -> core::pin::Pin<std::rc::Rc<dyn #trait_name + 'a>>
                where
                    Self: 'a;
            }

            impl<T: #trait_name + Sized> #pin_rc_trait_name for T {
                fn #method_name<'a>(self: core::pin::Pin<std::rc::Rc<Self>>) -> core::pin::Pin<std::rc::Rc<dyn #trait_name + 'a>>
                where
                    Self: 'a
                {
                    self
                }
            }
        })
    } else {
        None
    };

    let pin_arc_trait = if settings.enable_pin() && settings.enable_arc() {
        let pin_arc_trait_name = settings.pin_arc_trait_name();
        supertraits.push(make_trait_bound(&pin_arc_trait_name));

        let trait_doc = get_trait_doc(
            "`Pin<Arc<Self>>`",
            format!("`Pin<Arc<dyn {}>>`", trait_name),
        );
        let method_doc = get_method_doc(
            "`Pin<Arc<Self>>`",
            format!("`Pin<Arc<dyn {}>>`", trait_name),
        );

        Some(quote! {
            #[doc = #trait_doc]
            #vis trait #pin_arc_trait_name {
                #[doc = #method_doc]
                fn #method_name<'a>(self: core::pin::Pin<std::sync::Arc<Self>>) -> core::pin::Pin<std::sync::Arc<dyn #trait_name + 'a>>
                where
                    Self: 'a;
            }

            impl<T: #trait_name + Sized> #pin_arc_trait_name for T {
                fn #method_name<'a>(self: core::pin::Pin<std::sync::Arc<Self>>) -> core::pin::Pin<std::sync::Arc<dyn #trait_name + 'a>>
                where
                    Self: 'a
                {
                    self
                }
            }
        })
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
