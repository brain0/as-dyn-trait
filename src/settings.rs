use proc_macro2::Span;
use quote::{format_ident, IdentFragment, ToTokens};
use std::{
    borrow::Cow,
    fmt::{self, Write as _},
};
use syn::{
    parse::{self, Parse, ParseStream},
    punctuated::Punctuated,
    Error, Ident, LitBool, Token,
};

mod keywords {
    use syn::custom_keyword;

    custom_keyword!(trait_name_prefix);
    custom_keyword!(ref_trait_name);
    custom_keyword!(box_trait_name);
    custom_keyword!(rc_trait_name);
    custom_keyword!(arc_trait_name);
    custom_keyword!(pin_ref_trait_name);
    custom_keyword!(pin_box_trait_name);
    custom_keyword!(pin_rc_trait_name);
    custom_keyword!(pin_arc_trait_name);
    custom_keyword!(method_name);
    custom_keyword!(mut_method_name);

    custom_keyword!(enable_ref);
    custom_keyword!(enable_box);
    custom_keyword!(enable_rc);
    custom_keyword!(enable_arc);
    custom_keyword!(enable_pin);
}

enum NameKeyword {
    TraitNamePrefix(keywords::trait_name_prefix),
    RefTraitName(keywords::ref_trait_name),
    BoxTraitName(keywords::box_trait_name),
    RcTraitName(keywords::rc_trait_name),
    ArcTraitName(keywords::arc_trait_name),
    PinRefTraitName(keywords::pin_ref_trait_name),
    PinBoxTraitName(keywords::pin_box_trait_name),
    PinRcTraitName(keywords::pin_rc_trait_name),
    PinArcTraitName(keywords::pin_arc_trait_name),
    MethodName(keywords::method_name),
    MutMethodName(keywords::mut_method_name),
}

impl NameKeyword {
    fn peek(input: ParseStream<'_>) -> bool {
        use keywords::*;

        input.peek(trait_name_prefix)
            || input.peek(ref_trait_name)
            || input.peek(box_trait_name)
            || input.peek(rc_trait_name)
            || input.peek(arc_trait_name)
            || input.peek(pin_ref_trait_name)
            || input.peek(pin_box_trait_name)
            || input.peek(pin_rc_trait_name)
            || input.peek(pin_arc_trait_name)
            || input.peek(method_name)
            || input.peek(mut_method_name)
    }
}

impl Parse for NameKeyword {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        use NameKeyword::*;

        if let Ok(kw) = input.parse() {
            Ok(TraitNamePrefix(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(RefTraitName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(BoxTraitName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(RcTraitName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(ArcTraitName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(PinRefTraitName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(PinBoxTraitName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(PinRcTraitName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(PinArcTraitName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(MethodName(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(MutMethodName(kw))
        } else {
            Err(input.error("Invalid keyword."))
        }
    }
}

struct MacroNameOption {
    keyword: NameKeyword,
    ident: Ident,
}

impl Parse for MacroNameOption {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        let keyword = input.parse()?;
        let _: Token![=] = input.parse()?;
        let ident = input.parse()?;

        Ok(MacroNameOption { keyword, ident })
    }
}

enum EnableKeyword {
    Ref(keywords::enable_ref),
    Box(keywords::enable_box),
    Rc(keywords::enable_rc),
    Arc(keywords::enable_arc),
    Pin(keywords::enable_pin),
}

impl EnableKeyword {
    fn peek(input: ParseStream<'_>) -> bool {
        use keywords::*;

        input.peek(enable_ref)
            || input.peek(enable_box)
            || input.peek(enable_rc)
            || input.peek(enable_arc)
            || input.peek(enable_pin)
    }
}

impl Parse for EnableKeyword {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        use EnableKeyword::*;

        if let Ok(kw) = input.parse() {
            Ok(Ref(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(Box(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(Rc(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(Arc(kw))
        } else if let Ok(kw) = input.parse() {
            Ok(Pin(kw))
        } else {
            Err(input.error("Invalid keyword."))
        }
    }
}

struct MacroEnableOption {
    keyword: EnableKeyword,
    enabled: LitBool,
}

impl Parse for MacroEnableOption {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        let keyword = input.parse()?;
        let _: Token![=] = input.parse()?;
        let enabled = input.parse()?;

        Ok(MacroEnableOption { keyword, enabled })
    }
}

enum MacroOption {
    Name(MacroNameOption),
    Enable(MacroEnableOption),
}

impl Parse for MacroOption {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        if NameKeyword::peek(input) {
            Ok(MacroOption::Name(input.parse()?))
        } else if EnableKeyword::peek(input) {
            Ok(MacroOption::Enable(input.parse()?))
        } else {
            use keywords::*;

            let span = Span::call_site();

            Err(input.error(format!(
                "Expected one of the keywords {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} or {}.",
                trait_name_prefix(span).to_token_stream(),
                ref_trait_name(span).to_token_stream(),
                box_trait_name(span).to_token_stream(),
                rc_trait_name(span).to_token_stream(),
                arc_trait_name(span).to_token_stream(),
                pin_ref_trait_name(span).to_token_stream(),
                pin_box_trait_name(span).to_token_stream(),
                pin_rc_trait_name(span).to_token_stream(),
                pin_arc_trait_name(span).to_token_stream(),
                method_name(span).to_token_stream(),
                mut_method_name(span).to_token_stream(),
                enable_ref(span).to_token_stream(),
                enable_box(span).to_token_stream(),
                enable_rc(span).to_token_stream(),
                enable_arc(span).to_token_stream(),
                enable_pin(span).to_token_stream(),
            )))
        }
    }
}

pub(crate) struct MacroOptions(Punctuated<MacroOption, Token![,]>);

impl Parse for MacroOptions {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        Ok(MacroOptions(Punctuated::parse_terminated(input)?))
    }
}

pub(crate) struct Settings {
    default_trait_name_prefix: Ident,
    trait_name_prefix: Option<Ident>,
    ref_trait_name: Option<Ident>,
    box_trait_name: Option<Ident>,
    rc_trait_name: Option<Ident>,
    arc_trait_name: Option<Ident>,
    pin_ref_trait_name: Option<Ident>,
    pin_box_trait_name: Option<Ident>,
    pin_rc_trait_name: Option<Ident>,
    pin_arc_trait_name: Option<Ident>,
    default_method_name: Ident,
    method_name: Option<Ident>,
    default_mut_method_name: Ident,
    mut_method_name: Option<Ident>,
    enable_ref: Option<bool>,
    enable_box: Option<bool>,
    enable_rc: Option<bool>,
    enable_arc: Option<bool>,
    enable_pin: Option<bool>,
}

macro_rules! assign_ident {
    ($kw:ident, $settings:ident, $opt: ident, $i:ident) => {
        if $settings.$i.is_some() {
            return Err(Error::new_spanned(
                $kw,
                format!(
                    "The option {} appeared more than once.",
                    $kw.to_token_stream()
                ),
            ));
        } else {
            $settings.$i = Some($opt.ident);
        }
    };
}

macro_rules! assign_enabled {
    ($kw:ident, $settings:ident, $opt: ident, $i:ident) => {
        if $settings.$i.is_some() {
            return Err(Error::new_spanned(
                $kw,
                format!(
                    "The option {} appeared more than once.",
                    $kw.to_token_stream()
                ),
            ));
        } else {
            $settings.$i = Some($opt.enabled.value);
        }
    };
}

struct LowerSnakeCase<'a>(&'a Ident);

impl<'a> IdentFragment for LowerSnakeCase<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Formatter<'a, 'b> {
            first: bool,
            fmt: &'a mut fmt::Formatter<'b>,
        };

        impl<'a, 'b> fmt::Write for Formatter<'a, 'b> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                for c in s.chars() {
                    if c.is_uppercase() {
                        if !self.first {
                            self.fmt.write_char('_')?;
                        }
                        write!(self.fmt, "{}", c.to_lowercase())?;
                    } else {
                        self.fmt.write_char(c)?;
                    }
                    self.first = false;
                }

                Ok(())
            }
        }

        write!(Formatter { first: true, fmt }, "{}", self.0)
    }

    fn span(&self) -> Option<Span> {
        Some(self.0.span())
    }
}

impl Settings {
    fn new(trait_name: &Ident) -> Settings {
        let default_trait_name_prefix = format_ident!("AsDyn{}", trait_name);
        let default_method_name = format_ident!(
            "as_dyn_{}",
            LowerSnakeCase(trait_name),
            span = Span::call_site()
        );
        let default_mut_method_name = format_ident!(
            "as_dyn_{}_mut",
            LowerSnakeCase(trait_name),
            span = Span::call_site()
        );

        Settings {
            default_trait_name_prefix,
            trait_name_prefix: None,
            ref_trait_name: None,
            box_trait_name: None,
            rc_trait_name: None,
            arc_trait_name: None,
            pin_ref_trait_name: None,
            pin_box_trait_name: None,
            pin_rc_trait_name: None,
            pin_arc_trait_name: None,
            default_method_name,
            method_name: None,
            default_mut_method_name,
            mut_method_name: None,
            enable_ref: None,
            enable_box: None,
            enable_rc: None,
            enable_arc: None,
            enable_pin: None,
        }
    }

    pub(crate) fn read(
        trait_name: &Ident,
        parsed_options: MacroOptions,
    ) -> parse::Result<Settings> {
        let mut settings = Settings::new(trait_name);

        for opt in parsed_options.0 {
            match opt {
                MacroOption::Name(opt) => {
                    use NameKeyword::*;
                    match opt.keyword {
                        TraitNamePrefix(kw) => assign_ident!(kw, settings, opt, trait_name_prefix),
                        RefTraitName(kw) => assign_ident!(kw, settings, opt, ref_trait_name),
                        BoxTraitName(kw) => assign_ident!(kw, settings, opt, box_trait_name),
                        RcTraitName(kw) => assign_ident!(kw, settings, opt, rc_trait_name),
                        ArcTraitName(kw) => assign_ident!(kw, settings, opt, arc_trait_name),
                        PinRefTraitName(kw) => assign_ident!(kw, settings, opt, pin_ref_trait_name),
                        PinBoxTraitName(kw) => assign_ident!(kw, settings, opt, pin_box_trait_name),
                        PinRcTraitName(kw) => assign_ident!(kw, settings, opt, pin_rc_trait_name),
                        PinArcTraitName(kw) => assign_ident!(kw, settings, opt, pin_arc_trait_name),
                        MethodName(kw) => assign_ident!(kw, settings, opt, method_name),
                        MutMethodName(kw) => assign_ident!(kw, settings, opt, mut_method_name),
                    }
                }
                MacroOption::Enable(opt) => {
                    use EnableKeyword::*;
                    match opt.keyword {
                        Ref(kw) => assign_enabled!(kw, settings, opt, enable_ref),
                        Box(kw) => assign_enabled!(kw, settings, opt, enable_box),
                        Rc(kw) => assign_enabled!(kw, settings, opt, enable_rc),
                        Arc(kw) => assign_enabled!(kw, settings, opt, enable_arc),
                        Pin(kw) => assign_enabled!(kw, settings, opt, enable_pin),
                    }
                }
            }
        }

        Ok(settings)
    }

    fn trait_name_prefix(&self) -> &Ident {
        self.trait_name_prefix
            .as_ref()
            .unwrap_or(&self.default_trait_name_prefix)
    }

    pub(crate) fn ref_trait_name(&self) -> Cow<'_, Ident> {
        self.ref_trait_name
            .as_ref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(format_ident!("{}Ref", self.trait_name_prefix())))
    }

    pub(crate) fn box_trait_name(&self) -> Cow<'_, Ident> {
        self.box_trait_name
            .as_ref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(format_ident!("{}Box", self.trait_name_prefix())))
    }

    pub(crate) fn rc_trait_name(&self) -> Cow<'_, Ident> {
        self.rc_trait_name
            .as_ref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(format_ident!("{}Rc", self.trait_name_prefix())))
    }

    pub(crate) fn arc_trait_name(&self) -> Cow<'_, Ident> {
        self.arc_trait_name
            .as_ref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(format_ident!("{}Arc", self.trait_name_prefix())))
    }

    pub(crate) fn pin_ref_trait_name(&self) -> Cow<'_, Ident> {
        self.pin_ref_trait_name
            .as_ref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(format_ident!("{}PinRef", self.trait_name_prefix())))
    }

    pub(crate) fn pin_box_trait_name(&self) -> Cow<'_, Ident> {
        self.pin_box_trait_name
            .as_ref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(format_ident!("{}PinBox", self.trait_name_prefix())))
    }

    pub(crate) fn pin_rc_trait_name(&self) -> Cow<'_, Ident> {
        self.pin_rc_trait_name
            .as_ref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(format_ident!("{}PinRc", self.trait_name_prefix())))
    }

    pub(crate) fn pin_arc_trait_name(&self) -> Cow<'_, Ident> {
        self.pin_arc_trait_name
            .as_ref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(format_ident!("{}PinArc", self.trait_name_prefix())))
    }

    pub(crate) fn method_name(&self) -> &Ident {
        self.method_name
            .as_ref()
            .unwrap_or(&self.default_method_name)
    }

    pub(crate) fn mut_method_name(&self) -> &Ident {
        self.mut_method_name
            .as_ref()
            .unwrap_or(&self.default_mut_method_name)
    }

    pub(crate) fn enable_ref(&self) -> bool {
        self.enable_ref.unwrap_or(true)
    }

    pub(crate) fn enable_box(&self) -> bool {
        self.enable_box.unwrap_or(true)
    }

    pub(crate) fn enable_rc(&self) -> bool {
        self.enable_rc.unwrap_or(true)
    }

    pub(crate) fn enable_arc(&self) -> bool {
        self.enable_arc.unwrap_or(true)
    }

    pub(crate) fn enable_pin(&self) -> bool {
        self.enable_pin.unwrap_or(false)
    }
}
