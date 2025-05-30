#![deny(clippy::use_self)]

#[macro_use]
mod file_source;
mod generated;
pub mod stmt_ext;
mod syntax_node;

pub use self::generated::*;
pub use biome_rowan::{
    SyntaxNodeText, TextLen, TextRange, TextSize, TokenAtOffset, TriviaPieceKind, WalkEvent,
};
pub use file_source::CssFileSource;
pub use syntax_node::*;

use crate::CssSyntaxKind::*;
use biome_rowan::{AstNode, RawSyntaxKind, SyntaxKind};

impl From<u16> for CssSyntaxKind {
    fn from(d: u16) -> Self {
        assert!(d <= (Self::__LAST as u16));
        unsafe { std::mem::transmute::<u16, Self>(d) }
    }
}

impl From<CssSyntaxKind> for u16 {
    fn from(k: CssSyntaxKind) -> Self {
        k as Self
    }
}

impl CssSyntaxKind {
    /// Returns `true` for any contextual or non-contextual keyword
    #[inline]
    pub const fn is_keyword(self) -> bool {
        true
    }

    /// Returns `true` for contextual keywords
    #[inline]
    pub const fn is_contextual_keyword(self) -> bool {
        (self as u16) >= (MEDIA_KW as u16) && (self as u16) <= (FONT_FACE_KW as u16)
    }

    /// Returns `true` for css-wide keywords
    #[inline]
    pub const fn is_css_wide_keyword(self) -> bool {
        (self as u16) >= (INITIAL_KW as u16) && (self as u16) <= (DEFAULT_KW as u16)
    }

    /// Returns `true` for contextual attribute modifier keywords
    #[inline]
    pub const fn is_attribute_modifier_keyword(self) -> bool {
        let k = self as u16;
        k == (I_KW as u16) || k == (S_KW as u16)
    }

    /// Returns true for all non-contextual keywords (includes future reserved keywords)
    #[inline]
    pub const fn is_non_contextual_keyword(self) -> bool {
        self.is_keyword() && !self.is_contextual_keyword()
    }

    /// Returns true for all _known_ dimension units.
    ///
    /// Note that dimensions allow any identifier as the unit value, but only
    /// these known units will be parsed as a `CssRegularDimension`. All others
    /// will be parsed as `CssUnknownDimension` instead.
    #[inline]
    pub const fn is_known_dimension_unit(self) -> bool {
        (self as u16) >= (EM_KW as u16) && (self as u16) <= (FR_KW as u16)
    }
}

impl biome_rowan::SyntaxKind for CssSyntaxKind {
    const TOMBSTONE: Self = Self::TOMBSTONE;
    const EOF: Self = EOF;

    fn is_bogus(&self) -> bool {
        matches!(
            self,
            CSS_BOGUS
                | CSS_BOGUS_RULE
                | CSS_BOGUS_SELECTOR
                | CSS_BOGUS_SUB_SELECTOR
                | CSS_BOGUS_BLOCK
                | CSS_BOGUS_PSEUDO_CLASS
                | CSS_BOGUS_PSEUDO_ELEMENT
                | CSS_BOGUS_AT_RULE
                | CSS_BOGUS_MEDIA_QUERY
                | CSS_BOGUS_KEYFRAMES_ITEM
                | CSS_BOGUS_PAGE_SELECTOR_PSEUDO
                | CSS_BOGUS_LAYER
                | CSS_BOGUS_SCOPE_RANGE
                | CSS_BOGUS_PROPERTY
                | CSS_BOGUS_PROPERTY_VALUE
                | CSS_BOGUS_DOCUMENT_MATCHER
                | CSS_BOGUS_KEYFRAMES_NAME
                | CSS_BOGUS_CUSTOM_IDENTIFIER
                | CSS_BOGUS_UNICODE_RANGE_VALUE
                | CSS_BOGUS_SUPPORTS_CONDITION
        )
    }

    fn to_bogus(&self) -> Self {
        match self {
            kind if AnyCssSubSelector::can_cast(*kind) => CSS_BOGUS_SUB_SELECTOR,
            kind if AnyCssSelector::can_cast(*kind) => CSS_BOGUS_SELECTOR,
            kind if AnyCssRule::can_cast(*kind) => CSS_BOGUS_RULE,
            kind if AnyCssPseudoClass::can_cast(*kind) => CSS_BOGUS_PSEUDO_CLASS,
            kind if AnyCssPseudoElement::can_cast(*kind) => CSS_BOGUS_PSEUDO_ELEMENT,
            kind if AnyCssAtRule::can_cast(*kind) => CSS_BOGUS_AT_RULE,
            kind if AnyCssMediaQuery::can_cast(*kind) => CSS_BOGUS_MEDIA_QUERY,
            kind if AnyCssDeclarationBlock::can_cast(*kind) => CSS_BOGUS_BLOCK,
            kind if AnyCssRuleBlock::can_cast(*kind) => CSS_BOGUS_BLOCK,
            kind if AnyCssKeyframesSelector::can_cast(*kind) => CSS_BOGUS_SELECTOR,
            kind if AnyCssPageSelectorPseudo::can_cast(*kind) => CSS_BOGUS_PAGE_SELECTOR_PSEUDO,
            kind if AnyCssLayer::can_cast(*kind) => CSS_BOGUS_LAYER,
            kind if AnyCssScopeRange::can_cast(*kind) => CSS_BOGUS_SCOPE_RANGE,
            kind if AnyCssKeyframesItem::can_cast(*kind) => CSS_BOGUS_KEYFRAMES_ITEM,
            kind if AnyCssProperty::can_cast(*kind) => CSS_BOGUS_PROPERTY,
            kind if AnyCssDocumentMatcher::can_cast(*kind) => CSS_BOGUS_DOCUMENT_MATCHER,
            kind if AnyCssKeyframesName::can_cast(*kind) => CSS_BOGUS_KEYFRAMES_NAME,
            kind if AnyCssCustomIdentifier::can_cast(*kind) => CSS_BOGUS_CUSTOM_IDENTIFIER,
            kind if AnyCssDeclarationOrAtRuleBlock::can_cast(*kind) => CSS_BOGUS_BLOCK,
            kind if AnyCssDeclarationOrRuleBlock::can_cast(*kind) => CSS_BOGUS_BLOCK,
            kind if AnyCssConditionalBlock::can_cast(*kind) => CSS_BOGUS_BLOCK,
            kind if AnyCssFontFeatureValuesBlock::can_cast(*kind) => CSS_BOGUS_BLOCK,
            kind if AnyCssUnicodeValue::can_cast(*kind) => CSS_BOGUS_UNICODE_RANGE_VALUE,
            kind if AnyCssSupportsCondition::can_cast(*kind) => CSS_BOGUS_SUPPORTS_CONDITION,

            _ => CSS_BOGUS,
        }
    }

    #[inline]
    fn to_raw(&self) -> RawSyntaxKind {
        RawSyntaxKind(*self as u16)
    }

    #[inline]
    fn from_raw(raw: RawSyntaxKind) -> Self {
        Self::from(raw.0)
    }

    fn is_root(&self) -> bool {
        matches!(self, CSS_ROOT)
    }

    #[inline]
    fn is_list(&self) -> bool {
        Self::is_list(*self)
    }

    fn is_trivia(self) -> bool {
        matches!(
            self,
            Self::NEWLINE | Self::WHITESPACE | Self::COMMENT | Self::MULTILINE_COMMENT
        )
    }

    fn to_string(&self) -> Option<&'static str> {
        Self::to_string(self)
    }
}

impl TryFrom<CssSyntaxKind> for TriviaPieceKind {
    type Error = ();

    fn try_from(value: CssSyntaxKind) -> Result<Self, Self::Error> {
        if value.is_trivia() {
            match value {
                CssSyntaxKind::NEWLINE => Ok(Self::Newline),
                CssSyntaxKind::WHITESPACE => Ok(Self::Whitespace),
                CssSyntaxKind::COMMENT => Ok(Self::SingleLineComment),
                CssSyntaxKind::MULTILINE_COMMENT => Ok(Self::MultiLineComment),
                _ => unreachable!("Not Trivia"),
            }
        } else {
            Err(())
        }
    }
}
