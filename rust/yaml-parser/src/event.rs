// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML serialization-tree events.
//!
//! Events sit between lexing/emission and AST construction:
//!
//! ```text
//! Lexer -> Emitter (Event stream) -> Parser -> AST
//! ```
//!
//! They preserve structural and presentation details such as:
//! - scalar style (plain, quoted, block)
//! - collection style (block vs flow)
//! - document boundaries
//! - anchors and tags, including source spans
//!
//! Events use `Cow<'input, str>` for zero-copy payloads where possible. Use
//! [`Event::into_owned`] when you need a `'static` event stream.

use std::borrow::Cow;

use crate::span::Span;

const EMPTY_PROPERTIES: Properties<'static> = Properties {
    anchor: None,
    tag: None,
};

/// A node property (anchor or tag) with its source span.
///
/// This pairs the raw property text (usually borrowed from the input) with the
/// span covering its syntax in the source.
#[derive(Debug, Clone, PartialEq)]
pub struct Property<'input> {
    /// The property value (`&anchor` name or expanded tag).
    pub value: Cow<'input, str>,
    /// Span covering the property syntax in the source.
    pub span: Span,
}

impl Property<'_> {
    /// Create a new property from a value and source span.
    #[must_use]
    pub fn new(value: Cow<'_, str>, span: Span) -> Property<'_> {
        Property { value, span }
    }

    /// Convert this property to an owned `'static` property.
    #[must_use]
    pub fn into_owned(self) -> Property<'static> {
        Property {
            value: Cow::Owned(self.value.into_owned()),
            span: self.span,
        }
    }
}

/// A source comment with its text and span.
#[derive(Debug, Clone, PartialEq)]
pub struct Comment<'input> {
    /// Comment text after the `#` marker.
    pub text: Cow<'input, str>,
    /// Span covering the full comment syntax in the source.
    pub span: Span,
}

impl Comment<'_> {
    /// Convert this comment to an owned `'static` comment.
    #[must_use]
    pub fn into_owned(self) -> Comment<'static> {
        Comment {
            text: Cow::Owned(self.text.into_owned()),
            span: self.span,
        }
    }
}

/// A pair of optional properties (anchor, tag) with spans.
///
/// This is used at the event/emitter layer where we need to keep track of
/// both the property values and their precise source locations.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Properties<'input> {
    /// Optional anchor property.
    pub anchor: Option<Property<'input>>,
    /// Optional tag property.
    pub tag: Option<Property<'input>>,
}

#[allow(
    clippy::elidable_lifetime_names,
    reason = "need explicit lifetime for update/updated signatures"
)]
impl<'input> Properties<'input> {
    /// Create properties with just an anchor.
    #[must_use]
    pub fn with_anchor(anchor: Property<'input>) -> Self {
        Self {
            anchor: Some(anchor),
            tag: None,
        }
    }

    /// Create properties with just a tag.
    #[must_use]
    pub fn with_tag(tag: Property<'input>) -> Self {
        Self {
            anchor: None,
            tag: Some(tag),
        }
    }

    /// Returns true if both anchor and tag are absent.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.anchor.is_none() && self.tag.is_none()
    }

    /// Convert properties to owned `'static` properties.
    #[must_use]
    pub fn into_owned(self) -> Properties<'static> {
        Properties {
            anchor: self.anchor.map(Property::into_owned),
            tag: self.tag.map(Property::into_owned),
        }
    }

    /// Update this set of properties with another, overriding existing
    /// fields with any that are present in `other` (like Python's
    /// `dict.update`).
    pub fn update(&mut self, other: Properties<'input>) {
        if let Some(anchor) = other.anchor {
            self.anchor = Some(anchor);
        }
        if let Some(tag) = other.tag {
            self.tag = Some(tag);
        }
    }

    /// Return a new `Properties` value that is `self` updated with `other`.
    #[must_use]
    pub fn updated(mut self, other: Properties<'input>) -> Properties<'input> {
        self.update(other);
        self
    }

    /// Return boxed properties only when at least one property is present.
    #[must_use]
    pub fn into_boxed_option(self) -> Option<Box<Properties<'input>>> {
        (!self.is_empty()).then_some(Box::new(self))
    }
}

/// Scalar style information for presentation preservation.
///
/// This distinguishes how a scalar was written in the source, which is
/// needed for:
/// - Round-tripping (preserving original style)
/// - Test suite compliance (test.event format requires style)
/// - Tooling (formatters, linters)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScalarStyle {
    /// Plain (unquoted) scalar: `value`
    Plain,
    /// Single-quoted scalar: `'value'`
    SingleQuoted,
    /// Double-quoted scalar: `"value"`
    DoubleQuoted,
    /// Literal block scalar: `|`
    Literal,
    /// Folded block scalar: `>`
    Folded,
}

impl std::fmt::Display for ScalarStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plain => write!(f, "plain"),
            Self::SingleQuoted => write!(f, "single-quoted"),
            Self::DoubleQuoted => write!(f, "double-quoted"),
            Self::Literal => write!(f, "literal"),
            Self::Folded => write!(f, "folded"),
        }
    }
}

/// Collection style (block vs flow).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CollectionStyle {
    /// Block style: indentation-based
    Block,
    /// Flow style: bracket/brace-based (JSON-like)
    Flow,
}

/// A YAML serialization tree event.
///
/// Events represent the structure of a YAML document without type resolution.
/// They form a well-bracketed sequence: every `MappingStart` has a matching
/// `MappingEnd`, every `SequenceStart` has a matching `SequenceEnd`, etc.
///
/// The lifetime `'input` refers to the input string being parsed.
#[derive(Debug, Clone, PartialEq)]
pub enum Event<'input> {
    /// Start of the YAML stream.
    StreamStart,

    /// End of the YAML stream.
    StreamEnd,

    /// Start of a document.
    DocumentStart {
        /// Whether the document has an explicit `---` marker.
        explicit: bool,
        /// Span of the document start marker (or start of content if implicit).
        span: Span,
    },

    /// End of a document.
    DocumentEnd {
        /// Whether the document has an explicit `...` marker.
        explicit: bool,
        /// Span of the document end marker (or end of content if implicit).
        span: Span,
    },

    /// Start of a mapping (object/dictionary).
    MappingStart {
        /// Block or flow style.
        style: CollectionStyle,
        /// Collected properties (anchor, tag) for this mapping.
        /// Boxed only when present so the common empty-properties case avoids
        /// an allocation while keeping the enum compact.
        properties: Option<Box<Properties<'input>>>,
        /// Span covering the mapping start (indicator or first key).
        span: Span,
    },

    /// End of a mapping.
    MappingEnd {
        /// Span covering the mapping end (closing brace or last value).
        span: Span,
    },

    /// Start of a sequence (array/list).
    SequenceStart {
        /// Block or flow style.
        style: CollectionStyle,
        /// Collected properties (anchor, tag) for this sequence.
        /// Boxed only when present so the common empty-properties case avoids
        /// an allocation while keeping the enum compact.
        properties: Option<Box<Properties<'input>>>,
        /// Span covering the sequence start (indicator or bracket).
        span: Span,
    },

    /// End of a sequence.
    SequenceEnd {
        /// Span covering the sequence end (closing bracket or last item).
        span: Span,
    },

    /// Recovery sentinel indicating the current mapping pair should be dropped.
    ///
    /// This is emitted only in malformed recovery paths where the emitter has
    /// already committed to a key event but determines that the pair itself
    /// should not survive in higher-level consumers.
    ///
    /// Raw event-stream consumers should treat this as a signal to discard the
    /// immediately preceding key event. The invalid pair is always accompanied
    /// by at least one parse error and is not part of the YAML Test Suite event
    /// format, so consumers producing test-format output should skip it.
    InvalidatePair {
        /// Span identifying the insertion / recovery site for the invalid pair.
        span: Span,
    },

    /// A scalar value.
    Scalar {
        /// How the scalar was written (plain, quoted, block).
        style: ScalarStyle,
        /// The scalar's content (original text for plain, processed for others).
        value: Cow<'input, str>,
        /// Collected properties (anchor, tag) for this scalar.
        /// Boxed only when present so the common empty-properties case avoids
        /// an allocation while keeping the enum compact.
        properties: Option<Box<Properties<'input>>>,
        /// Span covering the scalar content only.
        span: Span,
    },

    /// An alias reference (`*name`).
    Alias {
        /// The alias name (without `*` prefix).
        /// Uses `Cow` for zero-copy (usually borrowed) while supporting `into_owned()`.
        name: Cow<'input, str>,
        /// Span covering the alias.
        span: Span,
    },
}

impl Event<'_> {
    /// Borrow this event's properties, if it carries any.
    #[must_use]
    pub fn properties(&self) -> Option<&Properties<'_>> {
        match self {
            Self::MappingStart { properties, .. }
            | Self::SequenceStart { properties, .. }
            | Self::Scalar { properties, .. } => properties.as_deref(),
            Self::StreamStart
            | Self::StreamEnd
            | Self::DocumentStart { .. }
            | Self::DocumentEnd { .. }
            | Self::MappingEnd { .. }
            | Self::SequenceEnd { .. }
            | Self::InvalidatePair { .. }
            | Self::Alias { .. } => None,
        }
    }

    /// Borrow this event's properties, or an empty shared value when absent.
    #[must_use]
    pub fn properties_or_empty(&self) -> &Properties<'_> {
        self.properties().unwrap_or(&EMPTY_PROPERTIES)
    }

    /// Get the span of this event, if applicable.
    #[must_use]
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::StreamStart | Self::StreamEnd => None,
            Self::DocumentStart { span, .. }
            | Self::DocumentEnd { span, .. }
            | Self::MappingStart { span, .. }
            | Self::MappingEnd { span }
            | Self::SequenceStart { span, .. }
            | Self::SequenceEnd { span }
            | Self::InvalidatePair { span }
            | Self::Scalar { span, .. }
            | Self::Alias { span, .. } => Some(*span),
        }
    }

    /// Convert to an owned event with `'static` lifetime.
    ///
    /// This is useful when you need to store events beyond the input's lifetime.
    #[must_use]
    pub fn into_owned(self) -> Event<'static> {
        match self {
            Self::StreamStart => Event::StreamStart,
            Self::StreamEnd => Event::StreamEnd,
            Self::DocumentStart { explicit, span } => Event::DocumentStart { explicit, span },
            Self::DocumentEnd { explicit, span } => Event::DocumentEnd { explicit, span },
            Self::MappingStart {
                style,
                properties,
                span,
            } => Event::MappingStart {
                style,
                properties: properties.map(|event_props| Box::new((*event_props).into_owned())),
                span,
            },
            Self::MappingEnd { span } => Event::MappingEnd { span },
            Self::SequenceStart {
                style,
                properties,
                span,
            } => Event::SequenceStart {
                style,
                properties: properties.map(|event_props| Box::new((*event_props).into_owned())),
                span,
            },
            Self::SequenceEnd { span } => Event::SequenceEnd { span },
            Self::InvalidatePair { span } => Event::InvalidatePair { span },
            Self::Scalar {
                style,
                value,
                properties,
                span,
            } => Event::Scalar {
                style,
                value: Cow::Owned(value.into_owned()),
                properties: properties.map(|event_props| Box::new((*event_props).into_owned())),
                span,
            },
            Self::Alias { name, span } => Event::Alias {
                name: Cow::Owned(name.into_owned()),
                span,
            },
        }
    }
}

impl std::fmt::Display for Event<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StreamStart => write!(f, "+STR"),
            Self::StreamEnd => write!(f, "-STR"),
            Self::DocumentStart { explicit, .. } => {
                if *explicit {
                    write!(f, "+DOC ---")
                } else {
                    write!(f, "+DOC")
                }
            }
            Self::DocumentEnd { explicit, .. } => {
                if *explicit {
                    write!(f, "-DOC ...")
                } else {
                    write!(f, "-DOC")
                }
            }
            Self::MappingStart { style, .. } => {
                write!(f, "+MAP")?;
                if *style == CollectionStyle::Flow {
                    write!(f, " {{}}")?;
                }
                if let Some(prop) = self
                    .properties()
                    .and_then(|properties| properties.anchor.as_ref())
                {
                    write!(f, " &{}", prop.value)?;
                }
                if let Some(prop) = self
                    .properties()
                    .and_then(|properties| properties.tag.as_ref())
                {
                    write!(f, " <{}>", prop.value)?;
                }
                Ok(())
            }
            Self::MappingEnd { .. } => write!(f, "-MAP"),
            Self::SequenceStart { style, .. } => {
                write!(f, "+SEQ")?;
                if *style == CollectionStyle::Flow {
                    write!(f, " []")?;
                }
                if let Some(prop) = self
                    .properties()
                    .and_then(|properties| properties.anchor.as_ref())
                {
                    write!(f, " &{}", prop.value)?;
                }
                if let Some(prop) = self
                    .properties()
                    .and_then(|properties| properties.tag.as_ref())
                {
                    write!(f, " <{}>", prop.value)?;
                }
                Ok(())
            }
            Self::SequenceEnd { .. } => write!(f, "-SEQ"),
            Self::InvalidatePair { .. } => write!(f, "=INV pair"),
            Self::Scalar { style, value, .. } => {
                write!(f, "=VAL")?;
                if let Some(prop) = self
                    .properties()
                    .and_then(|properties| properties.anchor.as_ref())
                {
                    write!(f, " &{}", prop.value)?;
                }
                if let Some(prop) = self
                    .properties()
                    .and_then(|properties| properties.tag.as_ref())
                {
                    write!(f, " <{}>", prop.value)?;
                }
                let style_char = match style {
                    ScalarStyle::Plain => ':',
                    ScalarStyle::SingleQuoted => '\'',
                    ScalarStyle::DoubleQuoted => '"',
                    ScalarStyle::Literal => '|',
                    ScalarStyle::Folded => '>',
                };
                // Escape special characters for test output
                let escaped = value
                    .replace('\\', "\\\\")
                    .replace('\n', "\\n")
                    .replace('\r', "\\r")
                    .replace('\t', "\\t")
                    .replace('\x08', "\\b");
                write!(f, " {style_char}{escaped}")
            }
            Self::Alias { name, .. } => write!(f, "=ALI *{name}"),
        }
    }
}
