// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML serialization tree events.
//!
//! This module implements event-based parsing following the YAML 1.2 spec's
//! "Serialization Tree" layer. Events represent structure + presentation,
//! preserving:
//! - Scalar style (plain, quoted, block)
//! - Original text representation
//! - Document boundaries
//!
//! The event stream can be consumed by:
//! - The parser to build a typed AST (`Node`/`Value`)
//! - Tools for round-tripping, reformatting, or analysis
//!
//! # Zero-Copy Design
//!
//! Events use `Cow<'input, str>` for scalar values, borrowing directly from
//! the input when possible. Block scalar processing may require owned strings
//! for folding/chomping.

use std::borrow::Cow;

use crate::span::Span;

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
        /// Optional anchor name (from `&name`).
        /// Uses `Cow` for zero-copy (usually borrowed) while supporting `into_owned()`.
        anchor: Option<Cow<'input, str>>,
        /// Optional tag (expanded to full URI).
        tag: Option<Cow<'input, str>>,
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
        /// Optional anchor name (from `&name`).
        /// Uses `Cow` for zero-copy (usually borrowed) while supporting `into_owned()`.
        anchor: Option<Cow<'input, str>>,
        /// Optional tag (expanded to full URI).
        tag: Option<Cow<'input, str>>,
        /// Span covering the sequence start (indicator or bracket).
        span: Span,
    },

    /// End of a sequence.
    SequenceEnd {
        /// Span covering the sequence end (closing bracket or last item).
        span: Span,
    },

    /// A scalar value.
    Scalar {
        /// How the scalar was written (plain, quoted, block).
        style: ScalarStyle,
        /// The scalar's content (original text for plain, processed for others).
        value: Cow<'input, str>,
        /// Optional anchor name (from `&name`).
        /// Uses `Cow` for zero-copy (usually borrowed) while supporting `into_owned()`.
        anchor: Option<Cow<'input, str>>,
        /// Optional tag (expanded to full URI).
        tag: Option<Cow<'input, str>>,
        /// Span covering the scalar.
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
            | Self::Scalar { span, .. }
            | Self::Alias { span, .. } => Some(*span),
        }
    }
}

#[allow(
    clippy::elidable_lifetime_names,
    reason = "need explicit lifetime for into_owned return type"
)]
impl<'input> Event<'input> {
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
                anchor,
                tag,
                span,
            } => Event::MappingStart {
                style,
                anchor: anchor.map(|cow| Cow::Owned(cow.into_owned())),
                tag: tag.map(|cow| Cow::Owned(cow.into_owned())),
                span,
            },
            Self::MappingEnd { span } => Event::MappingEnd { span },
            Self::SequenceStart {
                style,
                anchor,
                tag,
                span,
            } => Event::SequenceStart {
                style,
                anchor: anchor.map(|cow| Cow::Owned(cow.into_owned())),
                tag: tag.map(|cow| Cow::Owned(cow.into_owned())),
                span,
            },
            Self::SequenceEnd { span } => Event::SequenceEnd { span },
            Self::Scalar {
                style,
                value,
                anchor,
                tag,
                span,
            } => Event::Scalar {
                style,
                value: Cow::Owned(value.into_owned()),
                anchor: anchor.map(|cow| Cow::Owned(cow.into_owned())),
                tag: tag.map(|cow| Cow::Owned(cow.into_owned())),
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
            Self::MappingStart {
                style, anchor, tag, ..
            } => {
                write!(f, "+MAP")?;
                if *style == CollectionStyle::Flow {
                    write!(f, " {{}}")?;
                }
                if let Some(anc) = anchor {
                    write!(f, " &{anc}")?;
                }
                if let Some(tg) = tag {
                    write!(f, " <{tg}>")?;
                }
                Ok(())
            }
            Self::MappingEnd { .. } => write!(f, "-MAP"),
            Self::SequenceStart {
                style, anchor, tag, ..
            } => {
                write!(f, "+SEQ")?;
                if *style == CollectionStyle::Flow {
                    write!(f, " []")?;
                }
                if let Some(anc) = anchor {
                    write!(f, " &{anc}")?;
                }
                if let Some(tg) = tag {
                    write!(f, " <{tg}>")?;
                }
                Ok(())
            }
            Self::SequenceEnd { .. } => write!(f, "-SEQ"),
            Self::Scalar {
                style,
                value,
                anchor,
                tag,
                ..
            } => {
                write!(f, "=VAL")?;
                if let Some(anc) = anchor {
                    write!(f, " &{anc}")?;
                }
                if let Some(tg) = tag {
                    write!(f, " <{tg}>")?;
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
