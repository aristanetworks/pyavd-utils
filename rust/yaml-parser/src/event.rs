// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML serialization tree events.
//!
//! This module defines the event types following the YAML 1.2 spec's
//! "Serialization Tree" layer. Events are emitted by the `Emitter` during
//! parsing and represent structural elements that preserve presentation details.
//!
//! # Architecture
//!
//! ```text
//! Lexer → Emitter (emits Events) → Parser → AST
//! ```
//!
//! The `Emitter` emits events as it parses, which can be:
//! - Used directly for streaming/SAX-style processing
//! - Consumed by `Parser` to build an AST
//!
//! # Zero-Copy Design
//!
//! Events use `Cow<'input, str>` for values, borrowing from the input
//! when possible. Use [`Event::into_owned`] to convert to `'static` lifetime.
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
        /// Optional anchor name and its span (from `&name`).
        /// Uses `Cow` for zero-copy (usually borrowed) while supporting `into_owned()`.
        anchor: Option<(Cow<'input, str>, Span)>,
        /// Optional tag and its span (expanded to full URI).
        tag: Option<(Cow<'input, str>, Span)>,
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
        /// Optional anchor name and its span (from `&name`).
        /// Uses `Cow` for zero-copy (usually borrowed) while supporting `into_owned()`.
        anchor: Option<(Cow<'input, str>, Span)>,
        /// Optional tag and its span (expanded to full URI).
        tag: Option<(Cow<'input, str>, Span)>,
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
        /// Optional anchor name and its span (from `&name`).
        /// Uses `Cow` for zero-copy (usually borrowed) while supporting `into_owned()`.
        anchor: Option<(Cow<'input, str>, Span)>,
        /// Optional tag and its span (expanded to full URI).
        tag: Option<(Cow<'input, str>, Span)>,
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

    /// Offset a span by adding the given byte offset to both start and end.
    fn offset_span(span: Span, offset: usize) -> Span {
        Span::from_usize_range((span.start_usize() + offset)..(span.end_usize() + offset))
    }

    /// Offset an optional (value, span) tuple.
    fn offset_opt_span<T>(opt: Option<(T, Span)>, offset: usize) -> Option<(T, Span)> {
        opt.map(|(val, span)| (val, Self::offset_span(span, offset)))
    }
}

#[allow(
    clippy::elidable_lifetime_names,
    reason = "need explicit lifetime for into_owned return type"
)]
impl<'input> Event<'input> {
    /// Add a byte offset to all spans in this event.
    ///
    /// This is used to convert document-relative spans to absolute positions
    /// when documents have leading comments or directives.
    #[must_use]
    pub fn with_offset(self, offset: usize) -> Self {
        if offset == 0 {
            return self;
        }
        match self {
            Self::StreamStart | Self::StreamEnd => self,
            Self::DocumentStart { explicit, span } => Self::DocumentStart {
                explicit,
                span: Self::offset_span(span, offset),
            },
            Self::DocumentEnd { explicit, span } => Self::DocumentEnd {
                explicit,
                span: Self::offset_span(span, offset),
            },
            Self::MappingStart {
                style,
                anchor,
                tag,
                span,
            } => Self::MappingStart {
                style,
                anchor: Self::offset_opt_span(anchor, offset),
                tag: Self::offset_opt_span(tag, offset),
                span: Self::offset_span(span, offset),
            },
            Self::MappingEnd { span } => Self::MappingEnd {
                span: Self::offset_span(span, offset),
            },
            Self::SequenceStart {
                style,
                anchor,
                tag,
                span,
            } => Self::SequenceStart {
                style,
                anchor: Self::offset_opt_span(anchor, offset),
                tag: Self::offset_opt_span(tag, offset),
                span: Self::offset_span(span, offset),
            },
            Self::SequenceEnd { span } => Self::SequenceEnd {
                span: Self::offset_span(span, offset),
            },
            Self::Scalar {
                style,
                value,
                anchor,
                tag,
                span,
            } => Self::Scalar {
                style,
                value,
                anchor: Self::offset_opt_span(anchor, offset),
                tag: Self::offset_opt_span(tag, offset),
                span: Self::offset_span(span, offset),
            },
            Self::Alias { name, span } => Self::Alias {
                name,
                span: Self::offset_span(span, offset),
            },
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
                anchor,
                tag,
                span,
            } => Event::MappingStart {
                style,
                anchor: anchor.map(|(cow, sp)| (Cow::Owned(cow.into_owned()), sp)),
                tag: tag.map(|(cow, sp)| (Cow::Owned(cow.into_owned()), sp)),
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
                anchor: anchor.map(|(cow, sp)| (Cow::Owned(cow.into_owned()), sp)),
                tag: tag.map(|(cow, sp)| (Cow::Owned(cow.into_owned()), sp)),
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
                anchor: anchor.map(|(cow, sp)| (Cow::Owned(cow.into_owned()), sp)),
                tag: tag.map(|(cow, sp)| (Cow::Owned(cow.into_owned()), sp)),
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
                if let Some((anchor_name, _)) = anchor {
                    write!(f, " &{anchor_name}")?;
                }
                if let Some((tg, _)) = tag {
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
                if let Some((anchor_name, _)) = anchor {
                    write!(f, " &{anchor_name}")?;
                }
                if let Some((tg, _)) = tag {
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
                if let Some((anchor_name, _)) = anchor {
                    write!(f, " &{anchor_name}")?;
                }
                if let Some((tg, _)) = tag {
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
