// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token types for the YAML lexer.
//!
//! This module defines all token types produced by the unified lexer in
//! [`super::Lexer`].
//!
//! Token content uses `Cow<'input, str>` for zero-copy tokenization:
//! - `Borrowed`: Token content is a slice of the input (no allocation)
//! - `Owned`: Token content was transformed (e.g., escape sequences)

use std::borrow::Cow;

use crate::span::IndentLevel;

/// Quote style for quoted strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, derive_more::Display)]
pub(crate) enum QuoteStyle {
    /// Single quote (')
    #[display("'")]
    Single,
    /// Double quote (")
    #[display("\"")]
    Double,
}

/// Block scalar header information.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct BlockScalarHeader {
    /// Explicit indentation indicator (1-9), or None for auto-detect.
    pub indent: Option<u8>,
    /// Chomping behavior for trailing newlines.
    pub chomping: Chomping,
}

impl std::fmt::Display for BlockScalarHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(indent) = self.indent {
            write!(f, "{indent}")?;
        }
        write!(f, "{}", self.chomping)
    }
}

/// Block scalar chomping indicator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, derive_more::Display)]
pub(crate) enum Chomping {
    /// `-` strip all trailing newlines
    #[display("-")]
    Strip,
    /// (default) clip to single trailing newline
    #[default]
    #[display("")]
    Clip,
    /// `+` keep all trailing newlines
    #[display("+")]
    Keep,
}

/// Plain-scalar boundary classification captured while lexing.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct PlainScalarMeta {
    /// The scalar ended because `:` acts as a YAML value indicator.
    pub terminated_by_colon: bool,
    /// The scalar ended because whitespace before `#` starts a comment.
    pub terminated_by_comment: bool,
    /// In flow context, the next line begins another plain segment that should
    /// be folded into the same scalar by the emitter.
    pub may_continue_on_next_line_in_flow: bool,
}

/// Plain scalar payload plus lexer-owned boundary metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PlainScalarToken<'input> {
    text: Cow<'input, str>,
    meta: PlainScalarMeta,
}

impl<'input> PlainScalarToken<'input> {
    #[must_use]
    pub(crate) fn new(text: Cow<'input, str>, meta: PlainScalarMeta) -> Self {
        Self { text, meta }
    }

    #[must_use]
    pub(crate) fn as_str(&self) -> &str {
        self.text.as_ref()
    }

    #[must_use]
    pub(crate) fn into_text(self) -> Cow<'input, str> {
        self.text
    }

    #[must_use]
    pub(crate) fn meta(&self) -> PlainScalarMeta {
        self.meta
    }

    #[must_use]
    pub(crate) fn terminated_by_mapping_value_indicator(&self) -> bool {
        self.meta.terminated_by_colon
    }
}

impl std::fmt::Display for PlainScalarToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.text.fmt(f)
    }
}

impl<'input> From<Cow<'input, str>> for PlainScalarToken<'input> {
    fn from(text: Cow<'input, str>) -> Self {
        Self::new(text, PlainScalarMeta::default())
    }
}

impl<'input> From<&'input str> for PlainScalarToken<'input> {
    fn from(text: &'input str) -> Self {
        Self::from(Cow::Borrowed(text))
    }
}

impl From<String> for PlainScalarToken<'_> {
    fn from(text: String) -> Self {
        Self::from(Cow::Owned(text))
    }
}

/// A YAML token.
///
/// The lifetime `'input` refers to the input string being tokenized.
/// Token content uses `Cow<'input, str>` for zero-copy when possible.
#[derive(Debug, Clone, PartialEq, derive_more::Display)]
pub(crate) enum Token<'input> {
    // Indicators (single characters with special meaning)
    /// `-` block sequence entry indicator (when followed by whitespace or newline)
    #[display("'-'")]
    BlockSeqIndicator,
    /// `?` mapping key indicator (when followed by whitespace or newline)
    #[display("'?'")]
    MappingKey,
    /// `:` mapping value indicator
    #[display("':'")]
    Colon,
    /// `{` flow mapping start
    #[display("'{{'")]
    FlowMapStart,
    /// `}` flow mapping end
    #[display("'}}'")]
    FlowMapEnd,
    /// `[` flow sequence start
    #[display("'['")]
    FlowSeqStart,
    /// `]` flow sequence end
    #[display("']'")]
    FlowSeqEnd,
    /// `,` flow entry separator
    #[display("','")]
    Comma,

    // Document markers
    /// `---` document start
    #[display("'---'")]
    DocStart,
    /// `...` document end
    #[display("'...'")]
    DocEnd,

    // Scalars
    /// A plain (unquoted) scalar
    #[display("plain scalar '{_0}'")]
    Plain(PlainScalarToken<'input>),
    /// Opening quote for a quoted string (' or ")
    #[display("string start ({_0})")]
    StringStart(QuoteStyle),
    /// Closing quote for a quoted string (' or ")
    #[display("string end ({_0})")]
    StringEnd(QuoteStyle),
    /// Content segment inside a quoted string (escapes already processed)
    #[display("string content '{_0}'")]
    StringContent(Cow<'input, str>),
    /// A literal block scalar (`|`) - header info only, content parsed separately
    #[display("'|{_0}'")]
    LiteralBlockHeader(BlockScalarHeader),
    /// A folded block scalar (`>`) - header info only, content parsed separately
    #[display("'>{_0}'")]
    FoldedBlockHeader(BlockScalarHeader),

    // Anchors and aliases
    /// Anchor definition (`&name`) - always a slice of input
    #[display("anchor '&{_0}'")]
    Anchor(&'input str),
    /// Alias reference (`*name`) - always a slice of input
    #[display("alias '*{_0}'")]
    Alias(&'input str),

    // Tags
    /// Tag (`!tag` or `!!type` or `!<uri>`)
    #[display("tag '{_0}'")]
    Tag(Cow<'input, str>),

    // Directives
    /// `%YAML` directive
    #[display("%YAML {_0}")]
    YamlDirective(Cow<'input, str>),
    /// `%TAG` directive: `handle` and `prefix`, both borrowed from the input
    #[display("%TAG {_0} {_1}")]
    TagDirective(&'input str, &'input str),
    /// Reserved/unknown directive (e.g., `%FOO`)
    #[display("%{_0}")]
    ReservedDirective(Cow<'input, str>),

    // Whitespace and structure
    /// Start of a new line with indentation (number of spaces)
    #[display("line start (indent={_0})")]
    LineStart(IndentLevel),
    /// Whitespace (spaces only, not at line start)
    #[display("whitespace")]
    Whitespace,
    /// Whitespace containing at least one tab character (not at line start)
    #[display("whitespace (with tabs)")]
    WhitespaceWithTabs,
    /// Comment content (after `#`, without the `#` prefix)
    #[display("comment '{_0}'")]
    Comment(Cow<'input, str>),
}
