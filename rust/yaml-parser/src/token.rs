// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token types for the YAML lexer.
//!
//! This module defines all token types produced by the context-aware lexer
//! (`context_lexer.rs`).
//!
//! Token content uses `Cow<'input, str>` for zero-copy tokenization:
//! - `Borrowed`: Token content is a slice of the input (no allocation)
//! - `Owned`: Token content was transformed (e.g., escape sequences)

use std::borrow::Cow;

/// Quote style for quoted strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuoteStyle {
    /// Single quote (')
    Single,
    /// Double quote (")
    Double,
}

/// Block scalar header information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockScalarHeader {
    /// Explicit indentation indicator (1-9), or None for auto-detect.
    pub indent: Option<u8>,
    /// Chomping behavior for trailing newlines.
    pub chomping: Chomping,
}

/// Block scalar chomping indicator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Chomping {
    /// `-` strip all trailing newlines
    Strip,
    /// (default) clip to single trailing newline
    #[default]
    Clip,
    /// `+` keep all trailing newlines
    Keep,
}

/// A YAML token.
///
/// The lifetime `'input` refers to the input string being tokenized.
/// Token content uses `Cow<'input, str>` for zero-copy when possible.
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'input> {
    // Indicators (single characters with special meaning)
    /// `-` block sequence entry indicator (when followed by whitespace or newline)
    BlockSeqIndicator,
    /// `?` mapping key indicator (when followed by whitespace or newline)
    MappingKey,
    /// `:` mapping value indicator
    Colon,
    /// `{` flow mapping start
    FlowMapStart,
    /// `}` flow mapping end
    FlowMapEnd,
    /// `[` flow sequence start
    FlowSeqStart,
    /// `]` flow sequence end
    FlowSeqEnd,
    /// `,` flow entry separator
    Comma,

    // Document markers
    /// `---` document start
    DocStart,
    /// `...` document end
    DocEnd,

    // Scalars
    /// A plain (unquoted) scalar
    Plain(Cow<'input, str>),
    /// Opening quote for a quoted string (' or ")
    StringStart(QuoteStyle),
    /// Closing quote for a quoted string (' or ")
    StringEnd(QuoteStyle),
    /// Content segment inside a quoted string (escapes already processed)
    StringContent(Cow<'input, str>),
    /// A literal block scalar (`|`) - header info only, content parsed separately
    LiteralBlockHeader(BlockScalarHeader),
    /// A folded block scalar (`>`) - header info only, content parsed separately
    FoldedBlockHeader(BlockScalarHeader),

    // Anchors and aliases
    /// Anchor definition (`&name`)
    Anchor(Cow<'input, str>),
    /// Alias reference (`*name`)
    Alias(Cow<'input, str>),

    // Tags
    /// Tag (`!tag` or `!!type` or `!<uri>`)
    Tag(Cow<'input, str>),

    // Directives
    /// `%YAML` directive
    YamlDirective(Cow<'input, str>),
    /// `%TAG` directive
    TagDirective(Cow<'input, str>),
    /// Reserved/unknown directive (e.g., `%FOO`)
    ReservedDirective(Cow<'input, str>),

    // Whitespace and structure
    /// Start of a new line with indentation (number of spaces)
    LineStart(usize),
    /// Whitespace (spaces only, not at line start)
    Whitespace,
    /// Whitespace containing at least one tab character (not at line start)
    WhitespaceWithTabs,
    /// Comment content (after `#`, without the `#` prefix)
    Comment(Cow<'input, str>),

    // Indentation structure (Python-style INDENT/DEDENT)
    /// Indentation increased to this level (emitted after `LineStart` when indent > stack.top)
    Indent(usize),
    /// Indentation decreased by one level (emitted when indent < stack.top, one per level popped)
    Dedent,

    /// Invalid token (for error recovery)
    Invalid,
}

impl Token<'_> {
    /// Returns `true` if this is a scalar token.
    /// Note: StringStart/StringEnd/StringContent are components of a quoted scalar,
    /// not complete scalars themselves.
    #[must_use]
    pub const fn is_scalar(&self) -> bool {
        matches!(self, Self::Plain(_))
    }

    /// Returns `true` if this is a flow indicator.
    #[must_use]
    pub const fn is_flow_indicator(&self) -> bool {
        matches!(
            self,
            Self::FlowMapStart
                | Self::FlowMapEnd
                | Self::FlowSeqStart
                | Self::FlowSeqEnd
                | Self::Comma
        )
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BlockSeqIndicator => write!(f, "'-'"),
            Self::MappingKey => write!(f, "'?'"),
            Self::Colon => write!(f, "':'"),
            Self::FlowMapStart => write!(f, "'{{'"),
            Self::FlowMapEnd => write!(f, "'}}'"),
            Self::FlowSeqStart => write!(f, "'['"),
            Self::FlowSeqEnd => write!(f, "']'"),
            Self::Comma => write!(f, "','"),
            Self::DocStart => write!(f, "'---'"),
            Self::DocEnd => write!(f, "'...'"),
            Self::Plain(value) => write!(f, "plain scalar '{value}'"),
            Self::StringStart(QuoteStyle::Single) => write!(f, "string start (')"),
            Self::StringStart(QuoteStyle::Double) => write!(f, "string start (\")"),
            Self::StringEnd(QuoteStyle::Single) => write!(f, "string end (')"),
            Self::StringEnd(QuoteStyle::Double) => write!(f, "string end (\")"),
            Self::StringContent(value) => write!(f, "string content '{value}'"),
            Self::LiteralBlockHeader(_) => write!(f, "'|'"),
            Self::FoldedBlockHeader(_) => write!(f, "'>'"),
            Self::Anchor(name) => write!(f, "anchor '&{name}'"),
            Self::Alias(name) => write!(f, "alias '*{name}'"),
            Self::Tag(tag) => write!(f, "tag '{tag}'"),
            Self::YamlDirective(value) => write!(f, "%YAML {value}"),
            Self::TagDirective(value) => write!(f, "%TAG {value}"),
            Self::ReservedDirective(value) => write!(f, "%{value}"),
            Self::LineStart(n) => write!(f, "line start (indent={n})"),
            Self::Whitespace => write!(f, "whitespace"),
            Self::WhitespaceWithTabs => write!(f, "whitespace (with tabs)"),
            Self::Comment(comment) => write!(f, "comment '{comment}'"),
            Self::Indent(n) => write!(f, "INDENT({n})"),
            Self::Dedent => write!(f, "DEDENT"),
            Self::Invalid => write!(f, "<invalid>"),
        }
    }
}
