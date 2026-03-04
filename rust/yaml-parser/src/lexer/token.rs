// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token types for the YAML lexer.
//!
//! This module defines all token types produced by the document lexer
//! (`document.rs`).
//!
//! Token content uses `Cow<'input, str>` for zero-copy tokenization:
//! - `Borrowed`: Token content is a slice of the input (no allocation)
//! - `Owned`: Token content was transformed (e.g., escape sequences)

use std::borrow::Cow;

use crate::span::IndentLevel;

/// Quote style for quoted strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, derive_more::Display)]
pub enum QuoteStyle {
    /// Single quote (')
    #[display("'")]
    Single,
    /// Double quote (")
    #[display("\"")]
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
#[derive(Debug, Clone, PartialEq, derive_more::Display)]
pub enum Token<'input> {
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
    Plain(Cow<'input, str>),
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
    #[display("'|'")]
    LiteralBlockHeader(BlockScalarHeader),
    /// A folded block scalar (`>`) - header info only, content parsed separately
    #[display("'>'")]
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
    /// `%TAG` directive
    #[display("%TAG {_0}")]
    TagDirective(Cow<'input, str>),
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

    // Indentation structure (Python-style INDENT/DEDENT)
    /// Indentation increased to this level (emitted after `LineStart` when indent > stack.top)
    #[display("INDENT({_0})")]
    Indent(IndentLevel),
    /// Indentation decreased by one level (emitted when indent < stack.top, one per level popped)
    #[display("DEDENT")]
    Dedent,

    /// Invalid token (for error recovery)
    #[display("<invalid>")]
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
