// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token wrappers and cheap token-kind inspection helpers for the lexer.

use crate::span::Span;

use super::token::Token;

/// Token discriminant for cheap pattern matching without cloning.
///
/// This mirrors the variants of `Token` but contains no payload data, allowing
/// kind-only lookahead to avoid both cloning and repeated discriminant matches.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenKind {
    BlockSeqIndicator,
    MappingKey,
    Colon,
    FlowMapStart,
    FlowMapEnd,
    FlowSeqStart,
    FlowSeqEnd,
    Comma,
    DocStart,
    DocEnd,
    Plain,
    StringStart,
    StringEnd,
    StringContent,
    LiteralBlockHeader,
    FoldedBlockHeader,
    Anchor,
    Alias,
    Tag,
    YamlDirective,
    TagDirective,
    ReservedDirective,
    LineStart,
    Whitespace,
    WhitespaceWithTabs,
    Comment,
}

impl<'input> From<&Token<'input>> for TokenKind {
    #[inline]
    fn from(token: &Token<'input>) -> Self {
        match token {
            Token::BlockSeqIndicator => Self::BlockSeqIndicator,
            Token::MappingKey => Self::MappingKey,
            Token::Colon => Self::Colon,
            Token::FlowMapStart => Self::FlowMapStart,
            Token::FlowMapEnd => Self::FlowMapEnd,
            Token::FlowSeqStart => Self::FlowSeqStart,
            Token::FlowSeqEnd => Self::FlowSeqEnd,
            Token::Comma => Self::Comma,
            Token::DocStart => Self::DocStart,
            Token::DocEnd => Self::DocEnd,
            Token::Plain(_) => Self::Plain,
            Token::StringStart(_) => Self::StringStart,
            Token::StringEnd(_) => Self::StringEnd,
            Token::StringContent(_) => Self::StringContent,
            Token::LiteralBlockHeader(_) => Self::LiteralBlockHeader,
            Token::FoldedBlockHeader(_) => Self::FoldedBlockHeader,
            Token::Anchor(_) => Self::Anchor,
            Token::Alias(_) => Self::Alias,
            Token::Tag(_) => Self::Tag,
            Token::YamlDirective(_) => Self::YamlDirective,
            Token::TagDirective(_, _) => Self::TagDirective,
            Token::ReservedDirective(_) => Self::ReservedDirective,
            Token::LineStart(_) => Self::LineStart,
            Token::Whitespace => Self::Whitespace,
            Token::WhitespaceWithTabs => Self::WhitespaceWithTabs,
            Token::Comment(_) => Self::Comment,
        }
    }
}

/// A token with its associated Span.
///
/// The lifetime `'input` refers to the input string being tokenized.
///
/// Errors during lexing are collected internally by the lexer and retrieved
/// via [`Lexer::take_errors`](super::Lexer::take_errors).
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct RichToken<'input> {
    /// The actual token.
    pub token: Token<'input>,
    /// The source location of the token.
    pub span: Span,
}

impl<'input> RichToken<'input> {
    /// Create a new rich token.
    #[must_use]
    pub(crate) fn new(token: Token<'input>, span: Span) -> Self {
        Self { token, span }
    }
}

impl std::fmt::Display for RichToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.token.fmt(f)
    }
}
