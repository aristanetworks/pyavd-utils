// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token wrapper and trivia types.
//!
//! This module provides [`RichToken`] which wraps tokens with span information,
//! and trivia types for potential future IDE features (comment/whitespace attachment).
//!
//! **Current State**: Comments and whitespace are kept as real tokens in the stream
//! (not attached as trivia) because comments have semantic meaning in YAML - they
//! terminate plain scalars. The trivia types are preserved for future IDE features.
//!
//! Trivia content uses `Cow<'input, str>` for zero-copy when possible.

use std::borrow::Cow;

use crate::span::Span;

/// The kind of trivia (non-semantic content).
///
/// The lifetime `'input` refers to the input string being tokenized.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TriviaKind<'input> {
    /// A comment (content after `#` until end of line).
    Comment(Cow<'input, str>),
    /// Inline whitespace (spaces only, not newlines).
    Whitespace,
    /// Inline whitespace containing at least one tab character.
    /// This is separated from `Whitespace` to allow O(1) detection of tabs
    /// without re-scanning the content.
    WhitespaceWithTabs,
    /// A newline followed by indentation spaces.
    /// The value is the indentation level (number of spaces).
    LineBreak(usize),
}

/// A piece of trivia with its source location.
///
/// The lifetime `'input` refers to the input string being tokenized.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trivia<'input> {
    /// The kind of trivia.
    pub kind: TriviaKind<'input>,
    /// The source location of this trivia.
    pub span: Span,
}

impl<'input> Trivia<'input> {
    /// Create a new trivia item.
    #[must_use]
    pub const fn new(kind: TriviaKind<'input>, span: Span) -> Self {
        Self { kind, span }
    }

    /// Create a comment trivia.
    #[must_use]
    pub fn comment(content: Cow<'input, str>, span: Span) -> Self {
        Self::new(TriviaKind::Comment(content), span)
    }

    /// Create a whitespace trivia (spaces only).
    #[must_use]
    pub const fn whitespace(span: Span) -> Self {
        Self::new(TriviaKind::Whitespace, span)
    }

    /// Create a whitespace trivia containing tabs.
    #[must_use]
    pub const fn whitespace_with_tabs(span: Span) -> Self {
        Self::new(TriviaKind::WhitespaceWithTabs, span)
    }

    /// Create a line break trivia with indentation.
    #[must_use]
    pub const fn line_break(indent: usize, span: Span) -> Self {
        Self::new(TriviaKind::LineBreak(indent), span)
    }

    /// Check if this trivia is a comment.
    #[must_use]
    pub const fn is_comment(&self) -> bool {
        matches!(self.kind, TriviaKind::Comment(_))
    }

    /// Check if this trivia is whitespace (with or without tabs).
    #[must_use]
    pub const fn is_whitespace(&self) -> bool {
        matches!(
            self.kind,
            TriviaKind::Whitespace | TriviaKind::WhitespaceWithTabs
        )
    }

    /// Check if this trivia contains tabs.
    /// Returns true only for `WhitespaceWithTabs` trivia.
    #[must_use]
    pub const fn has_tabs(&self) -> bool {
        matches!(self.kind, TriviaKind::WhitespaceWithTabs)
    }

    /// Check if this trivia is a line break.
    #[must_use]
    pub const fn is_line_break(&self) -> bool {
        matches!(self.kind, TriviaKind::LineBreak(_))
    }

    /// Get the comment content if this is a comment trivia.
    #[must_use]
    pub fn comment_content(&self) -> Option<&str> {
        match &self.kind {
            TriviaKind::Comment(content) => Some(content),
            _ => None,
        }
    }
}

impl std::fmt::Display for TriviaKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(content) => write!(f, "#{content}"),
            Self::Whitespace => write!(f, "<whitespace>"),
            Self::WhitespaceWithTabs => write!(f, "<whitespace+tabs>"),
            Self::LineBreak(indent) => write!(f, "<newline+{indent}>"),
        }
    }
}

impl std::fmt::Display for Trivia<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

use crate::token::Token;

/// A token with its associated leading and trailing trivia.
///
/// Leading trivia includes comments and whitespace that appear before the token.
/// Trailing trivia includes comments and whitespace that appear after the token
/// on the same line (before a newline).
///
/// This follows the convention used by TypeScript and Roslyn where:
/// - Leading trivia is attached to the token that follows it
/// - Trailing trivia on the same line is attached to the preceding token
/// - Line breaks (newlines + indentation) are considered leading trivia of the next token
///
/// The lifetime `'input` refers to the input string being tokenized.
#[derive(Debug, Clone, PartialEq)]
pub struct RichToken<'input> {
    /// The actual token.
    pub token: Token<'input>,
    /// The source location of the token (excluding trivia).
    pub span: Span,
    /// Trivia appearing before this token (comments, whitespace, line breaks).
    pub leading_trivia: Vec<Trivia<'input>>,
    /// Trivia appearing after this token on the same line.
    pub trailing_trivia: Vec<Trivia<'input>>,
}

impl<'input> RichToken<'input> {
    /// Create a new rich token with no trivia.
    #[must_use]
    pub fn new(token: Token<'input>, span: Span) -> Self {
        Self {
            token,
            span,
            leading_trivia: Vec::new(),
            trailing_trivia: Vec::new(),
        }
    }

    /// Create a rich token with leading trivia.
    #[must_use]
    pub fn with_leading(token: Token<'input>, span: Span, leading: Vec<Trivia<'input>>) -> Self {
        Self {
            token,
            span,
            leading_trivia: leading,
            trailing_trivia: Vec::new(),
        }
    }

    /// Add leading trivia to this token.
    pub fn add_leading(&mut self, trivia: Trivia<'input>) {
        self.leading_trivia.push(trivia);
    }

    /// Add trailing trivia to this token.
    pub fn add_trailing(&mut self, trivia: Trivia<'input>) {
        self.trailing_trivia.push(trivia);
    }

    /// Check if this token has any trivia.
    #[must_use]
    pub fn has_trivia(&self) -> bool {
        !self.leading_trivia.is_empty() || !self.trailing_trivia.is_empty()
    }

    /// Check if this token has any comments in its trivia.
    #[must_use]
    pub fn has_comments(&self) -> bool {
        self.leading_trivia.iter().any(Trivia::is_comment)
            || self.trailing_trivia.iter().any(Trivia::is_comment)
    }

    /// Get all comment trivia (both leading and trailing).
    #[must_use]
    pub fn comments(&self) -> Vec<&Trivia<'input>> {
        self.leading_trivia
            .iter()
            .chain(self.trailing_trivia.iter())
            .filter(|trivia| trivia.is_comment())
            .collect()
    }

    /// Get the full span including trivia.
    #[must_use]
    pub fn full_span(&self) -> Span {
        use chumsky::span::Span as _;

        let start = self
            .leading_trivia
            .first()
            .map_or(self.span.start, |trivia| trivia.span.start);
        let end = self
            .trailing_trivia
            .last()
            .map_or(self.span.end, |trivia| trivia.span.end);
        Span::new((), start..end)
    }
}

impl std::fmt::Display for RichToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.leading_trivia.is_empty() {
            write!(f, "[")?;
            for (idx, trivia) in self.leading_trivia.iter().enumerate() {
                if idx > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{trivia}")?;
            }
            write!(f, "] ")?;
        }
        write!(f, "{}", self.token)?;
        if !self.trailing_trivia.is_empty() {
            write!(f, " [")?;
            for (idx, trivia) in self.trailing_trivia.iter().enumerate() {
                if idx > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{trivia}")?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

#[cfg(test)]
#[allow(
    clippy::indexing_slicing,
    reason = "Tests benefit from direct indexing for readability"
)]
mod tests {
    use super::*;
    use chumsky::span::Span as _;

    #[test]
    fn test_trivia_creation() {
        let comment = Trivia::comment(Cow::Owned("hello".to_owned()), Span::new((), 0..6));
        assert!(comment.is_comment());
        assert_eq!(comment.comment_content(), Some("hello"));

        let ws = Trivia::whitespace(Span::new((), 0..2));
        assert!(ws.is_whitespace());
        assert!(!ws.has_tabs());
        assert_eq!(ws.comment_content(), None);

        let ws_tabs = Trivia::whitespace_with_tabs(Span::new((), 0..2));
        assert!(ws_tabs.is_whitespace());
        assert!(ws_tabs.has_tabs());
        assert_eq!(ws_tabs.comment_content(), None);

        let lb = Trivia::line_break(4, Span::new((), 0..5));
        assert!(lb.is_line_break());
        assert!(!lb.has_tabs());
        assert_eq!(lb.comment_content(), None);
    }

    #[test]
    fn test_trivia_display() {
        assert_eq!(
            TriviaKind::Comment(Cow::Borrowed("test")).to_string(),
            "#test"
        );
        assert_eq!(TriviaKind::Whitespace.to_string(), "<whitespace>");
        assert_eq!(
            TriviaKind::WhitespaceWithTabs.to_string(),
            "<whitespace+tabs>"
        );
        assert_eq!(TriviaKind::LineBreak(4).to_string(), "<newline+4>");
    }

    #[test]
    fn test_rich_token_creation() {
        let token = RichToken::new(Token::Plain(Cow::Borrowed("test")), Span::new((), 0..4));
        assert!(!token.has_trivia());
        assert!(!token.has_comments());
        assert_eq!(token.span, Span::new((), 0..4));
        assert_eq!(token.full_span(), Span::new((), 0..4));
    }

    #[test]
    fn test_rich_token_with_trivia() {
        let mut token = RichToken::new(Token::Plain(Cow::Borrowed("test")), Span::new((), 5..9));

        // Add leading whitespace and comment
        token.add_leading(Trivia::whitespace(Span::new((), 0..2)));
        token.add_leading(Trivia::comment(
            Cow::Owned("comment".to_owned()),
            Span::new((), 2..10),
        ));

        // Add trailing comment
        token.add_trailing(Trivia::comment(
            Cow::Owned("trailing".to_owned()),
            Span::new((), 10..19),
        ));

        assert!(token.has_trivia());
        assert!(token.has_comments());
        assert_eq!(token.comments().len(), 2);
        assert_eq!(token.full_span(), Span::new((), 0..19));
    }

    #[test]
    fn test_rich_token_display() {
        let mut token = RichToken::new(Token::Colon, Span::new((), 5..6));
        token.add_leading(Trivia::whitespace(Span::new((), 4..5)));
        token.add_trailing(Trivia::comment(Cow::Borrowed("key"), Span::new((), 7..11)));

        let display = token.to_string();
        // Token::Colon displays as ':'
        assert!(display.contains("':'"));
        assert!(display.contains("#key"));
    }
}
