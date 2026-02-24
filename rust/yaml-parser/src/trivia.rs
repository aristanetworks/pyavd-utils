// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Trivia types for preserving comments and whitespace.
//!
//! In language server and IDE contexts, it's important to preserve "trivia" -
//! comments, whitespace, and other non-semantic content - so that refactoring
//! operations can maintain formatting and comments.
//!
//! This module provides types for attaching trivia to tokens, following the
//! industry standard pattern used by TypeScript, Roslyn, and rust-analyzer.

use crate::span::Span;

/// The kind of trivia (non-semantic content).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TriviaKind {
    /// A comment (content after `#` until end of line).
    Comment(String),
    /// Inline whitespace (spaces and tabs, not newlines).
    Whitespace,
    /// A newline followed by indentation spaces.
    /// The value is the indentation level (number of spaces).
    LineBreak(usize),
}

/// A piece of trivia with its source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trivia {
    /// The kind of trivia.
    pub kind: TriviaKind,
    /// The source location of this trivia.
    pub span: Span,
}

impl Trivia {
    /// Create a new trivia item.
    #[must_use]
    pub const fn new(kind: TriviaKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Create a comment trivia.
    #[must_use]
    pub fn comment(content: String, span: Span) -> Self {
        Self::new(TriviaKind::Comment(content), span)
    }

    /// Create a whitespace trivia.
    #[must_use]
    pub const fn whitespace(span: Span) -> Self {
        Self::new(TriviaKind::Whitespace, span)
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

    /// Check if this trivia is whitespace.
    #[must_use]
    pub const fn is_whitespace(&self) -> bool {
        matches!(self.kind, TriviaKind::Whitespace)
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

impl std::fmt::Display for TriviaKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(content) => write!(f, "#{content}"),
            Self::Whitespace => write!(f, "<whitespace>"),
            Self::LineBreak(indent) => write!(f, "<newline+{indent}>"),
        }
    }
}

impl std::fmt::Display for Trivia {
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
#[derive(Debug, Clone, PartialEq)]
pub struct RichToken {
    /// The actual token.
    pub token: Token,
    /// The source location of the token (excluding trivia).
    pub span: Span,
    /// Trivia appearing before this token (comments, whitespace, line breaks).
    pub leading_trivia: Vec<Trivia>,
    /// Trivia appearing after this token on the same line.
    pub trailing_trivia: Vec<Trivia>,
}

impl RichToken {
    /// Create a new rich token with no trivia.
    #[must_use]
    pub fn new(token: Token, span: Span) -> Self {
        Self {
            token,
            span,
            leading_trivia: Vec::new(),
            trailing_trivia: Vec::new(),
        }
    }

    /// Create a rich token with leading trivia.
    #[must_use]
    pub fn with_leading(token: Token, span: Span, leading: Vec<Trivia>) -> Self {
        Self {
            token,
            span,
            leading_trivia: leading,
            trailing_trivia: Vec::new(),
        }
    }

    /// Add leading trivia to this token.
    pub fn add_leading(&mut self, trivia: Trivia) {
        self.leading_trivia.push(trivia);
    }

    /// Add trailing trivia to this token.
    pub fn add_trailing(&mut self, trivia: Trivia) {
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
    pub fn comments(&self) -> Vec<&Trivia> {
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

impl std::fmt::Display for RichToken {
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
        let comment = Trivia::comment("hello".to_owned(), Span::new((), 0..6));
        assert!(comment.is_comment());
        assert_eq!(comment.comment_content(), Some("hello"));

        let ws = Trivia::whitespace(Span::new((), 0..2));
        assert!(ws.is_whitespace());
        assert_eq!(ws.comment_content(), None);

        let lb = Trivia::line_break(4, Span::new((), 0..5));
        assert!(lb.is_line_break());
        assert_eq!(lb.comment_content(), None);
    }

    #[test]
    fn test_trivia_display() {
        assert_eq!(TriviaKind::Comment("test".to_owned()).to_string(), "#test");
        assert_eq!(TriviaKind::Whitespace.to_string(), "<whitespace>");
        assert_eq!(TriviaKind::LineBreak(4).to_string(), "<newline+4>");
    }

    #[test]
    fn test_rich_token_creation() {
        let token = RichToken::new(Token::Plain("test".to_owned()), Span::new((), 0..4));
        assert!(!token.has_trivia());
        assert!(!token.has_comments());
        assert_eq!(token.span, Span::new((), 0..4));
        assert_eq!(token.full_span(), Span::new((), 0..4));
    }

    #[test]
    fn test_rich_token_with_trivia() {
        let mut token = RichToken::new(Token::Plain("test".to_owned()), Span::new((), 5..9));

        // Add leading whitespace and comment
        token.add_leading(Trivia::whitespace(Span::new((), 0..2)));
        token.add_leading(Trivia::comment("comment".to_owned(), Span::new((), 2..10)));

        // Add trailing comment
        token.add_trailing(Trivia::comment(
            "trailing".to_owned(),
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
        token.add_trailing(Trivia::comment("key".to_owned(), Span::new((), 7..11)));

        let display = token.to_string();
        // Token::Colon displays as ':'
        assert!(display.contains("':'"));
        assert!(display.contains("#key"));
    }
}
