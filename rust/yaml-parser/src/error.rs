// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Error types for YAML parsing.

use crate::span::Span;

/// An error encountered during YAML parsing.
///
/// Errors include their source span, enabling accurate error reporting
/// with line/column information.
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    /// The kind of error
    pub kind: ErrorKind,
    /// The span in the source where the error occurred
    pub span: Span,
    /// Expected tokens/patterns (for diagnostic messages)
    pub expected: Vec<String>,
    /// What was actually found (for diagnostic messages)
    pub found: Option<String>,
}

/// The kind of parse error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    /// Unexpected end of input
    UnexpectedEof,

    /// Unexpected character or token
    UnexpectedToken,

    /// Invalid indentation
    InvalidIndentation,

    /// Unterminated string literal
    UnterminatedString,

    /// Invalid escape sequence in a string (contains the invalid character)
    InvalidEscape(char),

    /// Invalid number format
    InvalidNumber,

    /// Duplicate key in a mapping
    DuplicateKey,

    /// Invalid anchor name
    InvalidAnchor,

    /// Duplicate anchor on same node (e.g., &a &b value)
    DuplicateAnchor,

    /// Undefined alias reference
    UndefinedAlias,

    /// Invalid tag
    InvalidTag,

    /// Duplicate tag on same node (e.g., !a !b value)
    DuplicateTag,

    /// Properties (anchor/tag) cannot be applied to alias
    PropertiesOnAlias,

    /// Invalid block scalar header
    InvalidBlockScalar,

    /// Tab character in indentation (not allowed in YAML)
    TabInIndentation,

    /// Duplicate directive (e.g., two %YAML directives)
    DuplicateDirective,

    /// Invalid directive format
    InvalidDirective,

    /// Custom error message
    Custom(String),
}

impl ParseError {
    /// Create a new error with just a kind and span.
    #[must_use]
    pub const fn new(kind: ErrorKind, span: Span) -> Self {
        Self {
            kind,
            span,
            expected: Vec::new(),
            found: None,
        }
    }

    /// Add expected tokens to the error.
    #[must_use]
    pub fn with_expected(mut self, expected: Vec<String>) -> Self {
        self.expected = expected;
        self
    }

    /// Add the found token to the error.
    #[must_use]
    pub fn with_found(mut self, found: String) -> Self {
        self.found = Some(found);
        self
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UnexpectedEof => write!(f, "unexpected end of input"),
            ErrorKind::UnexpectedToken => {
                if let Some(ref found) = self.found {
                    write!(f, "unexpected token '{found}'")?;
                } else {
                    write!(f, "unexpected token")?;
                }
                if !self.expected.is_empty() {
                    write!(f, ", expected one of: {}", self.expected.join(", "))?;
                }
                Ok(())
            }
            ErrorKind::InvalidIndentation => write!(f, "invalid indentation"),
            ErrorKind::UnterminatedString => write!(f, "unterminated string literal"),
            ErrorKind::InvalidEscape(c) => write!(f, "invalid escape sequence '\\{c}'"),
            ErrorKind::InvalidNumber => write!(f, "invalid number format"),
            ErrorKind::DuplicateKey => write!(f, "duplicate key in mapping"),
            ErrorKind::InvalidAnchor => write!(f, "invalid anchor name"),
            ErrorKind::DuplicateAnchor => write!(f, "duplicate anchor on same node"),
            ErrorKind::UndefinedAlias => write!(f, "undefined alias"),
            ErrorKind::InvalidTag => write!(f, "invalid tag"),
            ErrorKind::DuplicateTag => write!(f, "duplicate tag on same node"),
            ErrorKind::PropertiesOnAlias => write!(f, "anchor/tag cannot be applied to alias"),
            ErrorKind::InvalidBlockScalar => write!(f, "invalid block scalar header"),
            ErrorKind::TabInIndentation => {
                write!(f, "tab character in indentation (use spaces)")
            }
            ErrorKind::DuplicateDirective => write!(f, "duplicate directive"),
            ErrorKind::InvalidDirective => write!(f, "invalid directive format"),
            ErrorKind::Custom(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for ParseError {}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::span::Span as _;

    #[test]
    fn test_error_display() {
        let err = ParseError::new(ErrorKind::UnterminatedString, Span::new((), 0..10));
        assert_eq!(err.to_string(), "unterminated string literal");
    }
}
