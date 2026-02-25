// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Error types for YAML parsing.

use crate::span::Span;
use chumsky::span::Span as _;

/// An error encountered during YAML parsing.
///
/// Errors include their source span, enabling accurate error reporting
/// with line/column information.
///
/// # Span Coordinates
///
/// The `span` field contains byte offsets relative to the document being parsed.
/// For multi-document YAML streams, use `span_offset` to convert to global
/// coordinates (relative to the original input):
///
/// ```ignore
/// let global_start = error.span.start + error.span_offset;
/// let global_end = error.span.end + error.span_offset;
/// // Or use the helper method:
/// let global_span = error.global_span();
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    /// The kind of error
    pub kind: ErrorKind,
    /// The span in the source where the error occurred (document-relative)
    pub span: Span,
    /// Byte offset to add to span for global coordinates.
    /// For single-document parsing this is 0. For multi-document streams,
    /// this is the byte offset where the document starts in the original input.
    pub span_offset: usize,
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

    /// Invalid indentation.
    InvalidIndentation,

    /// Invalid indentation with context.
    InvalidIndentationContext { expected: usize, found: usize },

    /// Unterminated string literal (includes quote style for better messaging)
    UnterminatedString,

    /// Unterminated string literal with quote style
    UnterminatedQuotedString { double_quoted: bool },

    /// Invalid escape sequence in a string (contains the invalid character)
    InvalidEscape(char),

    /// Invalid number format
    InvalidNumber,

    /// Duplicate key in a mapping (includes key name if available)
    DuplicateKey,

    /// Duplicate key with the key name
    DuplicateKeyNamed(String),

    /// Invalid anchor name
    InvalidAnchor,

    /// Duplicate anchor on same node (e.g., &a &b value)
    DuplicateAnchor,

    /// Duplicate anchor with names
    DuplicateAnchorNamed { first: String, second: String },

    /// Undefined alias reference
    UndefinedAlias,

    /// Undefined alias with the alias name
    UndefinedAliasNamed(String),

    /// Invalid tag
    InvalidTag,

    /// Duplicate tag on same node (e.g., !a !b value)
    DuplicateTag,

    /// Duplicate tag with names
    DuplicateTagNamed { first: String, second: String },

    /// Properties (anchor/tag) cannot be applied to alias
    PropertiesOnAlias,

    /// Invalid block scalar header (with details)
    InvalidBlockScalar,

    /// Invalid block scalar header with detail message
    InvalidBlockScalarDetail(String),

    /// Tab character in indentation (not allowed in YAML)
    TabInIndentation,

    /// Duplicate directive (e.g., two %YAML directives)
    DuplicateDirective,

    /// Duplicate directive with name
    DuplicateDirectiveNamed(String),

    /// Invalid directive format
    InvalidDirective,

    /// Invalid directive with detail
    InvalidDirectiveDetail(String),

    /// Tag handle used but not declared in document prolog
    UndefinedTagHandle,

    /// Undefined tag handle with the handle name
    UndefinedTagHandleNamed(String),

    /// Custom error message
    Custom(String),
}

impl ErrorKind {
    /// Get a suggestion for how to fix this error.
    ///
    /// Returns `Some(suggestion)` if a helpful fix suggestion is available,
    /// or `None` if no specific suggestion applies.
    #[must_use]
    pub fn suggestion(&self) -> Option<&'static str> {
        match self {
            Self::InvalidIndentation | Self::InvalidIndentationContext { .. } => {
                Some("YAML uses spaces for indentation; ensure consistent indentation levels")
            }
            Self::TabInIndentation => {
                Some("Replace tabs with spaces; YAML requires space-based indentation")
            }
            Self::UnterminatedString | Self::UnterminatedQuotedString { .. } => {
                Some("Add the matching closing quote character")
            }
            Self::InvalidEscape(_) => {
                Some("Valid escape sequences: \\n, \\r, \\t, \\\\, \\\", \\', \\0, \\x##, \\u####")
            }
            Self::DuplicateKey | Self::DuplicateKeyNamed(_) => {
                Some("Remove or rename one of the duplicate keys")
            }
            Self::UndefinedAlias | Self::UndefinedAliasNamed(_) => {
                Some("Define the anchor with &name before referencing it with *name")
            }
            Self::DuplicateAnchor | Self::DuplicateAnchorNamed { .. } => {
                Some("A node can only have one anchor; remove the extra &anchor")
            }
            Self::DuplicateTag | Self::DuplicateTagNamed { .. } => {
                Some("A node can only have one tag; remove the extra !tag")
            }
            Self::PropertiesOnAlias => Some(
                "Aliases (*name) cannot have anchors or tags; apply them to the original value",
            ),
            Self::UndefinedTagHandle | Self::UndefinedTagHandleNamed(_) => Some(
                "Add a %TAG directive to define the handle, e.g., %TAG !e! tag:example.com,2000:",
            ),
            Self::InvalidBlockScalar | Self::InvalidBlockScalarDetail(_) => Some(
                "Block scalar header format: | or > followed by optional [1-9] indent and [-+] chomping",
            ),
            // No specific suggestion for these
            Self::UnexpectedEof
            | Self::UnexpectedToken
            | Self::InvalidNumber
            | Self::InvalidAnchor
            | Self::InvalidTag
            | Self::DuplicateDirective
            | Self::DuplicateDirectiveNamed(_)
            | Self::InvalidDirective
            | Self::InvalidDirectiveDetail(_)
            | Self::Custom(_) => None,
        }
    }
}

impl ParseError {
    /// Create a new error with just a kind and span.
    ///
    /// The `span_offset` is initialized to 0. Use [`with_offset`](Self::with_offset)
    /// to set the offset for multi-document streams.
    #[must_use]
    pub const fn new(kind: ErrorKind, span: Span) -> Self {
        Self {
            kind,
            span,
            span_offset: 0,
            expected: Vec::new(),
            found: None,
        }
    }

    /// Set the span offset for converting to global coordinates.
    #[must_use]
    pub const fn with_offset(mut self, offset: usize) -> Self {
        self.span_offset = offset;
        self
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

    /// Get the span in global coordinates (relative to original input).
    ///
    /// For single-document parsing, this returns the same as `span`.
    /// For multi-document streams, this adds `span_offset` to get the
    /// position relative to the original input.
    #[must_use]
    pub fn global_span(&self) -> Span {
        Span::new(
            (),
            self.span.start + self.span_offset..self.span.end + self.span_offset,
        )
    }

    /// Get a suggestion for how to fix this error.
    ///
    /// Delegates to [`ErrorKind::suggestion()`].
    #[must_use]
    pub fn suggestion(&self) -> Option<&'static str> {
        self.kind.suggestion()
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UnexpectedEof => write!(f, "unexpected end of input"),
            ErrorKind::UnexpectedToken => {
                if let Some(found) = &self.found {
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
            ErrorKind::InvalidIndentationContext { expected, found } => {
                write!(
                    f,
                    "invalid indentation: expected {expected} spaces, found {found}"
                )
            }
            ErrorKind::UnterminatedString => write!(f, "unterminated string literal"),
            ErrorKind::UnterminatedQuotedString { double_quoted } => {
                let quote = if *double_quoted { '"' } else { '\'' };
                write!(f, "unterminated string literal, missing closing {quote}")
            }
            ErrorKind::InvalidEscape(ch) => write!(f, "invalid escape sequence '\\{ch}'"),
            ErrorKind::InvalidNumber => write!(f, "invalid number format"),
            ErrorKind::DuplicateKey => write!(f, "duplicate key in mapping"),
            ErrorKind::DuplicateKeyNamed(key) => {
                write!(f, "duplicate key '{key}' in mapping")
            }
            ErrorKind::InvalidAnchor => write!(f, "invalid anchor name"),
            ErrorKind::DuplicateAnchor => write!(f, "duplicate anchor on same node"),
            ErrorKind::DuplicateAnchorNamed { first, second } => {
                write!(
                    f,
                    "duplicate anchor: node already has anchor '&{first}', cannot add '&{second}'"
                )
            }
            ErrorKind::UndefinedAlias => write!(f, "undefined alias"),
            ErrorKind::UndefinedAliasNamed(name) => {
                write!(f, "undefined alias '*{name}': anchor '&{name}' not defined")
            }
            ErrorKind::InvalidTag => write!(f, "invalid tag"),
            ErrorKind::DuplicateTag => write!(f, "duplicate tag on same node"),
            ErrorKind::DuplicateTagNamed { first, second } => {
                write!(
                    f,
                    "duplicate tag: node already has tag '{first}', cannot add '{second}'"
                )
            }
            ErrorKind::PropertiesOnAlias => write!(f, "anchor/tag cannot be applied to alias"),
            ErrorKind::InvalidBlockScalar => write!(f, "invalid block scalar header"),
            ErrorKind::InvalidBlockScalarDetail(detail) => {
                write!(f, "invalid block scalar header: {detail}")
            }
            ErrorKind::TabInIndentation => {
                write!(f, "tab character in indentation (use spaces)")
            }
            ErrorKind::DuplicateDirective => write!(f, "duplicate directive"),
            ErrorKind::DuplicateDirectiveNamed(name) => {
                write!(f, "duplicate %{name} directive")
            }
            ErrorKind::InvalidDirective => write!(f, "invalid directive format"),
            ErrorKind::InvalidDirectiveDetail(detail) => {
                write!(f, "invalid directive: {detail}")
            }
            ErrorKind::UndefinedTagHandle => write!(f, "tag handle not declared in document"),
            ErrorKind::UndefinedTagHandleNamed(handle) => {
                write!(f, "tag handle '{handle}' not declared in document")
            }
            ErrorKind::Custom(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for ParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = ParseError::new(ErrorKind::UnterminatedString, Span::new((), 0..10));
        assert_eq!(err.to_string(), "unterminated string literal");
    }

    #[test]
    fn test_error_display_with_context() {
        // Test error variants with contextual information
        let test_cases = [
            (
                ErrorKind::InvalidIndentationContext {
                    expected: 4,
                    found: 2,
                },
                "invalid indentation: expected 4 spaces, found 2",
            ),
            (
                ErrorKind::UnterminatedQuotedString {
                    double_quoted: true,
                },
                "unterminated string literal, missing closing \"",
            ),
            (
                ErrorKind::UnterminatedQuotedString {
                    double_quoted: false,
                },
                "unterminated string literal, missing closing '",
            ),
            (
                ErrorKind::DuplicateKeyNamed("name".to_owned()),
                "duplicate key 'name' in mapping",
            ),
            (
                ErrorKind::UndefinedAliasNamed("foo".to_owned()),
                "undefined alias '*foo': anchor '&foo' not defined",
            ),
            (
                ErrorKind::DuplicateAnchorNamed {
                    first: "a".to_owned(),
                    second: "b".to_owned(),
                },
                "duplicate anchor: node already has anchor '&a', cannot add '&b'",
            ),
            (
                ErrorKind::DuplicateTagNamed {
                    first: "!str".to_owned(),
                    second: "!int".to_owned(),
                },
                "duplicate tag: node already has tag '!str', cannot add '!int'",
            ),
            (
                ErrorKind::UndefinedTagHandleNamed("!e!".to_owned()),
                "tag handle '!e!' not declared in document",
            ),
            (
                ErrorKind::DuplicateDirectiveNamed("YAML".to_owned()),
                "duplicate %YAML directive",
            ),
            (
                ErrorKind::InvalidDirectiveDetail("expected version".to_owned()),
                "invalid directive: expected version",
            ),
            (
                ErrorKind::InvalidBlockScalarDetail("invalid indent".to_owned()),
                "invalid block scalar header: invalid indent",
            ),
        ];

        for (kind, expected_msg) in test_cases {
            let err = ParseError::new(kind, Span::new((), 0..10));
            assert_eq!(err.to_string(), expected_msg);
        }
    }

    #[test]
    fn test_error_suggestions() {
        // Test that error kinds have appropriate suggestions
        let with_suggestions = [
            ErrorKind::InvalidIndentation,
            ErrorKind::InvalidIndentationContext {
                expected: 4,
                found: 2,
            },
            ErrorKind::TabInIndentation,
            ErrorKind::UnterminatedString,
            ErrorKind::UnterminatedQuotedString {
                double_quoted: true,
            },
            ErrorKind::InvalidEscape('x'),
            ErrorKind::DuplicateKey,
            ErrorKind::DuplicateKeyNamed("key".to_owned()),
            ErrorKind::UndefinedAlias,
            ErrorKind::UndefinedAliasNamed("foo".to_owned()),
            ErrorKind::DuplicateAnchor,
            ErrorKind::DuplicateTag,
            ErrorKind::PropertiesOnAlias,
            ErrorKind::UndefinedTagHandle,
            ErrorKind::InvalidBlockScalar,
        ];

        for kind in with_suggestions {
            assert!(
                kind.suggestion().is_some(),
                "{kind:?} should have a suggestion"
            );
        }

        // These errors don't have specific suggestions
        let without_suggestions = [
            ErrorKind::UnexpectedEof,
            ErrorKind::UnexpectedToken,
            ErrorKind::InvalidNumber,
            ErrorKind::InvalidAnchor,
            ErrorKind::InvalidTag,
            ErrorKind::DuplicateDirective,
            ErrorKind::InvalidDirective,
            ErrorKind::Custom("custom error".to_owned()),
        ];

        for kind in without_suggestions {
            assert!(
                kind.suggestion().is_none(),
                "{kind:?} should not have a suggestion"
            );
        }
    }

    #[test]
    fn test_parse_error_suggestion_delegation() {
        let err = ParseError::new(ErrorKind::TabInIndentation, Span::new((), 0..1));
        assert!(err.suggestion().is_some());
        assert!(err.suggestion().unwrap().contains("spaces"));
    }

    #[test]
    fn test_global_span() {
        // Without offset, global_span equals span
        let err = ParseError::new(ErrorKind::UnexpectedToken, Span::new((), 10..20));
        let global = err.global_span();
        assert_eq!(global.start, 10);
        assert_eq!(global.end, 20);

        // With offset, global_span adds the offset
        let err_with_offset = err.with_offset(100);
        let global_with_offset = err_with_offset.global_span();
        assert_eq!(global_with_offset.start, 110);
        assert_eq!(global_with_offset.end, 120);
        // Original span is unchanged
        assert_eq!(err_with_offset.span.start, 10);
        assert_eq!(err_with_offset.span.end, 20);
    }
}
