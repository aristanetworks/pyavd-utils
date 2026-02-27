// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Error types for YAML parsing.

use crate::span::Span;

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

    /// Unexpected character or token (generic fallback)
    UnexpectedToken,

    /// Trailing content after a valid value
    /// e.g., `key: "value" extra content`
    TrailingContent,

    /// Missing comma between flow collection elements
    /// e.g., `[a b]` instead of `[a, b]`
    MissingSeparator,

    /// Extra closing bracket/brace in flow collection
    /// e.g., `[a, b]]`
    UnmatchedBracket,

    /// Content on same line as previous mapping entry
    /// e.g., `{y: z}invalid` or `- item- invalid`
    ContentOnSameLine,

    /// Invalid multiline implicit key (implicit keys must be single line)
    MultilineImplicitKey,

    /// Invalid colon placement (unexpected colon in value context)
    /// e.g., `a: b: c` in plain scalar context
    UnexpectedColon,

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

    /// Tag handle used but not declared in document prolog
    UndefinedTagHandle,

    /// Comment without preceding whitespace
    /// e.g., `key: "value"#comment` instead of `key: "value" #comment`
    InvalidComment,

    /// Missing colon after mapping key
    /// e.g., `key\n  value` instead of `key:\n  value`
    MissingColon,

    /// Block indicator used in flow context
    /// e.g., `[-]` using `-` block indicator inside flow sequence
    BlockIndicatorInFlow,

    /// Document marker (`---` or `...`) in flow context
    /// e.g., `[\n---\n]` using document marker inside flow sequence
    DocumentMarkerInFlow,

    /// Anchor or tag without a following value
    /// e.g., `key: &anchor` on its own line, or `&x\n- item` where the anchor
    /// cannot attach to the block sequence
    OrphanedProperties,
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
            Self::DuplicateKey => Some("Remove or rename one of the duplicate keys"),
            Self::UndefinedAlias => {
                Some("Define the anchor with &name before referencing it with *name")
            }
            Self::DuplicateAnchor => {
                Some("A node can only have one anchor; remove the extra &anchor")
            }
            Self::DuplicateTag => Some("A node can only have one tag; remove the extra !tag"),
            Self::PropertiesOnAlias => Some(
                "Aliases (*name) cannot have anchors or tags; apply them to the original value",
            ),
            Self::UndefinedTagHandle => Some(
                "Add a %TAG directive to define the handle, e.g., %TAG !e! tag:example.com,2000:",
            ),
            Self::InvalidBlockScalar => Some(
                "Block scalar header format: | or > followed by optional [1-9] indent and [-+] chomping",
            ),
            Self::TrailingContent => {
                Some("Remove extra content after the value, or quote the entire value")
            }
            Self::MissingSeparator => Some("Add a comma between flow collection elements"),
            Self::UnmatchedBracket => {
                Some("Remove the extra closing bracket/brace, or add the matching opening")
            }
            Self::ContentOnSameLine => {
                Some("Start new mapping entries or sequence items on their own line")
            }
            Self::MultilineImplicitKey => Some(
                "Use explicit key syntax (? key) for multiline keys, or keep the key on one line",
            ),
            Self::UnexpectedColon => {
                Some("Quote the value to include colons, or remove the extra colon")
            }
            Self::InvalidComment => Some("Add a space before the # character to start a comment"),
            Self::MissingColon => Some("Add a colon after the mapping key"),
            Self::BlockIndicatorInFlow => Some(
                "Block indicators (-, ?, :) cannot be used inside flow collections []/{}; use commas to separate elements",
            ),
            Self::DocumentMarkerInFlow => {
                Some("Document markers (--- and ...) cannot appear inside flow collections")
            }
            // No specific suggestion for these
            Self::UnexpectedEof
            | Self::UnexpectedToken
            | Self::InvalidNumber
            | Self::InvalidAnchor
            | Self::InvalidTag
            | Self::DuplicateDirective
            | Self::InvalidDirective
            | Self::OrphanedProperties => None,
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
        Span::new(self.span.start + self.span_offset..self.span.end + self.span_offset)
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
            ErrorKind::UndefinedTagHandle => write!(f, "tag handle not declared in document"),
            ErrorKind::TrailingContent => write!(f, "unexpected content after value"),
            ErrorKind::MissingSeparator => {
                write!(f, "missing separator (comma) in flow collection")
            }
            ErrorKind::UnmatchedBracket => write!(f, "unmatched closing bracket"),
            ErrorKind::ContentOnSameLine => {
                write!(f, "invalid content on same line as previous entry")
            }
            ErrorKind::MultilineImplicitKey => write!(f, "implicit keys must be on a single line"),
            ErrorKind::UnexpectedColon => write!(f, "unexpected colon in value"),
            ErrorKind::InvalidComment => {
                write!(f, "comment must be preceded by whitespace")
            }
            ErrorKind::MissingColon => write!(f, "missing colon after mapping key"),
            ErrorKind::BlockIndicatorInFlow => {
                write!(f, "block indicator not allowed in flow context")
            }
            ErrorKind::DocumentMarkerInFlow => {
                write!(f, "document marker not allowed in flow context")
            }
            ErrorKind::OrphanedProperties => {
                write!(f, "anchor or tag without a following value")
            }
        }
    }
}

impl std::error::Error for ParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = ParseError::new(ErrorKind::UnterminatedString, Span::new(0..10));
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
        ];

        for (kind, expected_msg) in test_cases {
            let err = ParseError::new(kind, Span::new(0..10));
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
            ErrorKind::UndefinedAlias,
            ErrorKind::DuplicateAnchor,
            ErrorKind::DuplicateTag,
            ErrorKind::PropertiesOnAlias,
            ErrorKind::UndefinedTagHandle,
            ErrorKind::InvalidBlockScalar,
            // New specific error kinds
            ErrorKind::TrailingContent,
            ErrorKind::MissingSeparator,
            ErrorKind::UnmatchedBracket,
            ErrorKind::ContentOnSameLine,
            ErrorKind::MultilineImplicitKey,
            ErrorKind::UnexpectedColon,
            ErrorKind::InvalidComment,
            ErrorKind::MissingColon,
            ErrorKind::BlockIndicatorInFlow,
            ErrorKind::DocumentMarkerInFlow,
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
        let err = ParseError::new(ErrorKind::TabInIndentation, Span::new(0..1));
        assert!(err.suggestion().is_some());
        assert!(err.suggestion().unwrap().contains("spaces"));
    }

    #[test]
    fn test_global_span() {
        // Without offset, global_span equals span
        let err = ParseError::new(ErrorKind::UnexpectedToken, Span::new(10..20));
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
