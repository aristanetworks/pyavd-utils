// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Error types for YAML parsing.

use crate::span::{IndentLevel, Span};
use derive_more::Display;

/// An error encountered during YAML parsing.
///
/// Errors include their source span, enabling accurate error reporting.
///
/// The `span` field contains byte offsets relative to the original input.
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    /// The kind of error
    pub kind: ErrorKind,
    /// The span in the source where the error occurred.
    pub span: Span,
}

/// The kind of parse error.
#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum ErrorKind {
    /// Unexpected end of input
    #[display("unexpected end of input")]
    UnexpectedEof,

    /// Invalid character that cannot be parsed
    #[display("invalid character")]
    InvalidCharacter,

    /// Invalid value where a scalar, sequence, or mapping was expected
    #[display("invalid value")]
    InvalidValue,

    /// Mismatched quote styles (e.g., opening with ' but closing with ")
    #[display("mismatched quote styles")]
    MismatchedQuotes,

    /// Trailing content after a valid value
    /// e.g., `key: "value" extra content`
    #[display("unexpected content after value")]
    TrailingContent,

    /// Missing comma between flow collection elements
    /// e.g., `[a b]` instead of `[a, b]`
    #[display("missing separator (comma) in flow collection")]
    MissingSeparator,

    /// Extra closing bracket/brace in flow collection
    /// e.g., `[a, b]]`
    #[display("unmatched closing bracket")]
    UnmatchedBracket,

    /// Mismatched bracket types
    /// e.g., `[a, b}` - opened with `[` but closed with `}`
    #[display("mismatched brackets (expected matching bracket type)")]
    MismatchedBrackets,

    /// Content on same line as previous mapping entry
    /// e.g., `{y: z}invalid` or `- item- invalid`
    #[display("invalid content on same line as previous entry")]
    ContentOnSameLine,

    /// Invalid multiline implicit key (implicit keys must be single line)
    #[display("implicit keys must be on a single line")]
    MultilineImplicitKey,

    /// Invalid colon placement (unexpected colon in value context)
    /// e.g., `a: b: c` in plain scalar context
    #[display("unexpected colon in value")]
    UnexpectedColon,

    /// Invalid indentation.
    #[display("invalid indentation")]
    InvalidIndentation,

    /// Invalid indentation with context.
    #[display("invalid indentation: expected {expected} spaces, found {found}")]
    InvalidIndentationContext {
        /// Expected indentation
        expected: IndentLevel,
        /// Found indentation
        found: IndentLevel,
    },

    /// Missing a block-sequence indicator (`-`) at sequence indentation.
    #[display("missing '-' for sequence entry")]
    MissingSequenceIndicator,

    /// Unterminated string literal
    #[display("unterminated string literal")]
    UnterminatedString,

    /// Invalid escape sequence in a string (contains the invalid character)
    #[display("invalid escape sequence '\\{_0}'")]
    InvalidEscape(char),

    /// Numeric escape sequence ended before the required number of hex digits.
    #[display("incomplete numeric escape sequence (expected {expected} hexadecimal digits)")]
    InvalidEscapeLength {
        /// Required number of hexadecimal digits for this escape kind.
        expected: u8,
    },

    /// Invalid non-hex character inside a numeric escape sequence.
    #[display("invalid hexadecimal digit in escape sequence")]
    InvalidEscapeCharacter,

    /// Numeric escape sequence encodes an invalid Unicode scalar value.
    #[display("invalid Unicode scalar value in escape sequence")]
    InvalidUnicodeEscape,

    /// Invalid number format
    #[display("invalid number format")]
    InvalidNumber,

    /// Invalid anchor name
    #[display("invalid anchor name")]
    InvalidAnchor,

    /// Duplicate anchor on same node (e.g., &a &b value)
    #[display("duplicate anchor on same node")]
    DuplicateAnchor,

    /// Undefined alias reference
    #[display("undefined alias")]
    UndefinedAlias,

    /// Invalid tag
    #[display("invalid tag")]
    InvalidTag,

    /// Duplicate tag on same node (e.g., !a !b value)
    #[display("duplicate tag on same node")]
    DuplicateTag,

    /// Properties (anchor/tag) cannot be applied to alias
    #[display("anchor/tag cannot be applied to alias")]
    PropertiesOnAlias,

    /// Invalid block scalar header
    #[display("invalid block scalar header")]
    InvalidBlockScalar,

    /// Tab character in indentation (not allowed in YAML)
    #[display("tab character in indentation (use spaces)")]
    TabInIndentation,

    /// Duplicate directive (e.g., two %YAML directives)
    #[display("duplicate directive")]
    DuplicateDirective,

    /// Invalid directive format
    #[display("invalid directive format")]
    InvalidDirective,

    /// Tag handle used but not declared in document prolog
    #[display("tag handle not declared in document")]
    UndefinedTagHandle,

    /// Comment without preceding whitespace
    /// e.g., `key: "value"#comment` instead of `key: "value" #comment`
    #[display("comment must be preceded by whitespace")]
    InvalidComment,

    /// Missing colon after mapping key
    /// e.g., `key\n  value` instead of `key:\n  value`
    #[display("missing colon after mapping key")]
    MissingColon,

    /// Block indicator used in flow context
    /// e.g., `[-]` using `-` block indicator inside flow sequence
    #[display("block indicator not allowed in flow context")]
    BlockIndicatorInFlow,

    /// Document marker (`---` or `...`) in flow context
    /// e.g., `[\n---\n]` using document marker inside flow sequence
    #[display("document marker not allowed in flow context")]
    DocumentMarkerInFlow,

    /// Document marker (`---` or `...`) inside a quoted scalar
    /// e.g., `"text\n---\nmore"` where `---` at column 0 is forbidden
    #[display("document marker not allowed in scalar content")]
    DocumentMarkerInScalar,

    /// Anchor or tag without a following value
    /// e.g., `key: &anchor` on its own line, or `&x\n- item` where the anchor
    /// cannot attach to the block sequence
    #[display("anchor or tag without a following value")]
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
            Self::MissingSequenceIndicator => Some(
                "add '-' to start a sequence entry, or adjust the indentation to match the intended structure",
            ),
            Self::TabInIndentation => {
                Some("replace tabs with spaces; YAML requires space-based indentation")
            }
            Self::UnterminatedString => Some("add the matching closing quote character"),
            Self::InvalidEscape(_) => {
                Some("valid escape sequences: \\n, \\r, \\t, \\\\, \\\", \\', \\0, \\x##, \\u####")
            }
            Self::InvalidEscapeLength { .. } => {
                Some("numeric escapes must provide the required number of hexadecimal digits")
            }
            Self::InvalidEscapeCharacter => {
                Some("use only hexadecimal digits (0-9, a-f, A-F) in numeric escapes")
            }
            Self::InvalidUnicodeEscape => Some(
                "use a Unicode scalar value in the valid range, excluding surrogate code points",
            ),
            Self::UndefinedAlias => {
                Some("define the anchor with &name before referencing it with *name")
            }
            Self::DuplicateAnchor => {
                Some("a node can only have one anchor; remove the extra &anchor")
            }
            Self::DuplicateTag => Some("a node can only have one tag; remove the extra !tag"),
            Self::PropertiesOnAlias => Some(
                "aliases (*name) cannot have anchors or tags; apply them to the original value",
            ),
            Self::OrphanedProperties => {
                Some("attach the anchor or tag to the following value at the correct indentation")
            }
            Self::UndefinedTagHandle => Some(
                "add a %TAG directive to define the handle, e.g., %TAG !e! tag:example.com,2000:",
            ),
            Self::InvalidBlockScalar => Some(
                "use | or > followed by optional indent [1-9] and chomping [+-], e.g., |2- or >+",
            ),
            Self::TrailingContent => {
                Some("remove extra content after the value, or quote the entire value")
            }
            Self::MissingSeparator => Some("add a comma between flow collection elements"),
            Self::UnmatchedBracket => {
                Some("remove the extra closing bracket/brace, or add the matching opening")
            }
            Self::MismatchedBrackets => Some("use matching bracket types: [ with ] or { with }"),
            Self::ContentOnSameLine => {
                Some("start new mapping entries or sequence items on their own line")
            }
            Self::MultilineImplicitKey => Some(
                "use explicit key syntax (? key) for multiline keys, or keep the key on one line",
            ),
            Self::UnexpectedColon => {
                Some("quote the value to include colons, or remove the extra colon")
            }
            Self::InvalidComment => Some("add a space before the # character to start a comment"),
            Self::MissingColon => Some("add a colon after the mapping key"),
            Self::BlockIndicatorInFlow => Some(
                "block indicators (-, ?, :) cannot be used inside flow collections []/{}; use commas to separate elements",
            ),
            Self::DocumentMarkerInFlow => {
                Some("document markers (--- and ...) cannot appear inside flow collections")
            }
            Self::DocumentMarkerInScalar => Some(
                "document markers (--- and ...) at column 0 are forbidden in multi-line scalars",
            ),
            // No specific suggestion for these
            Self::UnexpectedEof
            | Self::InvalidCharacter
            | Self::InvalidValue
            | Self::MismatchedQuotes
            | Self::InvalidNumber
            | Self::InvalidAnchor
            | Self::InvalidTag
            | Self::DuplicateDirective
            | Self::InvalidDirective => None,
        }
    }
}

impl ParseError {
    /// Create a new error with just a kind and span.
    #[must_use]
    pub const fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
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
        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for ParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = ParseError::new(ErrorKind::UnterminatedString, Span::from_usize_range(0..10));
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
                ErrorKind::InvalidEscapeLength { expected: 4 },
                "incomplete numeric escape sequence (expected 4 hexadecimal digits)",
            ),
            (
                ErrorKind::InvalidEscapeCharacter,
                "invalid hexadecimal digit in escape sequence",
            ),
            (
                ErrorKind::InvalidUnicodeEscape,
                "invalid Unicode scalar value in escape sequence",
            ),
        ];

        for (kind, expected_msg) in test_cases {
            let err = ParseError::new(kind, Span::from_usize_range(0..10));
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
            ErrorKind::MissingSequenceIndicator,
            ErrorKind::TabInIndentation,
            ErrorKind::UnterminatedString,
            ErrorKind::InvalidEscape('x'),
            ErrorKind::InvalidEscapeLength { expected: 4 },
            ErrorKind::InvalidEscapeCharacter,
            ErrorKind::InvalidUnicodeEscape,
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
            ErrorKind::InvalidCharacter,
            ErrorKind::InvalidValue,
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
        let err = ParseError::new(ErrorKind::TabInIndentation, Span::from_usize_range(0..1));
        assert!(err.suggestion().is_some());
        assert!(err.suggestion().unwrap().contains("spaces"));
    }
}
