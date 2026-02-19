// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Stream-level lexer for YAML.
//!
//! This lexer handles the top level of YAML: document markers and directives.
//! It recognizes:
//! - `---` (document start)
//! - `...` (document end)
//! - `%YAML`, `%TAG`, and reserved directives
//!
//! The output is a sequence of "raw documents" where each document contains
//! the raw content string to be parsed by the document-level lexer.

use chumsky::span::Span as _;

use crate::error::{ErrorKind, ParseError};
use crate::span::{Span, Spanned};

/// A directive found in the YAML stream.
#[derive(Debug, Clone, PartialEq)]
pub enum Directive {
    /// %YAML version
    Yaml(String),
    /// %TAG handle prefix
    Tag(String),
    /// Reserved directive (%NAME content)
    Reserved(String),
}

/// A raw document extracted from the stream.
#[derive(Debug, Clone)]
pub struct RawDocument {
    /// Directives before this document (in the directive prologue).
    pub directives: Vec<Spanned<Directive>>,
    /// Whether this document had an explicit `---` marker.
    pub explicit_start: bool,
    /// The span of the document start marker (if explicit).
    pub start_marker_span: Option<Span>,
    /// The raw content of the document (between markers).
    pub content: String,
    /// The span of the content in the original input.
    pub content_span: Span,
    /// Whether this document had an explicit `...` marker.
    pub explicit_end: bool,
}

/// Token types for stream-level lexing.
#[derive(Debug, Clone, PartialEq)]
enum StreamToken {
    DocStart,
    DocEnd,
    YamlDirective(String),
    TagDirective(String),
    ReservedDirective(String),
    /// Content line (not a marker or directive)
    Content(String),
    /// Newline character(s)
    Newline,
}

/// Parse the YAML stream into raw documents.
///
/// This is Layer 1 of the layered parser architecture. It extracts:
/// - Directives from the directive prologue
/// - Raw document content between markers
///
/// The raw content can then be passed to the document-level lexer for
/// context-aware tokenization.
///
/// Returns a tuple of (documents, errors).
pub fn parse_stream(input: &str) -> (Vec<RawDocument>, Vec<ParseError>) {
    let mut documents = Vec::new();
    let mut errors = Vec::new();
    let mut current_directives: Vec<Spanned<Directive>> = Vec::new();
    let mut current_content = String::new();
    let mut content_start: usize = 0;
    let mut explicit_start = false;
    let mut start_marker_span: Option<Span> = None;
    let mut in_directive_prologue = true; // At start, we can have directives
    let mut has_yaml_directive = false; // Track if we've seen a %YAML directive

    let mut pos: usize = 0;
    let chars: Vec<char> = input.chars().collect();

    while pos < chars.len() {
        let line_start = pos;

        // Check for document markers or directives at line start
        let remaining = &input[pos..];

        if remaining.starts_with("---") {
            let marker_end = pos + 3;
            // Check if followed by whitespace, newline, or EOF
            let next_char = chars.get(marker_end);
            if next_char.is_none() || matches!(next_char, Some(' ' | '\t' | '\n' | '\r')) {
                // This is a document start marker
                // First, finalize any pending document (only if there's actual content,
                // or if we previously had an explicit start - meaning empty doc between markers)
                // Directives alone without content belong to the NEXT document.
                if !current_content.is_empty() || explicit_start {
                    documents.push(RawDocument {
                        directives: std::mem::take(&mut current_directives),
                        explicit_start,
                        start_marker_span,
                        content: std::mem::take(&mut current_content),
                        content_span: Span::new((), content_start..pos),
                        explicit_end: false,
                    });
                    // Reset directive tracking for next document's prologue
                    has_yaml_directive = false;
                }
                // Note: current_directives are NOT taken if no document was pushed -
                // they carry over to the next document

                // Start new document
                explicit_start = true;
                start_marker_span = Some(Span::new((), pos..marker_end));
                in_directive_prologue = false;

                // Content after `---` on the same line is part of the document
                // (e.g., `--- value` or `--- >` for block scalars)
                // Skip past the marker
                pos = marker_end;
                // Skip any whitespace after the marker (but not the content)
                while pos < input.len() && matches!(chars.get(pos), Some(' ' | '\t')) {
                    pos += 1;
                }
                // Check if there's content on this line (not just whitespace/newline)
                if pos < input.len() && !matches!(chars.get(pos), Some('\n' | '\r') | None) {
                    // There's content on the same line as --- - include it
                    content_start = pos;
                } else {
                    // No content on this line, skip past newline
                    pos = skip_to_eol(input, pos);
                    content_start = pos;
                }
                continue;
            }
        }

        if remaining.starts_with("...") {
            let marker_end = pos + 3;
            let next_char = chars.get(marker_end);
            if next_char.is_none() || matches!(next_char, Some(' ' | '\t' | '\n' | '\r')) {
                // Document end marker - finalize current document
                // Check for directives without a document start before ...
                // If we have directives but no explicit_start and no content, that's an error
                if !current_directives.is_empty() && !explicit_start && current_content.is_empty() {
                    let span = current_directives
                        .first()
                        .map(|(_, s)| *s)
                        .unwrap_or(Span::new((), pos..marker_end));
                    errors.push(ParseError {
                        kind: ErrorKind::UnexpectedToken,
                        span,
                        expected: Vec::new(),
                        found: None,
                    });
                }

                if !current_content.is_empty() || explicit_start || !current_directives.is_empty() {
                    documents.push(RawDocument {
                        directives: std::mem::take(&mut current_directives),
                        explicit_start,
                        start_marker_span: start_marker_span.take(),
                        content: std::mem::take(&mut current_content),
                        content_span: Span::new((), content_start..pos),
                        explicit_end: true,
                    });
                }

                // After `...`, we're back in directive prologue
                explicit_start = false;
                in_directive_prologue = true;
                // Reset directive tracking for next document's prologue
                has_yaml_directive = false;

                // Check for invalid content after `...` on the same line
                // Only whitespace and comments are allowed
                let mut check_pos = marker_end;
                while check_pos < chars.len() {
                    let c = chars[check_pos];
                    if c == '\n' || c == '\r' {
                        break;
                    }
                    if c == ' ' || c == '\t' {
                        check_pos += 1;
                        continue;
                    }
                    if c == '#' {
                        // Comment is valid
                        break;
                    }
                    // Invalid content after document end marker
                    let line_end = find_eol(input, check_pos);
                    errors.push(ParseError {
                        kind: ErrorKind::UnexpectedToken,
                        span: Span::new((), check_pos..line_end),
                        expected: Vec::new(),
                        found: None,
                    });
                    break;
                }

                pos = marker_end;
                pos = skip_to_eol(input, pos);
                content_start = pos;
                continue;
            }
        }

        // Check for directives (only in directive prologue)
        if in_directive_prologue && remaining.starts_with('%') {
            let directive_start = pos;
            let line_end = find_eol(input, pos);
            let line = &input[pos..line_end];
            let directive_span = Span::new((), directive_start..line_end);

            let directive = if line.starts_with("%YAML") {
                // Check for duplicate %YAML directive
                if has_yaml_directive {
                    errors.push(ParseError {
                        kind: ErrorKind::DuplicateDirective,
                        span: directive_span,
                        expected: Vec::new(),
                        found: None,
                    });
                }
                has_yaml_directive = true;

                // Validate %YAML directive format: should be %YAML <version> optionally followed by comment
                // Version format is typically "1.0", "1.1", "1.2" etc.
                let after_yaml = line[5..].trim();
                // Strip trailing comment if present
                let version_and_rest = if let Some(hash_pos) = after_yaml.find('#') {
                    after_yaml[..hash_pos].trim()
                } else {
                    after_yaml
                };
                // Check that version_and_rest is a single word of digits and dots
                let parts: Vec<&str> = version_and_rest.split_whitespace().collect();
                let is_valid_version =
                    parts.len() == 1 && parts[0].chars().all(|c| c.is_ascii_digit() || c == '.');
                if !is_valid_version {
                    errors.push(ParseError {
                        kind: ErrorKind::InvalidDirective,
                        span: directive_span,
                        expected: Vec::new(),
                        found: None,
                    });
                }
                Some(Directive::Yaml(version_and_rest.to_string()))
            } else if line.starts_with("%TAG") {
                Some(Directive::Tag(line[4..].trim().to_string()))
            } else {
                Some(Directive::Reserved(line[1..].trim().to_string()))
            };

            if let Some(d) = directive {
                current_directives.push((d, directive_span));
            }

            pos = skip_to_eol(input, line_end);
            continue;
        }

        // Skip comment-only lines in directive prologue - they don't reset the prologue
        // Also handle comments with leading whitespace (e.g., "  # comment")
        let trimmed_remaining = remaining.trim_start();
        if in_directive_prologue
            && (remaining.starts_with('#')
                || trimmed_remaining.starts_with('#')
                || trimmed_remaining.is_empty())
        {
            let line_end = find_eol(input, pos);
            pos = line_end;
            if pos < input.len() {
                pos = skip_newline(input, pos);
            }
            continue;
        }

        // Check for directive-like content outside the directive prologue
        // If we see %YAML or %TAG after document content (without ...), AND it's followed by ---,
        // that's an invalid directive placement. If not followed by ---, it's just content.
        if !in_directive_prologue
            && remaining.starts_with('%')
            && (remaining.starts_with("%YAML") || remaining.starts_with("%TAG"))
        {
            let line_end = find_eol(input, pos);
            // Look ahead to see if there's a --- on a subsequent line (before any ...)
            // If yes, this is trying to be a directive (invalid). If no, it's content (valid).
            let mut lookahead_pos = line_end;
            if lookahead_pos < input.len() {
                lookahead_pos = skip_newline(input, lookahead_pos);
            }
            let mut found_doc_start = false;
            while lookahead_pos < input.len() {
                let lookahead_remaining = &input[lookahead_pos..];
                // Skip whitespace-only or comment lines
                let first_char = lookahead_remaining.chars().next();
                if first_char == Some('#') || lookahead_remaining.trim_start().is_empty() {
                    let la_line_end = find_eol(input, lookahead_pos);
                    lookahead_pos = la_line_end;
                    if lookahead_pos < input.len() {
                        lookahead_pos = skip_newline(input, lookahead_pos);
                    }
                    continue;
                }
                // Check for ---
                if lookahead_remaining.starts_with("---") {
                    let marker_end = lookahead_pos + 3;
                    let next_char = input[marker_end..].chars().next();
                    if next_char.is_none() || matches!(next_char, Some(' ' | '\t' | '\n' | '\r')) {
                        found_doc_start = true;
                    }
                }
                break;
            }
            if found_doc_start {
                errors.push(ParseError {
                    kind: ErrorKind::UnexpectedToken,
                    span: Span::new((), pos..line_end),
                    expected: Vec::new(),
                    found: None,
                });
            }
        }

        // Regular content line - we're no longer in directive prologue
        in_directive_prologue = false;

        // Add the line to current content
        let line_end = find_eol(input, pos);
        if current_content.is_empty() {
            content_start = pos;
        }
        current_content.push_str(&input[pos..line_end]);

        // Include the newline
        pos = line_end;
        if pos < input.len() {
            let newline_end = skip_newline(input, pos);
            current_content.push_str(&input[pos..newline_end]);
            pos = newline_end;
        }
    }

    // Finalize any remaining document
    if !current_content.is_empty() || explicit_start || !current_directives.is_empty() {
        // Check for directives without a following document
        // If we have directives but no explicit_start and no content, that's an error
        if !current_directives.is_empty() && !explicit_start && current_content.is_empty() {
            // Get the span of the first directive for the error
            let span = current_directives
                .first()
                .map(|(_, s)| *s)
                .unwrap_or(Span::new((), 0..0));
            errors.push(ParseError {
                kind: ErrorKind::UnexpectedToken,
                span,
                expected: Vec::new(),
                found: None,
            });
        }

        documents.push(RawDocument {
            directives: current_directives,
            explicit_start,
            start_marker_span,
            content: current_content,
            content_span: Span::new((), content_start..input.len()),
            explicit_end: false,
        });
    }

    (documents, errors)
}

/// Find the end of the current line (position of newline or EOF).
fn find_eol(input: &str, pos: usize) -> usize {
    let bytes = input.as_bytes();
    let mut p = pos;
    while p < bytes.len() {
        match bytes[p] {
            b'\n' | b'\r' => return p,
            _ => p += 1,
        }
    }
    p
}

/// Skip to the position after the current line (after newline).
fn skip_to_eol(input: &str, pos: usize) -> usize {
    let eol = find_eol(input, pos);
    skip_newline(input, eol)
}

/// Skip a newline character (handles \r\n, \r, \n).
fn skip_newline(input: &str, pos: usize) -> usize {
    let bytes = input.as_bytes();
    if pos >= bytes.len() {
        return pos;
    }
    match bytes[pos] {
        b'\r' => {
            if pos + 1 < bytes.len() && bytes[pos + 1] == b'\n' {
                pos + 2
            } else {
                pos + 1
            }
        }
        b'\n' => pos + 1,
        _ => pos,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_document_no_markers() {
        let input = "key: value\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        assert!(!docs[0].explicit_start);
        assert!(!docs[0].explicit_end);
        assert_eq!(docs[0].content, "key: value\n");
    }

    #[test]
    fn test_explicit_document_start() {
        let input = "---\nkey: value\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        assert!(docs[0].explicit_start);
        assert_eq!(docs[0].content, "key: value\n");
    }

    #[test]
    fn test_multiple_documents() {
        let input = "---\ndoc1\n---\ndoc2\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 2);
        assert!(docs[0].explicit_start);
        assert_eq!(docs[0].content.trim(), "doc1");
        assert!(docs[1].explicit_start);
        assert_eq!(docs[1].content.trim(), "doc2");
    }

    #[test]
    fn test_document_end_marker() {
        let input = "doc1\n...\n%YAML 1.2\n---\ndoc2\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 2);
        assert!(docs[0].explicit_end);
        assert_eq!(docs[1].directives.len(), 1);
        assert!(docs[1].explicit_start);
    }

    #[test]
    fn test_directives_at_start() {
        let input = "%YAML 1.2\n%TAG !e! tag:example.com,2000:\n---\nkey: value\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        assert_eq!(docs[0].directives.len(), 2);
        assert!(matches!(&docs[0].directives[0].0, Directive::Yaml(v) if v == "1.2"));
        assert!(matches!(&docs[0].directives[1].0, Directive::Tag(_)));
    }

    #[test]
    fn test_percent_inside_document() {
        // % inside document content should NOT be treated as directive
        let input = "{ matches\n% : 20 }\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        assert!(docs[0].directives.is_empty());
        // The % line should be part of content, not a directive
        assert!(docs[0].content.contains('%'));
    }

    #[test]
    fn test_empty_documents() {
        let input = "---\n---\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 2);
        assert!(docs[0].content.is_empty() || docs[0].content.trim().is_empty());
        assert!(docs[1].content.is_empty() || docs[1].content.trim().is_empty());
    }

    #[test]
    fn test_duplicate_yaml_directive() {
        let input = "%YAML 1.2\n%YAML 1.2\n---\nvalue\n";
        let (docs, errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        // Should have exactly one error for duplicate directive
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, ErrorKind::DuplicateDirective));
    }
}
