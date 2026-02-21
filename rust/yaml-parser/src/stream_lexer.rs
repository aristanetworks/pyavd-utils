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
///
/// The content includes document markers (`---`, `...`) so the parser can see them.
/// Only directives (`%YAML`, `%TAG`) are extracted separately.
#[derive(Debug, Clone)]
pub struct RawDocument {
    /// Directives before this document (in the directive prologue).
    pub directives: Vec<Spanned<Directive>>,
    /// The raw content of the document, including `---` and `...` markers.
    /// The parser is responsible for interpreting these tokens.
    pub content: String,
    /// The span of the content in the original input.
    #[allow(
        dead_code,
        reason = "Public API field for consumers to access document boundaries"
    )]
    pub content_span: Span,
}

/// Parse the YAML stream into raw documents.
///
/// This is Layer 1 of the layered parser architecture. It:
/// - Extracts directives from the directive prologue (they don't go to parser)
/// - Includes document markers (`---`, `...`) in content (parser sees them)
/// - Splits stream at document boundaries
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
    let mut in_directive_prologue = true; // At start, we can have directives
    let mut has_yaml_directive = false; // Track if we've seen a %YAML directive

    let mut pos: usize = 0;
    let chars: Vec<char> = input.chars().collect();

    while pos < chars.len() {
        // Check for document markers or directives at line start
        let remaining = &input[pos..];

        // Check for `---` document start marker
        if remaining.starts_with("---") {
            let marker_end = pos + 3;
            let next_char = chars.get(marker_end);
            if next_char.is_none() || matches!(next_char, Some(' ' | '\t' | '\n' | '\r')) {
                // This is a document start marker
                // If we have content, start a new document
                // Note: After `...`, if we only have directives (no content), those directives
                // belong to this new document, so don't finalize in that case.
                if !current_content.is_empty() {
                    // Finalize current document
                    documents.push(RawDocument {
                        directives: std::mem::take(&mut current_directives),
                        content: std::mem::take(&mut current_content),
                        content_span: Span::new((), content_start..pos),
                    });
                    has_yaml_directive = false;
                }

                // No longer in directive prologue after seeing ---
                in_directive_prologue = false;

                // Include the `---` marker in the content (parser will see it)
                if current_content.is_empty() {
                    content_start = pos;
                }
                // Add `---` and the rest of the line
                let line_end = find_eol(input, pos);
                current_content.push_str(&input[pos..line_end]);

                // Include the newline
                pos = line_end;
                if pos < input.len() {
                    let newline_end = skip_newline(input, pos);
                    current_content.push_str(&input[pos..newline_end]);
                    pos = newline_end;
                }
                continue;
            }
        }

        // Check for `...` document end marker
        if remaining.starts_with("...") {
            let marker_end = pos + 3;
            let next_char = chars.get(marker_end);
            if next_char.is_none() || matches!(next_char, Some(' ' | '\t' | '\n' | '\r')) {
                // This is a document end marker
                // Check for directives without a document start before ...
                if !current_directives.is_empty() && current_content.is_empty() {
                    let span = current_directives
                        .first()
                        .map_or(Span::new((), pos..marker_end), |(_, span)| *span);
                    errors.push(ParseError {
                        kind: ErrorKind::UnexpectedToken,
                        span,
                        expected: Vec::new(),
                        found: None,
                    });
                }

                // Include `...` in content (parser will see it)
                if current_content.is_empty() {
                    content_start = pos;
                }
                let line_end = find_eol(input, pos);
                current_content.push_str(&input[pos..line_end]);

                // Include the newline
                pos = line_end;
                if pos < input.len() {
                    let newline_end = skip_newline(input, pos);
                    current_content.push_str(&input[pos..newline_end]);
                    pos = newline_end;
                }

                // Finalize the current document BEFORE starting the directive prologue for the next
                // This ensures directives after `...` go to the next document, not the current one.
                if !current_content.is_empty() || !current_directives.is_empty() {
                    documents.push(RawDocument {
                        directives: std::mem::take(&mut current_directives),
                        content: std::mem::take(&mut current_content),
                        content_span: Span::new((), content_start..pos),
                    });
                }

                // After `...`, we're back in directive prologue for the NEXT document
                in_directive_prologue = true;
                has_yaml_directive = false;
                continue;
            }
        }

        // Check for directives (only in directive prologue)
        if in_directive_prologue && remaining.starts_with('%') {
            let directive_start = pos;
            let line_end = find_eol(input, pos);
            let line = &input[pos..line_end];
            let directive_span = Span::new((), directive_start..line_end);

            let directive = if let Some(stripped) = line.strip_prefix("%YAML") {
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

                // Validate %YAML directive format
                let after_yaml = stripped.trim();
                let version_and_rest = after_yaml.split('#').next().unwrap().trim();
                let parts: Vec<&str> = version_and_rest.split_whitespace().collect();
                let is_valid_version = parts.len() == 1
                    && parts
                        .first()
                        .unwrap()
                        .chars()
                        .all(|ch| ch.is_ascii_digit() || ch == '.');
                if !is_valid_version {
                    errors.push(ParseError {
                        kind: ErrorKind::InvalidDirective,
                        span: directive_span,
                        expected: Vec::new(),
                        found: None,
                    });
                }
                Some(Directive::Yaml(version_and_rest.to_owned()))
            } else if let Some(stripped) = line.strip_prefix("%TAG") {
                Some(Directive::Tag(stripped.trim().to_owned()))
            } else {
                Some(Directive::Reserved(line[1..].trim().to_owned()))
            };

            if let Some(di) = directive {
                current_directives.push((di, directive_span));
            }

            pos = skip_to_eol(input, line_end);
            continue;
        }

        // Skip comment-only or empty lines in directive prologue
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
        if !in_directive_prologue
            && remaining.starts_with('%')
            && (remaining.starts_with("%YAML") || remaining.starts_with("%TAG"))
        {
            let line_end = find_eol(input, pos);
            // Look ahead to see if there's a --- on a subsequent line
            let mut lookahead_pos = line_end;
            if lookahead_pos < input.len() {
                lookahead_pos = skip_newline(input, lookahead_pos);
            }
            let mut found_doc_start = false;
            while lookahead_pos < input.len() {
                let lookahead_remaining = &input[lookahead_pos..];
                let first_char = lookahead_remaining.chars().next();
                if first_char == Some('#') || lookahead_remaining.trim_start().is_empty() {
                    let la_line_end = find_eol(input, lookahead_pos);
                    lookahead_pos = la_line_end;
                    if lookahead_pos < input.len() {
                        lookahead_pos = skip_newline(input, lookahead_pos);
                    }
                    continue;
                }
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
    if !current_content.is_empty() || !current_directives.is_empty() {
        // Check for directives without a following document
        if !current_directives.is_empty() && current_content.is_empty() {
            let span = current_directives
                .first()
                .map_or(Span::new((), 0..0), |(_, span)| *span);
            errors.push(ParseError {
                kind: ErrorKind::UnexpectedToken,
                span,
                expected: Vec::new(),
                found: None,
            });
        }

        documents.push(RawDocument {
            directives: current_directives,
            content: current_content,
            content_span: Span::new((), content_start..input.len()),
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
        // No markers in content
        let content = &docs.first().unwrap().content;
        assert!(!content.contains("---"));
        assert!(!content.contains("..."));
        assert_eq!(content, "key: value\n");
    }

    #[test]
    fn test_explicit_document_start() {
        let input = "---\nkey: value\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        // Content now includes the --- marker
        assert!(docs.first().unwrap().content.starts_with("---"));
        assert!(docs.first().unwrap().content.contains("key: value"));
    }

    #[test]
    fn test_multiple_documents() {
        let input = "---\ndoc1\n---\ndoc2\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 2);
        // Each document starts with ---
        assert!(docs.first().unwrap().content.starts_with("---"));
        assert!(docs.first().unwrap().content.contains("doc1"));
        assert!(docs.last().unwrap().content.starts_with("---"));
        assert!(docs.last().unwrap().content.contains("doc2"));
    }

    #[test]
    fn test_document_end_marker() {
        let input = "doc1\n...\n%YAML 1.2\n---\ndoc2\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 2);
        // First doc includes ...
        assert!(docs.first().unwrap().content.contains("..."));
        assert_eq!(docs.last().unwrap().directives.len(), 1);
        // Second doc starts with ---
        assert!(docs.last().unwrap().content.starts_with("---"));
    }

    #[test]
    fn test_directives_at_start() {
        let input = "%YAML 1.2\n%TAG !e! tag:example.com,2000:\n---\nkey: value\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        let doc = docs.first().unwrap();
        assert_eq!(doc.directives.len(), 2);
        assert!(matches!(&doc.directives.first().unwrap().0, Directive::Yaml(v) if v == "1.2"));
        assert!(matches!(
            &doc.directives.last().unwrap().0,
            Directive::Tag(_)
        ));
    }

    #[test]
    fn test_percent_inside_document() {
        // % inside document content should NOT be treated as directive
        let input = "{ matches\n% : 20 }\n";
        let (docs, _errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        assert!(docs.first().unwrap().directives.is_empty());
        // The % line should be part of content, not a directive
        assert!(docs.first().unwrap().content.contains('%'));
    }

    #[test]
    fn test_empty_documents() {
        let input = "---\n---\n";
        let (docs, _errors) = parse_stream(input);
        // Each document includes its --- marker
        assert_eq!(docs.len(), 2);
        // First doc: "---\n" (only the marker)
        assert!(docs.first().unwrap().content.starts_with("---"));
        // Second doc: "---\n" (only the marker)
        assert!(docs.last().unwrap().content.starts_with("---"));
    }

    #[test]
    fn test_duplicate_yaml_directive() {
        let input = "%YAML 1.2\n%YAML 1.2\n---\nvalue\n";
        let (docs, errors) = parse_stream(input);
        assert_eq!(docs.len(), 1);
        // Should have exactly one error for duplicate directive
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            errors.first().unwrap().kind,
            ErrorKind::DuplicateDirective
        ));
    }
}
