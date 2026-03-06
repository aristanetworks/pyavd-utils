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

use crate::error::{ErrorKind, ParseError};
use crate::span::{Span, Spanned};

/// A directive found in the YAML stream.
///
/// The lifetime `'input` refers to the input string being parsed.
/// Directive content is borrowed from the original input for zero-copy parsing.
#[derive(Debug, Clone, PartialEq)]
pub enum Directive<'input> {
    /// %YAML version (e.g., "1.2")
    Yaml(&'input str),
    /// %TAG handle prefix (e.g., "!e! tag:example.com,2000:")
    Tag(&'input str),
    /// Reserved directive (%NAME content)
    Reserved(&'input str),
}

/// A raw document extracted from the stream.
///
/// The content includes document markers (`---`, `...`) so the parser can see them.
/// Only directives (`%YAML`, `%TAG`) are extracted separately.
///
/// The lifetime `'input` refers to the input string being parsed. The content
/// is a slice of the original input for zero-copy parsing.
#[derive(Debug, Clone)]
pub struct RawDocument<'input> {
    /// Directives before this document (in the directive prologue).
    pub directives: Vec<Spanned<Directive<'input>>>,
    /// The raw content of the document, including `---` and `...` markers.
    /// The parser is responsible for interpreting these tokens.
    /// This is a slice of the original input (zero-copy).
    pub content: &'input str,
    /// The span of the content in the original input.
    pub content_span: Span,
}

/// Stream lexer for YAML.
///
/// This is Layer 1 of the layered lexer architecture. It splits a YAML stream
/// into raw documents based on document markers (`---`, `...`) and extracts
/// directives from the directive prologue.
///
/// The lifetime `'input` refers to the input string being parsed.
pub struct StreamLexer<'input> {
    input: &'input str,
    /// Current byte position in the input string.
    pos: usize,
    documents: Vec<RawDocument<'input>>,
    errors: Vec<ParseError>,
    current_directives: Vec<Spanned<Directive<'input>>>,
    content_start: Option<usize>,
    content_end: usize,
    in_directive_prologue: bool,
    has_yaml_directive: bool,
    flow_depth: usize,
}

impl<'input> StreamLexer<'input> {
    /// Create a new stream lexer for the given input.
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            pos: 0,
            documents: Vec::new(),
            errors: Vec::new(),
            current_directives: Vec::new(),
            content_start: None,
            content_end: 0,
            in_directive_prologue: true,
            has_yaml_directive: false,
            flow_depth: 0,
        }
    }

    /// Parse a directive line and return the new position.
    #[allow(
        clippy::string_slice,
        reason = "All positions are calculated from byte-level scanning and guaranteed to be on UTF-8 boundaries"
    )]
    fn parse_directive(&mut self, line: &'input str, line_end: usize) -> usize {
        let directive_start = self.pos;
        let directive_span = Span::from_usize_range(directive_start..line_end);

        let directive = if let Some(stripped) = line.strip_prefix("%YAML") {
            // %YAML must be followed by whitespace, not more characters
            let first_char = stripped.chars().next();
            if matches!(first_char, Some(' ' | '\t') | None) {
                if self.has_yaml_directive {
                    self.errors.push(ParseError::new(
                        ErrorKind::DuplicateDirective,
                        directive_span,
                    ));
                }
                self.has_yaml_directive = true;

                let after_yaml = stripped.trim();
                let chars_vec: Vec<char> = after_yaml.chars().collect();
                let has_valid_comment_separator =
                    chars_vec.windows(2).any(|w| matches!(w, [' ' | '\t', '#']));
                let has_invalid_comment = after_yaml.contains('#') && !has_valid_comment_separator;

                let version_part = if let Some(hash_pos) = after_yaml.find('#') {
                    &after_yaml[..hash_pos]
                } else {
                    after_yaml
                };
                let version = version_part.trim();

                let is_valid_version = !version.is_empty()
                    && !version.contains(' ')
                    && !version.contains('\t')
                    && version.chars().all(|ch| ch.is_ascii_digit() || ch == '.');

                if !is_valid_version || has_invalid_comment {
                    self.errors
                        .push(ParseError::new(ErrorKind::InvalidDirective, directive_span));
                }
                Some(Directive::Yaml(version))
            } else {
                Some(Directive::Reserved(line[1..].trim()))
            }
        } else if let Some(stripped) = line.strip_prefix("%TAG") {
            let first_char = stripped.chars().next();
            if matches!(first_char, Some(' ' | '\t') | None) {
                Some(Directive::Tag(stripped.trim()))
            } else {
                Some(Directive::Reserved(line[1..].trim()))
            }
        } else {
            Some(Directive::Reserved(line[1..].trim()))
        };

        if let Some(di) = directive {
            self.current_directives.push((di, directive_span));
        }

        self.skip_to_eol(line_end)
    }

    /// Handle `---` document start marker. Returns `true` if marker was handled.
    #[allow(
        clippy::string_slice,
        reason = "All positions are calculated from byte-level scanning and guaranteed to be on UTF-8 boundaries"
    )]
    fn handle_document_start(&mut self) -> bool {
        let remaining = &self.input[self.pos..];
        if self.flow_depth != 0 || !remaining.starts_with("---") {
            return false;
        }

        let marker_end = self.pos + 3;
        // Get the character at byte position `marker_end` from the input string
        let next_char = self
            .input
            .get(marker_end..)
            .and_then(|rest| rest.chars().next());
        if next_char.is_some() && !matches!(next_char, Some(' ' | '\t' | '\n' | '\r')) {
            return false;
        }

        // Finalize current document if we have content
        if let Some(start) = self.content_start {
            self.documents.push(RawDocument {
                directives: std::mem::take(&mut self.current_directives),
                content: &self.input[start..self.content_end],
                content_span: Span::from_usize_range(start..self.pos),
            });
            self.has_yaml_directive = false;
            self.content_start = None;
        }

        // No longer in directive prologue after seeing ---
        self.in_directive_prologue = false;

        // Include the `---` marker in the content
        if self.content_start.is_none() {
            self.content_start = Some(self.pos);
        }
        let line_end = self.find_eol();
        (self.pos, self.content_end) = self.advance_past_newline(line_end);
        true
    }

    /// Handle `...` document end marker. Returns `true` if marker was handled.
    #[allow(
        clippy::string_slice,
        reason = "All positions are calculated from byte-level scanning and guaranteed to be on UTF-8 boundaries"
    )]
    fn handle_document_end(&mut self) -> bool {
        let remaining = &self.input[self.pos..];
        if self.flow_depth != 0 || !remaining.starts_with("...") {
            return false;
        }

        let marker_end = self.pos + 3;
        // Get the character at byte position `marker_end` from the input string
        let next_char = self
            .input
            .get(marker_end..)
            .and_then(|rest| rest.chars().next());
        if next_char.is_some() && !matches!(next_char, Some(' ' | '\t' | '\n' | '\r')) {
            return false;
        }

        // Check for directives without a document start before ...
        if !self.current_directives.is_empty() && self.content_start.is_none() {
            let span = self
                .current_directives
                .first()
                .map_or(Span::from_usize_range(self.pos..marker_end), |(_, span)| {
                    *span
                });
            self.errors
                .push(ParseError::new(ErrorKind::TrailingContent, span));
        }

        // Track if there was content before the `...` marker
        let had_content = self.content_start.is_some();

        // Advance past the `...` line
        let line_end = self.find_eol();
        let doc_end_pos = self.pos; // Position of `...`
        (self.pos, _) = self.advance_past_newline(line_end);

        // Only include `...` in content_end if there was preceding content
        if had_content {
            self.content_end = self.pos;
        }

        // Finalize the current document only if there was actual content
        // (directives alone without content/doc-start don't create a document)
        if had_content {
            let start = self.content_start.unwrap_or(doc_end_pos);
            self.documents.push(RawDocument {
                directives: std::mem::take(&mut self.current_directives),
                content: &self.input[start..self.content_end],
                content_span: Span::from_usize_range(start..self.pos),
            });
            self.content_start = None;
        } else {
            // Clear directives - they had no document to attach to
            self.current_directives.clear();
        }

        // After `...`, we're back in directive prologue for the NEXT document
        self.in_directive_prologue = true;
        self.has_yaml_directive = false;
        true
    }

    /// Find the end of the current line (position of newline or EOF).
    #[allow(
        clippy::indexing_slicing,
        reason = "Index is validated by bounds check in loop condition"
    )]
    fn find_eol(&self) -> usize {
        let bytes = self.input.as_bytes();
        let mut idx = self.pos;
        while idx < bytes.len() {
            match bytes[idx] {
                b'\n' | b'\r' => return idx,
                _ => idx += 1,
            }
        }
        idx
    }

    /// Skip to the position after the current line (after newline).
    fn skip_to_eol(&self, pos: usize) -> usize {
        let eol = find_eol_at(self.input, pos);
        skip_newline(self.input, eol)
    }

    /// Advance past a newline (if present) and return the new position and `content_end`.
    fn advance_past_newline(&self, line_end: usize) -> (usize, usize) {
        if line_end < self.input.len() {
            let newline_end = skip_newline(self.input, line_end);
            (newline_end, newline_end)
        } else {
            (line_end, line_end)
        }
    }

    /// Tokenize the stream into raw documents.
    ///
    /// This is the main entry point for stream lexing. It:
    /// - Extracts directives from the directive prologue (they don't go to parser)
    /// - Includes document markers (`---`, `...`) in content (parser sees them)
    /// - Splits stream at document boundaries
    ///
    /// Returns a tuple of (documents, errors).
    #[allow(
        clippy::string_slice,
        reason = "All positions are calculated from byte-level scanning and guaranteed to be on UTF-8 boundaries"
    )]
    pub fn tokenize(mut self) -> (Vec<RawDocument<'input>>, Vec<ParseError>) {
        // Use input.len() (byte count), not chars.len() (character count)
        // since self.pos is a byte position, not a character index
        while self.pos < self.input.len() {
            // Check for `---` document start marker
            if self.handle_document_start() {
                continue;
            }

            // Check for `...` document end marker
            if self.handle_document_end() {
                continue;
            }

            // Check for directives (only in directive prologue)
            let remaining = &self.input[self.pos..];
            if self.in_directive_prologue && remaining.starts_with('%') {
                let line_end = find_eol_at(self.input, self.pos);
                let line = &self.input[self.pos..line_end];
                self.pos = self.parse_directive(line, line_end);
                continue;
            }

            // Skip comment-only or empty lines in directive prologue
            // Important: check the CURRENT LINE only, not all remaining content
            // (trim_start() would trim across newlines and incorrectly match multi-line content)
            if self.in_directive_prologue {
                let line_end = find_eol_at(self.input, self.pos);
                #[allow(
                    clippy::string_slice,
                    reason = "positions from byte scanning are UTF-8 boundaries"
                )]
                let current_line = &self.input[self.pos..line_end];
                let trimmed_line = current_line.trim();
                if current_line.starts_with('#')
                    || trimmed_line.starts_with('#')
                    || trimmed_line.is_empty()
                {
                    self.pos = line_end;
                    if self.pos < self.input.len() {
                        self.pos = skip_newline(self.input, self.pos);
                    }
                    continue;
                }
            }

            // Check for directive-like content outside the directive prologue
            if !self.in_directive_prologue
                && remaining.starts_with('%')
                && (remaining.starts_with("%YAML") || remaining.starts_with("%TAG"))
            {
                let line_end = find_eol_at(self.input, self.pos);
                // Look ahead to see if there's a --- on a subsequent line
                let mut lookahead_pos = line_end;
                if lookahead_pos < self.input.len() {
                    lookahead_pos = skip_newline(self.input, lookahead_pos);
                }
                let mut found_doc_start = false;
                while lookahead_pos < self.input.len() {
                    let lookahead_remaining = &self.input[lookahead_pos..];
                    let first_char = lookahead_remaining.chars().next();
                    if first_char == Some('#') || lookahead_remaining.trim_start().is_empty() {
                        let la_line_end = find_eol_at(self.input, lookahead_pos);
                        lookahead_pos = la_line_end;
                        if lookahead_pos < self.input.len() {
                            lookahead_pos = skip_newline(self.input, lookahead_pos);
                        }
                        continue;
                    }
                    if lookahead_remaining.starts_with("---") {
                        let marker_end = lookahead_pos + 3;
                        let next_char = self.input[marker_end..].chars().next();
                        if next_char.is_none()
                            || matches!(next_char, Some(' ' | '\t' | '\n' | '\r'))
                        {
                            found_doc_start = true;
                        }
                    }
                    break;
                }
                if found_doc_start {
                    self.errors.push(ParseError::new(
                        ErrorKind::TrailingContent,
                        Span::from_usize_range(self.pos..line_end),
                    ));
                }
            }

            // Regular content line - we're no longer in directive prologue
            self.in_directive_prologue = false;

            // Add the line to current content
            let line_end = find_eol_at(self.input, self.pos);
            if self.content_start.is_none() {
                self.content_start = Some(self.pos);
            }

            // Track flow depth by scanning for flow indicators in this line
            // This is needed to avoid treating --- or ... inside flow collections as document markers
            for byte_idx in self.pos..line_end {
                if let Some(&byte) = self.input.as_bytes().get(byte_idx) {
                    match byte {
                        b'[' | b'{' => self.flow_depth += 1,
                        b']' | b'}' => self.flow_depth = self.flow_depth.saturating_sub(1),
                        // Note: We don't handle quoted strings here, which could contain
                        // unmatched brackets. This is a simplified heuristic that works
                        // for valid YAML and for detecting flow context in error cases.
                        _ => {}
                    }
                }
            }

            (self.pos, self.content_end) = self.advance_past_newline(line_end);
        }

        // Finalize any remaining document
        if self.content_start.is_some() || !self.current_directives.is_empty() {
            // Check for directives without a following document
            if !self.current_directives.is_empty() && self.content_start.is_none() {
                let span = self
                    .current_directives
                    .first()
                    .map_or(Span::from_usize_range(0..0), |(_, span)| *span);
                self.errors
                    .push(ParseError::new(ErrorKind::TrailingContent, span));
            }

            // If no content was found, use content_end for both start and end to get an empty slice
            let start = self.content_start.unwrap_or(self.content_end);
            self.documents.push(RawDocument {
                directives: self.current_directives,
                content: &self.input[start..self.content_end],
                content_span: Span::from_usize_range(start..self.content_end),
            });
        }

        (self.documents, self.errors)
    }
}

/// Parse the YAML stream into raw documents.
///
/// This is a convenience function that creates a `StreamLexer` and calls `tokenize()`.
/// For more control, use `StreamLexer::new(input).tokenize()` directly.
///
/// Returns a tuple of (documents, errors).
pub fn tokenize_stream(input: &str) -> (Vec<RawDocument<'_>>, Vec<ParseError>) {
    StreamLexer::new(input).tokenize()
}

/// Find the end of the current line (position of newline or EOF).
#[allow(
    clippy::indexing_slicing,
    reason = "Index is validated by bounds check in loop condition"
)]
fn find_eol_at(input: &str, pos: usize) -> usize {
    let bytes = input.as_bytes();
    let mut idx = pos;
    while idx < bytes.len() {
        match bytes[idx] {
            b'\n' | b'\r' => return idx,
            _ => idx += 1,
        }
    }
    idx
}

/// Skip a newline character (handles \r\n, \r, \n).
#[allow(
    clippy::indexing_slicing,
    reason = "Index is validated by bounds check before access"
)]
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
        let (docs, _errors) = tokenize_stream(input);
        assert_eq!(docs.len(), 1);
        // No markers in content
        let content = docs.first().unwrap().content;
        assert!(!content.contains("---"));
        assert!(!content.contains("..."));
        assert_eq!(content, "key: value\n");
    }

    #[test]
    fn test_explicit_document_start() {
        let input = "---\nkey: value\n";
        let (docs, _errors) = tokenize_stream(input);
        assert_eq!(docs.len(), 1);
        // Content now includes the --- marker
        assert!(docs.first().unwrap().content.starts_with("---"));
        assert!(docs.first().unwrap().content.contains("key: value"));
    }

    #[test]
    fn test_multiple_documents() {
        let input = "---\ndoc1\n---\ndoc2\n";
        let (docs, _errors) = tokenize_stream(input);
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
        let (docs, _errors) = tokenize_stream(input);
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
        let (docs, _errors) = tokenize_stream(input);
        assert_eq!(docs.len(), 1);
        let doc = docs.first().unwrap();
        assert_eq!(doc.directives.len(), 2);
        assert!(
            matches!(&doc.directives.first().unwrap().0, Directive::Yaml(ver) if *ver == "1.2")
        );
        assert!(matches!(
            &doc.directives.last().unwrap().0,
            Directive::Tag(_)
        ));
    }

    #[test]
    fn test_percent_inside_document() {
        // % inside document content should NOT be treated as directive
        let input = "{ matches\n% : 20 }\n";
        let (docs, _errors) = tokenize_stream(input);
        assert_eq!(docs.len(), 1);
        assert!(docs.first().unwrap().directives.is_empty());
        // The % line should be part of content, not a directive
        assert!(docs.first().unwrap().content.contains('%'));
    }

    #[test]
    fn test_empty_documents() {
        let input = "---\n---\n";
        let (docs, _errors) = tokenize_stream(input);
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
        let (docs, errors) = tokenize_stream(input);
        assert_eq!(docs.len(), 1);
        // Should have exactly one error for duplicate directive
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            errors.first().unwrap().kind,
            ErrorKind::DuplicateDirective
        ));
    }
}
