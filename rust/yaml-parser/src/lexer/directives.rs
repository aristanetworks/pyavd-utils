// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use super::{LexMode, Lexer, LexerPhase, Token};
use crate::error::ErrorKind;
use crate::span::Spanned;

impl<'input> Lexer<'input> {
    /// Check if we're at column 0 (start of input or right after a newline).
    pub(super) fn is_at_column_zero(&self) -> bool {
        if self.byte_pos == 0 {
            return true;
        }
        // Check if previous byte was a newline
        let prev_byte_pos = self.byte_pos.saturating_sub(1);
        if let Some(ch) = self.input.as_bytes().get(prev_byte_pos) {
            *ch == b'\n' || *ch == b'\r'
        } else {
            false
        }
    }

    /// Try to lex a document marker (`---` or `...`) at column 0.
    pub(super) fn try_lex_document_marker(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if !self.is_at_column_zero() {
            return None;
        }

        // Check for `---` followed by whitespace/newline/EOF
        if ch == '-' && self.matches_ascii_document_marker(*b"---") {
            self.advance(); // -
            self.advance(); // -
            self.advance(); // -
            let span = self.current_span(start);
            // Document markers are invalid in flow context
            if self.mode() == LexMode::Flow {
                self.add_error(ErrorKind::DocumentMarkerInFlow, span);
            }
            return Some((Token::DocStart, span));
        }

        // Check for `...` followed by whitespace/newline/EOF
        if ch == '.' && self.matches_ascii_document_marker(*b"...") {
            self.advance(); // .
            self.advance(); // .
            self.advance(); // .
            let span = self.current_span(start);
            // Document markers are invalid in flow context
            if self.mode() == LexMode::Flow {
                self.add_error(ErrorKind::DocumentMarkerInFlow, span);
            }
            return Some((Token::DocEnd, span));
        }

        None
    }

    /// Try to lex a directive (`%YAML`, `%TAG`, or reserved).
    ///
    /// Directives start with `%` at column 0 and continue to end of line.
    /// Returns the appropriate directive token.
    #[allow(
        clippy::string_slice,
        reason = "All positions are from byte-level scanning at UTF-8 boundaries"
    )]
    pub(super) fn try_lex_directive(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        // Directives start with `%` at column 0, only in directive prologue
        if ch != '%'
            || !self.is_at_column_zero()
            || self.phase_state != LexerPhase::DirectivePrologue
        {
            return None;
        }

        // In flow context, `%` is not a directive starter
        if self.flow_depth > 0 {
            return None;
        }

        // Consume the `%`
        self.advance();

        // Read the directive name
        let name_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if peek_ch.is_whitespace() {
                break;
            }
            self.advance();
        }
        let name = &self.input[name_start..self.byte_pos];

        // Skip whitespace after directive name
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.advance();
        }

        // Read the directive value (rest of line, excluding comments)
        let value_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if Self::is_newline(peek_ch) {
                break;
            }
            // Stop at comment (but only if preceded by whitespace)
            if peek_ch == '#' && self.byte_pos > value_start {
                let prev_byte = self.input.as_bytes().get(self.byte_pos - 1);
                if matches!(prev_byte, Some(b' ' | b'\t')) {
                    break;
                }
            }
            self.advance();
        }
        let value = self.input[value_start..self.byte_pos].trim();
        let span = self.current_span(start);

        // Track that we have a directive in this prologue (for "directive without document" check)
        if self.first_directive_span.is_none() {
            self.first_directive_span = Some(span);
        }
        self.has_directive_in_prologue = true;

        // Determine directive type
        let token = match name {
            "YAML" => {
                // Check for duplicate YAML directive in same document
                if self.has_yaml_directive {
                    self.add_error(ErrorKind::DuplicateDirective, span);
                }
                self.has_yaml_directive = true;

                // Validate YAML version format
                let is_valid = !value.is_empty()
                    && !value.contains(' ')
                    && !value.contains('\t')
                    && value.chars().all(|vc| vc.is_ascii_digit() || vc == '.');
                if !is_valid {
                    self.add_error(ErrorKind::InvalidDirective, span);
                }
                Token::YamlDirective(Cow::Borrowed(value))
            }
            "TAG" => {
                // `%TAG` directive: expects exactly two whitespace-separated parameters:
                // a tag handle (e.g. `!e!`) and a tag prefix (e.g. `tag:example,2000:`).
                let mut parts = value.split_whitespace();
                let handle = parts.next();
                let prefix = parts.next();
                let extra = parts.next();

                if let (Some(handle_str), Some(prefix_str), None) = (handle, prefix, extra) {
                    Token::TagDirective(handle_str, prefix_str)
                } else {
                    // Malformed TAG directive (wrong number of parameters).
                    // Report an error and treat it as a reserved/unknown directive
                    // so that it doesn't affect tag handle resolution.
                    self.add_error(ErrorKind::InvalidDirective, span);
                    let full_content = &self.input[name_start..self.byte_pos].trim_end();
                    Token::ReservedDirective(Cow::Borrowed(full_content))
                }
            }
            _ => {
                // Reserved directive: include the name in the value
                let full_content = &self.input[name_start..self.byte_pos].trim_end();
                Token::ReservedDirective(Cow::Borrowed(full_content))
            }
        };

        Some((token, span))
    }
}
