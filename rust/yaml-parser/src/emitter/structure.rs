// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::error::ErrorKind;
use crate::lexer::{Token, TokenKind};
use crate::span::{IndentLevel, Span};

use super::Emitter;

impl Emitter<'_> {
    pub(super) fn is_implicit_key(&self) -> bool {
        match self.peek_kind() {
            Some(TokenKind::Plain) => self.current_plain_terminated_by_mapping_value_indicator(),
            _ => self.with_lookahead(200, |window| Self::scan_is_implicit_key(&window)),
        }
    }

    /// Recover from a scalar-like line at mapping indentation that looks like a
    /// key but is missing a colon.
    ///
    /// Recovery policy:
    /// - report `MissingColon` at the insertion point after the scalar
    /// - drop the malformed line instead of inventing a mapping pair
    /// - consume any more-indented continuation lines as invalid indentation so
    ///   they are not absorbed by the parent mapping
    pub(super) fn recover_missing_colon_in_mapping(&mut self, indent: IndentLevel) -> bool {
        let recovered = match self.peek() {
            Some((Token::Plain(_text), span)) => {
                self.error(ErrorKind::MissingColon, Span::at(span.end));
                self.skip_plain_scalar_tokens();
                true
            }
            Some((Token::StringStart(_), span)) => {
                let (_value, full_span) = self.parse_quoted_string_content();
                let scalar_span = full_span.unwrap_or(span);
                self.error(ErrorKind::MissingColon, Span::at(scalar_span.end));
                true
            }
            Some((Token::Alias(_name), span)) => {
                self.error(ErrorKind::MissingColon, Span::at(span.end));
                let _ = self.take_current();
                true
            }
            _ => false,
        };

        if !recovered {
            return false;
        }

        self.skip_to_line_end();
        self.skip_invalid_indented_recovery_lines(indent);
        true
    }

    /// Consume tokens until the next `LineStart`, document marker, or EOF.
    pub(super) fn skip_to_line_end(&mut self) {
        while let Some(kind) = self.peek_kind() {
            if matches!(
                kind,
                TokenKind::LineStart | TokenKind::DocStart | TokenKind::DocEnd
            ) {
                break;
            }
            let _ = self.take_current();
        }
    }

    /// Skip lines indented more deeply than `indent`, reporting
    /// `InvalidIndentation` when they contain content.
    pub(super) fn skip_invalid_indented_recovery_lines(&mut self, indent: IndentLevel) {
        while let Some((next_indent, line_span)) = self.peek_line_start() {
            if next_indent <= indent {
                break;
            }

            let has_content = !self.line_start_is_blank_from(1);
            let _ = self.take_current();
            self.current_indent = next_indent;
            self.last_line_start_span = line_span;

            if has_content {
                self.error(
                    ErrorKind::InvalidIndentation,
                    Self::indented_line_error_span(line_span, next_indent),
                );
            }
            self.skip_to_line_end();
        }
    }

    /// Check if an indentation level is valid (exists in the indent stack).
    ///
    /// Returns `true` if the indent is in the stack (matches an active block's level).
    /// Returns `false` if the indent falls between existing stack entries (orphan).
    ///
    /// NOTE: This does NOT return true for `indent > top` because that case needs
    /// to be handled differently. When starting a new nested block, we push BEFORE
    /// checking. When checking continuation content, `indent > top` is orphan.
    pub(super) fn is_valid_indent(&self, indent: IndentLevel) -> bool {
        // Only exact matches with stack entries are valid
        self.indent_stack.contains(&indent)
    }

    /// Push an indentation level onto the stack when entering a block structure.
    pub(super) fn push_indent(&mut self, indent: IndentLevel) {
        self.indent_stack.push(indent);
    }

    /// Pop an indentation level from the stack when exiting a block structure.
    pub(super) fn pop_indent(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    /// Report an `InvalidIndentation` error.
    /// Uses `last_line_start_span` so that spans are consistent across nested blocks.
    pub(super) fn report_invalid_indent(&mut self) {
        self.error(ErrorKind::InvalidIndentation, self.last_line_start_span);
    }

    /// Check if there's content at an orphan level, starting from peek offset `start_offset`.
    pub(super) fn has_content_at_orphan_level_from(&self, start_offset: usize) -> bool {
        // Check if the next token is content that would trigger an error
        if let Some(kind) = self.peek_kind_nth(start_offset) {
            matches!(
                kind,
                TokenKind::Plain
                    | TokenKind::StringStart
                    | TokenKind::MappingKey
                    | TokenKind::Colon
                    | TokenKind::Anchor
                    | TokenKind::Tag
            )
        } else {
            false
        }
    }

    /// Return true if the line beginning at the current `LineStart` offset is effectively blank.
    ///
    /// The caller provides the starting offset just after the `LineStart` token and this helper
    /// skips structural trivia until it finds either content or another line/document boundary.
    pub(super) fn line_start_is_blank_from(&self, start_offset: usize) -> bool {
        self.with_lookahead(start_offset + 8, |window| {
            Self::scan_line_start_is_blank_from(&window, start_offset)
        })
    }

    /// Skip tokens comprising a plain scalar (Plain + whitespace on same line).
    pub(super) fn skip_plain_scalar_tokens(&mut self) {
        while let Some(kind) = self.peek_kind() {
            match kind {
                TokenKind::Plain | TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => {
                    let _ = self.take_current();
                }
                _ => break,
            }
        }
    }
}
