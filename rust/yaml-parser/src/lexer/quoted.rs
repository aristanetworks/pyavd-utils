// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use super::Lexer;
use super::QuoteStyle;
use super::RichToken;
use super::Token;
use crate::error::ErrorKind;
use crate::span::Span;
use crate::span::Spanned;

impl<'input> Lexer<'input> {
    /// Check if the current position has a forbidden document marker (`---` or `...`)
    /// at column 0 followed by whitespace/newline/EOF (YAML spec production c-forbidden [206]).
    /// Returns true if a forbidden marker was detected, and reports an error.
    fn check_forbidden_marker_in_quoted(&mut self) -> bool {
        // Only check at column 0 (indent 0, no leading spaces consumed yet)
        // The caller should check that we're at column 0 before calling this.

        if !self.matches_ascii_document_marker(*b"---")
            && !self.matches_ascii_document_marker(*b"...")
        {
            return false;
        }

        // Report the error - this is a forbidden marker inside a quoted string
        let span = Span::from_usize_range(self.byte_pos..self.byte_pos + 3);
        self.add_error(ErrorKind::DocumentMarkerInScalar, span);
        true
    }

    /// Shared tail logic for handling a newline in quoted strings.
    ///
    /// Assumes any per-style content handling (such as trimming) has already
    /// been applied and the current `content` flushed if needed.
    ///
    /// Consumes the newline and indentation, emits a `LineStart` token, and
    /// returns the new `content_start` position.
    fn finish_quoted_newline(&mut self) -> usize {
        // Consume newline
        let newline_start = self.byte_pos;
        let ch = self.advance().unwrap();
        if ch == '\r' && self.peek() == Some('\n') {
            self.advance();
        }

        // Count indentation (spaces only per YAML spec s-indent)
        let mut indent = 0;
        while self.peek() == Some(' ') {
            self.advance();
            indent += 1;
        }

        // In flow folding, skip the entire "s-separate-in-line" prefix.
        // This is "s-white+" which includes BOTH spaces and tabs (interleaved).
        // Note: escaped tabs (\t) are processed before newline handling, so they
        // end up in the previous StringContent, not here.
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.advance();
        }

        // Check for forbidden document markers at column 0 (c-forbidden [206])
        if indent == 0 {
            self.check_forbidden_marker_in_quoted();
        }

        // Emit LineStart token
        let line_span = Span::from_usize_range(newline_start..self.byte_pos);
        self.pending_tokens
            .push_back(RichToken::new(Token::LineStart(indent), line_span));

        self.byte_pos
    }

    /// Handle a newline within a quoted string.
    ///
    /// Emits the current content as a `StringContent` token (if non-empty),
    /// then delegates to `finish_quoted_newline` to consume the newline and
    /// emit the corresponding `LineStart` token. Returns the new
    /// `content_start` position.
    fn handle_quoted_newline(&mut self, content: &mut String, content_start: usize) -> usize {
        // Emit current content before newline
        if !content.is_empty() {
            let content_span = Span::from_usize_range(content_start..self.byte_pos);
            // Quoted strings always use Cow::Owned (escape processing)
            self.pending_tokens.push_back(RichToken::new(
                Token::StringContent(Cow::Owned(std::mem::take(content))),
                content_span,
            ));
        }

        self.finish_quoted_newline()
    }

    /// Handle newline in double-quoted strings, trimming trailing whitespace while preserving escaped content.
    ///
    /// `protected_len` is the content length that should not be trimmed (includes escaped characters).
    /// Trailing whitespace beyond this length will be trimmed.
    fn handle_quoted_newline_trimmed(
        &mut self,
        content: &mut String,
        content_start: usize,
        protected_len: usize,
    ) -> usize {
        // Trim trailing whitespace, but only beyond the protected length.
        // The protected_len is always at a character boundary (set after push_str).
        #[allow(
            clippy::string_slice,
            reason = "protected_len is always at char boundary"
        )]
        if content.len() > protected_len {
            let trimmable = &content[protected_len..];
            let trimmed = trimmable.trim_end_matches([' ', '\t']);
            let new_len = protected_len + trimmed.len();
            content.truncate(new_len);
        }

        // Emit current content before newline
        if !content.is_empty() {
            let content_span = Span::from_usize_range(content_start..self.byte_pos);
            self.pending_tokens.push_back(RichToken::new(
                Token::StringContent(Cow::Owned(std::mem::take(content))),
                content_span,
            ));
        }

        self.finish_quoted_newline()
    }

    /// Emit final string tokens after the main loop.
    ///
    /// Emits the remaining content (if any) and the `StringEnd` token.
    /// Reports an error if the string was not terminated.
    fn finalize_quoted_string(
        &mut self,
        start: usize,
        content: String,
        content_start: usize,
        terminated: bool,
        style: QuoteStyle,
    ) {
        // Emit final content segment if any
        if !content.is_empty() {
            let content_span = Span::from_usize_range(content_start..self.byte_pos);
            // Quoted strings always use Cow::Owned (escape processing)
            self.pending_tokens.push_back(RichToken::new(
                Token::StringContent(Cow::Owned(content)),
                content_span,
            ));
        }

        // Emit StringEnd
        let end_span = Span::from_usize_range(self.byte_pos.saturating_sub(1)..self.byte_pos);
        if !terminated {
            let full_span = self.current_span(start);
            self.add_error(ErrorKind::UnterminatedString, full_span);
        }
        self.pending_tokens
            .push_back(RichToken::new(Token::StringEnd(style), end_span));
    }

    /// Consume a single-quoted string, emitting `StringStart`, `StringContent`, `LineStart` and `StringEnd` tokens.
    /// Pushes tokens to `pending_tokens` and returns the first token.
    fn consume_single_quoted(&mut self, start: usize) -> Spanned<Token<'input>> {
        let start_span = Span::from_usize_range(start..start + 1);
        self.advance(); // consume opening '

        let mut content = String::new();
        let mut content_start = self.byte_pos;
        let mut terminated = false;

        loop {
            match self.peek() {
                None => break, // Unterminated
                Some('\'') => {
                    self.advance();
                    // Check for escaped quote ''
                    if self.peek() == Some('\'') {
                        content.push('\'');
                        self.advance();
                    } else {
                        terminated = true;
                        break;
                    }
                }
                Some('\n' | '\r') => {
                    content_start = self.handle_quoted_newline(&mut content, content_start);
                }
                Some(ch) => {
                    content.push(ch);
                    self.advance();
                }
            }
        }

        self.finalize_quoted_string(
            start,
            content,
            content_start,
            terminated,
            QuoteStyle::Single,
        );

        // Return StringStart as the immediate token
        (Token::StringStart(QuoteStyle::Single), start_span)
    }

    /// Consume a double-quoted string, emitting `StringStart`, `StringContent`, `LineStart` and `StringEnd` tokens.
    /// Pushes tokens to `pending_tokens` and returns the first token.
    fn consume_double_quoted(&mut self, start: usize) -> Spanned<Token<'input>> {
        let start_span = Span::from_usize_range(start..start + 1);
        self.advance(); // consume opening "

        let mut content = String::new();
        let mut content_start = self.byte_pos;
        let mut terminated = false;
        // Track the content length after the last non-escape character.
        // When we see a newline, we trim trailing whitespace but only up to this position.
        // This preserves escaped whitespace like \t while trimming literal trailing whitespace.
        let mut escape_protected_len = 0usize;

        loop {
            match self.peek() {
                None => break,
                Some('"') => {
                    self.advance();
                    terminated = true;
                    break;
                }
                Some('\\') => {
                    let escape_start = self.byte_pos;
                    self.advance();
                    if let Some(escaped) = self.consume_escape_sequence(escape_start) {
                        content.push_str(&escaped);
                        // The entire content including this escape is protected
                        escape_protected_len = content.len();
                    }
                }
                Some('\n' | '\r') => {
                    content_start = self.handle_quoted_newline_trimmed(
                        &mut content,
                        content_start,
                        escape_protected_len,
                    );
                    escape_protected_len = 0;
                }
                Some(ch) => {
                    content.push(ch);
                    // Track protection: non-whitespace chars protect all content before them
                    if !ch.is_ascii_whitespace() {
                        escape_protected_len = content.len();
                    }
                    // Whitespace doesn't update protection - can be trimmed if at end
                    self.advance();
                }
            }
        }

        self.finalize_quoted_string(
            start,
            content,
            content_start,
            terminated,
            QuoteStyle::Double,
        );

        // Return StringStart as the immediate token
        (Token::StringStart(QuoteStyle::Double), start_span)
    }

    fn consume_escape_sequence(&mut self, start_byte_pos: usize) -> Option<String> {
        let ch = self.advance()?;
        let result = match ch {
            '0' => String::from("\0"),
            'a' => String::from("\x07"),
            'b' => String::from("\x08"),
            't' | '\t' => String::from("\t"),
            'n' => String::from("\n"),
            'v' => String::from("\x0B"),
            'f' => String::from("\x0C"),
            'r' => String::from("\r"),
            'e' => String::from("\x1B"),
            ' ' => String::from(" "),
            '"' => String::from("\""),
            '/' => String::from("/"),
            '\\' => String::from("\\"),
            'N' => String::from("\u{0085}"),
            '_' => String::from("\u{00A0}"),
            'L' => String::from("\u{2028}"),
            'P' => String::from("\u{2029}"),
            'x' | 'u' | 'U' => self.consume_hex_escape(start_byte_pos, ch),
            '\n' | '\r' => {
                // Line continuation - skip whitespace on next line
                while matches!(self.peek(), Some(' ' | '\t' | '\n' | '\r')) {
                    self.advance();
                }
                String::new()
            }
            _ => {
                // Invalid escape sequence - report error
                let span = Span::from_usize_range(start_byte_pos..self.byte_pos);
                self.add_error(ErrorKind::InvalidEscape(ch), span);
                // Still return the escaped char for error recovery
                ch.to_string()
            }
        };
        Some(result)
    }

    #[allow(
        clippy::string_slice,
        reason = "self.byte_pos is always at proper UTF-8 boundaries"
    )]
    fn consume_hex_escape(&mut self, start_byte_pos: usize, escape_kind: char) -> String {
        let digits = match escape_kind {
            'x' => 2,
            'u' => 4,
            _ => 8, // 'U' case
        };
        let mut value = 0u32;
        let mut consumed = 0u8;
        for _ in 0..digits {
            if let Some(peek_ch) = self.peek() {
                if peek_ch.is_ascii_hexdigit() {
                    let digit = peek_ch.to_digit(16).unwrap();
                    value = value * 16 + digit;
                    consumed += 1;
                    self.advance();
                } else {
                    if peek_ch == '"' || Self::is_newline(peek_ch) {
                        // For closing quotes we will emit length error.
                        break;
                    }
                    let invalid_start = self.byte_pos;
                    let invalid_end = invalid_start + peek_ch.len_utf8();
                    self.add_error(
                        ErrorKind::InvalidEscapeCharacter,
                        Span::from_usize_range(invalid_start..invalid_end),
                    );
                    return self.input[start_byte_pos..self.byte_pos].to_string();
                }
            } else {
                break;
            }
        }
        if consumed == digits
            && let Some(ch) = char::from_u32(value)
        {
            return ch.to_string();
        }

        let span = Span::from_usize_range(start_byte_pos..self.byte_pos);
        if consumed < digits {
            self.add_error(ErrorKind::InvalidEscapeLength { expected: digits }, span);
        } else {
            self.add_error(ErrorKind::InvalidUnicodeEscape, span);
        }
        self.input[start_byte_pos..self.byte_pos].to_string()
    }

    /// Try to lex a quoted scalar (`'...'` or `"..."`).
    pub(super) fn try_lex_quoted_scalar(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if ch == '\'' {
            return Some(self.consume_single_quoted(start));
        }
        if ch == '"' {
            return Some(self.consume_double_quoted(start));
        }
        None
    }
}
