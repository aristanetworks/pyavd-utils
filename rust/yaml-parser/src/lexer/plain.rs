// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use super::{LexMode, Lexer, PlainScalarMeta, PlainScalarToken, Token, is_anchor_char};
use crate::error::ErrorKind;
use crate::span::{Span, Spanned};

impl<'input> Lexer<'input> {
    fn plain_continuation_start_from(&self, mut pos: usize) -> bool {
        while matches!(self.input.as_bytes().get(pos), Some(b' ' | b'\t')) {
            pos += 1;
        }

        let Some((newline_ch, newline_len)) = self.char_at(pos) else {
            return false;
        };
        if !Self::is_newline(newline_ch) {
            return false;
        }
        pos += newline_len;

        // Treat CRLF as a single line break.
        if newline_ch == '\r' && self.input.as_bytes().get(pos) == Some(&b'\n') {
            pos += 1;
        }

        while self.input.as_bytes().get(pos) == Some(&b' ') {
            pos += 1;
        }

        self.plain_scalar_starts_at(pos)
    }

    fn plain_scalar_starts_at(&self, pos: usize) -> bool {
        let Some((ch, _)) = self.char_at(pos) else {
            return false;
        };

        if Self::is_newline(ch)
            || ch == '#'
            || ch == '\''
            || ch == '"'
            || ch == '|'
            || ch == '>'
            || (self.mode() == LexMode::Flow && Self::is_flow_indicator(ch))
        {
            return false;
        }

        if matches!(ch, '&' | '*')
            && self
                .char_at(pos + ch.len_utf8())
                .is_some_and(|(next, _)| is_anchor_char(next))
        {
            return false;
        }

        if ch == '!' {
            return false;
        }

        if matches!(ch, '-' | '?' | ':') {
            let next_ch = self.char_at(pos + ch.len_utf8()).map(|(next, _)| next);
            return next_ch.is_some_and(|following_ch| {
                !(following_ch.is_whitespace()
                    || Self::is_newline(following_ch)
                    || (self.mode() == LexMode::Flow && Self::is_flow_indicator(following_ch)))
            });
        }

        true
    }

    fn char_at(&self, pos: usize) -> Option<(char, usize)> {
        self.input
            .get(pos..)?
            .chars()
            .next()
            .map(|ch| (ch, ch.len_utf8()))
    }

    /// Consume a plain scalar, respecting the current mode.
    ///
    /// Optimized for zero-copy: borrows directly from the input string and
    /// trims trailing whitespace by adjusting the end position rather than
    /// allocating a new string.
    pub(super) fn consume_plain_scalar(&mut self, start: usize) -> Spanned<Token<'input>> {
        let content_start = self.byte_pos;
        let mut at_start = true;
        // Track the end of non-whitespace content for trailing whitespace trimming
        let mut content_end_non_ws = content_start;
        let mut meta = PlainScalarMeta::default();

        while let Some(ch) = self.peek() {
            // Always stop at newlines
            if Self::is_newline(ch) {
                break;
            }

            // Handle special indicators at start: -, ?, :
            // These can only start a plain scalar if followed by a "safe" character
            // In flow context, flow indicators are not safe
            if at_start && (ch == '-' || ch == '?' || ch == ':') {
                let next = self.peek_char_after_current_ascii();
                // Check if next is "safe" for plain scalar start
                let is_safe = match next {
                    None => false, // EOF not safe
                    Some(n) => {
                        !(n.is_whitespace()
                            || Self::is_newline(n)
                            || (self.mode() == LexMode::Flow && Self::is_flow_indicator(n)))
                    }
                };
                if !is_safe {
                    // Cannot start plain scalar with this character - emit error and skip it
                    let span = self.current_span(start);
                    // In flow mode, block indicators like `-` are invalid
                    let error_kind = if self.mode() == LexMode::Flow {
                        ErrorKind::BlockIndicatorInFlow
                    } else {
                        ErrorKind::InvalidCharacter
                    };
                    self.add_error(error_kind, span);
                    self.advance(); // Consume the invalid character to avoid infinite loop
                    return (
                        Token::Plain(PlainScalarToken::new(
                            Cow::Borrowed(""),
                            PlainScalarMeta::default(),
                        )),
                        self.current_span(start),
                    );
                }
            }

            // Handle colon - behavior differs between block and flow mode
            if ch == ':' {
                if self.mode() == LexMode::Flow {
                    // In flow mode:
                    // - At the start of plain scalar, : can be included if followed by
                    //   non-whitespace and non-flow-indicator (e.g., :x, ://, etc.)
                    // - In the middle, : terminates if followed by whitespace/flow-indicator/EOF
                    let next = self.peek_char_after_current_ascii();
                    let colon_terminates = next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                        || next.is_some_and(Self::is_flow_indicator);

                    if colon_terminates {
                        // `:` acts as an indicator here, so the plain scalar stops
                        meta.terminated_by_colon = true;
                        break;
                    }
                    // Otherwise, : is followed by non-terminator, consume it
                } else {
                    // In block mode, only stop if followed by whitespace/newline/EOF
                    if let Some(next) = self.peek_char_after_current_ascii() {
                        if next == ' ' || next == '\t' || Self::is_newline(next) {
                            meta.terminated_by_colon = true;
                            break;
                        }
                    } else {
                        // : at EOF
                        meta.terminated_by_colon = true;
                        break;
                    }
                }
            }

            // Space followed by # is a comment start
            if (ch == ' ' || ch == '\t') && self.peek_ascii_byte(1) == Some(b'#') {
                meta.terminated_by_comment = true;
                break;
            }

            // Flow indicators - depends on mode
            if Self::is_flow_indicator(ch) && self.mode() == LexMode::Flow {
                break;
            }
            // In block mode, flow indicators are part of plain scalar

            self.advance();
            at_start = false;

            // Track end of non-whitespace for trailing whitespace trimming
            if !ch.is_whitespace() {
                content_end_non_ws = self.byte_pos;
            }
        }

        // Zero-copy: borrow directly from input, trimmed at content_end_non_ws
        // byte_pos is always at UTF-8 boundaries (maintained by advance())
        let content = self
            .input
            .get(content_start..content_end_non_ws)
            .unwrap_or("");
        if self.mode() == LexMode::Flow {
            meta.may_continue_on_next_line_in_flow =
                self.plain_continuation_start_from(self.byte_pos);
        }
        // Use content_end_non_ws for span end to match the trimmed content
        // (don't include trailing whitespace that was consumed but not part of the value)
        (
            Token::Plain(PlainScalarToken::new(Cow::Borrowed(content), meta)),
            Span::from_usize_range(start..content_end_non_ws),
        )
    }
}
