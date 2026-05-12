// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::LexMode;
use super::Lexer;
use super::Token;
use crate::span::Spanned;

impl<'input> Lexer<'input> {
    /// Try to lex a flow indicator (`{}[],`).
    pub(super) fn try_lex_flow_indicator(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        match ch {
            '{' => {
                self.advance();
                Some((Token::FlowMapStart, self.current_span(start)))
            }
            '}' => {
                self.advance();
                Some((Token::FlowMapEnd, self.current_span(start)))
            }
            '[' => {
                self.advance();
                Some((Token::FlowSeqStart, self.current_span(start)))
            }
            ']' => {
                self.advance();
                Some((Token::FlowSeqEnd, self.current_span(start)))
            }
            ',' if self.mode() == LexMode::Flow => {
                self.advance();
                Some((Token::Comma, self.current_span(start)))
            }
            _ => None,
        }
    }

    /// Try to lex a block indicator (`-` or `?` followed by whitespace).
    pub(super) fn try_lex_block_indicator(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        // Block sequence indicator: - followed by whitespace/newline
        if ch == '-' {
            if let Some(next) = self.peek_char_after_current_ascii() {
                if next == ' ' || next == '\t' || Self::is_newline(next) {
                    self.advance();
                    return Some((Token::BlockSeqIndicator, self.current_span(start)));
                }
            } else {
                // - at EOF
                self.advance();
                return Some((Token::BlockSeqIndicator, self.current_span(start)));
            }
        }

        // Explicit key indicator: ? followed by whitespace/newline
        if ch == '?' {
            if let Some(next) = self.peek_char_after_current_ascii() {
                if next == ' ' || next == '\t' || Self::is_newline(next) {
                    self.advance();
                    return Some((Token::MappingKey, self.current_span(start)));
                }
            } else {
                self.advance();
                return Some((Token::MappingKey, self.current_span(start)));
            }
        }

        None
    }

    /// Try to lex a colon as a mapping value indicator.
    pub(super) fn try_lex_colon(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if ch != ':' {
            return None;
        }

        let next = self.peek_char_after_current_ascii();
        let is_indicator = if self.prev_was_json_like && self.mode() == LexMode::Flow {
            // After a JSON-like value, : is ALWAYS an indicator in flow context
            true
        } else {
            match self.mode() {
                LexMode::Flow => {
                    // In flow context, : is indicator if followed by:
                    // - whitespace, newline, EOF, or flow indicator
                    next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                        || next.is_some_and(Self::is_flow_indicator)
                }
                LexMode::Block => {
                    // In block context, : is indicator only if followed by whitespace/newline/EOF
                    next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                }
            }
        };

        // Note: : starts a plain scalar if not an indicator - let caller handle it
        is_indicator.then(|| {
            self.advance();
            (Token::Colon, self.current_span(start))
        })
    }
}
