// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::{BlockScalarHeader, Chomping, Lexer, Token};
use crate::error::ErrorKind;
use crate::span::{Span, Spanned};

impl<'input> Lexer<'input> {
    /// Try to lex a block scalar header (`|` or `>`).
    pub(super) fn try_lex_block_scalar_header(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if ch == '|' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::LiteralBlockHeader(header), self.current_span(start)));
        }
        if ch == '>' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::FoldedBlockHeader(header), self.current_span(start)));
        }
        None
    }

    fn consume_block_header(&mut self) -> BlockScalarHeader {
        let mut indent = None;
        let mut chomping = Chomping::Clip;

        // Parse indent and chomping indicators (can be in any order)
        for _ in 0..2 {
            match self.peek() {
                Some('+') => {
                    chomping = Chomping::Keep;
                    self.advance();
                }
                Some('-') => {
                    chomping = Chomping::Strip;
                    self.advance();
                }
                Some(ch) if ch.is_ascii_digit() && ch != '0' => {
                    // ch is guaranteed to be ASCII digit 1-9, so to_digit is safe
                    indent = ch.to_digit(10).and_then(|digit| u8::try_from(digit).ok());
                    self.advance();
                }
                _ => break,
            }
        }

        // After block header, only whitespace and comments are allowed on the same line
        // Any other content is invalid (e.g., `> first line` is invalid)
        // Comments require preceding whitespace (e.g., `># comment` is invalid)
        let error_start = self.byte_pos;
        let mut has_invalid_content = false;
        let mut saw_whitespace = false;
        while let Some(peek_ch) = self.peek() {
            if Self::is_newline(peek_ch) {
                break;
            }
            if peek_ch == ' ' || peek_ch == '\t' {
                saw_whitespace = true;
                self.advance();
                continue;
            }
            if peek_ch == '#' {
                if !saw_whitespace {
                    // Comment without preceding whitespace is invalid
                    has_invalid_content = true;
                }
                // Consume comment regardless (for error recovery)
                while let Some(ch) = self.peek() {
                    if Self::is_newline(ch) {
                        break;
                    }
                    self.advance();
                }
                break;
            }
            // Invalid content on same line as block scalar indicator
            has_invalid_content = true;
            self.advance();
        }

        if has_invalid_content {
            let span = Span::from_usize_range(error_start..self.byte_pos);
            self.add_error(ErrorKind::ContentOnSameLine, span);
        }

        BlockScalarHeader { indent, chomping }
    }
}
