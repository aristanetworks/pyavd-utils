// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use super::{Lexer, Token};
use crate::error::ErrorKind;
use crate::span::Spanned;

impl<'input> Lexer<'input> {
    /// Try to lex a newline and subsequent indentation.
    pub(super) fn try_lex_newline(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if !Self::is_newline(ch) {
            return None;
        }

        self.advance();
        // Handle \r\n
        if ch == '\r' && self.peek() == Some('\n') {
            self.advance();
        }

        // Count indentation spaces only (tabs are NOT valid for indentation in YAML)
        let mut indent = 0;
        while self.peek() == Some(' ') {
            self.advance();
            indent += 1;
        }

        Some((Token::LineStart(indent), self.current_span(start)))
    }

    /// Try to lex inline whitespace.
    /// Returns `Token::Whitespace` for spaces only, or `Token::WhitespaceWithTabs` if tabs are present.
    pub(super) fn try_lex_whitespace(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if ch != ' ' && ch != '\t' {
            return None;
        }
        // Check if first char is a tab
        let first_is_tab = ch == '\t';
        // Continue consuming whitespace, detecting tabs
        let rest_has_tabs = self.skip_inline_whitespace_detecting_tabs();
        let has_tabs = first_is_tab || rest_has_tabs;
        let token = if has_tabs {
            Token::WhitespaceWithTabs
        } else {
            Token::Whitespace
        };
        Some((token, self.current_span(start)))
    }

    /// Try to lex a comment.
    pub(super) fn try_lex_comment(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if ch != '#' {
            return None;
        }

        if !self.prev_was_separator {
            // `#` without preceding whitespace is invalid - report error
            self.advance();
            let span = self.current_span(start);
            self.add_error(ErrorKind::InvalidComment, span);
            // Try to recover by consuming rest of line as if it were a comment
            while let Some(peek_ch) = self.peek() {
                if Self::is_newline(peek_ch) {
                    break;
                }
                self.advance();
            }
            return Some((Token::Comment(Cow::Borrowed("")), self.current_span(start)));
        }

        self.advance(); // consume #
        let content_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if Self::is_newline(peek_ch) {
                break;
            }
            self.advance();
        }
        // Borrow directly from input (zero-copy)
        // Byte positions are always at UTF-8 boundaries (maintained by advance())
        let content = self
            .input
            .get(content_start..self.byte_pos)
            .unwrap_or_default();
        Some((
            Token::Comment(Cow::Borrowed(content)),
            self.current_span(start),
        ))
    }
}
