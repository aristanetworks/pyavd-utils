// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use super::{LexMode, Lexer, PlainScalarMeta, PlainScalarToken, Token, is_anchor_char};
use crate::span::Spanned;

impl<'input> Lexer<'input> {
    /// Try to lex an anchor (`&name`) or alias (`*name`).
    pub(super) fn try_lex_anchor_or_alias(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        // Anchors: &name
        if ch == '&'
            && let Some(next) = self.peek_char_after_current_ascii()
            && is_anchor_char(next)
        {
            self.advance(); // consume &
            let name = self.consume_anchor_name();
            return Some((Token::Anchor(name), self.current_span(start)));
        }

        // Aliases: *name
        if ch == '*'
            && let Some(next) = self.peek_char_after_current_ascii()
            && is_anchor_char(next)
        {
            self.advance(); // consume *
            let name = self.consume_anchor_name();
            return Some((Token::Alias(name), self.current_span(start)));
        }

        None
    }

    /// Consume an anchor name, borrowing directly from input (zero-copy).
    fn consume_anchor_name(&mut self) -> &'input str {
        // According to YAML 1.2 spec, anchor names (ns-anchor-char+) can contain:
        // - Any non-whitespace character except c-flow-indicator ([]{},)
        // This includes colons and other special characters!
        let name_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if is_anchor_char(peek_ch) {
                self.advance();
            } else {
                break;
            }
        }
        // Borrow directly from input (zero-copy)
        // Byte positions are always at UTF-8 boundaries (maintained by advance())
        self.input
            .get(name_start..self.byte_pos)
            .unwrap_or_default()
    }

    fn borrowed_tag_token(&self, start: usize, tag_start: usize) -> Spanned<Token<'input>> {
        #[allow(
            clippy::string_slice,
            reason = "tag_start and byte_pos are valid UTF-8 boundaries"
        )]
        let tag_slice = &self.input[tag_start..self.byte_pos];
        (
            Token::Tag(Cow::Borrowed(tag_slice)),
            self.current_span(start),
        )
    }

    fn consume_plain_tag_fallback(&mut self, start: usize) -> Spanned<Token<'input>> {
        // `!` followed by non-tag character (like `!\"#$%...`)
        // This is NOT a tag - treat the `!` and following content as plain scalar
        // Note: # is only a comment if preceded by whitespace, so we include it here
        let in_flow = self.mode() == LexMode::Flow;
        while let Some(peek_ch) = self.peek() {
            // Stop at whitespace
            if peek_ch.is_whitespace() {
                break;
            }
            // Flow indicators only terminate in flow mode
            if in_flow && Self::is_flow_indicator(peek_ch) {
                break;
            }
            // Colon ends plain scalar if followed by whitespace (or EOF)
            if peek_ch == ':' {
                let next = self.peek_n(1);
                if next.is_none() || next.is_some_and(char::is_whitespace) {
                    break;
                }
            }
            self.advance();
        }
        #[allow(
            clippy::string_slice,
            reason = "start and byte_pos are valid UTF-8 boundaries"
        )]
        let plain = &self.input[start..self.byte_pos];
        (
            Token::Plain(PlainScalarToken::new(
                Cow::Borrowed(plain),
                PlainScalarMeta::default(),
            )),
            self.current_span(start),
        )
    }

    pub(super) fn consume_tag(&mut self, start: usize) -> Spanned<Token<'input>> {
        let tag_start = start; // Position of the '!'
        self.advance(); // consume !

        match self.peek() {
            Some('<') => {
                // Verbatim tag !<uri>
                // Mark with a leading '\0' so the parser knows not to expand it.
                // This internal marker is stripped by expand_tag().
                self.advance(); // consume '<'
                let mut tag = String::from("\0");
                while let Some(peek_ch) = self.peek() {
                    if peek_ch == '>' {
                        self.advance();
                        break;
                    }
                    tag.push(peek_ch);
                    self.advance();
                }
                (Token::Tag(Cow::Owned(tag)), self.current_span(start))
            }
            Some(ch) if Self::is_valid_tag_start_char(ch) => {
                // Regular tag !name - can borrow the whole thing from input
                while let Some(peek_ch) = self.peek() {
                    if peek_ch.is_whitespace() || Self::is_flow_indicator(peek_ch) {
                        break;
                    }
                    self.advance();
                }
                // Borrow from input: includes the ! prefix
                self.borrowed_tag_token(start, tag_start)
            }
            // ! followed by whitespace, EOF, or flow indicator is the non-specific tag
            Some(ch) if ch.is_whitespace() || Self::is_flow_indicator(ch) => {
                // Empty tag (non-specific tag `!`)
                // Borrow the single '!' from input
                self.borrowed_tag_token(start, tag_start)
            }
            None => {
                // ! at end of input is also a valid non-specific tag
                // Borrow the single '!' from input
                self.borrowed_tag_token(start, tag_start)
            }
            _ => self.consume_plain_tag_fallback(start),
        }
    }

    /// Check if a character is valid at the start of a tag name.
    /// Valid tag characters are URI characters: alphanumerics and certain punctuation.
    fn is_valid_tag_start_char(ch: char) -> bool {
        ch.is_alphanumeric()
            || matches!(
                ch,
                '!' | '-'
                    | '_'
                    | '.'
                    | '~'
                    | '%'
                    | '/'
                    | ':'
                    | '@'
                    | '&'
                    | '='
                    | '+'
                    | '$'
                    | ','
                    | ';'
            )
    }
}
