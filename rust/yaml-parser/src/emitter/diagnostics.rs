// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::error::ErrorKind;
use crate::lexer::{Token, TokenKind};
use crate::span::{BytePosition, IndentLevel, Span};

use super::Emitter;

impl Emitter<'_> {
    pub(super) fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(crate::error::ParseError { kind, span });
    }

    pub(super) fn error_unless_span_has_error(&mut self, kind: ErrorKind, span: Span) {
        if !self.errors.iter().any(|err| err.span == span) {
            self.error(kind, span);
        }
    }

    pub(super) fn check_multiline_implicit_key(&mut self, key_span: Span) {
        if self.flow_depth > 0 {
            return;
        }

        let start = key_span.start_usize();
        let end = key_span.end_usize().min(self.input.len());
        if start >= end {
            return;
        }
        #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
        let key_text = &self.input[start..end];

        if key_text.contains('\n') {
            let colon_span = (0..10)
                .find_map(|i| {
                    self.peek_nth_with(i, |tok, span| matches!(tok, Token::Colon).then_some(span))
                        .flatten()
                })
                .unwrap_or(key_span);

            self.error(ErrorKind::MultilineImplicitKey, colon_span);
        }
    }

    pub(super) fn check_multiline_flow_key(&mut self, start_span: Span, end_span: Span) {
        let mut check_idx = 0;
        let colon_span = loop {
            match self.peek_kind_nth(check_idx) {
                Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => check_idx += 1,
                Some(TokenKind::Colon) => {
                    break self
                        .peek_nth_with(check_idx, |_, span| span)
                        .unwrap_or_else(|| self.current_span());
                }
                _ => return,
            }
            if check_idx > 20 {
                return;
            }
        };

        let start = start_span.start_usize();
        let end = end_span.end_usize().min(self.input.len());
        if start >= end {
            return;
        }
        #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
        let key_text = &self.input[start..end];

        if key_text.contains('\n') {
            self.error(ErrorKind::MultilineImplicitKey, colon_span);
        }
    }

    pub(super) fn check_tabs_as_indentation(&mut self) {
        if self.flow_depth > 0 {
            return;
        }

        let active_indent = *self.indent_stack.last().unwrap_or(&0);
        if self.current_indent > active_indent {
            return;
        }

        if self.peek_kind_nth(0) == Some(TokenKind::WhitespaceWithTabs) {
            let tab_span = self
                .peek_nth_with(0, |_, span| span)
                .unwrap_or_else(|| self.current_span());
            let mut look_ahead = 1;
            while let Some(kind) = self.peek_kind_nth(look_ahead) {
                match kind {
                    TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => look_ahead += 1,
                    TokenKind::FlowMapStart
                    | TokenKind::FlowMapEnd
                    | TokenKind::FlowSeqStart
                    | TokenKind::FlowSeqEnd
                    | TokenKind::LineStart => return,
                    _ => break,
                }
            }
            if self.peek_kind_nth(look_ahead).is_none() {
                return;
            }
            self.error(ErrorKind::InvalidIndentation, tab_span);
        }
    }

    pub(super) fn check_tabs_at_column_zero_in_flow(&mut self) {
        if self.peek_kind() == Some(TokenKind::WhitespaceWithTabs) && self.current_indent == 0 {
            let tab_span = self.current_span();
            let mut look_ahead = 1;
            while let Some(kind) = self.peek_kind_nth(look_ahead) {
                match kind {
                    TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => look_ahead += 1,
                    TokenKind::LineStart | TokenKind::FlowMapEnd | TokenKind::FlowSeqEnd => {
                        return;
                    }
                    _ => {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                        return;
                    }
                }
            }
        }
    }

    pub(super) fn check_tabs_after_block_indicator(&mut self) {
        if self.flow_depth > 0 {
            return;
        }

        if self.peek_kind() == Some(TokenKind::WhitespaceWithTabs) {
            let tab_span = self.current_span();
            let mut lookahead = 1;
            while let Some(kind) = self.peek_kind_nth(lookahead) {
                match kind {
                    TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => lookahead += 1,
                    TokenKind::BlockSeqIndicator | TokenKind::MappingKey | TokenKind::Colon => {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                        return;
                    }
                    TokenKind::Plain => {
                        if self.peek_kind_nth(lookahead + 1) == Some(TokenKind::Colon) {
                            self.error(ErrorKind::InvalidIndentation, tab_span);
                            return;
                        }
                        return;
                    }
                    _ => return,
                }
            }
        }
    }

    pub(super) fn check_content_after_flow(&mut self, flow_end: BytePosition) {
        if matches!(
            self.peek_kind(),
            Some(
                TokenKind::Plain
                    | TokenKind::StringStart
                    | TokenKind::Anchor
                    | TokenKind::Alias
                    | TokenKind::Tag
                    | TokenKind::BlockSeqIndicator
            )
        ) {
            let span = self.current_span();
            if span.start == flow_end {
                self.error(ErrorKind::ContentOnSameLine, span);
            }
        }
    }

    pub(super) fn check_trailing_content_at_root(&mut self, root_indent: IndentLevel) {
        let mut peek_offset = 0;
        let mut last_peeked_indent = self.current_indent;
        let mut ws_after_linestart = false;
        while let Some(kind) = self.peek_kind_nth(peek_offset) {
            match kind {
                TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => {
                    ws_after_linestart = true;
                    peek_offset += 1;
                }
                TokenKind::Comment => {
                    peek_offset += 1;
                }
                TokenKind::LineStart => {
                    let Some(n) = self
                        .peek_nth_with(peek_offset, |tok, _| match tok {
                            Token::LineStart(n) => Some(*n),
                            _ => None,
                        })
                        .flatten()
                    else {
                        debug_assert!(false, "expected LineStart token");
                        break;
                    };
                    last_peeked_indent = n;
                    ws_after_linestart = false;
                    peek_offset += 1;
                }
                _ => break,
            }
        }

        if let Some(kind) = self.peek_kind_nth(peek_offset) {
            let span = self
                .peek_nth_with(peek_offset, |_, span| span)
                .unwrap_or_else(|| self.current_span());
            if matches!(kind, TokenKind::FlowSeqEnd | TokenKind::FlowMapEnd) {
                self.error(ErrorKind::UnmatchedBracket, span);
            } else {
                let is_content = matches!(
                    kind,
                    TokenKind::Plain
                        | TokenKind::StringStart
                        | TokenKind::Anchor
                        | TokenKind::Alias
                        | TokenKind::Tag
                        | TokenKind::FlowSeqStart
                        | TokenKind::FlowMapStart
                        | TokenKind::BlockSeqIndicator
                );

                let col = if ws_after_linestart {
                    last_peeked_indent + 1
                } else {
                    last_peeked_indent
                };

                if is_content && col <= root_indent {
                    self.error(ErrorKind::TrailingContent, span);
                }
            }
        }
    }
}
