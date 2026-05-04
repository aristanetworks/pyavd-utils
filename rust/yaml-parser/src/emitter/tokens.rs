// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::Emitter;
use super::cursor::LookaheadWindow;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::span::BytePosition;
use crate::span::IndentLevel;
use crate::span::Span;

impl<'input> Emitter<'input> {
    pub(super) fn mapping_key_insertion_span(&self) -> Span {
        self.last_content_span.map_or_else(
            || Span::at(self.current_span().start),
            |span| Span::at(span.end),
        )
    }

    #[inline]
    pub(super) fn peek(&self) -> Option<(Token<'input>, Span)> {
        self.cursor.peek(self.pos)
    }

    #[inline]
    pub(super) fn with_lookahead<R>(
        &self,
        max_offset: usize,
        func: impl FnOnce(LookaheadWindow<'_, 'input>) -> R,
    ) -> R {
        self.cursor.with_window(self.pos, max_offset, func)
    }

    #[inline]
    pub(super) fn peek_with<F, R>(&self, func: F) -> Option<R>
    where
        F: FnOnce(&Token<'input>, Span) -> R,
    {
        self.cursor.peek_with(self.pos, func)
    }

    #[inline]
    pub(super) fn peek_nth_with<F, R>(&self, n: usize, func: F) -> Option<R>
    where
        F: FnOnce(&Token<'input>, Span) -> R,
    {
        self.cursor.peek_nth_with(self.pos, n, func)
    }

    #[inline]
    pub(super) fn take_current(&mut self) -> Option<(Token<'input>, Span)> {
        if let Some((token, span)) = self.cursor.take(self.pos) {
            if let Token::LineStart(n) = &token {
                self.current_indent = *n;
                self.last_line_start_span = span;
                self.crossed_line_boundary = true;
            }
            self.pos += 1;
            Some((token, span))
        } else {
            None
        }
    }

    #[inline]
    pub(super) fn is_eof(&self) -> bool {
        self.cursor.is_eof(self.pos)
    }

    #[inline]
    pub(super) fn current_span(&self) -> Span {
        self.cursor.current_span(self.pos)
    }

    #[inline]
    pub(super) fn peek_kind(&self) -> Option<TokenKind> {
        self.cursor.peek_kind(self.pos)
    }

    #[inline]
    pub(super) fn peek_kind_with_span(&self) -> Option<(TokenKind, Span)> {
        self.peek_with(|tok, span| (TokenKind::from(tok), span))
    }

    #[inline]
    pub(super) fn peek_kind_nth(&self, n: usize) -> Option<TokenKind> {
        self.cursor.peek_kind_nth(self.pos, n)
    }

    #[inline]
    pub(super) fn peek_line_start(&self) -> Option<(IndentLevel, Span)> {
        self.peek_with(|tok, span| {
            if let Token::LineStart(n) = tok {
                Some((*n, span))
            } else {
                None
            }
        })
        .flatten()
    }

    #[inline]
    pub(super) fn current_plain_terminated_by_mapping_value_indicator(&self) -> bool {
        self.peek_with(|tok, _| match tok {
            Token::Plain(plain) => plain.terminated_by_mapping_value_indicator(),
            _ => false,
        })
        .unwrap_or(false)
    }

    #[inline]
    pub(super) fn plain_at_offset_terminated_by_mapping_value_indicator(
        &self,
        offset: usize,
    ) -> bool {
        self.peek_nth_with(offset, |tok, _| match tok {
            Token::Plain(plain) => plain.terminated_by_mapping_value_indicator(),
            _ => false,
        })
        .unwrap_or(false)
    }

    #[allow(
        clippy::string_slice,
        reason = "Positions from tokens are UTF-8 boundaries"
    )]
    pub(super) fn scan_skip_inline_whitespace_from(
        window: &LookaheadWindow<'_, 'input>,
        mut offset: usize,
    ) -> usize {
        while matches!(
            window.kind(offset),
            Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs)
        ) {
            offset += 1;
        }
        offset
    }

    pub(super) fn collection_end_span(&self) -> Span {
        if let Some(span) = self.last_content_span {
            Span::new(span.end..span.end)
        } else if self.pos > 0 {
            let prev_span = self.cursor.current_span(self.pos - 1);
            Span::at(prev_span.end)
        } else {
            self.current_span()
        }
    }

    pub(super) fn indented_line_error_span(line_span: Span, indent: IndentLevel) -> Span {
        let width = BytePosition::from(indent);
        if width == 0 {
            line_span
        } else {
            let end = line_span.end;
            Span::new(end.saturating_sub(width)..end)
        }
    }

    pub(super) fn enter_flow_collection(&mut self, flow_start_column: Option<IndentLevel>) {
        self.flow_depth += 1;
        self.flow_context_columns
            .push(flow_start_column.unwrap_or(self.current_indent));
    }

    pub(super) fn exit_flow_collection(&mut self) {
        self.flow_depth = self.flow_depth.saturating_sub(1);
        self.flow_context_columns.pop();
    }
}
