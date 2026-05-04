// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::Emitter;
use crate::error::ErrorKind;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::span::IndentLevel;
use crate::span::Span;
use crate::span::usize_to_indent;

impl Emitter<'_> {
    pub(super) fn skip_ws(&mut self) -> IndentLevel {
        let mut width: IndentLevel = 0;
        while matches!(
            self.peek_kind(),
            Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs)
        ) {
            if let Some((_tok, span)) = self.take_current() {
                width += usize_to_indent(span.end_usize() - span.start_usize());
            }
        }
        width
    }

    pub(super) fn skip_ws_and_newlines(&mut self) {
        self.skip_ws_and_newlines_impl();
    }

    pub(super) fn skip_ws_and_newlines_tracked(&mut self) -> (bool, IndentLevel) {
        let (crossed, _, ws_width) = self.skip_ws_and_newlines_impl();
        (crossed, ws_width)
    }

    #[inline]
    pub(super) fn current_token_starts_trivia_run(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(
                TokenKind::Whitespace
                    | TokenKind::WhitespaceWithTabs
                    | TokenKind::Comment
                    | TokenKind::LineStart
            )
        )
    }

    pub(super) fn skip_ws_and_newlines_impl(&mut self) -> (bool, Option<Span>, IndentLevel) {
        let mut crossed_line = false;
        let mut last_linestart_span = None;
        let mut ws_width: IndentLevel = 0;
        loop {
            match self.peek_kind() {
                Some(TokenKind::LineStart) => {
                    let (token, span) = self.take_current().unwrap();
                    let Token::LineStart(indent) = token else {
                        debug_assert!(false, "expected LineStart token");
                        break;
                    };
                    crossed_line = true;
                    last_linestart_span = Some(span);
                    ws_width = 0;

                    if let Some(&outermost_flow_col) = self.flow_context_columns.first()
                        && indent == 0
                        && outermost_flow_col > 0
                    {
                        let mut peek_offset = 0;
                        while matches!(
                            self.peek_kind_nth(peek_offset),
                            Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs)
                        ) {
                            peek_offset += 1;
                        }

                        if self.peek_nth_with(peek_offset, |tok, _| {
                            !matches!(
                                tok,
                                Token::LineStart(_)
                                    | Token::FlowSeqEnd
                                    | Token::FlowMapEnd
                                    | Token::DocEnd
                            )
                        }) == Some(true)
                            && peek_offset == 0
                        {
                            self.error(
                                ErrorKind::InvalidIndentationContext {
                                    expected: 1,
                                    found: 0,
                                },
                                span,
                            );
                        }
                    }

                    if self.flow_context_columns.is_empty() {
                        self.check_tabs_as_indentation();
                    } else {
                        self.check_tabs_at_column_zero_in_flow();
                    }
                }
                Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => {
                    if let Some((_tok, span)) = self.take_current() {
                        ws_width += usize_to_indent(span.end_usize() - span.start_usize());
                    }
                }
                Some(TokenKind::Comment) => {
                    let _ = self.take_current();
                }
                _ => break,
            }
        }
        (crossed_line, last_linestart_span, ws_width)
    }
}
