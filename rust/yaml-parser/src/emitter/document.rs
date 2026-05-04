// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::Emitter;
use crate::error::ErrorKind;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::span::IndentLevel;
use crate::span::Span;

impl Emitter<'_> {
    pub(super) fn reset_document_state(&mut self) {
        self.anchors.clear();
        self.populate_tag_handles();
    }

    pub(super) fn prepare_document(&mut self) -> Option<(bool, Span, IndentLevel)> {
        let (_, mut ws_width) = self.skip_ws_and_newlines_tracked();

        if self.is_eof() {
            return None;
        }

        while self.peek_kind() == Some(TokenKind::DocEnd) {
            let _ = self.take_current();
            let (crossed, skip_width) = self.skip_ws_and_newlines_tracked();
            ws_width = if crossed {
                skip_width
            } else {
                ws_width + skip_width
            };
        }

        if self.is_eof() {
            return None;
        }

        self.reset_document_state();

        let mut has_directive = false;
        let mut first_directive_span = Span::from_usize_range(0..0);

        while matches!(
            self.peek_kind(),
            Some(TokenKind::YamlDirective | TokenKind::TagDirective | TokenKind::ReservedDirective)
        ) {
            let Some((_token, span)) = self.take_current() else {
                break;
            };
            if !has_directive {
                first_directive_span = span;
            }
            has_directive = true;
            let (crossed_dir, dir_ws) = self.skip_ws_and_newlines_tracked();
            ws_width = if crossed_dir {
                dir_ws
            } else {
                ws_width + dir_ws
            };
        }

        let (crossed, skip_width) = self.skip_ws_and_newlines_tracked();
        ws_width = if crossed {
            skip_width
        } else {
            ws_width + skip_width
        };

        if has_directive {
            let at_end = self.is_eof() || self.peek_kind() == Some(TokenKind::DocEnd);
            if at_end {
                self.error(ErrorKind::TrailingContent, first_directive_span);
                return None;
            }
        }

        if self.is_eof() {
            return None;
        }

        let has_doc_start = self.peek_kind() == Some(TokenKind::DocStart);
        let span = self.current_span();

        if has_doc_start {
            let _ = self.take_current();
            let doc_start_ws = self.skip_ws();
            let content_on_line = !self.is_eof()
                && !matches!(
                    self.peek_kind(),
                    Some(TokenKind::LineStart | TokenKind::DocEnd)
                );
            let (crossed_doc, doc_ws) = self.skip_ws_and_newlines_tracked();
            ws_width = if crossed_doc {
                doc_ws
            } else {
                doc_start_ws + doc_ws
            };

            if content_on_line && !self.is_eof() && self.check_block_mapping_on_start_line() {
                self.error(ErrorKind::ContentOnSameLine, self.current_span());
            }
        }

        let initial_col = self.current_indent + ws_width;
        Some((has_doc_start, span, initial_col))
    }

    pub(super) fn check_block_mapping_on_start_line(&self) -> bool {
        self.with_lookahead(10, |window| {
            let mut i = 0;
            while let Some(kind) = window.kind(i) {
                match kind {
                    TokenKind::LineStart => return false,
                    TokenKind::Colon | TokenKind::MappingKey => return true,
                    _ => i += 1,
                }
                if i > 10 {
                    break;
                }
            }
            false
        })
    }

    pub(super) fn populate_tag_handles(&mut self) {
        self.tag_handles.clear();
        self.tag_handles.insert("!", "!");
        self.tag_handles.insert("!!", "tag:yaml.org,2002:");

        let mut idx = self.pos;
        loop {
            let Some(continue_scan) = self.cursor.peek_with(idx, |token, _| match token {
                Token::TagDirective(handle, prefix) => {
                    self.tag_handles.insert(handle, prefix);
                    true
                }
                Token::DocStart | Token::DocEnd => false,
                _ => true,
            }) else {
                break;
            };
            if !continue_scan {
                break;
            }
            idx += 1;
        }
    }

    pub(super) fn finish_document(&mut self) -> (bool, Span) {
        self.skip_ws_and_newlines();

        self.consume_trailing_content();

        let (has_doc_end, span) = if self.peek_kind() == Some(TokenKind::DocEnd) {
            let doc_end_span = self.current_span();
            let _ = self.take_current();
            self.skip_ws_and_newlines();
            (true, doc_end_span)
        } else {
            let span = if let Some(last_span) = self.last_content_span {
                Span::at(last_span.end)
            } else {
                self.current_span()
            };
            (false, span)
        };

        (has_doc_end, span)
    }

    pub(super) fn consume_trailing_content(&mut self) {
        while !self.is_eof() {
            if matches!(
                self.peek_kind(),
                Some(TokenKind::DocStart | TokenKind::DocEnd)
            ) {
                break;
            }
            if matches!(
                self.peek_kind(),
                Some(
                    TokenKind::YamlDirective
                        | TokenKind::TagDirective
                        | TokenKind::ReservedDirective
                )
            ) {
                break;
            }

            let kind = self.peek_kind();
            let span = self.current_span();
            if matches!(kind, Some(TokenKind::FlowSeqEnd | TokenKind::FlowMapEnd)) {
                self.error(ErrorKind::UnmatchedBracket, span);
            } else {
                let is_content = matches!(
                    kind,
                    Some(
                        TokenKind::Plain
                            | TokenKind::StringStart
                            | TokenKind::Colon
                            | TokenKind::MappingKey
                            | TokenKind::BlockSeqIndicator
                            | TokenKind::Anchor
                            | TokenKind::Tag
                            | TokenKind::Alias
                            | TokenKind::FlowMapStart
                            | TokenKind::FlowSeqStart
                    )
                );
                if is_content {
                    self.error_unless_span_has_error(ErrorKind::TrailingContent, span);
                }
            }
            let _ = self.take_current();
            self.skip_ws_and_newlines();
        }
    }
}
