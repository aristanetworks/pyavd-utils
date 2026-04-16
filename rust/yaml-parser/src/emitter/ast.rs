// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::ast_event::AstEvent;
use crate::event::{Comment, Event};
use crate::lexer::{Token, TokenKind};

use super::{Emitter, PendingAstWrap};

impl<'input> Emitter<'input> {
    pub(super) fn set_pending_ast_wrap(&mut self, wrap: PendingAstWrap) {
        self.pending_ast_wraps.push_back(wrap);
    }

    pub(super) fn wrap_ast_event(&mut self, event: Event<'input>) -> AstEvent<'input> {
        let leading_comment = self.pending_ast_leading_comment.take();
        let trailing_comment = if matches!(event, Event::Scalar { .. } | Event::Alias { .. }) {
            self.take_same_line_comment_after_ws()
        } else {
            None
        };
        match self.pending_ast_wraps.pop_front() {
            Some(PendingAstWrap::SequenceItem { item_start }) => match event {
                Event::MappingStart { .. }
                | Event::SequenceStart { .. }
                | Event::Scalar { .. }
                | Event::Alias { .. } => AstEvent::SequenceItem {
                    item_start,
                    event,
                    leading_comment,
                    trailing_comment,
                },
                _ => {
                    debug_assert!(false, "invalid event for pending sequence item wrapper");
                    if leading_comment.is_some() || trailing_comment.is_some() {
                        AstEvent::RichEvent {
                            event,
                            leading_comment,
                            trailing_comment,
                        }
                    } else {
                        AstEvent::Event(event)
                    }
                }
            },
            Some(PendingAstWrap::MappingPair { pair_start }) => match event {
                Event::MappingStart { .. }
                | Event::SequenceStart { .. }
                | Event::Scalar { .. }
                | Event::Alias { .. } => AstEvent::MappingKey {
                    pair_start,
                    key_event: event,
                    leading_comment,
                    trailing_comment,
                },
                _ => {
                    debug_assert!(false, "invalid event for pending mapping pair wrapper");
                    if leading_comment.is_some() || trailing_comment.is_some() {
                        AstEvent::RichEvent {
                            event,
                            leading_comment,
                            trailing_comment,
                        }
                    } else {
                        AstEvent::Event(event)
                    }
                }
            },
            None => {
                if leading_comment.is_some() || trailing_comment.is_some() {
                    AstEvent::RichEvent {
                        event,
                        leading_comment,
                        trailing_comment,
                    }
                } else {
                    AstEvent::Event(event)
                }
            }
        }
    }

    pub(super) fn discard_pending_ast_wrap(&mut self) {
        let _ = self.pending_ast_wraps.pop_front();
    }

    pub(super) fn set_pending_ast_leading_comment(&mut self, comment: Comment<'input>) {
        self.pending_ast_leading_comment = Some(comment);
    }

    pub(super) fn take_comment_token(&mut self) -> Option<Comment<'input>> {
        let Some((Token::Comment(text), span)) = self.take_current() else {
            return None;
        };
        Some(Comment { text, span })
    }

    #[inline]
    pub(super) fn can_start_same_line_comment_scan(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs | TokenKind::Comment)
        )
    }

    #[inline]
    pub(super) fn same_line_comment_offset(&self) -> Option<usize> {
        match self.peek_kind() {
            Some(TokenKind::Comment) => Some(0),
            Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => {
                match self.peek_kind_nth(1) {
                    Some(TokenKind::Comment) => Some(1),
                    Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => self
                        .with_lookahead(32, |window| {
                            let mut offset = 0;
                            loop {
                                match window.kind(offset) {
                                    Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => {
                                        offset += 1;
                                    }
                                    Some(TokenKind::Comment) => return Some(offset),
                                    _ => return None,
                                }
                            }
                        }),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub(super) fn take_same_line_comment_after_ws(&mut self) -> Option<Comment<'input>> {
        if !self.can_start_same_line_comment_scan() {
            return None;
        }

        let offset = self.same_line_comment_offset()?;

        for _ in 0..offset {
            let _ = self.take_current();
        }

        self.take_comment_token()
    }

    pub(super) fn track_emitted_event(&mut self, event: &Event<'input>) {
        match event {
            Event::Scalar { span, .. }
            | Event::Alias { span, .. }
            | Event::MappingEnd { span }
            | Event::SequenceEnd { span } => {
                self.last_content_span = Some(*span);
            }
            Event::MappingStart { .. } | Event::SequenceStart { .. } => {
                self.last_content_span = None;
            }
            _ => {}
        }
    }

    pub(crate) fn next_ast_event(&mut self) -> Option<AstEvent<'input>> {
        self.next_event_core()
            .map(|event| self.wrap_ast_event(event))
    }
}
