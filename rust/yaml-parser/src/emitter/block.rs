// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::error::ErrorKind;
use crate::event::{Event, ScalarStyle};
use crate::lexer::{Token, TokenKind};
use crate::span::{IndentLevel, Span};

use super::states::{
    BlockMapPhase, BlockSeqPhase, EmitterProperties, ParseState, ValueContext, ValueKind,
};
use super::{Emitter, PendingAstWrap};

impl<'input> Emitter<'input> {
    #[allow(clippy::too_many_lines, reason = "Complex state machine dispatch")]
    pub(super) fn process_block_seq(
        &mut self,
        indent: IndentLevel,
        mut phase: BlockSeqPhase,
        start_span: Span,
        _properties: EmitterProperties<'input>,
    ) -> Option<Event<'input>> {
        loop {
            match phase {
                BlockSeqPhase::BeforeEntryScan => loop {
                    match self.peek_kind() {
                        Some(TokenKind::LineStart) => {
                            let Some((n, span)) = self.peek_line_start() else {
                                debug_assert!(false, "expected LineStart token");
                                phase = BlockSeqPhase::BeforeEntryDispatch;
                                break;
                            };
                            if n < indent && self.line_start_is_blank_from(1) {
                                let _ = self.take_current();
                                continue;
                            }
                            if n < indent {
                                self.current_indent = n;
                                self.last_line_start_span = span;
                                phase = BlockSeqPhase::BeforeEntryDispatch;
                                break;
                            }

                            self.last_line_start_span = span;
                            self.current_indent = n;
                            if n > indent
                                && !self.is_valid_indent(n)
                                && self.has_content_at_orphan_level_from(1)
                            {
                                self.error(ErrorKind::InvalidIndentation, span);
                            }
                            let _ = self.take_current();
                            if self.flow_context_columns.is_empty() {
                                self.check_tabs_as_indentation();
                            }
                        }
                        Some(
                            TokenKind::Comment
                            | TokenKind::Whitespace
                            | TokenKind::WhitespaceWithTabs,
                        ) => {
                            let _ = self.take_current();
                        }
                        _ => {
                            phase = BlockSeqPhase::BeforeEntryDispatch;
                            break;
                        }
                    }
                },

                BlockSeqPhase::BeforeEntryDispatch => {
                    if self.current_indent < indent {
                        if !self.is_valid_indent(self.current_indent) {
                            self.report_invalid_indent();
                        }
                        if self.is_root_level_sequence(indent) {
                            self.check_trailing_content_at_root(0);
                        }
                        self.pop_indent();
                        return Some(Event::SequenceEnd {
                            span: self.collection_end_span(),
                        });
                    }

                    // Check for `-` at the sequence indent
                    match self.peek_kind() {
                        Some(TokenKind::BlockSeqIndicator) => {
                            // `current_indent` already matches the entry position:
                            // either a `LineStart` established it, or the caller set it
                            // when emitting the surrounding `SequenceStart`.
                            let entry_indent = self.current_indent;
                            if entry_indent < indent {
                                // Lower-indented, end sequence
                                self.pop_indent();
                                return Some(Event::SequenceEnd {
                                    span: self.collection_end_span(),
                                });
                            }

                            let indicator_span = self.current_span();
                            let _ = self.take_current(); // consume `-`
                            self.set_pending_ast_wrap(PendingAstWrap::SequenceItem {
                                item_start: indicator_span.start,
                            });
                            self.check_tabs_after_block_indicator();
                            let ws_width = self.skip_ws();

                            // Determine content_column: where content starts on the same line.
                            // Only meaningful if content follows on the same line as the `-`.
                            let content_col =
                                if matches!(self.peek_kind(), Some(TokenKind::LineStart) | None) {
                                    None // content on next line or EOF
                                } else {
                                    Some(entry_indent + 1 + ws_width)
                                };

                            // Push the next entry scan, then parse the value.
                            self.state_stack.push(ParseState::BlockSeq {
                                indent,
                                phase: BlockSeqPhase::BeforeEntryScan,
                                start_span,
                                properties: EmitterProperties::default(),
                            });
                            let min_indent = entry_indent + 1;
                            if content_col.is_none() {
                                self.state_stack.push(ParseState::Value {
                                    ctx: ValueContext {
                                        min_indent,
                                        content_column: content_col,
                                        kind: ValueKind::SeqEntryValue,
                                        allow_implicit_mapping: true,
                                        prior_crossed_line: false,
                                    },
                                    properties: EmitterProperties::default(),
                                });
                                return None;
                            }

                            match self.peek_kind() {
                                Some(TokenKind::Plain) => {
                                    if !self.current_plain_terminated_by_mapping_value_indicator() {
                                        return Some(self.parse_plain_scalar(
                                            EmitterProperties::default(),
                                            min_indent,
                                        ));
                                    }
                                    return Some(self.parse_scalar_or_mapping(
                                        min_indent,
                                        EmitterProperties::default(),
                                        false,
                                        false,
                                        true,
                                        content_col,
                                    ));
                                }
                                Some(TokenKind::StringStart) => {
                                    return Some(self.parse_scalar_or_mapping(
                                        min_indent,
                                        EmitterProperties::default(),
                                        false,
                                        false,
                                        true,
                                        content_col,
                                    ));
                                }
                                Some(TokenKind::Anchor | TokenKind::Tag) => {
                                    self.state_stack.push(ParseState::Value {
                                        ctx: ValueContext {
                                            min_indent,
                                            content_column: content_col,
                                            kind: ValueKind::SeqEntryValue,
                                            allow_implicit_mapping: true,
                                            prior_crossed_line: false,
                                        },
                                        properties: EmitterProperties::default(),
                                    });
                                    return None;
                                }
                                _ => {
                                    if let Some(comment) = self.take_same_line_comment_after_ws() {
                                        self.set_pending_ast_leading_comment(comment);
                                    }
                                    return self.process_value_after_properties(
                                        ValueContext {
                                            min_indent,
                                            content_column: content_col,
                                            kind: ValueKind::SeqEntryValue,
                                            allow_implicit_mapping: true,
                                            prior_crossed_line: false,
                                        },
                                        EmitterProperties::default(),
                                        false,
                                        false,
                                    );
                                }
                            }
                        }

                        Some(TokenKind::DocEnd | TokenKind::DocStart) | None => {
                            // End of sequence
                            // Only check trailing content for root-level sequences (not nested)
                            if self.is_root_level_sequence(indent) {
                                self.check_trailing_content_at_root(0);
                            }
                            self.pop_indent();
                            return Some(Event::SequenceEnd {
                                span: self.collection_end_span(),
                            });
                        }

                        Some(TokenKind::LineStart) => {
                            let (token, _) = self.take_current().unwrap_or_else(|| {
                                debug_assert!(false, "expected LineStart token");
                                (Token::Whitespace, Span::from_usize_range(0..0))
                            });
                            let Token::LineStart(n) = token else {
                                debug_assert!(false, "expected LineStart token");
                                phase = BlockSeqPhase::BeforeEntryScan;
                                continue;
                            };
                            if n < indent {
                                // Check for orphan indentation: n is not in the
                                // parser's indent stack (between valid levels)
                                if !self.is_valid_indent(n) {
                                    self.report_invalid_indent();
                                }
                                if self.is_root_level_sequence(indent) {
                                    self.check_trailing_content_at_root(0);
                                }
                                self.pop_indent();
                                return Some(Event::SequenceEnd {
                                    span: self.collection_end_span(),
                                });
                            }
                            phase = BlockSeqPhase::BeforeEntryScan;
                        }

                        _ => {
                            // Recover from malformed content at the sequence indentation by
                            // dropping the offending line and resuming entry scan. Keep this
                            // narrow so document markers and other structural cleanup still
                            // follow their dedicated paths.
                            let recoverable_same_indent_content = self.current_indent == indent
                                && matches!(
                                    self.peek_kind(),
                                    Some(
                                        TokenKind::Plain
                                            | TokenKind::StringStart
                                            | TokenKind::MappingKey
                                            | TokenKind::Colon
                                            | TokenKind::Anchor
                                            | TokenKind::Tag
                                            | TokenKind::Alias
                                            | TokenKind::FlowSeqStart
                                            | TokenKind::FlowMapStart
                                    )
                                );
                            if recoverable_same_indent_content {
                                self.error(ErrorKind::InvalidIndentation, self.current_span());
                                self.skip_to_line_end();
                                self.skip_invalid_indented_recovery_lines(indent);
                                phase = BlockSeqPhase::BeforeEntryScan;
                                continue;
                            }
                            // Root-level check
                            if self.is_root_level_sequence(indent) {
                                self.check_trailing_content_at_root(0);
                            }
                            self.pop_indent();
                            return Some(Event::SequenceEnd {
                                span: self.collection_end_span(),
                            });
                        }
                    }
                }
            }
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Block Mapping
    // ─────────────────────────────────────────────────────────────

    #[allow(clippy::too_many_lines, reason = "Complex state machine dispatch")]
    pub(super) fn process_block_map(
        &mut self,
        indent: IndentLevel,
        mut phase: BlockMapPhase,
        start_span: Span,
        _properties: EmitterProperties<'input>,
    ) -> Option<Event<'input>> {
        loop {
            match phase {
                BlockMapPhase::BeforeKeyScan {
                    require_line_boundary,
                    mut crossed_line,
                } => loop {
                    match self.peek_kind() {
                        Some(TokenKind::LineStart) => {
                            let Some((n, span)) = self.peek_line_start() else {
                                debug_assert!(false, "expected LineStart token");
                                phase = BlockMapPhase::BeforeKeyDispatch {
                                    require_line_boundary,
                                    crossed_line,
                                };
                                break;
                            };
                            crossed_line = true;
                            if n < indent && self.line_start_is_blank_from(1) {
                                let _ = self.take_current();
                                continue;
                            }
                            if n < indent {
                                self.current_indent = n;
                                self.last_line_start_span = span;
                                if !self.is_valid_indent(n)
                                    && self.has_content_at_orphan_level_from(1)
                                {
                                    self.error(ErrorKind::InvalidIndentation, span);
                                }
                                phase = BlockMapPhase::BeforeKeyDispatch {
                                    require_line_boundary,
                                    crossed_line,
                                };
                                break;
                            }

                            self.current_indent = n;
                            self.last_line_start_span = span;
                            if n > indent
                                && !self.is_valid_indent(n)
                                && self.has_content_at_orphan_level_from(1)
                            {
                                self.error(ErrorKind::InvalidIndentation, span);
                            }
                            let _ = self.take_current();
                            if self.flow_context_columns.is_empty() {
                                self.check_tabs_as_indentation();
                            }
                        }
                        Some(
                            TokenKind::Comment
                            | TokenKind::Whitespace
                            | TokenKind::WhitespaceWithTabs,
                        ) => {
                            let _ = self.take_current();
                        }
                        _ => {
                            phase = BlockMapPhase::BeforeKeyDispatch {
                                require_line_boundary,
                                crossed_line,
                            };
                            break;
                        }
                    }
                },

                BlockMapPhase::BeforeKeyDispatch {
                    require_line_boundary,
                    crossed_line,
                } => {
                    if require_line_boundary && !crossed_line {
                        if let Some(kind) = self.peek_kind() {
                            let error_kind = if kind == TokenKind::Colon {
                                ErrorKind::UnexpectedColon
                            } else {
                                ErrorKind::TrailingContent
                            };
                            self.error(error_kind, self.current_span());
                            self.skip_to_line_end();
                            phase = BlockMapPhase::BeforeKeyScan {
                                require_line_boundary: false,
                                crossed_line: false,
                            };
                            continue;
                        }
                        self.pop_indent();
                        return Some(Event::MappingEnd {
                            span: self.collection_end_span(),
                        });
                    }

                    if (crossed_line || self.crossed_line_boundary) && self.current_indent < indent
                    {
                        if !self.is_valid_indent(self.current_indent) {
                            self.report_invalid_indent();
                        }
                        self.pop_indent();
                        return Some(Event::MappingEnd {
                            span: self.collection_end_span(),
                        });
                    }

                    match self.peek_kind() {
                        Some(TokenKind::MappingKey) => {
                            let pair_start_span = self.current_span();
                            let _ = self.take_current();
                            self.set_pending_ast_wrap(PendingAstWrap::MappingPair {
                                pair_start: pair_start_span.start,
                            });
                            self.check_tabs_after_block_indicator();
                            let ws_width = self.skip_ws();

                            let content_col =
                                if matches!(self.peek_kind(), Some(TokenKind::LineStart) | None) {
                                    None
                                } else {
                                    Some(indent + 1 + ws_width)
                                };

                            self.state_stack.push(ParseState::BlockMap {
                                indent,
                                phase: BlockMapPhase::AfterKey {
                                    is_implicit_scalar_key: false,
                                    key_end_column: None,
                                },
                                start_span,
                                properties: EmitterProperties::default(),
                            });
                            self.state_stack.push(ParseState::Value {
                                ctx: ValueContext {
                                    min_indent: indent + 1,
                                    content_column: content_col,
                                    kind: ValueKind::ExplicitKey,
                                    allow_implicit_mapping: true,
                                    prior_crossed_line: false,
                                },
                                properties: EmitterProperties::default(),
                            });
                            return None;
                        }

                        Some(TokenKind::Colon) => {
                            self.set_pending_ast_wrap(PendingAstWrap::MappingPair {
                                pair_start: self.current_span().start,
                            });
                            self.state_stack.push(ParseState::BlockMap {
                                indent,
                                phase: BlockMapPhase::AfterKey {
                                    is_implicit_scalar_key: false,
                                    key_end_column: None,
                                },
                                start_span,
                                properties: EmitterProperties::default(),
                            });
                            return Some(self.emit_null());
                        }

                        Some(TokenKind::DocEnd | TokenKind::DocStart) | None => {
                            self.pop_indent();
                            return Some(Event::MappingEnd {
                                span: self.collection_end_span(),
                            });
                        }

                        Some(TokenKind::LineStart) => {
                            if let Some((Token::LineStart(n), _span)) = self.take_current() {
                                if n < indent {
                                    if !self.is_valid_indent(n) {
                                        self.report_invalid_indent();
                                    }
                                    self.pop_indent();
                                    return Some(Event::MappingEnd {
                                        span: self.collection_end_span(),
                                    });
                                }
                                phase = BlockMapPhase::BeforeKeyScan {
                                    require_line_boundary: false,
                                    crossed_line: true,
                                };
                                continue;
                            }
                            return None;
                        }

                        Some(TokenKind::Comment) => {
                            let _ = self.take_current();
                            phase = BlockMapPhase::BeforeKeyScan {
                                require_line_boundary: false,
                                crossed_line: false,
                            };
                        }

                        _ => {
                            if self.current_indent > indent
                                && !self.is_valid_indent(self.current_indent)
                            {
                                self.report_invalid_indent();
                                self.skip_to_line_end();
                                self.skip_invalid_indented_recovery_lines(indent);
                                phase = BlockMapPhase::BeforeKeyScan {
                                    require_line_boundary: false,
                                    crossed_line: false,
                                };
                                continue;
                            }

                            if self.is_implicit_key() {
                                self.set_pending_ast_wrap(PendingAstWrap::MappingPair {
                                    pair_start: self.current_span().start,
                                });
                                self.state_stack.push(ParseState::BlockMap {
                                    indent,
                                    phase: BlockMapPhase::AfterKey {
                                        is_implicit_scalar_key: true,
                                        key_end_column: None,
                                    },
                                    start_span,
                                    properties: EmitterProperties::default(),
                                });
                                self.state_stack.push(ParseState::Value {
                                    ctx: ValueContext {
                                        min_indent: indent,
                                        content_column: None,
                                        kind: ValueKind::ImplicitKey,
                                        allow_implicit_mapping: true,
                                        prior_crossed_line: false,
                                    },
                                    properties: EmitterProperties::default(),
                                });
                                return None;
                            }

                            if self.current_indent == indent
                                && self.recover_missing_colon_in_mapping(indent)
                            {
                                phase = BlockMapPhase::BeforeKeyScan {
                                    require_line_boundary: false,
                                    crossed_line: false,
                                };
                                continue;
                            }
                            self.pop_indent();
                            return Some(Event::MappingEnd {
                                span: self.collection_end_span(),
                            });
                        }
                    }
                }

                BlockMapPhase::AfterKey {
                    is_implicit_scalar_key,
                    key_end_column,
                } => {
                    // In block mappings, the colon can be on a following line after an explicit key
                    // e.g., `? [ key ]\n: value` - need to skip the newline to find the colon

                    // IMPORTANT: Use `self.last_line_start_span` which is updated by `advance()`
                    // whenever ANY LineStart is consumed, including by nested structures like
                    // sequences. The return value of `skip_ws_and_newlines_impl()` only captures
                    // LineStarts consumed within that call, but the LineStart may have already
                    // been consumed by the nested structure before we reached AfterKey.
                    // Track if we've crossed a line boundary (for same-line detection).
                    let crossed_before = self.crossed_line_boundary;
                    let (crossed, ws_after_key) = if self.current_token_starts_trivia_run() {
                        self.skip_ws_and_newlines_tracked()
                    } else {
                        (false, 0)
                    };

                    // Compute the colon's column from state:
                    // - If we crossed a line: colon is at current_indent (new line)
                    // - If not: colon is at key_end_column + ws consumed after key
                    let colon_column = if crossed {
                        self.current_indent
                    } else {
                        key_end_column.map_or(self.current_indent, |col| col + ws_after_key)
                    };

                    if self.peek_kind() == Some(TokenKind::Colon) {
                        let _ = self.take_current();
                        self.check_tabs_after_block_indicator();
                        let ws_width = self.skip_ws();

                        let next_kind = self.peek_kind();
                        let content_col = if matches!(next_kind, Some(TokenKind::LineStart) | None)
                        {
                            None
                        } else {
                            Some(colon_column + 1 + ws_width)
                        };

                        if is_implicit_scalar_key && next_kind == Some(TokenKind::BlockSeqIndicator)
                        {
                            self.error(ErrorKind::ContentOnSameLine, self.current_span());
                        }

                        self.crossed_line_boundary = false;

                        self.state_stack.push(ParseState::BlockMap {
                            indent,
                            phase: BlockMapPhase::AfterValue,
                            start_span,
                            properties: EmitterProperties::default(),
                        });
                        self.state_stack.push(ParseState::Value {
                            ctx: ValueContext {
                                min_indent: indent + 1,
                                content_column: content_col,
                                kind: ValueKind::MappingValue,
                                allow_implicit_mapping: !is_implicit_scalar_key,
                                prior_crossed_line: false,
                            },
                            properties: EmitterProperties::default(),
                        });
                        return None;
                    }

                    self.state_stack.push(ParseState::BlockMap {
                        indent,
                        phase: BlockMapPhase::AfterValue,
                        start_span,
                        properties: EmitterProperties::default(),
                    });
                    let empty_span = if crossed_before || self.crossed_line_boundary {
                        self.last_line_start_span
                    } else {
                        self.current_span()
                    };
                    return Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: None,
                        span: empty_span,
                    });
                }

                BlockMapPhase::AfterValue => {
                    // Determine if we need to require a line boundary for the next entry.
                    //
                    // `crossed_line_boundary` is set when ANY LineStart token is consumed,
                    // including by nested structures. It's reset only when we start parsing
                    // a new value (in AfterKey after finding a colon).
                    //
                    // IMPORTANT: We do NOT reset `crossed_line_boundary` here. If we did,
                    // nested structures would reset it before outer structures could see it.
                    // The flag remains true until the next value parsing begins.
                    //
                    // If crossed_line_boundary is true, we've moved to a new line during
                    // value parsing, so subsequent entries are valid (don't require new line).
                    // If false, we're still on the same line as the last key (like `a: b: c`),
                    // so we should prevent new entries by requiring a line boundary.
                    let require_line_boundary = !self.crossed_line_boundary;

                    phase = BlockMapPhase::BeforeKeyScan {
                        require_line_boundary,
                        crossed_line: false,
                    };
                }
            }
        }
    }
}
