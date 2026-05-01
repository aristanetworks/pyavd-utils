// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::error::ErrorKind;
use crate::event::{Event, ScalarStyle};
use crate::lexer::{Token, TokenKind};
use crate::span::{IndentLevel, Span};

use super::cursor::LookaheadWindow;
use super::states::{
    EmitterProperties, FlowMapPhase, FlowSeqPhase, ParseState, ValueContext, ValueKind,
};
use super::{Emitter, PendingAstWrap};

impl<'input> Emitter<'input> {
    pub(super) fn is_flow_seq_complex_key(&self) -> bool {
        self.with_lookahead(200, |window| Self::scan_is_flow_seq_complex_key(&window))
    }

    /// Check if the current `FlowSeqStart` token is followed immediately by
    /// `:` after the matching `]`.
    ///
    /// This is used for explicit `?` keys, where `? []: x` should parse as an
    /// inner mapping key node, but `?\n  []\n: x` must leave the `:` for the
    /// outer explicit-key separator.
    pub(super) fn is_flow_seq_inline_complex_key(&self) -> bool {
        self.with_lookahead(200, |window| {
            Self::scan_is_flow_seq_inline_complex_key(&window)
        })
    }

    /// Check if the current `FlowMapStart` token is part of a complex key pattern.
    ///
    /// This looks ahead through the flow mapping to find the closing `}`,
    /// then checks if it's followed by `:` (making it a mapping key).
    pub(super) fn is_flow_map_complex_key(&self) -> bool {
        self.with_lookahead(200, |window| Self::scan_is_flow_map_complex_key(&window))
    }

    /// Check if the current `FlowMapStart` token is followed immediately by
    /// `:` after the matching `}`.
    ///
    /// This is the explicit-key counterpart to `is_flow_map_complex_key()`.
    pub(super) fn is_flow_map_inline_complex_key(&self) -> bool {
        self.with_lookahead(200, |window| {
            Self::scan_is_flow_map_inline_complex_key(&window)
        })
    }

    pub(super) fn current_flow_collection_is_complex_key(&self, explicit_key: bool) -> bool {
        match (self.peek_kind(), explicit_key) {
            (Some(TokenKind::FlowMapStart), true) => self.is_flow_map_inline_complex_key(),
            (Some(TokenKind::FlowMapStart), false) => self.is_flow_map_complex_key(),
            (Some(TokenKind::FlowSeqStart), true) => self.is_flow_seq_inline_complex_key(),
            (Some(TokenKind::FlowSeqStart), false) => self.is_flow_seq_complex_key(),
            _ => false,
        }
    }

    /// Check if the current position is at an implicit flow mapping entry.
    ///
    /// Inside flow sequences, `[ key: value ]` creates an implicit mapping.
    /// This looks ahead to detect if the current entry is followed by a Colon.
    pub(super) fn is_implicit_flow_mapping_entry(&self) -> bool {
        self.with_lookahead(200, |window| {
            Self::scan_is_implicit_flow_mapping_entry(&window)
        })
    }

    /// Skip line-prefix whitespace tokens.
    pub(super) fn skip_indent_tokens(&mut self) {
        while let Some(kind) = self.peek_kind() {
            match kind {
                TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => {
                    let _ = self.take_current();
                }
                _ => break,
            }
        }
    }

    #[inline]
    pub(super) fn scan_skip_flow_key_trivia_from(
        window: &LookaheadWindow<'_, 'input>,
        mut offset: usize,
        allow_newlines: bool,
    ) -> usize {
        while let Some(kind) = window.kind(offset) {
            let is_trivia = matches!(kind, TokenKind::Whitespace | TokenKind::WhitespaceWithTabs)
                || (allow_newlines && kind == TokenKind::LineStart);
            if !is_trivia {
                break;
            }
            offset += 1;
        }
        offset
    }

    #[inline]
    pub(super) fn scan_skip_flow_entry_prefix_from(
        window: &LookaheadWindow<'_, 'input>,
        mut offset: usize,
    ) -> usize {
        offset = Self::scan_skip_flow_key_trivia_from(window, offset, true);
        while matches!(
            window.kind(offset),
            Some(TokenKind::Anchor | TokenKind::Tag)
        ) {
            offset += 1;
            offset = Self::scan_skip_flow_key_trivia_from(window, offset, true);
        }
        offset
    }

    #[inline]
    pub(super) fn scan_skip_flow_plain_continuations_from(
        window: &LookaheadWindow<'_, 'input>,
        mut offset: usize,
    ) -> usize {
        loop {
            offset = Self::scan_skip_inline_whitespace_from(window, offset);
            let Some(Token::Plain(plain)) = window.token(offset) else {
                return offset;
            };
            offset += 1;
            if !plain.meta().may_continue_on_next_line_in_flow {
                return Self::scan_skip_inline_whitespace_from(window, offset);
            }

            offset = Self::scan_skip_inline_whitespace_from(window, offset);
            if window.kind(offset) == Some(TokenKind::LineStart) {
                offset += 1;
                continue;
            }
            return Self::scan_skip_inline_whitespace_from(window, offset);
        }
    }

    pub(super) fn scan_find_string_end_from(
        window: &LookaheadWindow<'_, 'input>,
        start_offset: usize,
        max_width: usize,
    ) -> Option<usize> {
        let limit = start_offset + max_width;
        let mut i = start_offset;
        while let Some(kind) = window.kind(i) {
            match kind {
                TokenKind::StringEnd => return Some(i),
                TokenKind::DocStart | TokenKind::DocEnd => return None,
                _ => {}
            }
            i += 1;
            if i > limit {
                break;
            }
        }
        None
    }

    pub(super) fn scan_find_flow_collection_end(
        window: &LookaheadWindow<'_, 'input>,
        start_offset: usize,
        start_kind: TokenKind,
    ) -> Option<usize> {
        let target_end = match start_kind {
            TokenKind::FlowSeqStart => TokenKind::FlowSeqEnd,
            TokenKind::FlowMapStart => TokenKind::FlowMapEnd,
            _ => return None,
        };

        let mut depth = 0;
        let mut i = start_offset;
        while let Some(kind) = window.kind(i) {
            match kind {
                TokenKind::FlowSeqStart | TokenKind::FlowMapStart => depth += 1,
                TokenKind::FlowSeqEnd | TokenKind::FlowMapEnd => {
                    depth -= 1;
                    if depth == 0 && kind == target_end {
                        return Some(i);
                    }
                }
                TokenKind::DocEnd | TokenKind::DocStart => return None,
                _ => {}
            }
            i += 1;
            if i > start_offset + 200 {
                break;
            }
        }
        None
    }

    pub(super) fn scan_is_implicit_key(window: &LookaheadWindow<'_, 'input>) -> bool {
        let mut i = 0;
        let mut seen_property = false;

        loop {
            match window.kind(i) {
                Some(TokenKind::Anchor | TokenKind::Tag) => {
                    seen_property = true;
                    i += 1;
                }
                Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => {
                    i += 1;
                }
                Some(TokenKind::LineStart) if seen_property => {
                    i += 1;
                }
                _ => break,
            }
            if i > 20 {
                return false;
            }
        }

        match window.kind(i) {
            Some(TokenKind::Plain) => window
                .token(i)
                .and_then(|token| match token {
                    Token::Plain(plain) => Some(plain.terminated_by_mapping_value_indicator()),
                    _ => None,
                })
                .unwrap_or(false),
            Some(TokenKind::StringStart) => {
                let Some(end) = Self::scan_find_string_end_from(window, i + 1, 200) else {
                    return false;
                };
                let colon_offset = Self::scan_skip_inline_whitespace_from(window, end + 1);
                window.kind(colon_offset) == Some(TokenKind::Colon)
            }
            Some(TokenKind::Alias) => {
                let colon_offset = Self::scan_skip_inline_whitespace_from(window, i + 1);
                window.kind(colon_offset) == Some(TokenKind::Colon)
            }
            _ => false,
        }
    }

    pub(super) fn scan_line_start_is_blank_from(
        window: &LookaheadWindow<'_, 'input>,
        start_offset: usize,
    ) -> bool {
        let mut i = start_offset;
        loop {
            match window.kind(i) {
                Some(
                    TokenKind::Whitespace | TokenKind::WhitespaceWithTabs | TokenKind::Comment,
                ) => i += 1,
                Some(TokenKind::LineStart | TokenKind::DocStart | TokenKind::DocEnd) | None => {
                    return true;
                }
                Some(_) => return false,
            }
        }
    }

    pub(super) fn scan_is_flow_seq_complex_key(window: &LookaheadWindow<'_, 'input>) -> bool {
        if window.kind(0) != Some(TokenKind::FlowSeqStart) {
            return false;
        }

        Self::scan_find_flow_collection_end(window, 0, TokenKind::FlowSeqStart)
            .map(|end| Self::scan_skip_flow_key_trivia_from(window, end + 1, false))
            .is_some_and(|offset| window.kind(offset) == Some(TokenKind::Colon))
    }

    pub(super) fn scan_is_flow_seq_inline_complex_key(
        window: &LookaheadWindow<'_, 'input>,
    ) -> bool {
        if window.kind(0) != Some(TokenKind::FlowSeqStart) {
            return false;
        }

        Self::scan_find_flow_collection_end(window, 0, TokenKind::FlowSeqStart)
            .is_some_and(|end| window.kind(end + 1) == Some(TokenKind::Colon))
    }

    pub(super) fn scan_is_flow_map_complex_key(window: &LookaheadWindow<'_, 'input>) -> bool {
        if window.kind(0) != Some(TokenKind::FlowMapStart) {
            return false;
        }

        Self::scan_find_flow_collection_end(window, 0, TokenKind::FlowMapStart)
            .map(|end| Self::scan_skip_flow_key_trivia_from(window, end + 1, true))
            .is_some_and(|offset| window.kind(offset) == Some(TokenKind::Colon))
    }

    pub(super) fn scan_is_flow_map_inline_complex_key(
        window: &LookaheadWindow<'_, 'input>,
    ) -> bool {
        if window.kind(0) != Some(TokenKind::FlowMapStart) {
            return false;
        }

        Self::scan_find_flow_collection_end(window, 0, TokenKind::FlowMapStart)
            .is_some_and(|end| window.kind(end + 1) == Some(TokenKind::Colon))
    }

    pub(super) fn scan_is_implicit_flow_mapping_entry(
        window: &LookaheadWindow<'_, 'input>,
    ) -> bool {
        let mut i = Self::scan_skip_flow_entry_prefix_from(window, 0);

        match window.kind(i) {
            Some(TokenKind::Plain) => {
                i = Self::scan_skip_flow_plain_continuations_from(window, i);
                window.kind(i) == Some(TokenKind::Colon)
            }
            Some(TokenKind::StringStart) => {
                let Some(end) = Self::scan_find_string_end_from(window, i + 1, 200) else {
                    return false;
                };
                let colon_offset = Self::scan_skip_inline_whitespace_from(window, end + 1);
                window.kind(colon_offset) == Some(TokenKind::Colon)
            }
            Some(TokenKind::FlowSeqStart) => {
                let Some(end) =
                    Self::scan_find_flow_collection_end(window, i, TokenKind::FlowSeqStart)
                else {
                    return false;
                };
                let colon_offset = Self::scan_skip_flow_key_trivia_from(window, end + 1, false);
                window.kind(colon_offset) == Some(TokenKind::Colon)
            }
            Some(TokenKind::FlowMapStart) => {
                let Some(end) =
                    Self::scan_find_flow_collection_end(window, i, TokenKind::FlowMapStart)
                else {
                    return false;
                };
                let colon_offset = Self::scan_skip_flow_key_trivia_from(window, end + 1, false);
                window.kind(colon_offset) == Some(TokenKind::Colon)
            }
            Some(TokenKind::Alias) => {
                let colon_offset = Self::scan_skip_inline_whitespace_from(window, i + 1);
                window.kind(colon_offset) == Some(TokenKind::Colon)
            }
            Some(TokenKind::Colon) => true,
            _ => false,
        }
    }

    pub(super) fn scan_has_continuation_after_low_indent(
        window: &LookaheadWindow<'_, 'input>,
        min_indent: IndentLevel,
    ) -> bool {
        let mut i = 1;
        while let Some(kind) = window.kind(i) {
            match kind {
                TokenKind::LineStart => {
                    let Some(Token::LineStart(indent)) = window.token(i) else {
                        debug_assert!(false, "expected LineStart token");
                        return false;
                    };
                    if *indent >= min_indent {
                        return true;
                    }
                    i += 1;
                }
                TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => i += 1,
                TokenKind::Plain => return !Self::scan_is_mapping_key_at_offset(window, i),
                _ => return false,
            }
            if i > 20 {
                break;
            }
        }
        false
    }

    pub(super) fn scan_is_mapping_key_at_offset(
        window: &LookaheadWindow<'_, 'input>,
        offset: usize,
    ) -> bool {
        window
            .token(offset)
            .and_then(|token| match token {
                Token::Plain(plain) => Some(plain.terminated_by_mapping_value_indicator()),
                _ => None,
            })
            .unwrap_or(false)
    }

    pub(super) fn scan_is_anchor_tag_mapping_key(window: &LookaheadWindow<'_, 'input>) -> bool {
        let mut i = 0;
        while matches!(
            window.kind(i),
            Some(
                TokenKind::Anchor
                    | TokenKind::Tag
                    | TokenKind::Whitespace
                    | TokenKind::WhitespaceWithTabs
                    | TokenKind::LineStart
            )
        ) {
            i += 1;
        }

        match window.kind(i) {
            Some(TokenKind::Plain) => window
                .token(i)
                .and_then(|token| match token {
                    Token::Plain(plain) => Some(plain.terminated_by_mapping_value_indicator()),
                    _ => None,
                })
                .unwrap_or(false),
            Some(TokenKind::StringStart) => {
                let Some(end) = Self::scan_find_string_end_from(window, i + 1, 50) else {
                    return false;
                };
                let colon_offset = Self::scan_skip_inline_whitespace_from(window, end + 1);
                window.kind(colon_offset) == Some(TokenKind::Colon)
            }
            _ => false,
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Flow Sequence
    // ─────────────────────────────────────────────────────────────

    #[allow(clippy::too_many_lines, reason = "Complex state machine dispatch")]
    pub(super) fn process_flow_seq(
        &mut self,
        mut phase: FlowSeqPhase,
        start_span: Span,
    ) -> Option<Event<'input>> {
        loop {
            match phase {
                FlowSeqPhase::BeforeEntry => {
                    self.skip_ws_and_newlines();

                    match self.peek_kind_with_span() {
                        Some((TokenKind::FlowSeqEnd, span)) => {
                            let flow_end = span.end;
                            let _ = self.take_current();
                            self.exit_flow_collection();
                            if self.flow_depth == 0 {
                                self.check_content_after_flow(flow_end);
                                self.check_multiline_flow_key(start_span, span);
                            }
                            return Some(Event::SequenceEnd { span });
                        }

                        Some((TokenKind::Comma, comma_span)) => {
                            self.error(ErrorKind::MissingSeparator, comma_span);
                            let _ = self.take_current();
                        }

                        Some((TokenKind::DocStart | TokenKind::DocEnd, span)) => {
                            self.error(ErrorKind::DocumentMarkerInFlow, span);
                            let _ = self.take_current();
                        }

                        Some((TokenKind::MappingKey, _)) => {
                            // Explicit key inside flow sequence: [ ? key : value ]
                            // This creates a flow mapping entry
                            let map_start_span = self.current_span();
                            let _ = self.take_current(); // consume ?
                            self.set_pending_ast_wrap(PendingAstWrap::SequenceItem {
                                item_start: map_start_span.start,
                            });
                            self.skip_ws_and_newlines();

                            // Push states in reverse order:
                            // 1. After the mapping, continue with AfterEntry
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::AfterEntry,
                                start_span,
                            });
                            // 2. Emit MappingEnd after value
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::ImplicitMapEnd,
                                start_span,
                            });
                            // 3. Parse the value (after we see colon)
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::ImplicitMapValue,
                                start_span,
                            });
                            // 4. Parse the key
                            self.state_stack.push(ParseState::Value {
                                ctx: ValueContext {
                                    min_indent: 0,
                                    content_column: None,
                                    kind: ValueKind::ExplicitKey,
                                    allow_implicit_mapping: true, // Flow context - doesn't affect block mappings
                                    prior_crossed_line: false,
                                },
                                properties: EmitterProperties::default(),
                            });

                            // Emit MappingStart for the explicit mapping (flow style)
                            return Some(Event::MappingStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: None,
                                span: map_start_span,
                            });
                        }

                        Some(_) => {
                            // Check for implicit flow mapping: [ key: value ]
                            if self.is_implicit_flow_mapping_entry() {
                                // Get span for MappingStart (current position)
                                let map_start_span = self.current_span();

                                // Push states in reverse order:
                                // 1. After the implicit mapping, continue with AfterEntry
                                self.state_stack.push(ParseState::FlowSeq {
                                    phase: FlowSeqPhase::AfterEntry,
                                    start_span,
                                });
                                // 2. Emit MappingEnd after value
                                self.state_stack.push(ParseState::FlowSeq {
                                    phase: FlowSeqPhase::ImplicitMapEnd,
                                    start_span,
                                });
                                // 3. Parse the value (after we see colon)
                                self.state_stack.push(ParseState::FlowSeq {
                                    phase: FlowSeqPhase::ImplicitMapValue,
                                    start_span,
                                });

                                // Check if this is an empty key (colon directly without key)
                                // Pattern: [ : value ] where colon is at current position
                                if self.peek_kind() == Some(TokenKind::Colon) {
                                    // 4. Emit null key then MappingStart
                                    self.state_stack.push(ParseState::FlowSeq {
                                        phase: FlowSeqPhase::ImplicitMapEmptyKey { map_start_span },
                                        start_span,
                                    });
                                } else {
                                    // 4. Parse the key (non-empty)
                                    self.state_stack.push(ParseState::Value {
                                        ctx: ValueContext {
                                            min_indent: 0,
                                            content_column: None,
                                            kind: ValueKind::ImplicitKey, // This is the key of the implicit mapping
                                            allow_implicit_mapping: true, // Flow context
                                            prior_crossed_line: false,
                                        },
                                        properties: EmitterProperties::default(),
                                    });
                                }

                                // Emit MappingStart for the implicit mapping
                                self.set_pending_ast_wrap(PendingAstWrap::SequenceItem {
                                    item_start: map_start_span.start,
                                });
                                return Some(Event::MappingStart {
                                    style: crate::event::CollectionStyle::Flow,
                                    properties: None,
                                    span: map_start_span,
                                });
                            }

                            // Regular entry - parse as value
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::AfterEntry,
                                start_span,
                            });
                            self.set_pending_ast_wrap(PendingAstWrap::SequenceItem {
                                item_start: self.current_span().start,
                            });
                            self.state_stack.push(ParseState::Value {
                                ctx: ValueContext {
                                    min_indent: 0,
                                    content_column: None,
                                    kind: ValueKind::SeqEntryValue, // Sequence entry is a value
                                    allow_implicit_mapping: true,   // Flow context
                                    prior_crossed_line: false,
                                },
                                properties: EmitterProperties::default(),
                            });
                            return None;
                        }

                        None => {
                            // Unterminated
                            self.error(ErrorKind::UnexpectedEof, start_span);
                            self.exit_flow_collection();
                            return Some(Event::SequenceEnd {
                                span: self.current_span(),
                            });
                        }
                    }
                }

                FlowSeqPhase::ImplicitMapEmptyKey { map_start_span } => {
                    return Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: None,
                        span: map_start_span,
                    });
                }

                FlowSeqPhase::ImplicitMapValue => {
                    self.skip_ws_and_newlines();

                    if self.peek_kind() == Some(TokenKind::Colon) {
                        let _ = self.take_current();
                        self.skip_ws_and_newlines();

                        if matches!(
                            self.peek_kind(),
                            Some(TokenKind::Comma | TokenKind::FlowSeqEnd)
                        ) {
                            return Some(self.emit_null());
                        }

                        self.state_stack.push(ParseState::Value {
                            ctx: ValueContext {
                                min_indent: 0,
                                content_column: None,
                                kind: ValueKind::MappingValue,
                                allow_implicit_mapping: true,
                                prior_crossed_line: false,
                            },
                            properties: EmitterProperties::default(),
                        });
                        return None;
                    }

                    self.error(ErrorKind::MissingColon, self.mapping_key_insertion_span());
                    return Some(Event::InvalidatePair {
                        span: self.mapping_key_insertion_span(),
                    });
                }

                FlowSeqPhase::ImplicitMapEnd => {
                    return Some(Event::MappingEnd {
                        span: self.collection_end_span(),
                    });
                }

                FlowSeqPhase::AfterEntry => {
                    self.skip_ws_and_newlines();

                    match self.peek_kind_with_span() {
                        Some((TokenKind::Comma, _)) => {
                            let _ = self.take_current();
                            phase = FlowSeqPhase::BeforeEntry;
                        }

                        Some((TokenKind::FlowSeqEnd, span)) => {
                            let flow_end = span.end;
                            let _ = self.take_current();
                            self.exit_flow_collection();
                            if self.flow_depth == 0 {
                                self.check_content_after_flow(flow_end);
                                self.check_multiline_flow_key(start_span, span);
                            }
                            return Some(Event::SequenceEnd { span });
                        }

                        Some((TokenKind::FlowMapEnd, span)) => {
                            // A `}` cannot terminate a flow sequence. Close the
                            // current sequence and leave the token for the
                            // enclosing context instead of retrying on the same
                            // token forever.
                            self.error(ErrorKind::MismatchedBrackets, span);
                            self.exit_flow_collection();
                            return Some(Event::SequenceEnd {
                                span: self.collection_end_span(),
                            });
                        }

                        Some((TokenKind::DocStart | TokenKind::DocEnd, span)) => {
                            self.error(ErrorKind::DocumentMarkerInFlow, span);
                            let _ = self.take_current();
                        }

                        Some(_) => {
                            self.error(ErrorKind::MissingSeparator, self.collection_end_span());
                            phase = FlowSeqPhase::BeforeEntry;
                        }

                        None => {
                            self.error(ErrorKind::UnexpectedEof, start_span);
                            self.exit_flow_collection();
                            return Some(Event::SequenceEnd {
                                span: self.current_span(),
                            });
                        }
                    }
                }
            }
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Flow Mapping
    // ─────────────────────────────────────────────────────────────

    /// Handle `---` / `...` inside a flow mapping phase.
    ///
    /// Centralizes the `DocumentMarkerInFlow` behaviour for `FlowMap` states:
    /// - Record the error
    /// - Consume the marker token
    /// - Re-push the current `FlowMap` state to continue parsing.
    pub(super) fn handle_doc_marker_in_flow_map(
        &mut self,
        start_span: Span,
        doc_span: Span,
        phase: FlowMapPhase,
    ) -> Option<Event<'input>> {
        self.error(ErrorKind::DocumentMarkerInFlow, doc_span);
        let _ = self.take_current();
        self.state_stack
            .push(ParseState::FlowMap { phase, start_span });
        None
    }

    #[allow(clippy::too_many_lines, reason = "Complex state machine dispatch")]
    pub(super) fn process_flow_map(
        &mut self,
        phase: FlowMapPhase,
        start_span: Span,
    ) -> Option<Event<'input>> {
        match phase {
            FlowMapPhase::BeforeKey => {
                self.skip_ws_and_newlines();

                match self.peek_kind_with_span() {
                    Some((TokenKind::FlowMapEnd, span)) => {
                        let flow_end = span.end;
                        let _ = self.take_current();
                        self.exit_flow_collection();
                        // Check for content immediately after flow collection in block context
                        if self.flow_depth == 0 {
                            self.check_content_after_flow(flow_end);
                            // Check for multiline implicit key (flow collection spanning lines)
                            self.check_multiline_flow_key(start_span, span);
                        }
                        Some(Event::MappingEnd { span })
                    }

                    Some((TokenKind::Comma, comma_span)) => {
                        // Consecutive comma - report MissingSeparator
                        // (BeforeKey is entered after `{` or after consuming a comma,
                        // so seeing another comma means consecutive commas)
                        self.error(ErrorKind::MissingSeparator, comma_span);
                        let _ = self.take_current();
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::BeforeKey,
                            start_span,
                        });
                        None // Don't emit null for consecutive commas
                    }

                    Some((TokenKind::MappingKey, _)) => {
                        let pair_start_span = self.current_span();
                        let _ = self.take_current();
                        self.set_pending_ast_wrap(PendingAstWrap::MappingPair {
                            pair_start: pair_start_span.start,
                        });
                        self.skip_ws();
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterKey,
                            start_span,
                        });
                        self.state_stack.push(ParseState::Value {
                            ctx: ValueContext {
                                min_indent: 0,
                                content_column: None,
                                kind: ValueKind::ExplicitKey, // Flow mapping key
                                allow_implicit_mapping: true, // Flow context
                                prior_crossed_line: false,
                            },
                            properties: EmitterProperties::default(),
                        });
                        None
                    }

                    Some((TokenKind::Colon, _)) => {
                        // Null key
                        self.set_pending_ast_wrap(PendingAstWrap::MappingPair {
                            pair_start: self.current_span().start,
                        });
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterKey,
                            start_span,
                        });
                        Some(self.emit_null())
                    }

                    Some((TokenKind::DocStart | TokenKind::DocEnd, span)) => {
                        // Document markers inside flow context are invalid.
                        // Ignore them and continue parsing via a tiny helper.
                        self.handle_doc_marker_in_flow_map(
                            start_span,
                            span,
                            FlowMapPhase::BeforeKey,
                        )
                    }

                    Some(_) => {
                        // Implicit key
                        self.set_pending_ast_wrap(PendingAstWrap::MappingPair {
                            pair_start: self.current_span().start,
                        });
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterKey,
                            start_span,
                        });
                        self.state_stack.push(ParseState::Value {
                            ctx: ValueContext {
                                min_indent: 0,
                                content_column: None,
                                kind: ValueKind::ImplicitKey, // Flow mapping key
                                allow_implicit_mapping: true, // Flow context
                                prior_crossed_line: false,
                            },
                            properties: EmitterProperties::default(),
                        });
                        None
                    }
                    None => {
                        self.error(ErrorKind::UnexpectedEof, start_span);
                        self.exit_flow_collection();
                        Some(Event::MappingEnd {
                            span: self.current_span(),
                        })
                    }
                }
            }

            FlowMapPhase::AfterKey => {
                self.skip_ws_and_newlines();

                if self.peek_kind() == Some(TokenKind::Colon) {
                    let _ = self.take_current();
                    self.skip_ws_and_newlines();

                    // Check for empty value
                    if matches!(
                        self.peek_kind(),
                        Some(TokenKind::Comma | TokenKind::FlowMapEnd)
                    ) {
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterValue,
                            start_span,
                        });
                        Some(self.emit_null())
                    } else {
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterValue,
                            start_span,
                        });
                        self.state_stack.push(ParseState::Value {
                            ctx: ValueContext {
                                min_indent: 0,
                                content_column: None,
                                kind: ValueKind::MappingValue, // Flow mapping value
                                allow_implicit_mapping: true,  // Flow context
                                prior_crossed_line: false,
                            },
                            properties: EmitterProperties::default(),
                        });
                        None
                    }
                } else if matches!(
                    self.peek_kind(),
                    Some(TokenKind::Comma | TokenKind::FlowMapEnd)
                ) {
                    // YAML flow mappings allow omitted values, so `key,` and
                    // `key}` are valid shorthand for `key: null`.
                    self.state_stack.push(ParseState::FlowMap {
                        phase: FlowMapPhase::AfterValue,
                        start_span,
                    });
                    Some(self.emit_null())
                } else {
                    let span = self.mapping_key_insertion_span();
                    self.error(ErrorKind::MissingColon, span);
                    self.state_stack.push(ParseState::FlowMap {
                        phase: FlowMapPhase::AfterValue,
                        start_span,
                    });
                    Some(Event::InvalidatePair { span })
                }
            }

            FlowMapPhase::AfterValue => {
                self.skip_ws_and_newlines();

                match self.peek_kind_with_span() {
                    Some((TokenKind::Comma, _)) => {
                        let _ = self.take_current();
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::BeforeKey,
                            start_span,
                        });
                        None
                    }

                    Some((TokenKind::FlowMapEnd, span)) => {
                        let flow_end = span.end;
                        let _ = self.take_current();
                        self.exit_flow_collection();
                        // Check for content immediately after flow collection in block context
                        if self.flow_depth == 0 {
                            self.check_content_after_flow(flow_end);
                            // Check for multiline implicit key (flow collection spanning lines)
                            self.check_multiline_flow_key(start_span, span);
                        }
                        Some(Event::MappingEnd { span })
                    }

                    Some((TokenKind::FlowSeqEnd, span)) => {
                        // A `]` cannot terminate a flow mapping. Close the
                        // current mapping and leave the token for the enclosing
                        // sequence instead of re-entering recovery with no
                        // progress.
                        self.error(ErrorKind::MismatchedBrackets, span);
                        self.exit_flow_collection();
                        Some(Event::MappingEnd {
                            span: self.collection_end_span(),
                        })
                    }

                    Some((TokenKind::DocStart | TokenKind::DocEnd, span)) => {
                        // Document markers inside flow context - ignore and continue
                        self.handle_doc_marker_in_flow_map(
                            start_span,
                            span,
                            FlowMapPhase::AfterValue,
                        )
                    }

                    Some(_) => {
                        self.error(ErrorKind::MissingSeparator, self.collection_end_span());
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::BeforeKey,
                            start_span,
                        });
                        None
                    }

                    None => {
                        self.error(ErrorKind::UnexpectedEof, start_span);
                        self.exit_flow_collection();
                        Some(Event::MappingEnd {
                            span: self.current_span(),
                        })
                    }
                }
            }
        }
    }
}
