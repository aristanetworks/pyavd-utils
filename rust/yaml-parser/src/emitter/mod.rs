// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML event emitter with error recovery.
//!
//! This module provides `Emitter`, a YAML parser that produces events
//! using an explicit state stack instead of recursion. It consumes
//! tokens from the unified lexer via a small cursor abstraction and
//! emits events via the `Iterator` interface.
//!
//! The `Emitter` is validated against the YAML Test Suite to ensure
//! correct event sequences for all inputs.

mod cursor;
mod states;

use std::borrow::Cow;
use std::collections::HashSet;
use std::ops::ControlFlow;

use crate::ast_event::AstEvent;
use crate::error::{ErrorKind, ParseError};
use crate::event::{Comment, Event, Properties, Property, ScalarStyle};
use crate::lexer::{Token, TokenKind};
use crate::span::{BytePosition, IndentLevel, Span, usize_to_indent};

use cursor::{LookaheadWindow, TokenCursor};
use states::{
    BlockMapPhase, BlockSeqPhase, DocState, EmitterProperties, FlowMapPhase, FlowSeqPhase,
    ParseState, ValueContext, ValueKind,
};

/// Result of deciding what to do with collected properties at a dedented
/// indent: either emit an empty scalar now, or keep the properties attached to
/// the upcoming value.
#[derive(Debug)]
enum MaybeEmptyScalarDecision<'input> {
    /// Do not emit an empty scalar; continue parsing this value with the
    /// (possibly updated) properties.
    Continue {
        properties: EmitterProperties<'input>,
    },
    /// Emit an empty scalar event using the given properties, and stop
    /// parsing the current value.
    EmitEmptyScalar { event: Event<'input> },
}

/// Line classification used when folding block scalars.
#[derive(Clone, Copy, PartialEq, Debug)]
enum LineType {
    /// Regular content line
    Normal,
    /// No content on this line
    Empty,
    /// Line with extra indentation (preserve)
    MoreIndent,
}

#[derive(Debug, Clone, Copy)]
enum PendingAstWrap {
    SequenceItem { item_start: BytePosition },
    MappingPair { pair_start: BytePosition },
}

#[derive(Debug, Default)]
struct PendingAstWrapQueue {
    first: Option<PendingAstWrap>,
    second: Option<PendingAstWrap>,
}

impl PendingAstWrapQueue {
    fn clear(&mut self) {
        self.first = None;
        self.second = None;
    }

    fn push_back(&mut self, wrap: PendingAstWrap) {
        if self.first.is_none() {
            self.first = Some(wrap);
        } else if self.second.is_none() {
            self.second = Some(wrap);
        } else {
            debug_assert!(
                false,
                "pending AST wrap queue overflowed expected maximum depth"
            );
        }
    }

    fn pop_front(&mut self) -> Option<PendingAstWrap> {
        let first = self.first.take()?;
        self.first = self.second.take();
        Some(first)
    }
}

/// A YAML event emitter using an explicit state machine.
///
/// Processes tokens and produces YAML events via the `Iterator` interface.
/// Uses an explicit state stack instead of recursion for parsing.
///
/// ## Architecture Note
///
/// The emitter does not own the tokens directly; all tokenization and
/// buffering is handled by [`TokenCursor`], which in turn owns a
/// streaming [`Lexer`]. The emitter tracks only a logical position
/// (`pos`) into that cursor and focuses on higher-level YAML
/// structure, indentation, and error recovery.
pub struct Emitter<'input> {
    /// Token cursor wrapper. Provides read-only access helpers over the token stream.
    cursor: TokenCursor<'input>,
    /// Original input string.
    input: &'input str,
    /// Current position in tokens.
    pos: usize,
    /// Current line indent level.
    current_indent: IndentLevel,
    /// Flow depth (0 = block context).
    flow_depth: usize,
    /// Document-level state.
    doc_state: DocState,
    /// Parse state stack (replaces call stack).
    state_stack: Vec<ParseState<'input>>,
    /// Collected errors.
    errors: Vec<ParseError>,
    /// Defined anchors in current document.
    anchors: HashSet<&'input str>,
    /// Whether `StreamStart` has been emitted.
    emitted_stream_start: bool,
    /// Event already determined by the current state transition and scheduled
    /// to be emitted before any further parsing work.
    pending_event: Option<Event<'input>>,
    /// Tag handles from directives.
    tag_handles: std::collections::HashMap<&'input str, &'input str>,
    /// Last content span - used for `MappingEnd`/`SequenceEnd` to produce
    /// intuitive end spans based on the last piece of content rather than the
    /// next structural token.
    /// Updated when emitting content events (scalars, aliases, nested collection ends).
    last_content_span: Option<Span>,
    /// Whether we've crossed a line boundary since the last time this flag was cleared.
    /// Set when consuming `LineStart` tokens. Used by `AfterValue` to determine if
    /// a nested structure crossed a line, even after those tokens were consumed.
    crossed_line_boundary: bool,
    /// Indentation stack tracking active block structure levels.
    /// Each entry is the indentation level of an active block structure.
    /// Used to detect orphan indentation (content at levels not in the stack).
    indent_stack: Vec<IndentLevel>,
    /// Span of the last `LineStart` token that set `current_indent`.
    /// Used for reporting `InvalidIndentation` errors at the line start position.
    last_line_start_span: Span,
    /// Stack of columns where each flow context started.
    /// Used to validate that continuation lines are indented relative to the flow start.
    /// Empty when not in flow context.
    flow_context_columns: Vec<IndentLevel>,
    /// Structural AST metadata to apply to the next emitted node-start event.
    pending_ast_wraps: PendingAstWrapQueue,
    /// Comment captured after `key:` / `-` before the next node begins.
    pending_ast_leading_comment: Option<Comment<'input>>,
}

pub(crate) struct AstEmitter<'a, 'input> {
    emitter: &'a mut Emitter<'input>,
}

impl<'input> Emitter<'input> {
    /// Create a new emitter from raw input.
    ///
    /// This constructs an internal streaming lexer and cursor. Tokens are
    /// produced on demand as the emitter peeks and consumes them.
    #[must_use]
    pub fn new(input: &'input str) -> Self {
        Self {
            cursor: TokenCursor::new(input),
            input,
            pos: 0,
            current_indent: 0,
            flow_depth: 0,
            doc_state: DocState::Ready,
            state_stack: Vec::with_capacity(16),
            errors: Vec::new(),
            anchors: HashSet::new(),
            emitted_stream_start: false,
            pending_event: None,
            tag_handles: std::collections::HashMap::new(),
            last_content_span: None,
            crossed_line_boundary: false,
            indent_stack: vec![0],                 // Start with base level 0
            last_line_start_span: Span::new(0..0), // Default span at start
            flow_context_columns: Vec::new(),
            pending_ast_wraps: PendingAstWrapQueue::default(),
            pending_ast_leading_comment: None,
        }
    }

    pub(crate) fn ast_events(&mut self) -> AstEmitter<'_, 'input> {
        AstEmitter { emitter: self }
    }

    /// Take collected errors from both lexer and emitter.
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        let mut all = self.cursor.take_lexer_errors();
        all.extend(std::mem::take(&mut self.errors));
        all
    }

    fn set_pending_event(&mut self, event: Event<'input>) {
        debug_assert!(
            self.pending_event.is_none(),
            "pending_event slot unexpectedly occupied"
        );
        self.pending_event = Some(event);
    }

    fn set_pending_ast_wrap(&mut self, wrap: PendingAstWrap) {
        self.pending_ast_wraps.push_back(wrap);
    }

    fn wrap_ast_event(&mut self, event: Event<'input>) -> AstEvent<'input> {
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

    fn discard_pending_ast_wrap(&mut self) {
        let _ = self.pending_ast_wraps.pop_front();
    }

    fn set_pending_ast_leading_comment(&mut self, comment: Comment<'input>) {
        self.pending_ast_leading_comment = Some(comment);
    }

    fn take_comment_token(&mut self) -> Option<Comment<'input>> {
        let Some((Token::Comment(text), span)) = self.take_current() else {
            return None;
        };
        Some(Comment { text, span })
    }

    fn take_same_line_comment_after_ws(&mut self) -> Option<Comment<'input>> {
        let mut offset = 0;
        while matches!(
            self.peek_kind_nth(offset),
            Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs)
        ) {
            offset += 1;
        }

        if self.peek_kind_nth(offset) != Some(TokenKind::Comment) {
            return None;
        }

        for _ in 0..offset {
            let _ = self.take_current();
        }

        self.take_comment_token()
    }

    fn track_emitted_event(&mut self, event: &Event<'input>) {
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

    fn mapping_key_insertion_span(&self) -> Span {
        self.last_content_span.map_or_else(
            || Span::at(self.current_span().start),
            |span| Span::at(span.end),
        )
    }

    // ─────────────────────────────────────────────────────────────
    // Token access helpers
    // ─────────────────────────────────────────────────────────────

    /// View the current token at the logical position `pos` without consuming it.
    ///
    /// Conceptually this is a "current token" accessor; the emitter advances the
    /// position explicitly via [`take_current`] (or helpers that call it) when it
    /// wants to consume a token.
    #[inline]
    fn peek(&self) -> Option<(Token<'input>, Span)> {
        self.cursor.peek(self.pos)
    }

    #[inline]
    fn with_lookahead<R>(
        &self,
        max_offset: usize,
        func: impl FnOnce(LookaheadWindow<'_, 'input>) -> R,
    ) -> R {
        self.cursor.with_window(self.pos, max_offset, func)
    }

    #[inline]
    fn peek_nth(&self, n: usize) -> Option<(Token<'input>, Span)> {
        self.cursor.peek_nth(self.pos, n)
    }

    /// Peek at the current token and apply a function to it without cloning.
    ///
    /// This is more efficient than `peek()` when you don't need to keep
    /// the token, as it avoids cloning `Cow<str>` data.
    #[inline]
    fn peek_with<F, R>(&self, func: F) -> Option<R>
    where
        F: FnOnce(&Token<'input>, Span) -> R,
    {
        self.cursor.peek_with(self.pos, func)
    }

    /// Peek `n` tokens ahead and apply a function to it without cloning.
    #[inline]
    fn peek_nth_with<F, R>(&self, n: usize, func: F) -> Option<R>
    where
        F: FnOnce(&Token<'input>, Span) -> R,
    {
        self.cursor.peek_nth_with(self.pos, n, func)
    }

    /// Take the current token at `pos`, advancing the logical position.
    ///
    /// This is the single point where we update line/indent tracking based on
    /// `LineStart` tokens. Callers that conceptually "take" the current
    /// token should use this helper; if they don't need the token value,
    /// they can simply ignore the return value.
    #[inline]
    fn take_current(&mut self) -> Option<(Token<'input>, Span)> {
        // Use `take` instead of `peek` to avoid cloning the token.
        // The token is replaced with a cheap dummy sentinel in the buffer.
        if let Some((token, span)) = self.cursor.take(self.pos) {
            // Track indent from LineStart tokens
            if let Token::LineStart(n) = &token {
                self.current_indent = *n;
                self.last_line_start_span = span;
                // Mark that we crossed a line boundary - this persists until cleared
                self.crossed_line_boundary = true;
            }
            self.pos += 1;
            Some((token, span))
        } else {
            None
        }
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.cursor.is_eof(self.pos)
    }

    #[inline]
    fn current_span(&self) -> Span {
        self.cursor.current_span(self.pos)
    }

    /// Check the current token's kind without cloning.
    #[inline]
    fn peek_kind(&self) -> Option<TokenKind> {
        self.cursor.peek_kind(self.pos)
    }

    /// Get the current token kind and span without cloning the token payload.
    #[inline]
    fn peek_kind_with_span(&self) -> Option<(TokenKind, Span)> {
        self.peek_with(|tok, span| (TokenKind::from(tok), span))
    }

    /// Check the kind of token `n` positions ahead without cloning.
    #[inline]
    fn peek_kind_nth(&self, n: usize) -> Option<TokenKind> {
        self.cursor.peek_kind_nth(self.pos, n)
    }

    /// If the current token is `LineStart(n)`, return `Some((n, span))` without cloning.
    /// Otherwise return `None`.
    #[inline]
    fn peek_line_start(&self) -> Option<(IndentLevel, Span)> {
        self.peek_with(|tok, span| {
            if let Token::LineStart(n) = tok {
                Some((*n, span))
            } else {
                None
            }
        })
        .flatten()
    }

    /// Get span for collection end events (MappingEnd/SequenceEnd).
    /// Uses the last content span if available, otherwise falls back to current span.
    /// End spans point to the end of the last content rather than the next token
    /// (like a newline or dedent).
    fn collection_end_span(&self) -> Span {
        if let Some(span) = self.last_content_span {
            // Use end position of last content as a point span
            Span::new(span.end..span.end)
        } else {
            // Fallback: use end of previous token if available, otherwise the
            // current span. This keeps behaviour consistent with the
            // token-slice-based implementation while routing all token access
            // through the cursor.
            if self.pos > 0 {
                let prev_span = self.cursor.current_span(self.pos - 1);
                Span::from_usize_range(prev_span.end_usize()..prev_span.end_usize())
            } else {
                self.current_span()
            }
        }
    }

    fn indented_line_error_span(line_span: Span, indent: IndentLevel) -> Span {
        let width = usize::from(indent);
        if width == 0 {
            line_span
        } else {
            let end = line_span.end_usize();
            Span::from_usize_range(end.saturating_sub(width)..end)
        }
    }

    /// Enter a flow collection context.
    /// Tracks the column where the flow collection started (for `InvalidIndentationContext` errors).
    fn enter_flow_collection(&mut self, flow_start_column: Option<IndentLevel>) {
        self.flow_depth += 1;
        self.flow_context_columns
            .push(flow_start_column.unwrap_or(self.current_indent));
    }

    /// Exit a flow collection context.
    fn exit_flow_collection(&mut self) {
        self.flow_depth = self.flow_depth.saturating_sub(1);
        self.flow_context_columns.pop();
    }

    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError { kind, span });
    }

    /// Report an error only if no error has already been reported for this span.
    /// This prevents double-reporting when the same content is processed by multiple
    /// error detection paths (e.g., orphaned properties detection and trailing content scan).
    fn error_unless_span_has_error(&mut self, kind: ErrorKind, span: Span) {
        // Check if any error already has this span
        if !self.errors.iter().any(|err| err.span == span) {
            self.error(kind, span);
        }
    }

    /// Check if a span contains a newline (used for multiline implicit key detection).
    /// If the key spans multiple lines, it cannot be an implicit key.
    fn check_multiline_implicit_key(&mut self, key_span: Span) {
        // Multiline implicit keys are only an error in block context
        // In flow context, multiline keys are allowed
        if self.flow_depth > 0 {
            return;
        }

        // Get the key text from the input
        let start = key_span.start_usize();
        let end = key_span.end_usize().min(self.input.len());
        if start >= end {
            return;
        }
        #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
        let key_text = &self.input[start..end];

        // Check if the key text contains a newline
        if key_text.contains('\n') {
            // Find the colon span for error reporting (look ahead for colon)
            let colon_span = (0..10)
                .find_map(|i| {
                    self.peek_nth_with(i, |tok, span| matches!(tok, Token::Colon).then_some(span))
                        .flatten()
                })
                .unwrap_or(key_span);

            self.error(ErrorKind::MultilineImplicitKey, colon_span);
        }
    }

    /// Check if a flow collection used as an implicit key spans multiple lines.
    /// This is called when a flow collection ends in block context.
    fn check_multiline_flow_key(&mut self, start_span: Span, end_span: Span) {
        // Check if there's a colon immediately following (implicit key indicator).
        // If no colon is found immediately (only whitespace allowed), this is not an implicit key.
        let mut check_idx = 0;
        let colon_span = loop {
            match self.peek_kind_nth(check_idx) {
                Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => check_idx += 1,
                Some(TokenKind::Colon) => {
                    break self
                        .peek_nth_with(check_idx, |_, span| span)
                        .unwrap_or_else(|| self.current_span());
                }
                _ => return, // No colon immediately - not an implicit key
            }
            if check_idx > 20 {
                return;
            }
        };

        // Check if the flow collection spans multiple lines
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

    /// Check for tabs used as indentation after a `LineStart` token.
    /// In block context, tabs used for indentation are invalid.
    fn check_tabs_as_indentation(&mut self) {
        // Only check in block context
        if self.flow_depth > 0 {
            return;
        }

        // Tabs after a line that increased indentation via spaces are separation
        // whitespace, not indentation. This preserves cases like DK95-00 where a
        // value line starts with enough spaces for the new block indent and then
        // uses tabs before the scalar content.
        let active_indent = *self.indent_stack.last().unwrap_or(&0);
        if self.current_indent > active_indent {
            return;
        }

        // Check if current token is WhitespaceWithTabs immediately after LineStart.
        if self.peek_kind_nth(0) == Some(TokenKind::WhitespaceWithTabs) {
            let tab_span = self
                .peek_nth_with(0, |_, span| span)
                .unwrap_or_else(|| self.current_span());
            // Look ahead to see what follows the whitespace (starting after the tab)
            let mut look_ahead = 1;
            while let Some(kind) = self.peek_kind_nth(look_ahead) {
                match kind {
                    TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => look_ahead += 1,
                    // Tabs allowed before:
                    // - Flow collection start/end (entering/exiting flow)
                    // - Blank line (line has only whitespace)
                    TokenKind::FlowMapStart
                    | TokenKind::FlowMapEnd
                    | TokenKind::FlowSeqStart
                    | TokenKind::FlowSeqEnd
                    | TokenKind::LineStart => return,
                    // Any other content - tabs used for indentation, which is invalid
                    _ => break,
                }
            }
            // If we exhausted tokens (EOF), tabs are allowed (trailing whitespace)
            if self.peek_kind_nth(look_ahead).is_none() {
                return;
            }
            // Content after tabs - report error
            self.error(ErrorKind::InvalidIndentation, tab_span);
        }
    }

    /// Check for tabs at column 0 in flow context.
    /// In flow context, tabs at the start of a line (column 0) are invalid (Y79Y-003),
    /// but tabs after spaces are allowed (6HB6).
    fn check_tabs_at_column_zero_in_flow(&mut self) {
        // Only check if current token is WhitespaceWithTabs at column 0.
        // Called right after consuming a LineStart, so the tab (if present) is
        // the very next token — its column equals current_indent.
        if self.peek_kind() == Some(TokenKind::WhitespaceWithTabs) && self.current_indent == 0 {
            let tab_span = self.current_span();
            // Look ahead to check it's not a blank line
            let mut look_ahead = 1;
            while let Some(kind) = self.peek_kind_nth(look_ahead) {
                match kind {
                    TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => look_ahead += 1,
                    // Blank line or flow end - tabs allowed
                    TokenKind::LineStart | TokenKind::FlowMapEnd | TokenKind::FlowSeqEnd => {
                        return;
                    }
                    // Content at column 0 with leading tab - error
                    _ => {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                        return;
                    }
                }
            }
        }
    }

    /// Check for tabs after block indicators (`:`, `?`, `-`).
    /// Tabs are allowed as separation space before scalar content, but invalid
    /// before block structure indicators (`-`, `?`, `:`) as they would make
    /// the structure appear indented.
    fn check_tabs_after_block_indicator(&mut self) {
        // Only check in block context
        if self.flow_depth > 0 {
            return;
        }

        // Check if current token is WhitespaceWithTabs
        if self.peek_kind() == Some(TokenKind::WhitespaceWithTabs) {
            let tab_span = self.current_span();
            // Look ahead to see what follows
            let mut lookahead = 1;
            while let Some(kind) = self.peek_kind_nth(lookahead) {
                match kind {
                    TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => lookahead += 1,
                    // Block structure indicators after tab - error (ambiguous indentation)
                    TokenKind::BlockSeqIndicator | TokenKind::MappingKey | TokenKind::Colon => {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                        return;
                    }
                    // Scalar followed by colon - this creates an implicit mapping (like `key:`)
                    // which is also ambiguous after tabs
                    TokenKind::Plain => {
                        // Check if the scalar is followed by a colon
                        if self.peek_kind_nth(lookahead + 1) == Some(TokenKind::Colon) {
                            self.error(ErrorKind::InvalidIndentation, tab_span);
                            return;
                        }
                        // Simple scalar without colon - allowed
                        return;
                    }
                    // Other content after tab - allowed (separation space)
                    _ => return,
                }
            }
        }
    }

    /// Check for invalid content immediately after a flow collection in block context.
    /// e.g., `{ y: z }in: valid` - `in:` is immediately after `}` with no space.
    fn check_content_after_flow(&mut self, flow_end: usize) {
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
            if span.start_usize() == flow_end {
                self.error(ErrorKind::ContentOnSameLine, span);
            }
        }
    }

    /// Check for trailing content at column 0 after a closed structure.
    /// This is used for block sequences at the root level.
    /// Content at the same indentation level after them is invalid.
    fn check_trailing_content_at_root(&mut self, root_indent: IndentLevel) {
        // Skip whitespace, newlines, and comments using peek-only.
        // Track the indent of the last peeked LineStart so we can determine
        // the column of any content found without span arithmetic.
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

        // Check if there's content at or below the root indentation.
        // Content is at column `last_peeked_indent` if no whitespace followed the
        // last LineStart; otherwise it's further right (> root_indent for root_indent=0).
        if let Some(kind) = self.peek_kind_nth(peek_offset) {
            let span = self
                .peek_nth_with(peek_offset, |_, span| span)
                .unwrap_or_else(|| self.current_span());
            // Extra flow collection end tokens are always an error (unmatched brackets)
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

                // The content column is at least last_peeked_indent.
                // If there was whitespace after the last LineStart, it's further right.
                let col = if ws_after_linestart {
                    // Content is past whitespace — definitely > last_peeked_indent
                    last_peeked_indent + 1
                } else {
                    last_peeked_indent
                };

                if is_content && col <= root_indent {
                    // At document level, all trailing content is TrailingContent
                    self.error(ErrorKind::TrailingContent, span);
                }
            }
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Whitespace/newline skipping
    // ─────────────────────────────────────────────────────────────

    /// Skip whitespace tokens, returning the total number of columns consumed.
    fn skip_ws(&mut self) -> IndentLevel {
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

    fn skip_ws_and_newlines(&mut self) {
        self.skip_ws_and_newlines_impl();
    }

    /// Skip whitespace, newlines, indentation tokens, and comments.
    /// Returns `(crossed_line, ws_width)` where:
    /// - `crossed_line`: true if a `LineStart` token was encountered
    /// - `ws_width`: whitespace consumed on the final line (reset on each line crossing)
    fn skip_ws_and_newlines_tracked(&mut self) -> (bool, IndentLevel) {
        let (crossed, _, ws_width) = self.skip_ws_and_newlines_impl();
        (crossed, ws_width)
    }

    /// Core implementation of `skip_ws_and_newlines`.
    /// Returns (`crossed_line`, `last_linestart_span`, `ws_width_on_final_line`).
    fn skip_ws_and_newlines_impl(&mut self) -> (bool, Option<Span>, IndentLevel) {
        let mut crossed_line = false;
        let mut last_linestart_span = None;
        let mut ws_width: IndentLevel = 0;
        loop {
            // Use peek_kind() first to avoid cloning the token
            match self.peek_kind() {
                Some(TokenKind::LineStart) => {
                    // Need the indent value, so take the token
                    let (token, span) = self.take_current().unwrap();
                    let Token::LineStart(indent) = token else {
                        // Safety: peek_kind() confirmed LineStart
                        debug_assert!(false, "expected LineStart token");
                        break;
                    };
                    crossed_line = true;
                    last_linestart_span = Some(span);
                    ws_width = 0; // reset: ws before line crossing is irrelevant

                    // In flow context, check for invalid column 0 continuations.
                    // Column 0 is only allowed if the outermost flow context started at column 0.
                    if let Some(&outermost_flow_col) = self.flow_context_columns.first()
                        && indent == 0
                        && outermost_flow_col > 0
                    {
                        // Check if there's actual content after this LineStart
                        // Skip past Whitespace tokens (tabs used as indentation)
                        // and check the actual column of the content token
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
                        {
                            // Content is at column 0 iff no whitespace was peeked past
                            // (indent is already 0 from the guard above).
                            if peek_offset == 0 {
                                self.error(
                                    ErrorKind::InvalidIndentationContext {
                                        expected: 1,
                                        found: 0,
                                    },
                                    span,
                                );
                            }
                        }
                    }

                    // Check for tabs as indentation after crossing a line boundary
                    // In flow context, only tabs at column 0 are invalid
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
                    // No width contribution, just consume
                    let _ = self.take_current();
                }
                _ => break,
            }
        }
        (crossed_line, last_linestart_span, ws_width)
    }

    // ─────────────────────────────────────────────────────────────
    // Document-level handling
    // ─────────────────────────────────────────────────────────────

    /// Reset per-document state such as anchors and tag handles.
    ///
    /// This is called when starting to prepare a new document and keeps
    /// all document-lifecycle initialization in one place.
    fn reset_document_state(&mut self) {
        // Clear anchors from the previous document
        self.anchors.clear();
        // Recompute tag handles based on any directives at the start of
        // the upcoming document.
        self.populate_tag_handles();
    }

    /// Prepare for the next document. Returns `Some((explicit, span))` if a
    /// document should start, or `None` if at EOF.
    /// Prepare a document for parsing: consume whitespace, directives, doc markers.
    /// Returns `(has_doc_start, span, initial_content_column)` where
    /// `initial_content_column` is the tracked column of the first content token.
    fn prepare_document(&mut self) -> Option<(bool, Span, IndentLevel)> {
        // Track ws_width through each skip to compute the initial content column.
        // Each line crossing resets ws_width; at the end, ws_width reflects
        // whitespace consumed on the final line.
        let (_, mut ws_width) = self.skip_ws_and_newlines_tracked();

        if self.is_eof() {
            return None;
        }

        // Skip orphan DocEnd markers
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

        // Reset document-level state (anchors, tag handles, etc.).
        self.reset_document_state();

        // Track directives for "directive without document" error
        let mut has_directive = false;
        let mut first_directive_span = Span::from_usize_range(0..0);

        // Skip directive tokens
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

        // After the initial/trailing scanner pass, skip any remaining whitespace/newlines
        // (e.g., trailing blank lines after indented comments)
        let (crossed, skip_width) = self.skip_ws_and_newlines_tracked();
        ws_width = if crossed {
            skip_width
        } else {
            ws_width + skip_width
        };

        // Check for "directive without document" error
        if has_directive {
            let at_end = self.is_eof() || self.peek_kind() == Some(TokenKind::DocEnd);
            if at_end {
                self.error(ErrorKind::TrailingContent, first_directive_span);
                return None;
            }
        }

        // After skipping line-prefix trivia, check if we're at EOF
        // (e.g., comment-only input with indented comments)
        if self.is_eof() {
            return None;
        }

        // Check for explicit `---`
        let has_doc_start = self.peek_kind() == Some(TokenKind::DocStart);
        let span = self.current_span();

        if has_doc_start {
            let _ = self.take_current();
            let doc_start_ws = self.skip_ws();
            // Check for content on same line as ---
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

            // Block mapping on start line error
            if content_on_line && !self.is_eof() && self.check_block_mapping_on_start_line() {
                self.error(ErrorKind::ContentOnSameLine, self.current_span());
            }
        }

        // Initial content column: current_indent from the last LineStart + any
        // trailing whitespace consumed on that line.
        let initial_col = self.current_indent + ws_width;
        Some((has_doc_start, span, initial_col))
    }

    /// Check if there's a block mapping starting on the `---` line.
    fn check_block_mapping_on_start_line(&self) -> bool {
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

    /// Populate tag handles from `TagDirective` tokens.
    fn populate_tag_handles(&mut self) {
        self.tag_handles.clear();
        // Add default handles
        self.tag_handles.insert("!", "!");
        self.tag_handles.insert("!!", "tag:yaml.org,2002:");

        // Scan for TagDirective tokens and insert their handle/prefix pairs.
        // We walk forward from the current logical position using the cursor
        // so that the implementation does not depend on the underlying token
        // storage representation.
        let mut idx = self.pos;
        loop {
            let Some((token, _span)) = self.cursor.peek(idx) else {
                break;
            };
            match token {
                Token::TagDirective(handle, prefix) => {
                    self.tag_handles.insert(handle, prefix);
                }
                Token::DocStart | Token::DocEnd => break,
                _ => {}
            }
            idx += 1;
        }
    }

    /// Finish the current document. Returns `(explicit, span)` for `DocumentEnd`.
    fn finish_document(&mut self) -> (bool, Span) {
        // Skip trailing line-prefix trivia.
        self.skip_ws_and_newlines();

        // Consume orphan content
        self.consume_trailing_content();

        // Check for `...`
        let (has_doc_end, span) = if self.peek_kind() == Some(TokenKind::DocEnd) {
            // Explicit document end: use the `...` token's span
            let doc_end_span = self.current_span();
            let _ = self.take_current();
            self.skip_ws_and_newlines();
            (true, doc_end_span)
        } else {
            // Implicit document end: use the end position of the last content
            let span = if let Some(last_span) = self.last_content_span {
                Span::from_usize_range(last_span.end_usize()..last_span.end_usize())
            } else {
                // No content in document, use current position
                self.current_span()
            };
            (false, span)
        };

        (has_doc_end, span)
    }

    /// Consume trailing content before the next document marker.
    fn consume_trailing_content(&mut self) {
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

            // Check for orphan content and emit error
            let kind = self.peek_kind();
            let span = self.current_span();
            if matches!(kind, Some(TokenKind::FlowSeqEnd | TokenKind::FlowMapEnd)) {
                // Extra flow collection end tokens are always an error (unmatched brackets)
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
                    // At document level, all trailing content is TrailingContent
                    // (even anchors/tags - they're not OrphanedProperties because
                    // they weren't collected as part of a value context)
                    // Use error_unless_span_has_error to avoid double-reporting
                    // spans that were already flagged as OrphanedProperties
                    self.error_unless_span_has_error(ErrorKind::TrailingContent, span);
                }
            }
            let _ = self.take_current();
            self.skip_ws_and_newlines();
        }
    }

    /// Emit a null scalar event.
    fn emit_null(&self) -> Event<'input> {
        Event::Scalar {
            style: ScalarStyle::Plain,
            value: Cow::Borrowed(""),
            properties: None,
            span: self.current_span(),
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "Complex state machine with value dispatch logic"
    )]
    /// Process the state stack and return the next event, if any.
    ///
    /// Returns `None` when the stack is empty (document content complete).
    fn process_state_stack(&mut self) -> Option<Event<'input>> {
        loop {
            let state = self.state_stack.pop()?;

            match state {
                ParseState::Value {
                    mut ctx,
                    properties,
                } => {
                    if (ctx.prior_crossed_line || self.peek_kind() != Some(TokenKind::LineStart))
                        && let Some(comment) = self.take_same_line_comment_after_ws()
                    {
                        self.set_pending_ast_leading_comment(comment);
                    }

                    // Phase 1: Skip initial whitespace/newlines and update content_column.
                    let has_leading_linestart = self.peek_kind() == Some(TokenKind::LineStart);
                    let (crossed, ws_width) = self.skip_ws_and_newlines_tracked();
                    let initial_crossed_line =
                        ctx.prior_crossed_line || has_leading_linestart || crossed;

                    // UPDATE content_column
                    if initial_crossed_line {
                        ctx.content_column = Some(self.current_indent + ws_width);
                    } else if ws_width > 0 {
                        ctx.content_column =
                            Some(ctx.content_column.map_or(ws_width, |col| col + ws_width));
                    }

                    self.handle_invalid_indent_after_line_cross(
                        ctx.min_indent,
                        initial_crossed_line,
                    );

                    if properties.is_empty()
                        && !matches!(self.peek_kind(), Some(TokenKind::Anchor | TokenKind::Tag))
                    {
                        if let Some(event) = self.process_value_after_properties(
                            ctx,
                            properties,
                            initial_crossed_line,
                            false,
                        ) {
                            return Some(event);
                        }
                        continue;
                    }

                    self.state_stack.push(ParseState::ValueCollectProperties {
                        ctx,
                        properties,
                        initial_crossed_line,
                        crossed_property_line_boundary: false,
                        consumed_width: 0,
                    });
                }
                ParseState::ValueCollectProperties {
                    mut ctx,
                    mut properties,
                    initial_crossed_line,
                    mut crossed_property_line_boundary,
                    mut consumed_width,
                } => loop {
                    let has_props = !properties.is_empty();

                    match self.peek_kind() {
                        Some(TokenKind::Anchor) => {
                            let Some((Token::Anchor(name_ref), span)) = self.take_current() else {
                                debug_assert!(false, "expected Anchor token");
                                break;
                            };
                            if properties.has_anchor() {
                                self.error(ErrorKind::DuplicateAnchor, span);
                            }

                            self.anchors.insert(name_ref);
                            let token_width =
                                usize_to_indent(span.end_usize() - span.start_usize());
                            properties.set_anchor(Property {
                                value: Cow::Borrowed(name_ref),
                                span,
                            });
                            let ws_width = self.skip_ws();
                            consumed_width += token_width + ws_width;
                            continue;
                        }
                        Some(TokenKind::Tag) => {
                            let Some((Token::Tag(tag_cow), span)) = self.take_current() else {
                                debug_assert!(false, "expected Tag token");
                                break;
                            };
                            let tag_str = tag_cow.as_ref();
                            let tag_looks_legitimate =
                                !tag_str.contains('"') && !tag_str.contains('`');

                            if properties.has_tag() {
                                self.error(ErrorKind::DuplicateTag, span);
                            }

                            let expanded = self.expand_tag(tag_cow, span);
                            let token_width =
                                usize_to_indent(span.end_usize() - span.start_usize());
                            properties.set_tag(Property {
                                value: expanded,
                                span,
                            });

                            let tag_end = span.end_usize();
                            if tag_looks_legitimate
                                && let Some(next_span) = self
                                    .peek_with(|next_tok, next_span| {
                                        matches!(
                                            next_tok,
                                            Token::Plain(_)
                                                | Token::StringStart(_)
                                                | Token::FlowSeqStart
                                                | Token::FlowMapStart
                                                | Token::BlockSeqIndicator
                                        )
                                        .then_some(next_span)
                                    })
                                    .flatten()
                                && next_span.start_usize() == tag_end
                            {
                                self.error(ErrorKind::ContentOnSameLine, next_span);
                            }

                            let ws_width = self.skip_ws();
                            consumed_width += token_width + ws_width;
                            continue;
                        }
                        Some(TokenKind::Comment) if has_props => {
                            let _ = self.take_current();
                            continue;
                        }
                        Some(TokenKind::LineStart) if has_props => {
                            let Some((next_indent, _)) = self.peek_line_start() else {
                                debug_assert!(false, "expected LineStart token");
                                break;
                            };
                            let should_continue = self.should_continue_collecting_properties();

                            if next_indent < ctx.min_indent {
                                crossed_property_line_boundary = true;
                            } else if should_continue {
                                crossed_property_line_boundary = true;
                                consumed_width = 0;
                                let _ = self.take_current();
                                continue;
                            } else {
                                crossed_property_line_boundary = true;
                                consumed_width = 0;
                            }
                        }
                        _ => {}
                    }

                    if crossed_property_line_boundary {
                        ctx.content_column = Some(self.current_indent + consumed_width);
                    } else if consumed_width > 0 {
                        ctx.content_column = Some(
                            ctx.content_column
                                .map_or(consumed_width, |col| col + consumed_width),
                        );
                    }

                    self.report_orphaned_properties_after_invalid_indent(
                        ctx.min_indent,
                        crossed_property_line_boundary,
                    );
                    let property_indent = (!properties.is_empty()).then_some(self.current_indent);

                    self.state_stack.push(ParseState::ValueDispatch {
                        ctx,
                        properties,
                        initial_crossed_line,
                        prop_crossed_line: crossed_property_line_boundary,
                        property_indent,
                    });
                    break;
                },
                ParseState::ValueDispatch {
                    ctx,
                    properties,
                    initial_crossed_line,
                    prop_crossed_line,
                    property_indent,
                } => {
                    if let Some(event) = self.process_value_dispatch_state(
                        ctx,
                        properties,
                        initial_crossed_line,
                        prop_crossed_line,
                        property_indent,
                    ) {
                        return Some(event);
                    }
                    // No value produced, continue with next state
                }
                ParseState::ValueDispatchToken {
                    ctx,
                    properties,
                    initial_crossed_line,
                    prop_crossed_line,
                } => {
                    if let Some(event) = self.process_value_after_properties(
                        ctx,
                        properties,
                        initial_crossed_line,
                        prop_crossed_line,
                    ) {
                        return Some(event);
                    }
                }
                ParseState::AliasValue {
                    name,
                    span,
                    properties,
                    crossed_line_after_properties,
                } => {
                    return Some(self.process_alias_value_state(
                        name,
                        span,
                        properties,
                        crossed_line_after_properties,
                    ));
                }
                ParseState::PlainScalarBlock {
                    first_line,
                    properties,
                    start_span,
                    end_span,
                    min_indent,
                    consecutive_newlines,
                    has_continuation,
                    content,
                } => {
                    return Some(self.process_plain_scalar_block_state(
                        first_line,
                        properties,
                        start_span,
                        end_span,
                        min_indent,
                        consecutive_newlines,
                        has_continuation,
                        content,
                    ));
                }
                ParseState::PlainScalarFlow {
                    first_line,
                    properties,
                    start_span,
                    end_span,
                    has_continuation,
                    content,
                } => {
                    return Some(self.process_plain_scalar_flow_state(
                        first_line,
                        properties,
                        start_span,
                        end_span,
                        has_continuation,
                        content,
                    ));
                }
                ParseState::QuotedScalar {
                    properties,
                    quote_style,
                    min_indent,
                    start_span,
                    parts,
                    end_span,
                    pending_newlines,
                    needs_trim,
                } => {
                    return Some(self.process_quoted_scalar_state(
                        properties,
                        quote_style,
                        min_indent,
                        start_span,
                        parts,
                        end_span,
                        pending_newlines,
                        needs_trim,
                    ));
                }
                ParseState::BlockScalar {
                    properties,
                    header,
                    start_span,
                    min_indent,
                    is_literal,
                } => {
                    return Some(self.process_block_scalar_state(
                        properties, header, start_span, min_indent, is_literal,
                    ));
                }
                ParseState::AdditionalPropertiesCollect {
                    mut ctx,
                    outer,
                    mut inner,
                    mut crossed_line_boundary,
                    mut consumed_width,
                } => loop {
                    let has_props = !inner.is_empty();
                    match self.peek_kind() {
                        Some(TokenKind::Anchor) => {
                            let Some((Token::Anchor(name_ref), span)) = self.take_current() else {
                                debug_assert!(false, "expected Anchor token");
                                break;
                            };
                            if inner.has_anchor() {
                                self.error(ErrorKind::DuplicateAnchor, span);
                            }

                            self.anchors.insert(name_ref);
                            let token_width =
                                usize_to_indent(span.end_usize() - span.start_usize());
                            inner.set_anchor(Property {
                                value: Cow::Borrowed(name_ref),
                                span,
                            });
                            let ws_width = self.skip_ws();
                            consumed_width += token_width + ws_width;
                            continue;
                        }
                        Some(TokenKind::Tag) => {
                            let Some((Token::Tag(tag_cow), span)) = self.take_current() else {
                                debug_assert!(false, "expected Tag token");
                                break;
                            };
                            let tag_str = tag_cow.as_ref();
                            let tag_looks_legitimate =
                                !tag_str.contains('"') && !tag_str.contains('`');

                            if inner.has_tag() {
                                self.error(ErrorKind::DuplicateTag, span);
                            }

                            let expanded = self.expand_tag(tag_cow, span);
                            let token_width =
                                usize_to_indent(span.end_usize() - span.start_usize());
                            inner.set_tag(Property {
                                value: expanded,
                                span,
                            });

                            let tag_end = span.end_usize();
                            if tag_looks_legitimate
                                && let Some(next_span) = self
                                    .peek_with(|next_tok, next_span| {
                                        matches!(
                                            next_tok,
                                            Token::Plain(_)
                                                | Token::StringStart(_)
                                                | Token::FlowSeqStart
                                                | Token::FlowMapStart
                                                | Token::BlockSeqIndicator
                                        )
                                        .then_some(next_span)
                                    })
                                    .flatten()
                                && next_span.start_usize() == tag_end
                            {
                                self.error(ErrorKind::ContentOnSameLine, next_span);
                            }

                            let ws_width = self.skip_ws();
                            consumed_width += token_width + ws_width;
                            continue;
                        }
                        Some(TokenKind::Comment) if has_props => {
                            let _ = self.take_current();
                            continue;
                        }
                        Some(TokenKind::LineStart) if has_props => {
                            let should_continue = self.should_continue_collecting_properties();
                            if should_continue {
                                crossed_line_boundary = true;
                                consumed_width = 0;
                                let _ = self.take_current();
                                continue;
                            }
                        }
                        _ => {}
                    }

                    let ws_width = self.skip_ws();
                    if crossed_line_boundary {
                        ctx.content_column = Some(self.current_indent + consumed_width + ws_width);
                    } else {
                        let extra = consumed_width + ws_width;
                        if extra > 0 {
                            ctx.content_column =
                                Some(ctx.content_column.map_or(extra, |col| col + extra));
                        }
                    }

                    if self.peek_kind() == Some(TokenKind::LineStart) {
                        let (_, w) = self.skip_ws_and_newlines_tracked();
                        ctx.content_column = Some(self.current_indent + w);
                        if matches!(self.peek_kind(), Some(TokenKind::Anchor | TokenKind::Tag)) {
                            self.state_stack
                                .push(ParseState::AdditionalPropertiesCollect {
                                    ctx,
                                    outer,
                                    inner,
                                    crossed_line_boundary: true,
                                    consumed_width: 0,
                                });
                        } else {
                            self.state_stack
                                .push(ParseState::AdditionalPropertiesValue { ctx, outer, inner });
                        }
                    } else {
                        self.state_stack
                            .push(ParseState::AdditionalPropertiesValue { ctx, outer, inner });
                    }
                    break;
                },
                ParseState::AdditionalPropertiesValue { ctx, outer, inner } => {
                    if !inner.is_empty()
                        && matches!(
                            self.peek_kind(),
                            Some(TokenKind::FlowSeqStart | TokenKind::FlowMapStart)
                        )
                        && self.current_flow_collection_is_complex_key(false)
                    {
                        let (outer_anchor, outer_tag) = outer.into_parts();
                        let span = self.current_span();
                        let map_indent = self.current_indent;

                        let is_seq = self.peek_kind() == Some(TokenKind::FlowSeqStart);
                        let _ = self.take_current();
                        self.enter_flow_collection(ctx.content_column);

                        self.state_stack.push(ParseState::BlockMap {
                            indent: map_indent,
                            phase: BlockMapPhase::AfterKey {
                                is_implicit_scalar_key: false,
                                key_end_column: None,
                            },
                            start_span: span,
                            properties: EmitterProperties::default(),
                        });

                        if is_seq {
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::BeforeEntry,
                                start_span: span,
                            });
                            self.set_pending_event(Event::SequenceStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: inner.into_event_box(),
                                span,
                            });
                        } else {
                            self.state_stack.push(ParseState::FlowMap {
                                phase: FlowMapPhase::BeforeKey,
                                start_span: span,
                            });
                            self.set_pending_event(Event::MappingStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: inner.into_event_box(),
                                span,
                            });
                        }

                        self.push_indent(map_indent);
                        return Some(Event::MappingStart {
                            style: crate::event::CollectionStyle::Block,
                            properties: EmitterProperties::from_parts(outer_anchor, outer_tag)
                                .into_event_box(),
                            span,
                        });
                    }
                    self.state_stack
                        .push(ParseState::AdditionalPropertiesDispatchToken { ctx, outer, inner });
                }
                ParseState::AdditionalPropertiesDispatchToken { ctx, outer, inner } => {
                    let (outer_anchor, outer_tag) = outer.into_parts();
                    let min_indent = ctx.min_indent;
                    let is_implicit_key = matches!(ctx.kind, ValueKind::ImplicitKey);

                    match self.peek_kind() {
                        Some(TokenKind::Plain | TokenKind::StringStart) => {
                            if !is_implicit_key && self.flow_depth == 0 && self.is_implicit_key() {
                                let span = self.current_span();
                                let map_indent = self.current_indent;
                                let key_event = self.parse_plain_scalar(inner, min_indent);
                                let mapping_start = self.build_block_mapping_from_scalar_key(
                                    map_indent,
                                    span,
                                    EmitterProperties::from_parts(outer_anchor, outer_tag),
                                    key_event,
                                    ctx.content_column,
                                );
                                return Some(mapping_start);
                            }

                            let result = self.parse_plain_scalar(inner, min_indent);
                            if is_implicit_key && let Event::Scalar { span, .. } = &result {
                                self.check_multiline_implicit_key(*span);
                            }
                            return Some(result);
                        }
                        Some(TokenKind::BlockSeqIndicator) => {
                            let merged = inner
                                .merged(EmitterProperties::from_parts(outer_anchor, outer_tag));
                            let span = self.current_span();
                            let seq_indent = ctx.content_column.unwrap_or(self.current_indent);
                            self.push_indent(seq_indent);
                            self.current_indent = seq_indent;
                            self.state_stack.push(ParseState::BlockSeq {
                                indent: seq_indent,
                                phase: BlockSeqPhase::BeforeEntryScan,
                                start_span: span,
                                properties: EmitterProperties::default(),
                            });
                            return Some(Event::SequenceStart {
                                style: crate::event::CollectionStyle::Block,
                                properties: merged.into_event_box(),
                                span,
                            });
                        }
                        Some(TokenKind::FlowSeqStart) => {
                            let merged = inner
                                .merged(EmitterProperties::from_parts(outer_anchor, outer_tag));
                            let span = self.current_span();
                            let _ = self.take_current();
                            self.enter_flow_collection(ctx.content_column);
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::BeforeEntry,
                                start_span: span,
                            });
                            return Some(Event::SequenceStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: merged.into_event_box(),
                                span,
                            });
                        }
                        Some(TokenKind::FlowMapStart) => {
                            let merged = inner
                                .merged(EmitterProperties::from_parts(outer_anchor, outer_tag));
                            let span = self.current_span();
                            let _ = self.take_current();
                            self.enter_flow_collection(ctx.content_column);
                            self.state_stack.push(ParseState::FlowMap {
                                phase: FlowMapPhase::BeforeKey,
                                start_span: span,
                            });
                            return Some(Event::MappingStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: merged.into_event_box(),
                                span,
                            });
                        }
                        Some(_) | None => {
                            let merged = inner
                                .merged(EmitterProperties::from_parts(outer_anchor, outer_tag));
                            return Some(Event::Scalar {
                                style: ScalarStyle::Plain,
                                value: Cow::Borrowed(""),
                                properties: merged.into_event_box(),
                                span: self.current_span(),
                            });
                        }
                    }
                }
                ParseState::FlowCollectionValue {
                    is_map,
                    span,
                    properties,
                    kind,
                    content_column,
                } => {
                    return Some(self.process_flow_collection_value_state(
                        is_map,
                        span,
                        properties,
                        kind,
                        content_column,
                    ));
                }

                ParseState::BlockSeq {
                    indent,
                    phase,
                    start_span,
                    properties,
                } => {
                    if let Some(event) =
                        self.process_block_seq(indent, phase, start_span, properties)
                    {
                        return Some(event);
                    }
                }

                ParseState::BlockMap {
                    indent,
                    phase,
                    start_span,
                    properties,
                } => {
                    if let Some(event) =
                        self.process_block_map(indent, phase, start_span, properties)
                    {
                        return Some(event);
                    }
                }

                ParseState::FlowSeq { phase, start_span } => {
                    if let Some(event) = self.process_flow_seq(phase, start_span) {
                        return Some(event);
                    }
                }

                ParseState::FlowMap { phase, start_span } => {
                    if let Some(event) = self.process_flow_map(phase, start_span) {
                        return Some(event);
                    }
                }
            }
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Value parsing helpers and dispatch
    // ─────────────────────────────────────────────────────────────

    /// Return true if we're currently in the context of a block sequence entry
    /// after having parsed a `-` indicator.
    fn in_sequence_entry_context(&self) -> bool {
        self.state_stack.last().is_some_and(|state| {
            matches!(
                state,
                ParseState::BlockSeq {
                    phase: BlockSeqPhase::BeforeEntryScan,
                    ..
                }
            )
        })
    }

    /// Decide whether collected properties at a dedented indent should result in
    /// an empty scalar value instead of "bridging" to a block collection.
    ///
    /// This encapsulates the bridging rules used by `parse_value`:
    /// - For sequence entries, never bridge: `- !!str\n-` means `!!str` applies
    ///   to an empty scalar and the second `-` is a sibling entry.
    /// - For mapping values, only bridge when the dedented content is at the
    ///   parent collection's indent and the next content is a block collection
    ///   indicator (`-` or `?`).
    ///
    /// Returns a `MaybeEmptyScalarDecision<'input>` describing whether to emit
    /// an empty scalar now or continue parsing this value with updated
    /// properties.
    fn maybe_emit_empty_scalar_for_non_bridging_properties(
        &mut self,
        min_indent: IndentLevel,
        properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        property_indent: Option<IndentLevel>,
    ) -> MaybeEmptyScalarDecision<'input> {
        // No properties collected – nothing to decide.
        if properties.is_empty() {
            return MaybeEmptyScalarDecision::Continue { properties };
        }

        // Only care about the case where the next token is a LineStart at a
        // dedented level (< min_indent). Otherwise, we leave the properties
        // attached to the upcoming value.
        let Some((next_indent, _)) = self.peek_line_start() else {
            return MaybeEmptyScalarDecision::Continue { properties };
        };
        if next_indent >= min_indent {
            return MaybeEmptyScalarDecision::Continue { properties };
        }

        // Check if we're in a sequence entry context.
        let in_sequence_entry = self.in_sequence_entry_context();

        // Check if the dedented line is too far outside the current context.
        let too_dedented = next_indent < min_indent.saturating_sub(1);

        // Check if properties were collected at an invalid (dedented) indent.
        // Properties are invalid if:
        // 1. We crossed a line BEFORE collecting them (`initial_crossed_line`)
        //    meaning they're on a separate line from the key
        // 2. AND the indent of that line is < min_indent
        // E.g., `seq:\n&anchor\n- a` where &anchor is at indent 0 < min_indent 1
        // But `seq: !!seq\n- a` is valid - the tag is inline with the key
        // (initial_crossed_line=false, prop_crossed_line=true means we crossed
        // AFTER collecting, not before).
        let properties_at_invalid_indent =
            initial_crossed_line && property_indent.is_some_and(|prop_ind| prop_ind < min_indent);

        // Even if not too dedented, we should NOT bridge to a sibling mapping key.
        // Look ahead past the `LineStart` token to see what follows.
        // Only bridge if the next content token is a block collection indicator (- or ?).
        let can_bridge = !in_sequence_entry
            && !too_dedented
            && !properties_at_invalid_indent
            && matches!(
                self.peek_kind_nth(1),
                Some(TokenKind::BlockSeqIndicator | TokenKind::MappingKey)
            );

        if !can_bridge {
            // Can't bridge - emit empty scalar.
            // If properties were collected at an invalid indent (below min_indent),
            // drop them (e.g. `seq:\n&anchor\n- a` where &anchor is at indent 0
            // < min_indent 1).
            let event_properties = if properties_at_invalid_indent {
                EmitterProperties::default() // Drop properties collected at invalid indent
            } else {
                properties
            };
            return MaybeEmptyScalarDecision::EmitEmptyScalar {
                event: Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties: event_properties.into_event_box(),
                    span: self.current_span(),
                },
            };
        }

        // Bridging to block collection at valid dedent level.
        MaybeEmptyScalarDecision::Continue { properties }
    }

    /// Check if crossing to a lower indent after a line boundary indicates an
    /// empty value (the lower-indented content belongs to a parent context).
    ///
    /// In block context, if we're at an indent < `ctx.min_indent` after crossing
    /// a line, the value is empty. For example:
    ///
    /// ```yaml
    /// key:
    ///   value
    /// ```
    ///
    /// has `min_indent = 1` for the value. But in:
    ///
    /// ```yaml
    /// key:
    /// next:
    /// ```
    ///
    /// `next:` is at indent 0 < 1, so `key` has an empty value.
    ///
    /// EXCEPTION: Block collection indicators (-, ?, :) can appear at the key's
    /// indent level to start a collection as the value. E.g.:
    ///
    /// ```yaml
    /// key:
    /// - item
    /// ```
    ///
    /// Here `-` is at indent 0, same as `key`, but it's the value (a sequence).
    ///
    /// BUT NOT for sequence entries: a `-` at the sequence's indent is a
    /// sibling entry, not a nested value. E.g.:
    ///
    /// ```yaml
    /// - # Empty
    /// - value
    /// ```
    ///
    /// The second `-` is a sibling, so the first entry's value is empty.
    fn maybe_emit_empty_due_to_lower_indent(
        &mut self,
        ctx: ValueContext,
        crossed_line_after_properties: bool,
        properties: &EmitterProperties<'input>,
    ) -> Option<Event<'input>> {
        let min_indent = ctx.min_indent;

        // Only lower-indent-based emptiness if we crossed a line.
        if !crossed_line_after_properties {
            return None;
        }

        // If we haven't actually dedented below the minimum indent, there is no
        // empty-value-to-parent decision to make. Early-return before any
        // lookahead to keep the common non-dedent paths cheap (especially for
        // block scalars and multi-line values that remain at the same indent).
        if self.current_indent >= min_indent {
            return None;
        }

        let in_sequence_entry = self.in_sequence_entry_context();

        let at_block_indicator = !in_sequence_entry
            && matches!(
                self.peek_kind(),
                Some(TokenKind::BlockSeqIndicator | TokenKind::MappingKey | TokenKind::Colon)
            );

        if self.current_indent < min_indent && !at_block_indicator {
            return Some(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                properties: properties.clone().into_event_box(),
                span: self.current_span(),
            });
        }

        None
    }

    /// Decide whether properties collected before a colon in a null-key mapping
    /// (`: value`) belong to the implicit null key or the mapping itself.
    ///
    /// This encapsulates the `props_for_key` logic used for cases like:
    /// - `-\n  !!null : a` → properties belong to the key
    /// - `!!null : a` (at root) → properties belong to the key
    fn properties_belong_to_null_key(
        &self,
        prop_crossed_line: bool,
        properties: &EmitterProperties<'input>,
    ) -> bool {
        !prop_crossed_line
            && (properties.has_anchor() || properties.has_tag())
            && self.peek_kind() == Some(TokenKind::Colon)
    }

    /// Common helper for creating a block mapping from a scalar key that has
    /// already been parsed.
    ///
    /// This sets up the `BlockMap` continuation state, queues the already
    /// parsed key event for the next `next()` call, and returns the
    /// `MappingStart` event. The `start_span` is the span of the indicator that
    /// triggered implicit mapping detection (used as `start_span` for
    /// `BlockMap`), while the returned event's span is the full key span.
    fn build_block_mapping_from_scalar_key(
        &mut self,
        map_indent: IndentLevel,
        start_span: Span,
        map_properties: EmitterProperties<'input>,
        key_event: Event<'input>,
        content_column: Option<IndentLevel>,
    ) -> Event<'input> {
        if let Event::Scalar {
            value,
            properties,
            span: k_span,
            style,
        } = key_event
        {
            let (k_anchor, k_tag) = properties.map_or((None, None), |event_props| {
                let Properties { anchor, tag } = *event_props;
                (anchor, tag)
            });

            // Check for multiline implicit key error.
            self.check_multiline_implicit_key(k_span);

            // Compute key_end_column: key started at content_column, span gives width.
            let key_end_column = content_column
                .map(|col| col + usize_to_indent(k_span.end_usize() - k_span.start_usize()));

            // Stack setup: emit the key first, then parse the value after colon.
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: true,
                    key_end_column,
                },
                start_span,
                properties: EmitterProperties::default(),
            });
            self.set_pending_ast_wrap(PendingAstWrap::MappingPair {
                pair_start: start_span.start,
            });
            self.set_pending_event(Event::Scalar {
                style,
                value,
                properties: EmitterProperties::from_parts(k_anchor, k_tag).into_event_box(),
                span: k_span,
            });
            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);

            Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: map_properties.into_event_box(),
                span: k_span, // Use full key span for MappingStart
            }
        } else {
            // Fallback - shouldn't happen, but keep stack consistent.
            self.push_indent(map_indent);
            Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: map_properties.into_event_box(),
                span: start_span,
            }
        }
    }

    /// Handle invalid indentation after crossing a line boundary at the start of a value.
    /// When we cross a line and land at `indent < min_indent`, we may need to report
    /// `InvalidIndentation` and/or `OrphanedProperties` errors for properties or
    /// scalar-like content at that position.
    fn handle_invalid_indent_after_line_cross(
        &mut self,
        min_indent: IndentLevel,
        initial_crossed_line: bool,
    ) {
        if initial_crossed_line && self.current_indent < min_indent {
            // Check if there's content (properties or values) at this invalid indent
            if let Some(kind) = self.peek_kind() {
                let span = self.current_span();
                if matches!(kind, TokenKind::Anchor | TokenKind::Tag) {
                    // Properties at invalid indent get both InvalidIndentation and OrphanedProperties
                    self.error(ErrorKind::InvalidIndentation, span);
                    self.error(ErrorKind::OrphanedProperties, span);
                } else if matches!(kind, TokenKind::Plain | TokenKind::StringStart) {
                    // Don't report InvalidIndentation if this is a mapping key (followed by colon).
                    // E.g., `: # comment\n"key":` - the "key": is a new mapping entry, not a value.
                    let is_mapping_key = if kind == TokenKind::Plain {
                        self.is_plain_followed_by_colon()
                    } else {
                        self.with_lookahead(50, |window| {
                            Self::scan_find_string_end_from(&window, 1, 50)
                                .map(|end| Self::scan_skip_inline_whitespace_from(&window, end + 1))
                                .is_some_and(|offset| window.kind(offset) == Some(TokenKind::Colon))
                        })
                    };

                    if !is_mapping_key {
                        self.error(ErrorKind::InvalidIndentation, span);
                    }
                }
            }
        }
    }

    /// Return true if a block sequence at the given `indent` is at the
    /// root level. Root-level sequences have `indent_stack == [0, 0]`
    /// (base + sequence level).
    fn is_root_level_sequence(&self, indent: IndentLevel) -> bool {
        indent == 0 && self.indent_stack.len() == 2
    }

    /// Report orphaned properties when property collection crossed a line boundary and
    /// landed at an invalid indent level for the current value.
    fn report_orphaned_properties_after_invalid_indent(
        &mut self,
        min_indent: IndentLevel,
        prop_crossed_line: bool,
    ) {
        // Check for orphaned properties: if we crossed a line boundary but stopped because
        // of invalid indent, and the next content is a property, report OrphanedProperties.
        // This handles cases like `key: &x\n!!map\n  a: b` where !!map is at invalid indent.
        if prop_crossed_line
            && let Some((indent, _)) = self.peek_line_start()
            && indent < min_indent
        {
            // Look for property tokens after the LineStart
            let mut lookahead = 1;
            while matches!(
                self.peek_kind_nth(lookahead),
                Some(TokenKind::Anchor | TokenKind::Tag)
            ) {
                let span = self
                    .peek_nth_with(lookahead, |_, span| span)
                    .unwrap_or_else(|| self.current_span());
                self.error(ErrorKind::OrphanedProperties, span);
                lookahead += 1;
                // Skip whitespace between properties
                while matches!(
                    self.peek_kind_nth(lookahead),
                    Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs)
                ) {
                    lookahead += 1;
                }
            }
        }
    }

    /// Handle a flow sequence start token as a value, including the case where
    /// the flow sequence acts as a complex mapping key in block context.
    /// Handle a flow collection start token (`[` or `{`) as a value, including
    /// the case where the flow collection acts as a complex mapping key in
    /// block context.
    fn process_flow_collection_value_state(
        &mut self,
        is_map: bool,
        span: Span,
        properties: EmitterProperties<'input>,
        kind: ValueKind,
        content_column: Option<IndentLevel>,
    ) -> Event<'input> {
        let explicit_key = matches!(kind, ValueKind::ExplicitKey);
        // In block context, check if this flow collection is a complex key.
        debug_assert_eq!(
            self.peek_kind(),
            Some(if is_map {
                TokenKind::FlowMapStart
            } else {
                TokenKind::FlowSeqStart
            })
        );
        let is_complex_key =
            self.flow_depth == 0 && self.current_flow_collection_is_complex_key(explicit_key);
        if is_complex_key {
            // This is a block mapping with a flow collection as key.
            let map_indent = self.current_indent;

            let _ = self.take_current();
            self.enter_flow_collection(content_column);

            // Push BlockMap state for after the key.
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: false, // Flow collection key, not plain scalar
                    key_end_column: None,
                },
                start_span: span,
                properties: EmitterProperties::default(),
            });

            if is_map {
                self.state_stack.push(ParseState::FlowMap {
                    phase: FlowMapPhase::BeforeKey,
                    start_span: span,
                });
            } else {
                self.state_stack.push(ParseState::FlowSeq {
                    phase: FlowSeqPhase::BeforeEntry,
                    start_span: span,
                });
            }
            self.set_pending_event(if is_map {
                Event::MappingStart {
                    style: crate::event::CollectionStyle::Flow,
                    properties: None,
                    span,
                }
            } else {
                Event::SequenceStart {
                    style: crate::event::CollectionStyle::Flow,
                    properties: None,
                    span,
                }
            });

            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);
            // Emit MappingStart with the properties.
            return Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: properties.into_event_box(),
                span,
            };
        }

        // Not a complex key: treat as regular flow collection value.
        let _ = self.take_current();
        self.enter_flow_collection(content_column);
        if is_map {
            self.state_stack.push(ParseState::FlowMap {
                phase: FlowMapPhase::BeforeKey,
                start_span: span,
            });
            Event::MappingStart {
                style: crate::event::CollectionStyle::Flow,
                properties: properties.into_event_box(),
                span,
            }
        } else {
            self.state_stack.push(ParseState::FlowSeq {
                phase: FlowSeqPhase::BeforeEntry,
                start_span: span,
            });
            Event::SequenceStart {
                style: crate::event::CollectionStyle::Flow,
                properties: properties.into_event_box(),
                span,
            }
        }
    }

    /// Handle an alias token as a value, including error reporting and the
    /// special case where the alias acts as a mapping key.
    ///
    /// This logic is driven from the `AliasValue` parse state, which
    /// centralises how aliases behave in different contexts.
    fn process_alias_value_state(
        &mut self,
        alias_name: Cow<'input, str>,
        alias_span: Span,
        properties: EmitterProperties<'input>,
        crossed_line_boundary: bool,
    ) -> Event<'input> {
        // Advance past the alias token and skip any immediate whitespace.
        let _ = self.take_current();
        self.skip_ws();

        // Error: Properties (anchor/tag) on alias are invalid when on the same line.
        if !properties.is_empty() && !crossed_line_boundary {
            self.error(ErrorKind::PropertiesOnAlias, alias_span);
        }

        // Error: Undefined alias name.
        if !self.anchors.contains(alias_name.as_ref()) {
            self.error(ErrorKind::UndefinedAlias, alias_span);
        }

        // Check if alias is a mapping key (followed by colon) after crossing line boundary.
        // This can create a nested mapping even without outer anchor/tag.
        if crossed_line_boundary && self.peek_kind() == Some(TokenKind::Colon) {
            let map_indent = self.current_indent;

            // Push BlockMap state in AfterKey phase (we already have the key).
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: false, // Alias, not a plain scalar
                    key_end_column: None,
                },
                start_span: alias_span,
                properties: EmitterProperties::default(),
            });
            self.set_pending_event(Event::Alias {
                name: alias_name,
                span: alias_span,
            });

            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);
            // Emit MappingStart with any outer anchor/tag.
            return Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: properties.into_event_box(),
                span: alias_span,
            };
        }

        // Not a mapping key: emit the alias as a simple value.
        Event::Alias {
            name: alias_name,
            span: alias_span,
        }
    }

    /// Parse any value. Returns the first event for this value.
    ///
    /// Dispatch a value after properties have been collected and
    ///
    /// This function hosts the main dispatch logic that was previously the
    /// second half of `parse_value`, including:
    /// - Deciding whether properties at a dedented indent should bridge to a
    ///   block collection or emit an empty scalar.
    /// - Handling dedented empty values.
    /// - Dispatching to scalars, block/flow collections, aliases, and
    ///   additional property handling.
    #[allow(clippy::too_many_lines, reason = "Complex value dispatch logic")]
    fn process_value_dispatch_state(
        &mut self,
        ctx: ValueContext,
        mut properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        prop_crossed_line: bool,
        property_indent: Option<IndentLevel>,
    ) -> Option<Event<'input>> {
        let min_indent = ctx.min_indent;

        // Decide whether collected properties at a dedented indent should stay attached
        // to this value or result in an empty scalar before a parent context.
        // This is only relevant when we actually have properties, which is encoded
        // by `property_indent` being `Some(..)`. Skipping the helper entirely for
        // the common "no properties" case avoids extra lookahead work on the hot
        // plain-scalar path.
        if property_indent.is_some() {
            match self.maybe_emit_empty_scalar_for_non_bridging_properties(
                min_indent,
                properties,
                initial_crossed_line,
                property_indent,
            ) {
                MaybeEmptyScalarDecision::EmitEmptyScalar { event } => {
                    return Some(event);
                }
                MaybeEmptyScalarDecision::Continue {
                    properties: new_props,
                } => {
                    properties = new_props;
                }
            }
        }

        self.state_stack.push(ParseState::ValueDispatchToken {
            ctx,
            properties,
            initial_crossed_line,
            prop_crossed_line,
        });
        None
    }

    #[allow(
        clippy::too_many_lines,
        reason = "value dispatch stays inline to keep the hot state-machine flow easy to follow"
    )]
    fn process_value_after_properties(
        &mut self,
        mut ctx: ValueContext,
        properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        prop_crossed_line: bool,
    ) -> Option<Event<'input>> {
        let min_indent = ctx.min_indent;
        let is_implicit_key = matches!(ctx.kind, ValueKind::ImplicitKey);
        let mut allow_implicit_mapping = ctx.allow_implicit_mapping;

        let mut crossed_line_after_properties = initial_crossed_line || prop_crossed_line;
        allow_implicit_mapping = allow_implicit_mapping || crossed_line_after_properties;

        // Skip any more whitespace/newlines after properties and record if we
        // cross additional line boundaries. UPDATE content_column.
        let (additional_crossed, ws_width) = self.skip_ws_and_newlines_tracked();
        crossed_line_after_properties |= additional_crossed;
        if additional_crossed {
            ctx.content_column = Some(self.current_indent + ws_width);
        } else if ws_width > 0 {
            ctx.content_column = Some(ctx.content_column.map_or(ws_width, |col| col + ws_width));
        }

        // Lower-indent empty-value check. This is only meaningful when we've
        // actually crossed a line boundary; avoid calling the helper at all on
        // the very common single-line value paths.
        if crossed_line_after_properties
            && let Some(event) = self.maybe_emit_empty_due_to_lower_indent(
                ctx,
                crossed_line_after_properties,
                &properties,
            )
        {
            return Some(event);
        }

        // Dispatch based on current token kind. Only fetch payload-bearing
        // tokens when a branch actually needs them.
        match self.peek_kind() {
            None => {
                // EOF - emit empty value / null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties: properties.into_event_box(),
                    span: self.current_span(),
                })
            }

            Some(TokenKind::DocEnd | TokenKind::DocStart) => {
                let span = self.current_span();
                if self.flow_depth > 0 {
                    // Document markers inside flow context are invalid.
                    // Report error and continue - the flow state machine will handle them.
                    // Don't emit a null here; let the caller re-dispatch.
                    self.error(ErrorKind::DocumentMarkerInFlow, span);
                    let _ = self.take_current();
                    // Re-enter Value state to parse the actual value
                    self.state_stack.push(ParseState::Value {
                        ctx: ValueContext {
                            min_indent,
                            content_column: None,
                            kind: ctx.kind,
                            allow_implicit_mapping,
                            prior_crossed_line: !properties.is_empty(),
                        },
                        properties,
                    });
                    None
                } else {
                    // Block context - document marker ends the value, emit null
                    Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: properties.into_event_box(),
                        span: self.current_span(),
                    })
                }
            }

            Some(TokenKind::FlowSeqEnd | TokenKind::FlowMapEnd) if self.flow_depth == 0 => {
                let span = self.current_span();
                self.error(ErrorKind::UnmatchedBracket, span);
                let _ = self.take_current();
                self.state_stack.push(ParseState::Value {
                    ctx: ValueContext {
                        min_indent,
                        content_column: ctx.content_column,
                        kind: ctx.kind,
                        allow_implicit_mapping,
                        prior_crossed_line: !properties.is_empty(),
                    },
                    properties,
                });
                None
            }

            Some(TokenKind::Alias) => {
                // Defer alias handling to the `AliasValue` state so that
                // complex-key behaviour is driven by the state machine.
                let Some((alias_name, alias_span)) = self
                    .peek_with(|tok, span| match tok {
                        Token::Alias(name) => Some((Cow::Borrowed(*name), span)),
                        _ => None,
                    })
                    .flatten()
                else {
                    debug_assert!(false, "expected Alias token");
                    return Some(self.emit_null());
                };
                self.state_stack.push(ParseState::AliasValue {
                    name: alias_name,
                    span: alias_span,
                    properties,
                    crossed_line_after_properties,
                });
                None
            }

            Some(TokenKind::BlockSeqIndicator) => {
                let span = self.current_span();

                // YAML spec: Anchors/tags on the same line as a block sequence indicator
                // are ambiguous and disallowed. They must be on a separate line.
                // E.g., `&anchor - item` is invalid, but `&anchor\n- item` is valid.
                //
                // Check if properties are on the same line as the block indicator.
                // We can't rely on crossed_line_boundary because it may be reset when
                // we cross a line boundary during value scanning.
                // Properties are on the same line as `-` iff no LineStart was consumed
                // at any point: before properties (initial), during collection (prop),
                // or after collection (additional).
                let properties_on_same_line = !properties.is_empty()
                    && !initial_crossed_line
                    && !prop_crossed_line
                    && !additional_crossed;

                if properties_on_same_line {
                    // Properties on same line as block sequence indicator - error
                    self.error(ErrorKind::ContentOnSameLine, span);
                    // Continue parsing to provide better error recovery
                }

                // content_column has been updated through each state transition.
                let seq_indent = ctx.content_column.unwrap_or(self.current_indent);
                // Emit the collection start immediately and leave the entry scan
                // as the next parse-state step.
                self.push_indent(seq_indent);
                self.current_indent = seq_indent;
                self.state_stack.push(ParseState::BlockSeq {
                    indent: seq_indent,
                    phase: BlockSeqPhase::BeforeEntryScan,
                    start_span: span,
                    properties: EmitterProperties::default(),
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Block,
                    properties: properties.into_event_box(),
                    span,
                })
            }

            Some(TokenKind::MappingKey | TokenKind::Colon) => {
                // In flow context, a tag/anchor followed by colon means an empty tagged/anchored
                // scalar as the key - do NOT start a block mapping inside flow context.
                // E.g., `{ !!str : bar }` - the `!!str` is an empty key with tag
                if self.flow_depth > 0 {
                    return Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: properties.into_event_box(),
                        span: self.current_span(),
                    });
                }

                let span = self.current_span();
                // content_column tracks the token position through state transitions.
                // For compact notation (same line as parent indicator) it gives the
                // actual column; for regular notation it equals current_indent.
                let map_indent = ctx.content_column.unwrap_or(self.current_indent);

                // Determine if properties belong to the mapping or the first key.
                // If we crossed a line boundary BEFORE the properties (initial_crossed_line),
                // AND there was no line crossing AFTER the properties (prop_crossed_line=false),
                // AND we see Colon (not MappingKey), then the properties are for the
                // implicit null key, not the mapping.
                //
                // Examples:
                // - `!!map\n  key: value` → initial_crossed_line=false, prop_crossed_line=true
                //   → tag belongs to mapping (it's on a line before the mapping content)
                // - `-\n  !!null : a` → initial_crossed_line=true, prop_crossed_line=false
                //   → tag belongs to the key (it's on the same line as the colon)
                // - `!!null : a` (at root) → initial_crossed_line=false, prop_crossed_line=false
                //   → tag belongs to the key
                let props_for_key =
                    self.properties_belong_to_null_key(prop_crossed_line, &properties);

                if props_for_key {
                    // Properties belong to the implicit null key
                    // Emit MappingStart with no properties, then emit null scalar with properties
                    // Use current_indent for mapping indent (the line's indent level),
                    // not the colon's column position
                    self.state_stack.push(ParseState::BlockMap {
                        indent: self.current_indent,
                        phase: BlockMapPhase::AfterKey {
                            is_implicit_scalar_key: false, // Null key with properties, not plain scalar
                            key_end_column: None,
                        },
                        start_span: span,
                        properties: EmitterProperties::default(),
                    });
                    self.set_pending_event(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: properties.into_event_box(),
                        span,
                    });
                    // Push indent level for orphan indent detection
                    self.push_indent(self.current_indent);
                    Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        properties: None,
                        span,
                    })
                } else {
                    // Properties belong to the mapping.
                    // Emit the mapping start immediately and resume through the
                    // regular block-mapping phases on the next turn.
                    self.push_indent(map_indent);
                    self.crossed_line_boundary = false;
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::BeforeKeyScan {
                            require_line_boundary: false,
                            crossed_line: false,
                        },
                        start_span: span,
                        properties: EmitterProperties::default(),
                    });
                    Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        properties: properties.into_event_box(),
                        span,
                    })
                }
            }

            Some(TokenKind::FlowSeqStart) => {
                let span = self.current_span();
                self.state_stack.push(ParseState::FlowCollectionValue {
                    is_map: false,
                    span,
                    properties,
                    kind: ctx.kind,
                    content_column: ctx.content_column,
                });
                None
            }

            Some(TokenKind::FlowMapStart) => {
                let span = self.current_span();
                self.state_stack.push(ParseState::FlowCollectionValue {
                    is_map: true,
                    span,
                    properties,
                    kind: ctx.kind,
                    content_column: ctx.content_column,
                });
                None
            }

            Some(TokenKind::LiteralBlockHeader | TokenKind::FoldedBlockHeader) => {
                Some(self.parse_block_scalar(min_indent, properties))
            }

            Some(TokenKind::Plain) => {
                if ctx.content_column == Some(0)
                    && self
                        .peek_with(|tok, span| match tok {
                            Token::Plain(text) if text.starts_with('%') => Some(span),
                            _ => None,
                        })
                        .flatten()
                        .is_some_and(|span| {
                            self.error(ErrorKind::InvalidDirective, span);
                            true
                        })
                {
                    return Some(self.parse_scalar_or_mapping(
                        min_indent,
                        properties,
                        is_implicit_key,
                        prop_crossed_line,
                        allow_implicit_mapping || prop_crossed_line || initial_crossed_line,
                        ctx.content_column,
                    ));
                }

                // Could be scalar or start of block mapping (unless we're parsing a key)
                // NOTE: Pass `prop_crossed_line` (not `crossed_line_boundary`) to determine
                // property ownership. What matters is whether there's a line boundary AFTER
                // the properties, not whether we crossed a line from the parent context.
                // - `&anchor\n  key:` → prop_crossed_line=true → anchor on MAPPING
                // - `\n  &anchor key:` → prop_crossed_line=false → anchor on KEY
                //
                // For allow_implicit_mapping: Only allow nested implicit mappings if:
                // - We explicitly allow them (not same-line after colon), OR
                // - We crossed a line boundary (properties or whitespace)
                // This prevents `a: b: c` from creating nested mappings.
                let effective_allow =
                    allow_implicit_mapping || prop_crossed_line || initial_crossed_line;
                Some(self.parse_scalar_or_mapping(
                    min_indent,
                    properties,
                    is_implicit_key,
                    prop_crossed_line,
                    effective_allow,
                    ctx.content_column,
                ))
            }

            Some(TokenKind::StringStart) => {
                // Could be scalar or start of block mapping (unless we're parsing a key)
                let effective_allow =
                    allow_implicit_mapping || prop_crossed_line || initial_crossed_line;
                Some(self.parse_scalar_or_mapping(
                    min_indent,
                    properties,
                    is_implicit_key,
                    prop_crossed_line,
                    effective_allow,
                    ctx.content_column,
                ))
            }

            Some(TokenKind::Anchor | TokenKind::Tag) => {
                // More properties after line boundary - indicates nested structure.
                // Properties may span multiple lines (e.g., `&anchor\n!!str\nvalue`).
                // Defer to the `AdditionalPropertiesValue` state so complex
                // key behaviour is driven by the state machine.
                self.state_stack
                    .push(ParseState::AdditionalPropertiesCollect {
                        ctx,
                        outer: properties,
                        inner: EmitterProperties::default(),
                        crossed_line_boundary: false,
                        consumed_width: 0,
                    });
                None
            }

            Some(_) => {
                // Default: emit null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties: properties.into_event_box(),
                    span: self.current_span(),
                })
            }
        }
    }

    /// Check if we should continue collecting properties across a line boundary.
    ///
    /// We should continue if:
    /// - The next line has properties (anchor/tag)
    /// - AND those properties are NOT followed by an implicit mapping key
    ///
    /// An implicit mapping key is content followed by `:`, like `[...]:` or `key:`.
    /// When we see this pattern, the properties before the line boundary should go
    /// on the parent mapping, not merged with the key's properties.
    fn should_continue_collecting_properties(&self) -> bool {
        let mut idx = 1; // Start after the LineStart at position 0

        // First, check if there are any properties on the next line
        let mut found_property = false;
        loop {
            match self.peek_kind_nth(idx) {
                Some(TokenKind::Anchor | TokenKind::Tag) => {
                    found_property = true;
                    idx += 1;
                }
                Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => {
                    idx += 1;
                }
                _ => break,
            }
            if idx > 20 {
                return false; // Safety limit
            }
        }

        if !found_property {
            // No property on the next line - don't continue collecting
            return false;
        }

        // Now check what follows the properties.
        // If it's LineStart, this line is "property-only" - continue collecting.
        // If it's content, check if that content is an implicit key (followed by colon).
        match self.peek_kind_nth(idx) {
            Some(TokenKind::FlowSeqStart | TokenKind::FlowMapStart) => {
                // Flow collection - check if it's a complex key
                // Look for matching close bracket, then colon
                let is_key = self.is_implicit_key_at_offset(idx);
                // If it's a key, DON'T continue (parent mapping gets first props)
                !is_key
            }
            Some(TokenKind::Plain) => {
                // Plain scalar - check if it's a key
                // Simple check: skip the Plain token and look for Colon
                let is_key = self.is_implicit_key_at_offset(idx);
                // If it's a key, DON'T continue
                !is_key
            }
            _ => {
                // Property-only line - continue collecting
                // Other content (quoted string, etc.) - continue collecting
                // These cases generally don't form implicit keys in this context
                true
            }
        }
    }

    /// Check if token at offset `start_idx` is an implicit mapping key.
    fn is_implicit_key_at_offset(&self, start_idx: usize) -> bool {
        self.with_lookahead(start_idx + 200, |window| match window.kind(start_idx) {
            Some(TokenKind::Plain) => Self::scan_is_mapping_key_at_offset(&window, start_idx),
            Some(TokenKind::FlowSeqStart) => {
                let Some(end) = Self::scan_find_flow_collection_end(
                    &window,
                    start_idx,
                    TokenKind::FlowSeqStart,
                ) else {
                    return false;
                };
                let idx = Self::scan_skip_inline_whitespace_from(&window, end + 1);
                window.kind(idx) == Some(TokenKind::Colon)
            }
            _ => false,
        })
    }

    /// Expand a tag handle to its full form.
    ///
    /// The lexer transforms tags as follows:
    /// - `!!str` → `Tag("!str")` (secondary handle)
    /// - `!name!suffix` → `Tag("name!suffix")` (named handle)
    /// - `!<uri>` → `Tag("\0uri")` (verbatim, marked with NUL)
    /// - `!` alone → `Tag("")` (non-specific)
    #[allow(clippy::too_many_lines, reason = "Tag expansion with many cases")]
    fn expand_tag(&mut self, tag_cow: Cow<'input, str>, span: Span) -> Cow<'input, str> {
        /// Decode percent-encoded characters in a tag suffix.
        /// E.g., `tag%21` → `tag!` (since %21 is '!')
        /// Returns None if no decoding was needed (no '%' found).
        fn percent_decode(input: &str) -> Option<String> {
            if !input.contains('%') {
                return None; // No decoding needed
            }

            let mut result = String::with_capacity(input.len());
            let mut chars = input.chars().peekable();
            while let Some(ch) = chars.next() {
                if ch == '%' {
                    // Try to read two hex digits
                    let hex: String = chars.by_ref().take(2).collect();
                    if hex.len() == 2
                        && let Ok(byte) = u8::from_str_radix(&hex, 16)
                    {
                        result.push(char::from(byte));
                        continue;
                    }
                    // Failed to decode, keep as-is
                    result.push('%');
                    result.push_str(&hex);
                } else {
                    result.push(ch);
                }
            }
            Some(result)
        }

        fn join_prefix_and_suffix(prefix: &str, suffix: &str) -> String {
            let mut result = String::with_capacity(prefix.len() + suffix.len());
            result.push_str(prefix);
            result.push_str(suffix);
            result
        }

        // Verbatim tag: marked with leading '\0' by lexer - return as-is (without marker)
        if let Some(verbatim) = tag_cow.as_ref().strip_prefix('\0') {
            return Cow::Owned(String::from(verbatim));
        }

        // Now tags include the '!' prefix from the lexer
        // Empty tag (just `!`) - non-specific tag
        if tag_cow.as_ref() == "!" {
            // Can return the borrowed tag directly!
            return tag_cow;
        }

        // Secondary handle: !!type
        if let Some(suffix) = tag_cow.as_ref().strip_prefix("!!") {
            const TAG_PREFIX: &str = "tag:yaml.org,2002:";
            let prefix = self.tag_handles.get("!!").copied().unwrap_or(TAG_PREFIX);

            if let Some(decoded_suffix) = percent_decode(suffix) {
                return Cow::Owned(join_prefix_and_suffix(prefix, &decoded_suffix));
            }

            if prefix == TAG_PREFIX {
                match suffix {
                    "str" => return Cow::Borrowed("tag:yaml.org,2002:str"),
                    "seq" => return Cow::Borrowed("tag:yaml.org,2002:seq"),
                    "map" => return Cow::Borrowed("tag:yaml.org,2002:map"),
                    "int" => return Cow::Borrowed("tag:yaml.org,2002:int"),
                    "float" => return Cow::Borrowed("tag:yaml.org,2002:float"),
                    "bool" => return Cow::Borrowed("tag:yaml.org,2002:bool"),
                    "null" => return Cow::Borrowed("tag:yaml.org,2002:null"),
                    "timestamp" => return Cow::Borrowed("tag:yaml.org,2002:timestamp"),
                    _ => {}
                }
            }

            return Cow::Owned(join_prefix_and_suffix(prefix, suffix));
        }

        // Named handle: !name!suffix
        #[allow(
            clippy::string_slice,
            reason = "Slicing at positions returned by find('!'), which are guaranteed UTF-8 boundaries"
        )]
        if let Some(first_bang) = tag_cow.as_ref().find('!')
            && let Some(second_bang) = tag_cow.as_ref()[first_bang + 1..].find('!')
        {
            let tag_str = tag_cow.as_ref();
            let second_bang_pos = first_bang + 1 + second_bang;
            let handle = &tag_str[0..=second_bang_pos]; // e.g., "!yaml!"
            let suffix = &tag_str[second_bang_pos + 1..];
            // Look up handle using as_str() since HashMap keys are &str
            if let Some(prefix) = self.tag_handles.get(handle) {
                if let Some(decoded_suffix) = percent_decode(suffix) {
                    return Cow::Owned(join_prefix_and_suffix(prefix, &decoded_suffix));
                }
                return Cow::Owned(join_prefix_and_suffix(prefix, suffix));
            }
            // Handle not declared - emit error and return unexpanded
            self.error(ErrorKind::UndefinedTagHandle, span);
            return tag_cow; // Return as-is
        }

        // Primary handle: !type
        if let Some(suffix) = tag_cow.as_ref().strip_prefix('!') {
            let prefix = self.tag_handles.get("!").copied().unwrap_or("!");
            if prefix == "!" && !suffix.contains('%') {
                return tag_cow;
            }
            if let Some(decoded_suffix) = percent_decode(suffix) {
                return Cow::Owned(join_prefix_and_suffix(prefix, &decoded_suffix));
            }
            return Cow::Owned(join_prefix_and_suffix(prefix, suffix));
        }

        // Shouldn't reach here, but return as-is
        tag_cow
    }

    // ─────────────────────────────────────────────────────────────
    // Block Sequence
    // ─────────────────────────────────────────────────────────────

    #[allow(clippy::too_many_lines, reason = "Complex state machine dispatch")]
    fn process_block_seq(
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
                                    if !self.is_plain_followed_by_colon() {
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
                            // No more entries - check for trailing content at seq_indent level.
                            // Use current_indent as the content column: in this catch-all,
                            // the token is on the current line at or past current_indent.
                            // This is an error diagnostic for malformed input.
                            if self.current_indent == indent
                                && self.peek_kind() == Some(TokenKind::Plain)
                                && self.peek_kind_nth(1) != Some(TokenKind::Colon)
                            {
                                self.error(ErrorKind::TrailingContent, self.current_span());
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
    fn process_block_map(
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
                                self.pop_indent();
                                return Some(Event::MappingEnd {
                                    span: self.collection_end_span(),
                                });
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
                    let (crossed, ws_after_key) = self.skip_ws_and_newlines_tracked();

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

                        let content_col =
                            if matches!(self.peek_kind(), Some(TokenKind::LineStart) | None) {
                                None
                            } else {
                                Some(colon_column + 1 + ws_width)
                            };

                        if is_implicit_scalar_key
                            && self.peek_kind() == Some(TokenKind::BlockSeqIndicator)
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

    /// Check if we're at an implicit mapping key (value followed by colon).
    /// Handles properties (anchor/tag) that may span lines before the key.
    fn is_implicit_key(&self) -> bool {
        self.with_lookahead(200, |window| Self::scan_is_implicit_key(&window))
    }

    /// Recover from a scalar-like line at mapping indentation that looks like a
    /// key but is missing a colon.
    ///
    /// Recovery policy:
    /// - report `MissingColon` at the insertion point after the scalar
    /// - drop the malformed line instead of inventing a mapping pair
    /// - consume any more-indented continuation lines as invalid indentation so
    ///   they are not absorbed by the parent mapping
    fn recover_missing_colon_in_mapping(&mut self, indent: IndentLevel) -> bool {
        let recovered = match self.peek() {
            Some((Token::Plain(_text), span)) => {
                self.error(ErrorKind::MissingColon, Span::at(span.end));
                self.skip_plain_scalar_tokens();
                true
            }
            Some((Token::StringStart(_), span)) => {
                let (_value, full_span) = self.parse_quoted_string_content();
                let scalar_span = full_span.unwrap_or(span);
                self.error(ErrorKind::MissingColon, Span::at(scalar_span.end));
                true
            }
            Some((Token::Alias(_name), span)) => {
                self.error(ErrorKind::MissingColon, Span::at(span.end));
                let _ = self.take_current();
                true
            }
            _ => false,
        };

        if !recovered {
            return false;
        }

        self.skip_to_line_end();
        self.skip_invalid_indented_recovery_lines(indent);
        true
    }

    /// Consume tokens until the next `LineStart`, document marker, or EOF.
    fn skip_to_line_end(&mut self) {
        while let Some(kind) = self.peek_kind() {
            if matches!(
                kind,
                TokenKind::LineStart | TokenKind::DocStart | TokenKind::DocEnd
            ) {
                break;
            }
            let _ = self.take_current();
        }
    }

    /// Skip lines indented more deeply than `indent`, reporting
    /// `InvalidIndentation` when they contain content.
    fn skip_invalid_indented_recovery_lines(&mut self, indent: IndentLevel) {
        while let Some((next_indent, line_span)) = self.peek_line_start() {
            if next_indent <= indent {
                break;
            }

            let has_content = !self.line_start_is_blank_from(1);
            let _ = self.take_current();
            self.current_indent = next_indent;
            self.last_line_start_span = line_span;

            if has_content {
                self.error(
                    ErrorKind::InvalidIndentation,
                    Self::indented_line_error_span(line_span, next_indent),
                );
            }
            self.skip_to_line_end();
        }
    }

    /// Parse a quoted string content, consuming tokens from `StringStart` through `StringEnd`.
    /// Returns the assembled string value and the span covering the entire quoted string.
    fn parse_quoted_string_content(&mut self) -> (Cow<'input, str>, Option<Span>) {
        let start_span = self.current_span();
        let mut content = String::new();

        // Skip StringStart
        if self.peek_kind() == Some(TokenKind::StringStart) {
            let _ = self.take_current();
        }

        // Collect content until StringEnd
        loop {
            match self.peek() {
                Some((Token::StringContent(text), _)) => {
                    content.push_str(&text);
                    let _ = self.take_current();
                }
                Some((Token::StringEnd(_), span)) => {
                    let end_span = span;
                    let _ = self.take_current();
                    let full_span =
                        Span::from_usize_range(start_span.start_usize()..end_span.end_usize());
                    return (Cow::Owned(content), Some(full_span));
                }
                None => break,
                _ => {
                    let _ = self.take_current();
                }
            }
        }
        (Cow::Owned(content), None)
    }

    /// Check if an indentation level is valid (exists in the indent stack).
    ///
    /// Returns `true` if the indent is in the stack (matches an active block's level).
    /// Returns `false` if the indent falls between existing stack entries (orphan).
    ///
    /// NOTE: This does NOT return true for `indent > top` because that case needs
    /// to be handled differently. When starting a new nested block, we push BEFORE
    /// checking. When checking continuation content, `indent > top` is orphan.
    fn is_valid_indent(&self, indent: IndentLevel) -> bool {
        // Only exact matches with stack entries are valid
        self.indent_stack.contains(&indent)
    }

    /// Push an indentation level onto the stack when entering a block structure.
    fn push_indent(&mut self, indent: IndentLevel) {
        self.indent_stack.push(indent);
    }

    /// Pop an indentation level from the stack when exiting a block structure.
    fn pop_indent(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    /// Report an `InvalidIndentation` error.
    /// Uses `last_line_start_span` so that spans are consistent across nested blocks.
    fn report_invalid_indent(&mut self) {
        self.error(ErrorKind::InvalidIndentation, self.last_line_start_span);
    }

    /// Check if there's content at an orphan level, starting from peek offset `start_offset`.
    fn has_content_at_orphan_level_from(&self, start_offset: usize) -> bool {
        // Check if the next token is content that would trigger an error
        if let Some(kind) = self.peek_kind_nth(start_offset) {
            matches!(
                kind,
                TokenKind::Plain
                    | TokenKind::StringStart
                    | TokenKind::MappingKey
                    | TokenKind::Colon
                    | TokenKind::Anchor
                    | TokenKind::Tag
            )
        } else {
            false
        }
    }

    /// Return true if the line beginning at the current `LineStart` offset is effectively blank.
    ///
    /// The caller provides the starting offset just after the `LineStart` token and this helper
    /// skips structural trivia until it finds either content or another line/document boundary.
    fn line_start_is_blank_from(&self, start_offset: usize) -> bool {
        self.with_lookahead(start_offset + 8, |window| {
            Self::scan_line_start_is_blank_from(&window, start_offset)
        })
    }

    /// Skip tokens comprising a plain scalar (Plain + whitespace on same line).
    fn skip_plain_scalar_tokens(&mut self) {
        while let Some(kind) = self.peek_kind() {
            match kind {
                TokenKind::Plain | TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => {
                    let _ = self.take_current();
                }
                _ => break,
            }
        }
    }

    /// Check if the current Plain token is followed by a Colon (making it a mapping key).
    ///
    /// This is used to stop plain scalar continuation when the next line
    /// looks like a mapping key (`key: value`).
    fn is_plain_followed_by_colon(&self) -> bool {
        self.with_lookahead(11, |window| Self::scan_is_plain_followed_by_colon(&window))
    }

    /// Check if the current `FlowSeqStart` token is part of a complex key pattern.
    ///
    /// This looks ahead through the flow sequence to find the closing `]`,
    /// then checks if it's followed by `:` (making it a mapping key).
    fn is_flow_seq_complex_key(&self) -> bool {
        self.with_lookahead(200, |window| Self::scan_is_flow_seq_complex_key(&window))
    }

    /// Check if the current `FlowSeqStart` token is followed immediately by
    /// `:` after the matching `]`.
    ///
    /// This is used for explicit `?` keys, where `? []: x` should parse as an
    /// inner mapping key node, but `?\n  []\n: x` must leave the `:` for the
    /// outer explicit-key separator.
    fn is_flow_seq_inline_complex_key(&self) -> bool {
        self.with_lookahead(200, |window| {
            Self::scan_is_flow_seq_inline_complex_key(&window)
        })
    }

    /// Check if the current `FlowMapStart` token is part of a complex key pattern.
    ///
    /// This looks ahead through the flow mapping to find the closing `}`,
    /// then checks if it's followed by `:` (making it a mapping key).
    fn is_flow_map_complex_key(&self) -> bool {
        self.with_lookahead(200, |window| Self::scan_is_flow_map_complex_key(&window))
    }

    /// Check if the current `FlowMapStart` token is followed immediately by
    /// `:` after the matching `}`.
    ///
    /// This is the explicit-key counterpart to `is_flow_map_complex_key()`.
    fn is_flow_map_inline_complex_key(&self) -> bool {
        self.with_lookahead(200, |window| {
            Self::scan_is_flow_map_inline_complex_key(&window)
        })
    }

    fn current_flow_collection_is_complex_key(&self, explicit_key: bool) -> bool {
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
    #[allow(clippy::too_many_lines, reason = "Complex lookahead logic")]
    fn is_implicit_flow_mapping_entry(&self) -> bool {
        self.with_lookahead(200, |window| {
            Self::scan_is_implicit_flow_mapping_entry(&window)
        })
    }

    /// Skip line-prefix whitespace tokens.
    fn skip_indent_tokens(&mut self) {
        while let Some(kind) = self.peek_kind() {
            match kind {
                TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => {
                    let _ = self.take_current();
                }
                _ => break,
            }
        }
    }

    /// Try to consume a plain scalar continuation.
    /// Returns true if continuation was found, false if we should stop.
    ///
    /// `min_indent` specifies the minimum indent for content. A `BlockSeqIndicator`
    /// at column `c` is only a valid entry marker if `c < min_indent`. Otherwise,
    /// it's part of the plain scalar text (e.g., `- foo\n - bar` where the second
    /// `-` is at the content indent level, not the sequence indent level).
    #[allow(
        clippy::string_slice,
        reason = "Positions from tokens are UTF-8 boundaries"
    )]
    fn try_consume_plain_continuation(
        &mut self,
        content: &mut String,
        end_span: &mut Span,
        consecutive_newlines: &mut usize,
        _min_indent: IndentLevel,
    ) -> bool {
        match self.peek() {
            Some((Token::Plain(next_text), next_span)) => {
                // Before consuming as continuation, check if it's a mapping key
                if self.is_plain_followed_by_colon() {
                    return false;
                }

                // Apply folding: single newline → space, multiple → n-1 newlines
                Self::append_folded_separator(content, *consecutive_newlines);
                content.push_str(&next_text);
                *end_span = next_span;
                *consecutive_newlines = 0;
                let _ = self.take_current();
                true
            }
            Some((Token::LineStart(_), _)) => {
                // Another newline, continue counting (caller will handle)
                true
            }
            // Handle BlockSeqIndicator inside the content area.
            // We only reach here when the line's indent >= min_indent (checked by
            // the caller). Any `-` on such a line is at column >= current_indent
            // >= min_indent, so it's always plain text, never a valid entry marker.
            // (Valid entry markers are at indent < min_indent, handled by
            // has_continuation_after_low_indent.)
            Some((Token::BlockSeqIndicator, span)) => {
                let start_pos = span.start_usize();
                let (line_text, line_end) = self.consume_line_as_text(start_pos);

                if !line_text.is_empty() {
                    Self::append_folded_separator(content, *consecutive_newlines);
                    content.push_str(line_text);
                    *consecutive_newlines = 0;
                    *end_span = Span::from_usize_range(start_pos..line_end);
                }
                true
            }
            // Handle Anchor, Tag, Alias as plain text (continuation)
            // These look like special tokens but in plain scalar context they're just text.
            // IMPORTANT: If an Anchor/Tag is at the start of a line and followed by a
            // scalar+colon pattern, it's a mapping key, not continuation text.
            Some((Token::Anchor(_) | Token::Tag(_) | Token::Alias(_), span)) => {
                // Check if this anchor/tag starts a mapping key pattern
                // Look ahead: (Anchor|Tag)* (Plain|Quoted) Colon
                if self.is_anchor_tag_mapping_key() {
                    return false;
                }

                let start_pos = span.start_usize();
                let (line_text, line_end) = self.consume_line_as_text(start_pos);

                if !line_text.is_empty() {
                    Self::append_folded_separator(content, *consecutive_newlines);
                    content.push_str(line_text);
                    *consecutive_newlines = 0;
                    *end_span = Span::from_usize_range(start_pos..line_end);
                }
                true
            }
            _ => false,
        }
    }

    /// Consume all tokens until `LineStart`, returning the text as a slice.
    /// Used for treating anchor/tag tokens as plain text in scalar continuations.
    #[allow(
        clippy::string_slice,
        reason = "Positions from tokens are UTF-8 boundaries"
    )]
    fn consume_line_as_text(&mut self, start_pos: usize) -> (&'input str, usize) {
        let mut line_end = start_pos;
        while let Some(is_line_start) = self.peek_with(|tok, tok_span| {
            if matches!(tok, Token::LineStart(_)) {
                return ControlFlow::Break(());
            }
            ControlFlow::Continue(tok_span.end_usize())
        }) {
            let ControlFlow::Continue(end_usize) = is_line_start else {
                break;
            };
            line_end = end_usize;
            let _ = self.take_current();
        }
        let line_text = &self.input[start_pos..line_end];
        (line_text.trim_end(), line_end)
    }

    /// Append folded separator based on number of consecutive newlines.
    /// Single newline → space, multiple → n-1 newlines.
    fn append_folded_separator(content: &mut String, consecutive_newlines: usize) {
        if consecutive_newlines == 1 {
            content.push(' ');
        } else {
            for _ in 1..consecutive_newlines {
                content.push('\n');
            }
        }
    }

    /// Check if there's a valid continuation after a low-indent line (empty line case).
    /// Looks ahead to see if there's a properly indented `LineStart` followed by content.
    fn has_continuation_after_low_indent(&self, min_indent: IndentLevel) -> bool {
        self.with_lookahead(20, |window| {
            Self::scan_has_continuation_after_low_indent(&window, min_indent)
        })
    }

    /// Check if current Anchor/Tag starts a mapping key pattern.
    /// Pattern: (Anchor|Tag)+ (Whitespace|LineStart)* (Plain|Quoted) Whitespace* Colon
    /// This handles properties that span multiple lines, e.g.:
    /// ```yaml
    /// &m2
    /// key2: val2
    /// ```
    fn is_anchor_tag_mapping_key(&self) -> bool {
        self.with_lookahead(50, |window| Self::scan_is_anchor_tag_mapping_key(&window))
    }

    #[inline]
    fn scan_skip_inline_whitespace_from(
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

    #[inline]
    fn scan_skip_flow_key_trivia_from(
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
    fn scan_skip_flow_entry_prefix_from(
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
    fn scan_skip_flow_plain_continuations_from(
        window: &LookaheadWindow<'_, 'input>,
        mut offset: usize,
    ) -> usize {
        loop {
            offset = Self::scan_skip_inline_whitespace_from(window, offset);
            if window.kind(offset) == Some(TokenKind::LineStart)
                && window.kind(offset + 1) == Some(TokenKind::Plain)
            {
                offset += 2;
                continue;
            }
            return Self::scan_skip_inline_whitespace_from(window, offset);
        }
    }

    fn scan_find_string_end_from(
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

    fn scan_find_flow_collection_end(
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

    fn scan_is_plain_followed_by_colon(window: &LookaheadWindow<'_, 'input>) -> bool {
        if window.kind(0) != Some(TokenKind::Plain) {
            return false;
        }
        let i = Self::scan_skip_inline_whitespace_from(window, 1);
        i <= 11 && window.kind(i) == Some(TokenKind::Colon)
    }

    fn scan_is_implicit_key(window: &LookaheadWindow<'_, 'input>) -> bool {
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
            Some(TokenKind::Plain) => {
                i += 1;
                while let Some(kind) = window.kind(i) {
                    match kind {
                        TokenKind::Colon => return true,
                        TokenKind::LineStart
                        | TokenKind::DocEnd
                        | TokenKind::DocStart
                        | TokenKind::FlowSeqStart
                        | TokenKind::FlowMapStart => return false,
                        _ => i += 1,
                    }
                    if i > 30 {
                        break;
                    }
                }
                false
            }
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

    fn scan_line_start_is_blank_from(
        window: &LookaheadWindow<'_, 'input>,
        start_offset: usize,
    ) -> bool {
        let mut i = start_offset;
        loop {
            match window.kind(i) {
                Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => i += 1,
                Some(TokenKind::LineStart | TokenKind::DocStart | TokenKind::DocEnd) | None => {
                    return true;
                }
                Some(_) => return false,
            }
        }
    }

    fn scan_is_flow_seq_complex_key(window: &LookaheadWindow<'_, 'input>) -> bool {
        if window.kind(0) != Some(TokenKind::FlowSeqStart) {
            return false;
        }

        Self::scan_find_flow_collection_end(window, 0, TokenKind::FlowSeqStart)
            .map(|end| Self::scan_skip_flow_key_trivia_from(window, end + 1, false))
            .is_some_and(|offset| window.kind(offset) == Some(TokenKind::Colon))
    }

    fn scan_is_flow_seq_inline_complex_key(window: &LookaheadWindow<'_, 'input>) -> bool {
        if window.kind(0) != Some(TokenKind::FlowSeqStart) {
            return false;
        }

        Self::scan_find_flow_collection_end(window, 0, TokenKind::FlowSeqStart)
            .is_some_and(|end| window.kind(end + 1) == Some(TokenKind::Colon))
    }

    fn scan_is_flow_map_complex_key(window: &LookaheadWindow<'_, 'input>) -> bool {
        if window.kind(0) != Some(TokenKind::FlowMapStart) {
            return false;
        }

        Self::scan_find_flow_collection_end(window, 0, TokenKind::FlowMapStart)
            .map(|end| Self::scan_skip_flow_key_trivia_from(window, end + 1, true))
            .is_some_and(|offset| window.kind(offset) == Some(TokenKind::Colon))
    }

    fn scan_is_flow_map_inline_complex_key(window: &LookaheadWindow<'_, 'input>) -> bool {
        if window.kind(0) != Some(TokenKind::FlowMapStart) {
            return false;
        }

        Self::scan_find_flow_collection_end(window, 0, TokenKind::FlowMapStart)
            .is_some_and(|end| window.kind(end + 1) == Some(TokenKind::Colon))
    }

    fn scan_is_implicit_flow_mapping_entry(window: &LookaheadWindow<'_, 'input>) -> bool {
        let mut i = Self::scan_skip_flow_entry_prefix_from(window, 0);

        match window.kind(i) {
            Some(TokenKind::Plain) => {
                i = Self::scan_skip_flow_plain_continuations_from(window, i + 1);
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

    fn scan_has_continuation_after_low_indent(
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

    fn scan_is_mapping_key_at_offset(window: &LookaheadWindow<'_, 'input>, offset: usize) -> bool {
        if window.kind(offset) != Some(TokenKind::Plain) {
            return false;
        }
        let i = Self::scan_skip_inline_whitespace_from(window, offset + 1);
        i <= offset + 11 && window.kind(i) == Some(TokenKind::Colon)
    }

    fn scan_is_anchor_tag_mapping_key(window: &LookaheadWindow<'_, 'input>) -> bool {
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
            Some(TokenKind::Plain) => {
                let colon_offset = Self::scan_skip_inline_whitespace_from(window, i + 1);
                window.kind(colon_offset) == Some(TokenKind::Colon)
            }
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
    fn process_flow_seq(
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
                            let flow_end = span.end_usize();
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
                            let flow_end = span.end_usize();
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
    fn handle_doc_marker_in_flow_map(
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
    fn process_flow_map(&mut self, phase: FlowMapPhase, start_span: Span) -> Option<Event<'input>> {
        match phase {
            FlowMapPhase::BeforeKey => {
                self.skip_ws_and_newlines();

                match self.peek_kind_with_span() {
                    Some((TokenKind::FlowMapEnd, span)) => {
                        let flow_end = span.end_usize();
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
                        let flow_end = span.end_usize();
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

    // ─────────────────────────────────────────────────────────────
    // Scalar Parsing
    // ─────────────────────────────────────────────────────────────

    /// Parse a scalar or detect if it's actually a mapping key.
    ///
    /// If the scalar is followed by `:`, it's an implicit mapping key.
    /// In that case, emit `MappingStart` and set up states to parse the mapping.
    ///
    /// If `is_implicit_key` is true, we're already parsing an implicit key, so
    /// we don't look for nested implicit mappings.
    ///
    /// If `crossed_line_boundary` is true, any anchor/tag was collected BEFORE a line break,
    /// which means they belong to the mapping, not the first key.
    ///
    /// If `allow_implicit_mapping` is false, don't create nested implicit block mappings.
    /// This is used when parsing values immediately after `:` on the same line - YAML
    /// requires a line break for nested implicit mappings like `a:\n  b: c`.
    fn parse_scalar_or_mapping(
        &mut self,
        min_indent: IndentLevel,
        properties: EmitterProperties<'input>,
        is_implicit_key: bool,
        crossed_line_boundary: bool,
        allow_implicit_mapping: bool,
        content_column: Option<IndentLevel>,
    ) -> Event<'input> {
        // Check if this is an implicit mapping key (scalar followed by colon)
        // Skip this check if:
        // - we're already parsing an implicit key (avoid infinite recursion)
        // - we're in flow context (different rules apply)
        // - nested implicit mappings aren't allowed (same line as parent colon)
        let looks_like_implicit_key = !is_implicit_key
            && self.flow_depth == 0
            && allow_implicit_mapping
            && match self.peek_kind() {
                Some(TokenKind::Plain) => self.is_plain_followed_by_colon(),
                Some(TokenKind::StringStart | TokenKind::Alias) => self.is_implicit_key(),
                _ => false,
            };
        if looks_like_implicit_key {
            // This is a block mapping with an implicit key
            let span = self.current_span();

            // Determine where properties belong based on crossed_line_boundary:
            // - If crossed_line_boundary: properties belong to MAPPING, key has none
            // - Otherwise: properties belong to KEY (same line), mapping has none
            let (map_props, key_props) = if crossed_line_boundary {
                // Properties crossed a line boundary, so they belong to the mapping
                (properties, EmitterProperties::default())
            } else {
                // Same-line properties already flowed through ValueCollectProperties,
                // so there is nothing left to collect here.
                (EmitterProperties::default(), properties)
            };
            self.skip_ws();

            // Parse the key scalar with its properties
            let mut key_props = key_props;
            let key_event = self.parse_plain_scalar(key_props.take(), min_indent);

            // Determine mapping indent based on context:
            // - If there are properties (anchor or tag), use current_indent because
            //   the mapping's indent is the line's indent (e.g., `&a a: b` at root = indent 0)
            // - Otherwise (no properties, e.g., `- key: value`), use key's column position
            //   because that's where continuation lines need to align
            let has_properties = !map_props.is_empty()
                || matches!(
                    &key_event,
                    Event::Scalar {
                        properties: ev_props,
                        ..
                    } if ev_props
                        .as_ref()
                        .is_some_and(|event_props| {
                            event_props.anchor.is_some() || event_props.tag.is_some()
                        })
                );
            // content_column tracks where the key started on the line.
            // When there are properties or we crossed a line, use current_indent
            // (mapping is at line indent level). Otherwise use content_column
            // (mapping is at the key's column position).
            let map_indent = if crossed_line_boundary || has_properties {
                self.current_indent
            } else {
                content_column.unwrap_or(self.current_indent)
            };

            let mapping_start = self.build_block_mapping_from_scalar_key(
                map_indent,
                span,
                map_props,
                key_event,
                content_column,
            );
            return mapping_start;
        }

        // Not a mapping key, just parse as a scalar
        let result = self.parse_plain_scalar(properties, min_indent);
        // If this is a mapping key, check for multiline implicit key error
        if is_implicit_key && let Event::Scalar { span, .. } = &result {
            self.check_multiline_implicit_key(*span);
        }
        result
    }

    /// Parse a plain scalar (potentially multiline).
    ///
    /// Implements basic multiline plain scalar handling:
    /// - Single newline followed by content becomes a space
    /// - Multiple consecutive newlines preserve (n-1) newlines
    ///
    /// `min_indent` specifies the minimum indentation for continuation lines.
    /// Continuation lines must have indent >= `min_indent` to be considered part of the scalar.
    fn parse_plain_scalar(
        &mut self,
        properties: EmitterProperties<'input>,
        min_indent: IndentLevel,
    ) -> Event<'input> {
        match self.peek() {
            Some((Token::Plain(text), span)) => {
                // Extract values before releasing the borrow
                let first_line = text.clone(); // Clone the Cow (cheap if Borrowed)
                let start_span = span;
                let _ = self.take_current();

                // Check for reserved indicator `%` at column 0 starting a plain scalar.
                // Per YAML 1.2 spec production [22] c-indicator and [126] ns-plain-first,
                // `%` is a c-indicator and cannot start a plain scalar.

                if self.flow_depth > 0 {
                    let mut offset = 0;
                    while matches!(
                        self.peek_kind_nth(offset),
                        Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs)
                    ) {
                        offset += 1;
                    }
                    let has_continuation = self.peek_kind_nth(offset) == Some(TokenKind::LineStart)
                        && self.peek_kind_nth(offset + 1) == Some(TokenKind::Plain);
                    if !has_continuation {
                        return Event::Scalar {
                            style: ScalarStyle::Plain,
                            value: first_line,
                            properties: properties.into_event_box(),
                            span,
                        };
                    }

                    self.state_stack.push(ParseState::PlainScalarFlow {
                        first_line,
                        properties,
                        start_span,
                        end_span: span,
                        has_continuation: false,
                        content: None,
                    });
                    return self
                        .process_state_stack()
                        .unwrap_or_else(|| self.emit_null());
                }

                if self.peek_kind() == Some(TokenKind::LineStart) {
                    if let Some((next_indent, _)) = self.peek_line_start()
                        && next_indent < min_indent
                    {
                        match self.peek_kind_nth(1) {
                            Some(
                                TokenKind::BlockSeqIndicator
                                | TokenKind::MappingKey
                                | TokenKind::DocStart
                                | TokenKind::DocEnd,
                            )
                            | None => {
                                return Event::Scalar {
                                    style: ScalarStyle::Plain,
                                    value: first_line,
                                    properties: properties.into_event_box(),
                                    span,
                                };
                            }
                            _ if !self.has_continuation_after_low_indent(min_indent) => {
                                return Event::Scalar {
                                    style: ScalarStyle::Plain,
                                    value: first_line,
                                    properties: properties.into_event_box(),
                                    span,
                                };
                            }
                            _ => {}
                        }
                    }

                    self.state_stack.push(ParseState::PlainScalarBlock {
                        first_line,
                        properties,
                        start_span,
                        end_span: span,
                        min_indent,
                        consecutive_newlines: 0,
                        has_continuation: false,
                        content: None,
                    });
                    return self
                        .process_state_stack()
                        .unwrap_or_else(|| self.emit_null());
                }

                // Combine spans
                Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: first_line,
                    properties: properties.into_event_box(),
                    span,
                }
            }

            Some((Token::StringStart(quote_style), _)) => {
                self.parse_quoted_scalar(properties, quote_style, min_indent)
            }

            _ => self.emit_null(),
        }
    }

    /// Continue parsing a block-context plain scalar after the first line has been consumed.
    #[allow(
        clippy::too_many_arguments,
        reason = "arguments mirror the resumable plain-scalar block state payload"
    )]
    fn process_plain_scalar_block_state(
        &mut self,
        first_line: Cow<'input, str>,
        properties: EmitterProperties<'input>,
        start_span: Span,
        mut end_span: Span,
        min_indent: IndentLevel,
        mut consecutive_newlines: usize,
        mut has_continuation: bool,
        mut content: Option<String>,
    ) -> Event<'input> {
        if let Some((indent, line_span)) = self.peek_line_start() {
            if indent >= min_indent {
                let _ = self.take_current();
                consecutive_newlines += 1;

                self.skip_indent_tokens();

                let content_str = content.get_or_insert_with(|| first_line.clone().into_owned());

                if self.try_consume_plain_continuation(
                    content_str,
                    &mut end_span,
                    &mut consecutive_newlines,
                    min_indent,
                ) {
                    has_continuation = true;
                    self.state_stack.push(ParseState::PlainScalarBlock {
                        first_line,
                        properties,
                        start_span,
                        end_span,
                        min_indent,
                        consecutive_newlines,
                        has_continuation,
                        content,
                    });
                    return self
                        .process_state_stack()
                        .unwrap_or_else(|| self.emit_null());
                }

                if !self.is_valid_indent(indent) && self.has_content_at_orphan_level_from(1) {
                    self.error(
                        ErrorKind::InvalidIndentation,
                        Self::indented_line_error_span(line_span, indent),
                    );
                    self.skip_to_line_end();
                    self.skip_invalid_indented_recovery_lines(min_indent.saturating_sub(1));
                }
            } else if self.has_continuation_after_low_indent(min_indent) {
                let _ = self.take_current();
                consecutive_newlines += 1;
                self.state_stack.push(ParseState::PlainScalarBlock {
                    first_line,
                    properties,
                    start_span,
                    end_span,
                    min_indent,
                    consecutive_newlines,
                    has_continuation,
                    content,
                });
                return self
                    .process_state_stack()
                    .unwrap_or_else(|| self.emit_null());
            }
        }

        let final_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());
        let value = if has_continuation {
            Cow::Owned(content.unwrap_or_else(|| first_line.into_owned()))
        } else {
            first_line
        };

        Event::Scalar {
            style: ScalarStyle::Plain,
            value,
            properties: properties.into_event_box(),
            span: final_span,
        }
    }

    /// Continue parsing a flow-context plain scalar after the first line has been consumed.
    fn process_plain_scalar_flow_state(
        &mut self,
        first_line: Cow<'input, str>,
        properties: EmitterProperties<'input>,
        start_span: Span,
        mut end_span: Span,
        mut has_continuation: bool,
        mut content: Option<String>,
    ) -> Event<'input> {
        loop {
            while let Some(kind) = self.peek_kind() {
                match kind {
                    TokenKind::Whitespace | TokenKind::WhitespaceWithTabs => {
                        let _ = self.take_current();
                    }
                    _ => break,
                }
            }

            if self.peek_kind() == Some(TokenKind::LineStart)
                && let Some((Token::Plain(continuation), next_span)) = self.peek_nth(1)
            {
                has_continuation = true;
                let content_str = content.get_or_insert_with(|| first_line.clone().into_owned());
                content_str.push(' ');
                content_str.push_str(&continuation);
                end_span = next_span;
                let _ = self.take_current();
                let _ = self.take_current();
                continue;
            }

            let final_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());
            let value = if has_continuation {
                Cow::Owned(content.unwrap_or_else(|| first_line.into_owned()))
            } else {
                first_line
            };

            return Event::Scalar {
                style: ScalarStyle::Plain,
                value,
                properties: properties.into_event_box(),
                span: final_span,
            };
        }
    }

    /// Parse a quoted scalar.
    /// `min_indent` is used to validate that continuation lines have proper indentation.
    #[allow(
        clippy::too_many_lines,
        reason = "Quoted scalar parsing with many cases"
    )]
    fn parse_quoted_scalar(
        &mut self,
        properties: EmitterProperties<'input>,
        quote_style: crate::lexer::QuoteStyle,
        min_indent: IndentLevel,
    ) -> Event<'input> {
        let start_span = self.current_span();
        let _ = self.take_current(); // consume StringStart

        // Try to optimize for single-content-token case (most common)
        // Check if we have: StringContent followed immediately by StringEnd
        let (single_token_value, first_content) =
            if let Some((Token::StringContent(content), content_span)) = self.peek() {
                let content_cow = content.clone();
                let _ = self.take_current();

                // Check if next is StringEnd
                if self.peek_kind() == Some(TokenKind::StringEnd) {
                    (Some((content_cow.clone(), content_span)), None)
                } else {
                    // Multi-line case: save the first content we already consumed
                    (None, Some(content_cow))
                }
            } else {
                (None, None)
            };

        if let Some((content_cow, content_span)) = single_token_value {
            // Fast path: single content token, zero-copy!
            let end_span = if self.peek_kind() == Some(TokenKind::StringEnd) {
                let span = self.current_span();
                let _ = self.take_current();
                span
            } else {
                content_span
            };

            let style = match quote_style {
                crate::lexer::QuoteStyle::Single => ScalarStyle::SingleQuoted,
                crate::lexer::QuoteStyle::Double => ScalarStyle::DoubleQuoted,
            };

            let full_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

            return Event::Scalar {
                style,
                value: content_cow,
                properties: properties.into_event_box(),
                span: full_span,
            };
        }

        let mut parts: Vec<Cow<'input, str>> = Vec::new();
        if let Some(first) = first_content {
            parts.push(first);
        }
        self.state_stack.push(ParseState::QuotedScalar {
            properties,
            quote_style,
            min_indent,
            start_span,
            parts,
            end_span: start_span,
            pending_newlines: 0,
            needs_trim: false,
        });
        self.process_state_stack()
            .unwrap_or_else(|| self.emit_null())
    }

    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "arguments and control flow mirror the resumable quoted-scalar state payload"
    )]
    fn process_quoted_scalar_state(
        &mut self,
        properties: EmitterProperties<'input>,
        quote_style: crate::lexer::QuoteStyle,
        min_indent: IndentLevel,
        start_span: Span,
        mut parts: Vec<Cow<'input, str>>,
        mut end_span: Span,
        mut pending_newlines: usize,
        mut needs_trim: bool,
    ) -> Event<'input> {
        match self.peek() {
            Some((Token::StringContent(content), span)) => {
                if needs_trim {
                    if let Some(last) = parts.last_mut() {
                        match last {
                            Cow::Borrowed(text) => {
                                let trimmed = text.trim_end_matches(' ');
                                if trimmed.len() != text.len() {
                                    *last = Cow::Borrowed(trimmed);
                                }
                            }
                            Cow::Owned(text) => {
                                let new_len = text.trim_end_matches(' ').len();
                                text.truncate(new_len);
                            }
                        }
                    }
                    needs_trim = false;
                }

                if pending_newlines > 0 {
                    if pending_newlines == 1 {
                        parts.push(Cow::Borrowed(" "));
                    } else {
                        for _ in 1..pending_newlines {
                            parts.push(Cow::Borrowed("\n"));
                        }
                    }
                    pending_newlines = 0;
                }

                parts.push(content.clone());
                end_span = span;
                let _ = self.take_current();
                self.state_stack.push(ParseState::QuotedScalar {
                    properties,
                    quote_style,
                    min_indent,
                    start_span,
                    parts,
                    end_span,
                    pending_newlines,
                    needs_trim,
                });
                return self
                    .process_state_stack()
                    .unwrap_or_else(|| self.emit_null());
            }
            Some((Token::LineStart(indent), line_start_span)) => {
                let is_content_line = self.peek_kind_nth(1) == Some(TokenKind::StringContent);
                if is_content_line && indent < min_indent {
                    self.error(
                        ErrorKind::InvalidIndentationContext {
                            expected: min_indent,
                            found: indent,
                        },
                        line_start_span,
                    );
                }

                needs_trim = true;
                pending_newlines += 1;
                end_span = line_start_span;
                let _ = self.take_current();
                self.state_stack.push(ParseState::QuotedScalar {
                    properties,
                    quote_style,
                    min_indent,
                    start_span,
                    parts,
                    end_span,
                    pending_newlines,
                    needs_trim,
                });
                return self
                    .process_state_stack()
                    .unwrap_or_else(|| self.emit_null());
            }
            Some((Token::StringEnd(_), span)) => {
                if needs_trim && let Some(last) = parts.last_mut() {
                    match last {
                        Cow::Borrowed(text) => {
                            let trimmed = text.trim_end_matches(' ');
                            if trimmed.len() != text.len() {
                                *last = Cow::Borrowed(trimmed);
                            }
                        }
                        Cow::Owned(text) => {
                            let new_len = text.trim_end_matches(' ').len();
                            text.truncate(new_len);
                        }
                    }
                }

                if pending_newlines == 1 {
                    parts.push(Cow::Borrowed(" "));
                } else if pending_newlines > 1 {
                    for _ in 1..pending_newlines {
                        parts.push(Cow::Borrowed("\n"));
                    }
                }
                end_span = span;
                let _ = self.take_current();
            }
            _ => {
                self.error(ErrorKind::UnterminatedString, start_span);
            }
        }

        let value: String = parts.iter().map(std::convert::AsRef::as_ref).collect();

        let style = match quote_style {
            crate::lexer::QuoteStyle::Single => ScalarStyle::SingleQuoted,
            crate::lexer::QuoteStyle::Double => ScalarStyle::DoubleQuoted,
        };

        let full_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

        Event::Scalar {
            style,
            value: Cow::Owned(value),
            properties: properties.into_event_box(),
            span: full_span,
        }
    }

    /// Parse a block scalar (literal or folded).
    ///
    /// This implementation tracks line types for proper folding:
    /// - Normal lines: fold single newlines to spaces (for folded style)
    /// - Empty lines: preserve as newlines
    /// - More-indented lines: preserve with newlines (don't fold)
    #[allow(
        clippy::too_many_lines,
        reason = "Block scalar parsing with many cases"
    )]
    fn parse_block_scalar(
        &mut self,
        min_indent: IndentLevel,
        properties: EmitterProperties<'input>,
    ) -> Event<'input> {
        let Some((
            Token::LiteralBlockHeader(header) | Token::FoldedBlockHeader(header),
            start_span,
        )) = self.peek()
        else {
            return self.emit_null();
        };

        let is_literal = self.peek_kind() == Some(TokenKind::LiteralBlockHeader);
        let _ = self.take_current(); // consume header
        self.state_stack.push(ParseState::BlockScalar {
            properties,
            header,
            start_span,
            min_indent,
            is_literal,
        });
        self.process_state_stack()
            .unwrap_or_else(|| self.emit_null())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "block scalar folding and chomping rules are easier to validate in one place"
    )]
    fn process_block_scalar_state(
        &mut self,
        properties: EmitterProperties<'input>,
        header: crate::lexer::BlockScalarHeader,
        start_span: Span,
        min_indent: IndentLevel,
        is_literal: bool,
    ) -> Event<'input> {
        let mut end_span = start_span;
        // Streaming builder for the block scalar value. For literal style we
        // append one newline per logical line. For folded style we track
        // previous and last-content line types to implement YAML's folding
        // rules (mirroring `join_folded_lines`).
        let mut value = String::new();
        let mut prev_type = LineType::Empty;
        let mut last_content_type: Option<LineType> = None;
        let mut scalar_has_any_part = false; // any non-empty line (including whitespace-only)
        let mut had_any_line = false; // any logical line at all

        // Per YAML spec 8.1.1.1: content_indent = block_scalar_indent + explicit_indicator
        // The block_scalar_indent is min_indent - 1 (the parent block's indentation).
        let explicit_indent: Option<usize> = header.indent.map(usize::from);
        let min_indent_usize = usize::from(min_indent);
        // When explicit indicator is present, calculate content_indent from it
        // Otherwise, auto-detect from first content line
        let mut content_indent: Option<usize> =
            explicit_indent.map(|ei| min_indent_usize.saturating_sub(1) + ei);

        // Track empty lines before content for indentation validation.
        // If an empty line has MORE indentation than the first content line,
        // it's an InvalidIndentation error.
        let mut empty_lines_before_content: Vec<(usize, Span)> = Vec::new();

        // Skip to first line of content
        while let Some(kind) = self.peek_kind() {
            match kind {
                TokenKind::Comment => {
                    let _ = self.take_current();
                }
                _ => break,
            }
        }

        // Expect LineStart to begin each line
        while let Some((indent_level, line_start_span)) = self.peek_line_start() {
            let n = usize::from(indent_level);
            let _ = self.take_current();

            // Check for tabs used as indentation in block scalar content.
            // Per YAML spec, tabs are allowed as content but not as indentation.
            // Tabs are invalid if they appear BEFORE reaching the content indent level.
            // For example, in `foo: |\n\t\tbar`, if content_indent is 2, the tabs at column 0
            // are indentation (invalid). But in `foo: |\n  \tbar`, the tab at column 2 is
            // content (valid).
            if self.peek_kind() == Some(TokenKind::WhitespaceWithTabs) {
                let tab_span = self.current_span();
                // Tab is right after LineStart — its column is current_indent.
                let tab_col = usize::from(self.current_indent);
                if let Some(ci) = content_indent {
                    // Content indent is known - tabs before it are indentation (invalid)
                    if tab_col < ci {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                    }
                } else {
                    // Content indent not yet determined.
                    // Tabs before min_indent are definitely indentation (invalid).
                    // Tabs at or after min_indent might be content, so we can't check yet.
                    if tab_col < min_indent_usize {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                    }
                }
            }

            // Check if this line ends the block scalar
            let is_terminator = matches!(
                self.peek_kind(),
                None | Some(TokenKind::DocEnd | TokenKind::DocStart)
            );
            if is_terminator && n == 0 {
                break;
            }

            // Check if line has content (for termination and indent detection)
            // Include flow indicators since they can be content in block scalars.
            // Also include block structure indicators (BlockSeqIndicator, MappingKey) since they
            // indicate sibling nodes and should trigger termination checks.
            let has_content = matches!(
                self.peek_kind(),
                Some(
                    TokenKind::Plain
                        | TokenKind::Whitespace
                        | TokenKind::WhitespaceWithTabs
                        | TokenKind::Comment
                        | TokenKind::FlowSeqStart
                        | TokenKind::FlowSeqEnd
                        | TokenKind::FlowMapStart
                        | TokenKind::FlowMapEnd
                        | TokenKind::Colon
                        | TokenKind::Comma
                        | TokenKind::BlockSeqIndicator
                        | TokenKind::MappingKey
                )
            );

            // For block scalars with no content yet: a non-empty line at or below the parent
            // indent level (min_indent - 1) cannot be block scalar content - it must be
            // the next sibling node. For example, empty block scalars like:
            //   strip: >-
            //
            //   clip: >
            // The "clip:" is at the same indent as "strip:", so it's not block scalar content.
            if content_indent.is_none() && has_content && n < min_indent_usize {
                break;
            }

            // Track empty lines before content for indentation validation
            if content_indent.is_none() && !has_content {
                empty_lines_before_content.push((n, line_start_span));
            }

            // Determine content indent from first non-empty line
            if content_indent.is_none() && has_content && n > 0 {
                content_indent = Some(n);
                // Validate preceding empty lines - they should not have MORE indentation
                // than the first content line.
                for (empty_indent, empty_span) in &empty_lines_before_content {
                    if *empty_indent > n {
                        self.error(ErrorKind::InvalidIndentation, *empty_span);
                    }
                }
            }

            // Check termination: lines with content at indent < content_indent terminate the block
            // When explicit indicator is present, content_indent was already calculated correctly
            let should_terminate = if let Some(ci) = content_indent {
                has_content && n < ci
            } else {
                false
            };

            if should_terminate {
                break;
            }

            had_any_line = true;

            // Collect content on this line as parts (zero-copy where possible).
            // Use a small pre-allocation since typical lines have a handful of
            // segments (plain + some whitespace/punctuation).
            let mut line_parts: Vec<Cow<'input, str>> = Vec::with_capacity(4);
            // Track whether this line has any non-whitespace content so we can
            // classify purely-whitespace lines without re-scanning all parts.
            let mut has_non_whitespace = false;
            let mut has_tab_in_prefix = false;
            let line_type;
            let mut line_end_span = line_start_span;

            // Check for extra indentation (more-indented)
            let extra_indent = content_indent.map_or(0, |ci| n.saturating_sub(ci));
            if extra_indent > 0 {
                // Add extra spaces for more-indented lines
                // Optimize: use a static buffer for common indent sizes
                const SPACES: &str = "                                "; // 32 spaces
                if let Some(spaces) = SPACES.get(..extra_indent) {
                    line_parts.push(Cow::Borrowed(spaces));
                } else {
                    line_parts.push(Cow::Owned(" ".repeat(extra_indent)));
                }
            }

            while let Some((tok, span)) = self.peek() {
                match tok {
                    Token::Plain(text) => {
                        line_parts.push(text.clone());
                        // `Plain` tokens always contain some non-whitespace content;
                        // the lexer emits dedicated whitespace tokens.
                        has_non_whitespace = true;
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    Token::Whitespace => {
                        line_parts.push(Cow::Borrowed(" "));
                        let _ = self.take_current();
                    }
                    Token::WhitespaceWithTabs => {
                        // Tabs in content are preserved - read actual content from input
                        // since it could be multiple characters (tab + space, etc.)
                        #[allow(
                            clippy::string_slice,
                            reason = "Span positions are UTF-8 boundaries"
                        )]
                        {
                            let ws = &self.input[span.start_usize()..span.end_usize()];
                            if !has_non_whitespace {
                                // Tabs before any non-whitespace content are part of
                                // the leading indentation within the block scalar
                                // content. Treating such lines as "more-indented"
                                // matches the YAML folding semantics tested by MJS9.
                                has_tab_in_prefix = true;
                            }
                            line_parts.push(Cow::Borrowed(ws));
                        }
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    Token::Comment(text) => {
                        // In block scalars, # is NOT a comment indicator - it's literal content
                        // The lexer incorrectly tokenizes it as a comment, so we need to
                        // reconstruct the original text including the #
                        // Optimize: avoid format! macro, use string concatenation
                        let mut comment_text = String::with_capacity(1 + text.len());
                        comment_text.push('#');
                        comment_text.push_str(&text);
                        line_parts.push(Cow::Owned(comment_text));
                        has_non_whitespace = true;
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    // In block scalars, flow indicators are literal content, not structure.
                    // The lexer doesn't know we're in a block scalar context.
                    Token::FlowSeqStart => {
                        line_parts.push(Cow::Borrowed("["));
                        has_non_whitespace = true;
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    Token::FlowSeqEnd => {
                        line_parts.push(Cow::Borrowed("]"));
                        has_non_whitespace = true;
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    Token::FlowMapStart => {
                        line_parts.push(Cow::Borrowed("{"));
                        has_non_whitespace = true;
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    Token::FlowMapEnd => {
                        line_parts.push(Cow::Borrowed("}"));
                        has_non_whitespace = true;
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    Token::Colon => {
                        // Colon is literal content in block scalars
                        line_parts.push(Cow::Borrowed(":"));
                        has_non_whitespace = true;
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    Token::Comma => {
                        // Comma is literal content in block scalars
                        line_parts.push(Cow::Borrowed(","));
                        has_non_whitespace = true;
                        line_end_span = span;
                        let _ = self.take_current();
                    }
                    _ => break,
                }
            }

            // Recover trailing whitespace that the lexer didn't include in tokens.
            // The lexer creates Plain("foo") for "foo " (missing trailing space).
            // If the next token is a LineStart, any gap between line_end_span.end and
            // LineStart.start is trailing whitespace to preserve.
            #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
            if !line_parts.is_empty()
                && let Some((_, next_span)) = self.peek_line_start()
            {
                let end_pos = line_end_span.end_usize();
                let next_start = next_span.start_usize();
                if next_start > end_pos {
                    // There's a gap - check if it's all whitespace
                    let trailing = &self.input[end_pos..next_start];
                    if trailing.chars().all(|ch| ch == ' ' || ch == '\t') {
                        // This is guaranteed to be whitespace-only
                        line_parts.push(Cow::Borrowed(trailing));
                    }
                }
            }

            // Classify line type using the tracked non-whitespace flag.
            if line_parts.is_empty() || !has_non_whitespace {
                if line_parts.is_empty() {
                    line_type = LineType::Empty;
                } else {
                    // Whitespace-only line: treat as more-indented whitespace.
                    line_type = LineType::MoreIndent;
                }
            } else if extra_indent > 0 || has_tab_in_prefix {
                // Lines that are visually more-indented than the content
                // indent, including lines whose leading whitespace contains
                // tabs before the first non-whitespace character, are
                // treated as `MoreIndent`. This matches the YAML test suite
                // case MJS9 (Spec Example 6.7. Block Folding), where a line
                // starting with a tab must be preserved more literally.
                line_type = LineType::MoreIndent;
            } else {
                line_type = LineType::Normal;
            }

            // Only update end_span if this line had actual content (including
            // whitespace-only content). Structurally empty lines don't extend the span.
            if !line_parts.is_empty() {
                end_span = line_end_span;
                scalar_has_any_part = true;
            }

            // Stream this line directly into the final value instead of
            // accumulating all lines first.
            if is_literal {
                // Literal: preserve all newlines. Each logical line contributes
                // exactly one newline, regardless of whether it has content.
                for part in &line_parts {
                    value.push_str(part);
                }
                value.push('\n');
            } else {
                // Folded: mirror `join_folded_lines` but operate per-line.
                match line_type {
                    LineType::Empty => {
                        // Empty line contributes newline
                        if prev_type == LineType::MoreIndent {
                            value.push('\n'); // Line break after more-indented
                        }
                        value.push('\n');
                    }
                    LineType::MoreIndent => {
                        // More-indented lines preserve the preceding newline, but we
                        // need to be careful about Empty→MoreIndent transitions.
                        if !value.is_empty() {
                            match prev_type {
                                LineType::Normal | LineType::MoreIndent => {
                                    // Line break before more-indented is NOT folded
                                    value.push('\n');
                                }
                                LineType::Empty => {
                                    // Empty already added its newline. We only add
                                    // another if we came from Normal context through
                                    // Empty. MoreIndent→Empty→MoreIndent: no extra
                                    // newline needed. Normal→Empty→MoreIndent: need
                                    // extra newline.
                                    if last_content_type == Some(LineType::Normal) {
                                        value.push('\n');
                                    }
                                }
                            }
                        }
                        for part in &line_parts {
                            value.push_str(part);
                        }
                        last_content_type = Some(LineType::MoreIndent);
                    }
                    LineType::Normal => {
                        if !value.is_empty() {
                            match prev_type {
                                LineType::Normal => {
                                    value.push(' '); // Fold
                                }
                                LineType::MoreIndent => {
                                    value.push('\n'); // Preserve
                                }
                                LineType::Empty => {
                                    // Newlines already added
                                }
                            }
                        }
                        for part in &line_parts {
                            value.push_str(part);
                        }
                        last_content_type = Some(LineType::Normal);
                    }
                }
                prev_type = line_type;
            }
        }

        // For folded scalars, mirror the final newline behaviour of
        // `join_folded_lines`: add a trailing newline when there was at
        // least one line and the last line was not Empty.
        if !is_literal && had_any_line && prev_type != LineType::Empty {
            value.push('\n');
        }

        // For empty block scalars (no actual content), use just the header span
        // Otherwise, span from header start to end of last content
        // A scalar is empty if no line contributed any parts at all (even
        // whitespace-only lines).
        let is_empty_scalar = !scalar_has_any_part;

        // Apply chomping. We pass `is_empty_scalar` so that `Chomping::Clip`
        // can distinguish truly empty scalars (like K858's `clip: >` with no
        // content) from scalars that merely end with newlines.
        let chomped_value = Self::apply_chomping(&value, header, is_empty_scalar);

        let style = if is_literal {
            ScalarStyle::Literal
        } else {
            ScalarStyle::Folded
        };

        let full_span = if is_empty_scalar {
            start_span
        } else {
            Span::from_usize_range(start_span.start_usize()..end_span.end_usize())
        };

        Event::Scalar {
            style,
            value: Cow::Owned(chomped_value),
            properties: properties.into_event_box(),
            span: full_span,
        }
    }

    // `join_folded_lines` logic is now inlined into `parse_block_scalar` for
    // a streaming implementation; helper removed.

    /// Apply chomping indicator to block scalar value.
    ///
    /// `is_empty_scalar` is true when no logical line contributed any
    /// content parts at all. This lets us distinguish truly empty block
    /// scalars (like K858's `clip: >` with no content lines) from scalars
    /// that merely end with trailing newlines.
    fn apply_chomping(
        value: &str,
        header: crate::lexer::BlockScalarHeader,
        is_empty_scalar: bool,
    ) -> String {
        use crate::lexer::Chomping;

        match header.chomping {
            Chomping::Strip => {
                // Strip: remove all trailing newlines.
                // Work on the borrowed input first, then allocate exactly once.
                let trimmed = value.trim_end_matches('\n');
                trimmed.to_owned()
            }
            Chomping::Clip => {
                // Clip: ensure at most one trailing newline, and only if
                // there's actual content.
                //
                // For *truly empty* block scalars (no content lines at all),
                // the YAML Test Suite (K858) expects an empty string even for
                // `>` (clip). Treat those separately based on
                // `is_empty_scalar` instead of just looking at `value`.
                if is_empty_scalar {
                    return String::new();
                }

                if value.ends_with('\n') {
                    // Has at least one trailing newline: keep exactly one.
                    let trimmed = value.trim_end_matches('\n');
                    let mut result = String::with_capacity(trimmed.len() + 1);
                    result.push_str(trimmed);
                    result.push('\n');
                    result
                } else if value.is_empty() {
                    // Non-empty scalar should always have at least one
                    // newline, but be defensive.
                    String::new()
                } else {
                    // No trailing newline yet: add exactly one.
                    let mut result = String::with_capacity(value.len() + 1);
                    result.push_str(value);
                    result.push('\n');
                    result
                }
            }
            Chomping::Keep => {
                // Keep: the builder for block scalars already preserves all
                // trailing newlines and ensures at least one when there is
                // content, so we can return the value unchanged.
                value.to_owned()
            }
        }
    }

    // ═══════════════════════════════════════════════════════════════════
    // Iterator Implementation - The State Machine
    // ═══════════════════════════════════════════════════════════════════
    fn next_event_core(&mut self) -> Option<Event<'input>> {
        // Emit StreamStart first
        if !self.emitted_stream_start {
            self.emitted_stream_start = true;
            self.pending_ast_wraps.clear();
            return Some(Event::StreamStart);
        }

        if let Some(event) = self.pending_event.take() {
            self.track_emitted_event(&event);
            return Some(event);
        }

        loop {
            // Document-level state machine
            match &self.doc_state {
                DocState::Ready => {
                    if let Some((explicit, span, initial_col)) = self.prepare_document() {
                        self.doc_state = DocState::EmitDocStart {
                            explicit,
                            span,
                            initial_col,
                        };
                    } else {
                        self.doc_state = DocState::Done;
                        return Some(Event::StreamEnd);
                    }
                }

                DocState::EmitDocStart {
                    explicit,
                    span,
                    initial_col,
                } => {
                    let event = Event::DocumentStart {
                        explicit: *explicit,
                        span: *span,
                    };
                    let initial_col = *initial_col;
                    // Push initial value parse state (top-level document value).
                    // content_column is seeded from prepare_document's tracked position.
                    self.state_stack.push(ParseState::Value {
                        ctx: ValueContext {
                            min_indent: 0,
                            content_column: Some(initial_col),
                            kind: ValueKind::TopLevelValue,
                            allow_implicit_mapping: true, // Document root allows implicit mappings
                            prior_crossed_line: false,
                        },
                        properties: EmitterProperties::default(),
                    });
                    self.doc_state = DocState::Content;
                    return Some(event);
                }

                DocState::Content => {
                    // Process state stack
                    if let Some(event) = self.process_state_stack() {
                        self.track_emitted_event(&event);
                        return Some(event);
                    }
                    // Stack empty, finish document
                    let (explicit, span) = self.finish_document();
                    self.doc_state = DocState::EmitDocEnd { explicit, span };
                }

                DocState::EmitDocEnd { explicit, span } => {
                    let event = Event::DocumentEnd {
                        explicit: *explicit,
                        span: *span,
                    };
                    self.doc_state = DocState::Ready;
                    return Some(event);
                }

                DocState::Done => {
                    return None;
                }
            }
        }
    }

    pub(crate) fn next_ast_event(&mut self) -> Option<AstEvent<'input>> {
        self.next_event_core()
            .map(|event| self.wrap_ast_event(event))
    }
}

impl<'input> Iterator for Emitter<'input> {
    type Item = Event<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        let event = self.next_event_core()?;
        self.discard_pending_ast_wrap();
        Some(event)
    }
}

impl<'input> Iterator for AstEmitter<'_, 'input> {
    type Item = AstEvent<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.emitter.next_ast_event()
    }
}

#[cfg(test)]
pub(crate) fn internal_type_sizes() -> (usize, usize) {
    (
        std::mem::size_of::<states::EmitterProperties<'static>>(),
        std::mem::size_of::<states::ParseState<'static>>(),
    )
}

#[cfg(test)]
mod tests {
    // ============================================================================
    // Event Emitter Tests - Testing event generation directly
    // ============================================================================

    #[test]
    fn test_emit_e76z() {
        // Test E76Z: Aliases as implicit block mapping keys
        // Input: &a a: &b b\n*b : *a\n
        // Expected: Both aliases (*b key and *a value) emit Alias events
        let input = "&a a: &b b\n*b : *a\n";
        let (events, _errors) = crate::emit_events(input);

        let alias_events: Vec<_> = events
            .iter()
            .filter(|event| matches!(event, crate::Event::Alias { .. }))
            .collect();
        assert_eq!(
            alias_events.len(),
            2,
            "Expected 2 Alias events (for *b key and *a value)"
        );
    }

    mod event_generation {
        use crate::{
            error::ErrorKind,
            event::{CollectionStyle, Event, ScalarStyle},
        };

        /// Helper to get events from YAML input using the emitter
        fn events_from(input: &str) -> Vec<Event<'static>> {
            let (events, errors) = crate::emit_events(input);
            assert!(
                errors.is_empty(),
                "unexpected emitter errors for input:\n{input}\nerrors: {errors:?}",
            );
            events.into_iter().map(Event::into_owned).collect()
        }

        #[test]
        fn test_plain_scalar() {
            let events = events_from("hello");
            assert!(events.iter().any(|ev| matches!(
                ev,
                Event::Scalar {
                    style: ScalarStyle::Plain,
                    value,
                    ..
                } if value == "hello"
            )));
        }

        #[test]
        fn test_flow_mapping() {
            let events = events_from("{a: 1}");

            let has_map_start = events.iter().any(|ev| {
                matches!(
                    ev,
                    Event::MappingStart {
                        style: CollectionStyle::Flow,
                        ..
                    }
                )
            });
            let has_map_end = events
                .iter()
                .any(|ev| matches!(ev, Event::MappingEnd { .. }));

            assert!(has_map_start, "Expected MappingStart, got: {events:?}");
            assert!(has_map_end, "Expected MappingEnd, got: {events:?}");
        }

        #[test]
        fn test_flow_sequence() {
            let events = events_from("[1, 2, 3]");

            let has_seq_start = events.iter().any(|ev| {
                matches!(
                    ev,
                    Event::SequenceStart {
                        style: CollectionStyle::Flow,
                        ..
                    }
                )
            });
            let has_seq_end = events
                .iter()
                .any(|ev| matches!(ev, Event::SequenceEnd { .. }));

            assert!(has_seq_start, "Expected SequenceStart, got: {events:?}");
            assert!(has_seq_end, "Expected SequenceEnd, got: {events:?}");
        }

        #[test]
        fn test_block_sequence() {
            let events = events_from("- a\n- b");

            let seq_starts: Vec<_> = events
                .iter()
                .filter(|ev| {
                    matches!(
                        ev,
                        Event::SequenceStart {
                            style: CollectionStyle::Block,
                            ..
                        }
                    )
                })
                .collect();

            // Should have exactly ONE block sequence start, not one per entry
            assert_eq!(
                seq_starts.len(),
                1,
                "Expected 1 SequenceStart for block sequence, got {}: {events:?}",
                seq_starts.len()
            );
        }

        #[test]
        fn test_block_mapping() {
            let events = events_from("a: 1\nb: 2");

            let map_starts: Vec<_> = events
                .iter()
                .filter(|ev| {
                    matches!(
                        ev,
                        Event::MappingStart {
                            style: CollectionStyle::Block,
                            ..
                        }
                    )
                })
                .collect();

            // Should have exactly ONE block mapping start
            assert_eq!(
                map_starts.len(),
                1,
                "Expected 1 MappingStart for block mapping, got {}: {events:?}",
                map_starts.len()
            );
        }

        #[test]
        fn test_block_mapping_with_comment_between_keys() {
            let input = "a: 1\n# comment between keys\nb: 2\n";
            let (raw_events, errors) = crate::emit_events(input);
            let events: Vec<Event<'static>> =
                raw_events.into_iter().map(Event::into_owned).collect();

            assert!(
                errors.is_empty(),
                "expected no errors for mapping with comment between keys, got: {errors:?}"
            );

            // We should still have exactly one block MappingStart and MappingEnd
            let map_starts = events
                .iter()
                .filter(|ev| {
                    matches!(
                        ev,
                        Event::MappingStart {
                            style: CollectionStyle::Block,
                            ..
                        }
                    )
                })
                .count();
            let map_ends = events
                .iter()
                .filter(|ev| matches!(ev, Event::MappingEnd { .. }))
                .count();

            assert_eq!(
                map_starts, 1,
                "expected exactly 1 MappingStart, got {map_starts}: {events:?}",
            );
            assert_eq!(
                map_ends, 1,
                "expected exactly 1 MappingEnd, got {map_ends}: {events:?}",
            );
        }

        #[test]
        fn test_quoted_string() {
            let events = events_from("\"hello world\"");

            let has_quoted = events.iter().any(|ev| {
                matches!(
                    ev,
                    Event::Scalar {
                        style: ScalarStyle::DoubleQuoted,
                        ..
                    }
                )
            });
            assert!(has_quoted, "Expected double-quoted scalar, got: {events:?}");
        }

        #[test]
        fn test_document_markers() {
            let events = events_from("---\nhello\n...");

            let has_doc_start = events
                .iter()
                .any(|ev| matches!(ev, Event::DocumentStart { explicit: true, .. }));
            let has_doc_end = events
                .iter()
                .any(|ev| matches!(ev, Event::DocumentEnd { explicit: true, .. }));

            assert!(has_doc_start, "Expected DocumentStart, got: {events:?}");
            assert!(has_doc_end, "Expected DocumentEnd, got: {events:?}");
        }

        #[test]
        fn test_anchor_and_alias() {
            // Use a sequence so both anchor and alias are in the same document
            let events = events_from("- &anchor value\n- *anchor");

            // Check for anchored scalar
            let has_anchor = events.iter().any(|ev| {
                matches!(
                    ev,
                    Event::Scalar { properties, .. }
                        if properties
                            .as_ref()
                            .and_then(|event_props| event_props.anchor.as_ref())
                            .map(|prop| prop.value.as_ref())
                            == Some("anchor")
                )
            });
            assert!(has_anchor, "Expected scalar with anchor, got: {events:?}");

            // Check for alias
            let has_alias = events.iter().any(|ev| {
                matches!(
                    ev,
                    Event::Alias { name, .. } if name == "anchor"
                )
            });
            assert!(has_alias, "Expected alias, got: {events:?}");
        }

        #[test]
        fn test_tagged_scalar() {
            let events = events_from("!!str 42");

            // Check for tagged scalar with expanded tag
            let has_tag = events.iter().any(|ev| {
                matches!(
                    ev,
                    Event::Scalar { properties, .. }
                        if properties
                            .as_ref()
                            .and_then(|event_props| event_props.tag.as_ref())
                            .map(|prop| prop.value.as_ref())
                            == Some("tag:yaml.org,2002:str")
                )
            });
            assert!(
                has_tag,
                "Expected scalar with expanded tag, got: {events:?}"
            );
        }

        #[test]
        fn test_nested_block_structures() {
            // Test nested mapping inside sequence
            let events = events_from("- a: 1\n- b: 2");

            let seq_count = events
                .iter()
                .filter(|ev| matches!(ev, Event::SequenceStart { .. }))
                .count();
            let map_count = events
                .iter()
                .filter(|ev| matches!(ev, Event::MappingStart { .. }))
                .count();

            assert_eq!(
                seq_count, 1,
                "Expected 1 sequence, got {seq_count}: {events:?}"
            );
            assert!(
                map_count >= 2,
                "Expected at least 2 mappings, got {map_count}: {events:?}"
            );
        }

        #[test]
        fn test_nested_block_mapping_with_comment_between_keys() {
            let input = "outer:\n  a: 1\n  # comment between nested keys\n  b: 2\n";
            let (events, errors) = events_and_errors_from(input);

            assert!(
                errors.is_empty(),
                "expected no errors for nested mapping with comment between keys, got: {errors:?}",
            );

            let map_count = events
                .iter()
                .filter(|ev| matches!(ev, Event::MappingStart { .. }))
                .count();

            assert!(
                map_count >= 2,
                "expected at least 2 mappings (outer + nested), got {map_count}: {events:?}",
            );
        }

        // Helper to get events and errors using the emitter
        fn events_and_errors_from(
            input: &str,
        ) -> (Vec<Event<'static>>, Vec<crate::error::ParseError>) {
            let (events, errors) = crate::emit_events(input);
            (events.into_iter().map(Event::into_owned).collect(), errors)
        }

        #[test]
        fn test_pending_event_single_slot_stress_cases() {
            let input = "\
---
[flow]: block
---
? []: x
---
{ first: Sammy, last: Sosa }:
  hr: 65
  avg: 0.278
---
&a a: &b b
*b : *a
---
!!str : bar
---
top1:
  key1: val1
top2
";
            let (events, errors) = events_and_errors_from(input);

            let flow_seq_starts = events
                .iter()
                .filter(|event| {
                    matches!(
                        event,
                        Event::SequenceStart {
                            style: CollectionStyle::Flow,
                            ..
                        }
                    )
                })
                .count();
            let flow_map_starts = events
                .iter()
                .filter(|event| {
                    matches!(
                        event,
                        Event::MappingStart {
                            style: CollectionStyle::Flow,
                            ..
                        }
                    )
                })
                .count();
            let alias_events = events
                .iter()
                .filter(|event| matches!(event, Event::Alias { .. }))
                .count();
            let empty_tagged_keys = events
                .iter()
                .filter(|event| {
                    matches!(
                        event,
                        Event::Scalar {
                            style: ScalarStyle::Plain,
                            value,
                            properties,
                            ..
                        } if value.is_empty()
                            && properties
                                .as_ref()
                                .and_then(|event_props| event_props.tag.as_ref())
                                .map(|tag| tag.value.as_ref())
                                == Some("tag:yaml.org,2002:str")
                    )
                })
                .count();

            assert!(
                flow_seq_starts >= 2,
                "expected at least 2 flow sequence starts, got {flow_seq_starts}: {events:?}"
            );
            assert!(
                flow_map_starts >= 1,
                "expected at least 1 flow mapping start, got {flow_map_starts}: {events:?}"
            );
            assert!(
                alias_events >= 2,
                "expected alias events from deferred alias-key path, got {alias_events}: {events:?}"
            );
            assert!(
                empty_tagged_keys >= 1,
                "expected tagged empty scalar key from deferred empty-key path, got {events:?}"
            );
            assert!(
                errors
                    .iter()
                    .any(|error| error.kind == ErrorKind::MissingColon),
                "expected MissingColon from stress input, got: {errors:?}"
            );
        }

        #[test]
        fn test_unclosed_flow_sequence_produces_error() {
            // Unclosed flow sequence should report error and auto-close
            let (events, errors) = events_and_errors_from("[a, b");

            // Should produce SequenceStart, scalars, and SequenceEnd (auto-closed)
            assert!(
                events
                    .iter()
                    .any(|event| matches!(event, Event::SequenceStart { .. })),
                "Should have SequenceStart: {events:?}"
            );
            assert!(
                events
                    .iter()
                    .any(|event| matches!(event, Event::SequenceEnd { .. })),
                "Should have SequenceEnd (auto-closed): {events:?}"
            );
            // Should report error
            assert!(
                errors
                    .iter()
                    .any(|err| matches!(err.kind, crate::error::ErrorKind::UnexpectedEof)),
                "Should report UnexpectedEof error: {errors:?}"
            );
        }

        #[test]
        fn test_unclosed_flow_mapping_produces_error() {
            // Unclosed flow mapping should report error and auto-close
            let (events, errors) = events_and_errors_from("{a: 1");

            assert!(
                events
                    .iter()
                    .any(|event| matches!(event, Event::MappingStart { .. })),
                "Should have MappingStart: {events:?}"
            );
            assert!(
                events
                    .iter()
                    .any(|event| matches!(event, Event::MappingEnd { .. })),
                "Should have MappingEnd (auto-closed): {events:?}"
            );
            assert!(
                errors
                    .iter()
                    .any(|err| matches!(err.kind, crate::error::ErrorKind::UnexpectedEof)),
                "Should report UnexpectedEof error: {errors:?}"
            );
        }

        #[test]
        fn test_mismatched_brackets_produces_error() {
            // Mismatched brackets: opened with [ but closed with }
            let (events, errors) = events_and_errors_from("[a, b}");

            // Should produce SequenceStart and SequenceEnd (correct type despite mismatch)
            let seq_starts = events
                .iter()
                .filter(|event| matches!(event, Event::SequenceStart { .. }))
                .count();
            let seq_ends = events
                .iter()
                .filter(|event| matches!(event, Event::SequenceEnd { .. }))
                .count();
            assert_eq!(seq_starts, 1, "Should have 1 SequenceStart: {events:?}");
            assert_eq!(seq_ends, 1, "Should have 1 SequenceEnd: {events:?}");
            // Should report some error for the invalid syntax
            // The exact error type may vary (MismatchedBrackets, MissingSeparator, UnexpectedEof, etc.)
            assert!(
                !errors.is_empty(),
                "Should report at least one error for mismatched brackets: {errors:?}"
            );
        }
    }
}
