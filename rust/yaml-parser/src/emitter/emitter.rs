// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Event emitter - a state-machine YAML parser.
//!
//! This module provides `Emitter`, a YAML parser that produces events
//! using an explicit state stack instead of recursion. It processes
//! a slice of tokens and emits events via the `Iterator` interface.
//!
//! The `Emitter` is validated against the YAML Test Suite to ensure
//! correct event sequences for all inputs.

// This module is work-in-progress. Allow dead code during development.
#![allow(dead_code, reason = "WIP: Emitter is under active development")]
#![allow(
    clippy::map_unwrap_or,
    clippy::match_same_arms,
    clippy::collapsible_if,
    clippy::too_many_lines,
    clippy::shadow_reuse,
    clippy::redundant_locals,
    clippy::unnecessary_wraps,
    clippy::type_complexity,
    clippy::unused_self,
    clippy::string_slice,
    reason = "WIP: These will be addressed as the emitter matures"
)]

use std::borrow::Cow;
use std::collections::HashSet;

use crate::error::{ErrorKind, ParseError};
use crate::event::{Event, ScalarStyle};
use crate::lexer::{RichToken, Token};
use crate::span::{IndentLevel, Span, usize_to_indent};

/// Phase within a block sequence.
#[derive(Debug, Clone, Copy)]
enum BlockSeqPhase {
    /// Emit `SequenceStart`, then transition to `BeforeEntry`.
    EmitStart,
    /// Before parsing an entry - check for `-` indicator.
    BeforeEntry,
    /// After parsing entry value - check for next entry or end.
    AfterEntry,
}

/// Phase within a block mapping.
#[derive(Debug, Clone, Copy)]
enum BlockMapPhase {
    /// Emit `MappingStart`, then transition to `BeforeKey`.
    EmitStart,
    /// Before parsing a key.
    /// `require_line_boundary`: If true, a new entry requires a line boundary.
    /// This is set to true after processing a key-value pair to prevent
    /// same-line entries like `a: b: c`.
    BeforeKey { require_line_boundary: bool },
    /// After key, expect `:` and value.
    /// `is_implicit_scalar_key`: If true, the key was an implicit scalar (like `key:`).
    /// Block sequences on the same line as such keys are invalid (`key: - item`).
    AfterKey { is_implicit_scalar_key: bool },
    /// After value, check for next pair or end.
    AfterValue,
}

/// Phase within a flow sequence.
#[derive(Debug, Clone, Copy)]
enum FlowSeqPhase {
    /// Emit `SequenceStart`, then transition to `BeforeEntry`.
    EmitStart,
    /// Before parsing an entry.
    BeforeEntry,
    /// After entry, expect `,` or `]`.
    AfterEntry,
    /// Emit empty key scalar for implicit mapping with empty key (e.g., `[ : value ]`).
    ImplicitMapEmptyKey { map_start_span: Span },
    /// After implicit mapping key, expect `:` then parse value.
    ImplicitMapValue { map_start_span: Span },
    /// After implicit mapping value, emit `MappingEnd`.
    ImplicitMapEnd { map_start_span: Span },
}

/// Phase within a flow mapping.
#[derive(Debug, Clone, Copy)]
enum FlowMapPhase {
    /// Emit `MappingStart`, then transition to `BeforeKey`.
    EmitStart,
    /// Before parsing a key.
    BeforeKey,
    /// After key, expect `:`.
    AfterKey,
    /// After value, expect `,` or `}`.
    AfterValue,
}

/// A parsing state on the stack.
///
/// Each variant represents a construct being parsed and its current phase.
/// The stack replaces the call stack from recursive descent parsing.
#[derive(Debug, Clone)]
enum ParseState {
    /// Parse any value at a given minimum indent.
    Value {
        min_indent: IndentLevel,
        /// Collected properties (anchor, tag) to apply to this value.
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        /// Whether this value is being parsed as a mapping key.
        /// If true, don't detect implicit mappings (avoid infinite nesting).
        is_key: bool,
        /// Whether this value position allows implicit nested mappings.
        /// Set to `false` when parsing a value immediately after `:` on the same line.
        /// Nested implicit mappings require a line break and indentation.
        allow_implicit_mapping: bool,
    },
    /// Block sequence parsing.
    BlockSeq {
        indent: IndentLevel,
        phase: BlockSeqPhase,
        start_span: Span,
        /// Anchor/tag to attach to `SequenceStart` (only used in `EmitStart` phase).
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    },
    /// Block mapping parsing.
    BlockMap {
        indent: IndentLevel,
        phase: BlockMapPhase,
        start_span: Span,
        /// Anchor/tag to attach to `MappingStart` (only used in `EmitStart` phase).
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    },
    /// Flow sequence parsing.
    FlowSeq {
        phase: FlowSeqPhase,
        start_span: Span,
    },
    /// Flow mapping parsing.
    FlowMap {
        phase: FlowMapPhase,
        start_span: Span,
    },
    /// Emit `SequenceEnd` for block sequence.
    EmitSeqEnd { span: Span },
    /// Emit `MappingEnd` for block mapping.
    EmitMapEnd { span: Span },
    /// Emit `SequenceStart` (for complex key scenarios where `MappingStart` is emitted first).
    EmitSeqStart {
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        span: Span,
    },
    /// Emit a scalar event (used for deferred key emission).
    EmitScalar {
        value: Cow<'static, str>,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        span: Span,
        style: ScalarStyle,
    },
    /// Emit an alias event (used for deferred key emission when alias is a mapping key).
    EmitAlias { name: Cow<'static, str>, span: Span },
}

/// Document-level state.
#[derive(Debug, Clone)]
enum DocState {
    /// Ready to start a new document.
    Ready,
    /// About to emit `DocumentStart`.
    EmitDocStart { explicit: bool, span: Span },
    /// Parsing document content.
    Content,
    /// About to emit `DocumentEnd`.
    EmitDocEnd { explicit: bool, span: Span },
    /// Stream ended.
    Done,
}

/// A YAML event emitter using an explicit state machine.
///
/// Processes a slice of tokens and produces YAML events via the `Iterator` interface.
/// Uses an explicit state stack instead of recursion for parsing.
///
/// ## Architecture Note
///
/// The emitter requires a token slice (not an iterator) because YAML parsing
/// fundamentally requires lookahead to resolve ambiguities. This matches the
/// approach used by `LibYAML` and other production YAML parsers, which buffer
/// tokens internally to support lookahead operations.
#[derive(Debug)]
pub struct Emitter<'tokens, 'input> {
    /// Token slice. Requires slice access for lookahead operations (`peek_nth`).
    tokens: &'tokens [RichToken<'input>],
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
    state_stack: Vec<ParseState>,
    /// Collected errors.
    errors: Vec<ParseError>,
    /// Defined anchors in current document.
    anchors: HashSet<&'input str>,
    /// Whether `StreamStart` has been emitted.
    emitted_stream_start: bool,
    /// Tag handles from directives.
    tag_handles: std::collections::HashMap<&'input str, &'input str>,
    /// Last content span - used for `MappingEnd`/`SequenceEnd` to match batch parser behavior.
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
}

impl<'tokens: 'input, 'input> Emitter<'tokens, 'input> {
    /// Create a new emitter from a token slice.
    #[must_use]
    pub fn new(tokens: &'tokens [RichToken<'input>], input: &'input str) -> Self {
        Self {
            tokens,
            input,
            pos: 0,
            current_indent: 0,
            flow_depth: 0,
            doc_state: DocState::Ready,
            state_stack: Vec::with_capacity(16),
            errors: Vec::new(),
            anchors: HashSet::new(),
            emitted_stream_start: false,
            tag_handles: std::collections::HashMap::new(),
            last_content_span: None,
            crossed_line_boundary: false,
            indent_stack: vec![0],                 // Start with base level 0
            last_line_start_span: Span::new(0..0), // Default span at start
            flow_context_columns: Vec::new(),
        }
    }

    /// Get collected errors.
    #[must_use]
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    /// Take collected errors.
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.errors)
    }

    // ─────────────────────────────────────────────────────────────
    // Token access helpers
    // ─────────────────────────────────────────────────────────────

    fn peek(&self) -> Option<(&Token<'input>, Span)> {
        self.tokens.get(self.pos).map(|rt| (&rt.token, rt.span))
    }

    fn peek_nth(&self, n: usize) -> Option<(&Token<'input>, Span)> {
        self.tokens.get(self.pos + n).map(|rt| (&rt.token, rt.span))
    }

    fn advance(&mut self) {
        if let Some(rt) = self.tokens.get(self.pos) {
            // Track indent from LineStart tokens
            if let Token::LineStart(n) = &rt.token {
                self.current_indent = *n;
                self.last_line_start_span = rt.span;
                // Mark that we crossed a line boundary - this persists until cleared
                self.crossed_line_boundary = true;
            }
            self.pos += 1;
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|rt| rt.span)
            .unwrap_or_else(|| {
                // At EOF, return span at end of last token.
                // NOTE: This differs from batch parser which returns 0..0 for EOF.
                // The emitter behavior is more correct as it provides actual position info.
                self.tokens
                    .last()
                    .map(|rt| Span::from_usize_range(rt.span.end_usize()..rt.span.end_usize()))
                    .unwrap_or_else(|| Span::from_usize_range(0..0))
            })
    }

    /// Get span for collection end events (MappingEnd/SequenceEnd).
    /// Uses the last content span if available, otherwise falls back to current span.
    /// This matches the batch parser behavior where end spans point to the end of
    /// the last content rather than the next token (like a newline or dedent).
    fn collection_end_span(&self) -> Span {
        if let Some(span) = self.last_content_span {
            // Use end position of last content as a point span
            Span::from_usize_range(span.end_usize()..span.end_usize())
        } else if self.pos > 0 {
            // Fallback: use end of previous token
            self.tokens
                .get(self.pos - 1)
                .map(|rt| Span::from_usize_range(rt.span.end_usize()..rt.span.end_usize()))
                .unwrap_or_else(|| self.current_span())
        } else {
            self.current_span()
        }
    }

    /// Update the last content span after emitting content.
    fn set_last_content_span(&mut self, span: Span) {
        self.last_content_span = Some(span);
    }

    /// Get the column (0-based) of a byte position by looking back to the last newline.
    #[allow(
        clippy::string_slice,
        reason = "Position is validated to be within input bounds"
    )]
    fn column_of_position(&self, pos: usize) -> IndentLevel {
        let before = &self.input[..pos];
        let col = if let Some(newline_pos) = before.rfind('\n') {
            pos - newline_pos - 1
        } else {
            pos // No newline, column is the byte position from start of input
        };
        usize_to_indent(col)
    }

    /// Enter a flow collection context.
    /// Tracks the column where the flow collection started (for `InvalidIndentationContext` errors).
    fn enter_flow_collection(&mut self, start_pos: usize) {
        self.flow_depth += 1;
        let flow_start_column = self.column_of_position(start_pos);
        self.flow_context_columns.push(flow_start_column);
    }

    /// Exit a flow collection context.
    fn exit_flow_collection(&mut self) {
        self.flow_depth = self.flow_depth.saturating_sub(1);
        self.flow_context_columns.pop();
    }

    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError {
            kind,
            span,
            span_offset: 0,
        });
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
        let key_text = &self.input[start..end];

        // Check if the key text contains a newline
        if key_text.contains('\n') {
            // Find the colon span for error reporting (look ahead for colon)
            let colon_span = (0..10)
                .find_map(|i| match self.peek_nth(i) {
                    Some((Token::Colon, span)) => Some(span),
                    _ => None,
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
        // This matches the batch parser's check_multiline_implicit_key which returns early
        // if no colon is found.
        let mut check_idx = 0;
        let colon_span = loop {
            match self.peek_nth(check_idx) {
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => check_idx += 1,
                Some((Token::Colon, span)) => break span,
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

        // If there's an Indent token, it means indentation increased via spaces.
        // Any whitespace (including tabs) AFTER the Indent is separation, not indentation.
        // So tabs are only invalid if WhitespaceWithTabs appears IMMEDIATELY after LineStart
        // (i.e., no Indent token in between).
        if matches!(self.peek_nth(0), Some((Token::Indent(_), _))) {
            // Indent token present - tabs after it are allowed (separation whitespace)
            return;
        }

        // Check if current token is WhitespaceWithTabs (immediately after LineStart, no Indent)
        if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek_nth(0) {
            let tab_span = tab_span;
            // Look ahead to see what follows the whitespace (starting after the tab)
            let mut look_ahead = 1;
            while let Some((tok, _)) = self.peek_nth(look_ahead) {
                match tok {
                    Token::Whitespace | Token::WhitespaceWithTabs => look_ahead += 1,
                    // Tabs allowed before:
                    // - Flow collection start/end (entering/exiting flow)
                    // - Blank line (line has only whitespace)
                    Token::FlowMapStart
                    | Token::FlowMapEnd
                    | Token::FlowSeqStart
                    | Token::FlowSeqEnd
                    | Token::LineStart(_) => return,
                    // Any other content - tabs used for indentation, which is invalid
                    _ => break,
                }
            }
            // If we exhausted tokens (EOF), tabs are allowed (trailing whitespace)
            if self.peek_nth(look_ahead).is_none() {
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
        // Only check if current token is WhitespaceWithTabs at column 0
        if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek() {
            let col = self.column_of_position(tab_span.start_usize());
            if col == 0 {
                // Look ahead to check it's not a blank line
                let mut look_ahead = 1;
                while let Some((tok, _)) = self.peek_nth(look_ahead) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => look_ahead += 1,
                        // Blank line or flow end - tabs allowed
                        Token::LineStart(_) | Token::FlowMapEnd | Token::FlowSeqEnd => return,
                        // Content at column 0 with leading tab - error
                        _ => {
                            self.error(ErrorKind::InvalidIndentation, tab_span);
                            return;
                        }
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
        if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek() {
            // Look ahead to see what follows
            let mut lookahead = 1;
            while let Some((tok, _)) = self.peek_nth(lookahead) {
                match tok {
                    Token::Whitespace | Token::WhitespaceWithTabs => lookahead += 1,
                    // Block structure indicators after tab - error (ambiguous indentation)
                    Token::BlockSeqIndicator | Token::MappingKey | Token::Colon => {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                        return;
                    }
                    // Scalar followed by colon - this creates an implicit mapping (like `key:`)
                    // which is also ambiguous after tabs
                    Token::Plain(_) => {
                        // Check if the scalar is followed by a colon
                        if self
                            .peek_nth(lookahead + 1)
                            .is_some_and(|(next_tok, _)| matches!(next_tok, Token::Colon))
                        {
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
        if let Some((tok, span)) = self.peek() {
            let is_content = matches!(
                tok,
                Token::Plain(_)
                    | Token::StringStart(_)
                    | Token::Anchor(_)
                    | Token::Alias(_)
                    | Token::Tag(_)
                    | Token::BlockSeqIndicator
            );
            if is_content && span.start_usize() == flow_end {
                self.error(ErrorKind::ContentOnSameLine, span);
            }
        }
    }

    /// Check for trailing content at column 0 after a closed structure.
    /// This is used for block sequences at the root level.
    /// Content at the same indentation level after them is invalid.
    fn check_trailing_content_at_root(&mut self, root_indent: IndentLevel) {
        // Skip whitespace, newlines, and dedent tokens using peek-only
        let mut peek_offset = 0;
        while let Some((tok, _)) = self.peek_nth(peek_offset) {
            match tok {
                Token::Whitespace
                | Token::WhitespaceWithTabs
                | Token::Comment(_)
                | Token::Dedent
                | Token::LineStart(_) => {
                    peek_offset += 1;
                }
                _ => break,
            }
        }

        // Check if there's content at or below the root indentation
        if let Some((tok, span)) = self.peek_nth(peek_offset) {
            // Extra flow collection end tokens are always an error (unmatched brackets)
            if matches!(tok, Token::FlowSeqEnd | Token::FlowMapEnd) {
                self.error(ErrorKind::UnmatchedBracket, span);
            } else {
                let is_content = matches!(
                    tok,
                    Token::Plain(_)
                        | Token::StringStart(_)
                        | Token::Anchor(_)
                        | Token::Alias(_)
                        | Token::Tag(_)
                        | Token::FlowSeqStart
                        | Token::FlowMapStart
                        | Token::BlockSeqIndicator
                );

                // Get the column of this token by its span position
                let col = self.column_of_position(span.start_usize());

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

    fn skip_ws(&mut self) {
        while matches!(
            self.peek(),
            Some((Token::Whitespace | Token::WhitespaceWithTabs, _))
        ) {
            self.advance();
        }
    }

    fn skip_ws_and_newlines(&mut self) {
        self.skip_ws_and_newlines_returns_crossed_line();
    }

    /// Skip whitespace, newlines, indentation tokens, and comments.
    /// Returns `true` if a `LineStart` token was encountered (line boundary crossed).
    fn skip_ws_and_newlines_returns_crossed_line(&mut self) -> bool {
        self.skip_ws_and_newlines_impl().0
    }

    /// Skip whitespace, newlines, indentation tokens, and comments.
    /// Returns (`crossed_line`, `last_linestart_span`) where:
    /// - `crossed_line`: true if a `LineStart` token was encountered
    /// - `last_linestart_span`: span of the last `LineStart` token (for empty value positioning)
    fn skip_ws_and_newlines_impl(&mut self) -> (bool, Option<Span>) {
        let mut crossed_line = false;
        let mut last_linestart_span = None;
        loop {
            match self.peek() {
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => self.advance(),
                Some((Token::LineStart(indent), span)) => {
                    let indent = *indent;
                    let span = span;
                    crossed_line = true;
                    last_linestart_span = Some(span);
                    self.advance();

                    // In flow context, check for invalid column 0 continuations.
                    // Column 0 is only allowed if the outermost flow context started at column 0.
                    if let Some(&outermost_flow_col) = self.flow_context_columns.first() {
                        if indent == 0 && outermost_flow_col > 0 {
                            // Check if there's actual content after this LineStart
                            // Skip past Whitespace tokens (tabs used as indentation)
                            // and check the actual column of the content token
                            let mut peek_offset = 0;
                            while let Some((tok, _)) = self.peek_nth(peek_offset) {
                                if matches!(tok, Token::Whitespace | Token::WhitespaceWithTabs) {
                                    peek_offset += 1;
                                } else {
                                    break;
                                }
                            }

                            if let Some((tok, content_span)) = self.peek_nth(peek_offset) {
                                let is_content = !matches!(
                                    tok,
                                    Token::LineStart(_)
                                        | Token::FlowSeqEnd
                                        | Token::FlowMapEnd
                                        | Token::Dedent
                                        | Token::DocEnd
                                );
                                // Check actual column of content token
                                let content_col =
                                    self.column_of_position(content_span.start_usize());
                                if is_content && content_col == 0 {
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
                    }

                    // Check for tabs as indentation after crossing a line boundary
                    // In flow context, only tabs at column 0 are invalid
                    if self.flow_context_columns.is_empty() {
                        self.check_tabs_as_indentation();
                    } else {
                        self.check_tabs_at_column_zero_in_flow();
                    }
                }
                Some((Token::Indent(_), _)) => self.advance(),
                Some((Token::Comment(_), _)) => self.advance(),
                // NOTE: Do NOT consume Dedent tokens here!
                // Block mapping/sequence handlers need to see them to know when to end.
                // Each Dedent token represents one level of dedentation, so each nested
                // structure should consume one Dedent when it ends.
                _ => break,
            }
        }
        (crossed_line, last_linestart_span)
    }

    // ─────────────────────────────────────────────────────────────
    // Document-level handling
    // ─────────────────────────────────────────────────────────────

    /// Prepare for the next document. Returns `Some((explicit, span))` if a
    /// document should start, or `None` if at EOF.
    fn prepare_document(&mut self) -> Option<(bool, Span)> {
        self.skip_ws_and_newlines();

        if self.is_eof() {
            return None;
        }

        // Skip orphan DocEnd markers
        while matches!(self.peek(), Some((Token::DocEnd, _))) {
            self.advance();
            self.skip_ws_and_newlines();
        }

        if self.is_eof() {
            return None;
        }

        // Reset document state
        self.anchors.clear();

        // Populate tag handles from directive tokens
        self.populate_tag_handles();

        // Track directives for "directive without document" error
        let mut has_directive = false;
        let mut first_directive_span = Span::from_usize_range(0..0);

        // Skip directive tokens
        while let Some((tok, span)) = self.peek() {
            match tok {
                Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_) => {
                    if !has_directive {
                        first_directive_span = span;
                    }
                    has_directive = true;
                    self.advance();
                    self.skip_ws_and_newlines();
                }
                _ => break,
            }
        }

        // Check for "directive without document" error
        if has_directive {
            let at_end = self.is_eof() || matches!(self.peek(), Some((Token::DocEnd, _)));
            if at_end {
                self.error(ErrorKind::TrailingContent, first_directive_span);
                return None;
            }
        }

        if self.is_eof() {
            return None;
        }

        // Check for explicit `---`
        let has_doc_start = matches!(self.peek(), Some((Token::DocStart, _)));
        let span = self.current_span();

        if has_doc_start {
            self.advance();
            self.skip_ws();
            // Check for content on same line as ---
            let content_on_line = !self.is_eof()
                && !matches!(self.peek(), Some((Token::LineStart(_) | Token::DocEnd, _)));
            self.skip_ws_and_newlines();

            // Block mapping on start line error
            if content_on_line && !self.is_eof() {
                if self.check_block_mapping_on_start_line() {
                    self.error(ErrorKind::ContentOnSameLine, self.current_span());
                }
            }
        }

        Some((has_doc_start, span))
    }

    /// Check if there's a block mapping starting on the `---` line.
    fn check_block_mapping_on_start_line(&self) -> bool {
        // Look for key: pattern without preceding newline
        let mut i = 0;
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                Token::LineStart(_) => return false,
                Token::Colon => return true,
                Token::MappingKey => return true,
                _ => i += 1,
            }
            if i > 10 {
                break;
            }
        }
        false
    }

    /// Populate tag handles from `TagDirective` tokens.
    fn populate_tag_handles(&mut self) {
        self.tag_handles.clear();
        // Add default handles
        self.tag_handles.insert("!", "!");
        self.tag_handles.insert("!!", "tag:yaml.org,2002:");

        // Scan for TagDirective tokens
        let mut i = self.pos;
        while let Some(rt) = self.tokens.get(i) {
            match &rt.token {
                Token::TagDirective(handle_and_prefix) => {
                    if let Some((handle, prefix)) = handle_and_prefix.split_once(' ') {
                        self.tag_handles.insert(handle, prefix);
                    }
                }
                Token::DocStart | Token::DocEnd => break,
                _ => {}
            }
            i += 1;
        }
    }

    /// Finish the current document. Returns `(explicit, span)` for `DocumentEnd`.
    fn finish_document(&mut self) -> (bool, Span) {
        // Skip trailing whitespace/dedents
        loop {
            self.skip_ws_and_newlines();
            if matches!(self.peek(), Some((Token::Dedent, _))) {
                self.advance();
            } else {
                break;
            }
        }

        // Consume orphan content
        self.consume_trailing_content();

        // Check for `...`
        let has_doc_end = matches!(self.peek(), Some((Token::DocEnd, _)));
        let span = if has_doc_end {
            // Explicit document end: use the `...` token's span
            let doc_end_span = self.current_span();
            self.advance();
            self.skip_ws_and_newlines();
            doc_end_span
        } else {
            // Implicit document end: use the end position of the last content
            // This matches the batch parser behavior and provides more accurate spans
            if let Some(last_span) = self.last_content_span {
                Span::from_usize_range(last_span.end_usize()..last_span.end_usize())
            } else {
                // No content in document, use current position
                self.current_span()
            }
        };

        // Skip trailing dedents
        while matches!(self.peek(), Some((Token::Dedent, _))) {
            self.advance();
        }

        (has_doc_end, span)
    }

    /// Consume trailing content before the next document marker.
    fn consume_trailing_content(&mut self) {
        while !self.is_eof() {
            if matches!(self.peek(), Some((Token::Dedent, _))) {
                self.advance();
                continue;
            }
            if matches!(self.peek(), Some((Token::DocStart | Token::DocEnd, _))) {
                break;
            }
            if matches!(
                self.peek(),
                Some((
                    Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_),
                    _
                ))
            ) {
                break;
            }

            // Check for orphan content and emit error
            if let Some((token, span)) = self.peek() {
                // Extra flow collection end tokens are always an error (unmatched brackets)
                if matches!(token, Token::FlowSeqEnd | Token::FlowMapEnd) {
                    self.error(ErrorKind::UnmatchedBracket, span);
                } else {
                    let is_content = matches!(
                        token,
                        Token::Plain(_)
                            | Token::StringStart(_)
                            | Token::Colon
                            | Token::MappingKey
                            | Token::BlockSeqIndicator
                            | Token::Anchor(_)
                            | Token::Tag(_)
                            | Token::Alias(_)
                            | Token::FlowMapStart
                            | Token::FlowSeqStart
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
            }
            self.advance();
            self.skip_ws_and_newlines();
        }
    }

    /// Emit a null scalar event.
    fn emit_null(&self) -> Event<'input> {
        Event::Scalar {
            style: ScalarStyle::Plain,
            value: Cow::Borrowed(""),
            anchor: None,
            tag: None,
            span: self.current_span(),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════
// Iterator Implementation - The State Machine
// ═══════════════════════════════════════════════════════════════════

impl<'tokens: 'input, 'input> Iterator for Emitter<'tokens, 'input> {
    type Item = Event<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        // Emit StreamStart first
        if !self.emitted_stream_start {
            self.emitted_stream_start = true;
            return Some(Event::StreamStart);
        }

        loop {
            // Document-level state machine
            match &self.doc_state {
                DocState::Ready => {
                    if let Some((explicit, span)) = self.prepare_document() {
                        self.doc_state = DocState::EmitDocStart { explicit, span };
                    } else {
                        self.doc_state = DocState::Done;
                        return Some(Event::StreamEnd);
                    }
                }

                DocState::EmitDocStart { explicit, span } => {
                    let event = Event::DocumentStart {
                        explicit: *explicit,
                        span: *span,
                    };
                    // Push initial value parse state
                    self.state_stack.push(ParseState::Value {
                        min_indent: 0,
                        anchor: None,
                        tag: None,
                        is_key: false,
                        allow_implicit_mapping: true, // Document root allows implicit mappings
                    });
                    self.doc_state = DocState::Content;
                    return Some(event);
                }

                DocState::Content => {
                    // Process state stack
                    if let Some(event) = self.process_state_stack() {
                        // Update last_content_span for content events
                        // This is used by collection_end_span() for MappingEnd/SequenceEnd
                        match &event {
                            Event::Scalar { span, .. }
                            | Event::Alias { span, .. }
                            | Event::MappingEnd { span }
                            | Event::SequenceEnd { span } => {
                                self.last_content_span = Some(*span);
                            }
                            Event::MappingStart { .. } | Event::SequenceStart { .. } => {
                                // Reset on collection start - content will update it
                                self.last_content_span = None;
                            }
                            _ => {}
                        }
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
}

impl<'tokens: 'input, 'input> Emitter<'tokens, 'input> {
    /// Process the state stack and return the next event, if any.
    ///
    /// Returns `None` when the stack is empty (document content complete).
    fn process_state_stack(&mut self) -> Option<Event<'input>> {
        loop {
            let state = self.state_stack.pop()?;

            match state {
                ParseState::Value {
                    min_indent,
                    anchor,
                    tag,
                    is_key,
                    allow_implicit_mapping,
                } => {
                    if let Some(event) =
                        self.parse_value(min_indent, anchor, tag, is_key, allow_implicit_mapping)
                    {
                        return Some(event);
                    }
                    // No value produced, continue with next state
                }

                ParseState::BlockSeq {
                    indent,
                    phase,
                    start_span,
                    anchor,
                    tag,
                } => {
                    if let Some(event) =
                        self.process_block_seq(indent, phase, start_span, anchor, tag)
                    {
                        return Some(event);
                    }
                }

                ParseState::BlockMap {
                    indent,
                    phase,
                    start_span,
                    anchor,
                    tag,
                } => {
                    if let Some(event) =
                        self.process_block_map(indent, phase, start_span, anchor, tag)
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

                ParseState::EmitSeqEnd { span } => {
                    self.pop_indent();
                    return Some(Event::SequenceEnd { span });
                }

                ParseState::EmitMapEnd { span } => {
                    self.pop_indent();
                    return Some(Event::MappingEnd { span });
                }

                ParseState::EmitScalar {
                    value,
                    anchor,
                    tag,
                    span,
                    style,
                } => {
                    return Some(Event::Scalar {
                        style,
                        value,
                        anchor,
                        tag,
                        span,
                    });
                }

                ParseState::EmitAlias { name, span } => {
                    return Some(Event::Alias { name, span });
                }

                ParseState::EmitSeqStart { anchor, tag, span } => {
                    return Some(Event::SequenceStart {
                        style: crate::event::CollectionStyle::Flow,
                        anchor,
                        tag,
                        span,
                    });
                }
            }
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Value parsing - the dispatch point
    // ─────────────────────────────────────────────────────────────

    /// Parse any value. Returns the first event for this value.
    ///
    /// This is the main dispatch point that determines what kind of value
    /// we're looking at and either emits an event directly or pushes states.
    ///
    /// If `is_key` is true, this value is being parsed as a mapping key and
    /// we should NOT detect implicit mappings (to avoid infinite nesting).
    ///
    /// If `allow_implicit_mapping` is false, don't create nested implicit mappings
    /// even if a colon follows. This is used when parsing values immediately after
    /// a colon on the same line (YAML requires a line break for nested implicit mappings).
    #[allow(clippy::too_many_lines, reason = "Complex value dispatch logic")]
    fn parse_value(
        &mut self,
        min_indent: IndentLevel,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        is_key: bool,
        allow_implicit_mapping: bool,
    ) -> Option<Event<'input>> {
        // Track if we see a LineStart before value content
        // IMPORTANT: We must always call skip_ws_and_newlines_returns_crossed_line() to consume
        // the LineStart token, even if we detect it with matches!. Otherwise, collect_properties
        // won't see the anchor/tag tokens that follow.
        let has_leading_linestart = matches!(self.peek(), Some((Token::LineStart(_), _)));
        let skipped_crossed_line = self.skip_ws_and_newlines_returns_crossed_line();
        let initial_crossed_line = has_leading_linestart || skipped_crossed_line;

        // Check if we crossed a line and landed at an invalid indentation level.
        // The batch parser's advance_to_same_indent reports InvalidIndentation when
        // content appears at indent < min_indent or at an orphan level.
        // Check if the current position has content at an invalid indent.
        if initial_crossed_line && self.current_indent < min_indent {
            // Check if there's content (properties or values) at this invalid indent
            if let Some((tok, span)) = self.peek() {
                if matches!(tok, Token::Anchor(_) | Token::Tag(_)) {
                    // Properties at invalid indent get both InvalidIndentation and OrphanedProperties
                    self.error(ErrorKind::InvalidIndentation, span);
                    self.error(ErrorKind::OrphanedProperties, span);
                } else if matches!(tok, Token::Plain(_) | Token::StringStart(_)) {
                    self.error(ErrorKind::InvalidIndentation, span);
                }
            }
        }

        // Check for properties (anchor, tag) before the value
        // Use min_indent to prevent collecting properties that are at invalid indentation
        let (anchor, tag, prop_crossed_line) =
            self.collect_properties_with_min_indent(anchor, tag, min_indent);

        // Check for orphaned properties: if we crossed a line boundary but stopped because
        // of invalid indent, and the next content is a property, report OrphanedProperties.
        // This handles cases like `key: &x\n!!map\n  a: b` where !!map is at invalid indent.
        if prop_crossed_line {
            if let Some((Token::LineStart(indent), _)) = self.peek() {
                if *indent < min_indent {
                    // Look for property tokens after the LineStart
                    let mut lookahead = 1;
                    if matches!(self.peek_nth(lookahead), Some((Token::Indent(_), _))) {
                        lookahead += 1;
                    }
                    while let Some((Token::Anchor(_) | Token::Tag(_), span)) =
                        self.peek_nth(lookahead)
                    {
                        self.error(ErrorKind::OrphanedProperties, span);
                        lookahead += 1;
                        // Skip whitespace between properties
                        while matches!(
                            self.peek_nth(lookahead),
                            Some((Token::Whitespace | Token::WhitespaceWithTabs, _))
                        ) {
                            lookahead += 1;
                        }
                    }
                }
            }
        }

        // Capture the indent where properties were collected (if any)
        let property_indent = (anchor.is_some() || tag.is_some()).then_some(self.current_indent);

        // Track line crossing from either initial skip or property collection
        let mut crossed_line_boundary = initial_crossed_line || prop_crossed_line;

        // If we crossed a line boundary, implicit mappings are now allowed
        // (even if they weren't before, a line break resets the context)
        let allow_implicit_mapping = allow_implicit_mapping || crossed_line_boundary;

        // IMPORTANT: If we collected properties and the next line is dedented,
        // we need to decide if the properties apply to an empty scalar or should
        // "bridge" to a block collection on the next line.
        //
        // Bridging rules:
        // 1. For SEQUENCE ENTRIES: Never bridge. `- !!str\n-` means !!str is for an
        //    empty scalar, and the second `-` is a sibling entry.
        // 2. For MAPPING VALUES: Only bridge if:
        //    - The dedented content is at the parent collection's indent (min_indent - 1)
        //    - AND the next content is a block collection indicator (- or ?)
        //    E.g., `key: !!seq\n- item` bridges because `-` starts a sequence value.
        //    But `a: &anchor\nb:` does NOT bridge because `b:` is a sibling key.
        if anchor.is_some() || tag.is_some() {
            if let Some((Token::LineStart(next_indent), _)) = self.peek() {
                if *next_indent < min_indent {
                    // Check if we're in a sequence entry context
                    let in_sequence_entry = self.state_stack.last().is_some_and(|state| {
                        matches!(
                            state,
                            ParseState::BlockSeq {
                                phase: BlockSeqPhase::AfterEntry,
                                ..
                            }
                        )
                    });

                    // Check if the dedented line is too far outside the current context.
                    let too_dedented = *next_indent < min_indent.saturating_sub(1);

                    // Check if properties were collected at an invalid (dedented) indent.
                    // Properties are invalid if:
                    // 1. We crossed a line BEFORE collecting them (`initial_crossed_line`)
                    //    meaning they're on a separate line from the key
                    // 2. AND the indent of that line is < min_indent
                    // E.g., `seq:\n&anchor\n- a` where &anchor is at indent 0 < min_indent 1
                    // But `seq: !!seq\n- a` is valid - the tag is inline with the key
                    // (initial_crossed_line=false, prop_crossed_line=true means we crossed
                    // AFTER collecting, not before).
                    let properties_at_invalid_indent = initial_crossed_line
                        && property_indent.is_some_and(|prop_ind| prop_ind < min_indent);

                    // Even if not too dedented, we should NOT bridge to a sibling mapping key.
                    // Look ahead past the LineStart and Dedent tokens to see what follows.
                    // Only bridge if the next content token is a block collection indicator (- or ?).
                    let mut lookahead_idx = 1;
                    while let Some((tok, _)) = self.peek_nth(lookahead_idx) {
                        if matches!(tok, Token::Dedent | Token::Indent(_)) {
                            lookahead_idx += 1;
                        } else {
                            break;
                        }
                    }
                    let can_bridge = !in_sequence_entry
                        && !too_dedented
                        && !properties_at_invalid_indent
                        && self.peek_nth(lookahead_idx).is_some_and(|(token, _)| {
                            matches!(token, Token::BlockSeqIndicator | Token::MappingKey)
                        });

                    if !can_bridge {
                        // Can't bridge - emit empty scalar.
                        // If properties were collected at an invalid indent (below min_indent),
                        // drop them. This matches the batch parser's behavior for cases like:
                        // `seq:\n&anchor\n- a` where &anchor is at indent 0 < min_indent 1
                        let (anchor, tag) = if properties_at_invalid_indent {
                            (None, None) // Drop properties collected at invalid indent
                        } else {
                            (anchor, tag)
                        };
                        return Some(Event::Scalar {
                            style: ScalarStyle::Plain,
                            value: Cow::Borrowed(""),
                            anchor,
                            tag,
                            span: self.current_span(),
                        });
                    }
                    // Bridging to block collection at valid dedent level
                }
            }
        }

        // Skip any more whitespace/newlines after properties
        crossed_line_boundary |= self.skip_ws_and_newlines_returns_crossed_line();

        // Check if we've crossed a line boundary and the content is dedented.
        // In block context, if we're at an indent < min_indent after crossing a line,
        // the value is empty (the dedented content belongs to a parent context).
        // E.g., `key:\n  value` has min_indent=1 for value, but `key:\nnext:` means
        // `next:` is at indent 0 < 1, so `key` has an empty value.
        //
        // EXCEPTION: Block collection indicators (-, ?, :) can appear at the key's
        // indent level to start a collection as the value. E.g.:
        // ```yaml
        // key:
        // - item
        // ```
        // Here `-` is at indent 0, same as `key`, but it's the value (a sequence).
        //
        // BUT NOT for sequence entries: a `-` at the sequence's indent is a SIBLING
        // entry, not a nested value. E.g.:
        // ```yaml
        // - # Empty
        // - value
        // ```
        // The second `-` is a sibling, so the first entry's value is empty.
        let in_sequence_entry = self.state_stack.last().is_some_and(|state| {
            matches!(
                state,
                ParseState::BlockSeq {
                    phase: BlockSeqPhase::AfterEntry,
                    ..
                }
            )
        });
        // Look past Dedent tokens to find the actual content token.
        // This handles bridging cases where properties on a higher-indented line
        // should attach to a block collection on a lower-indented line.
        let mut lookahead_for_indicator = 0;
        while let Some((tok, _)) = self.peek_nth(lookahead_for_indicator) {
            if matches!(tok, Token::Dedent | Token::Indent(_)) {
                lookahead_for_indicator += 1;
            } else {
                break;
            }
        }
        let at_block_indicator = !in_sequence_entry
            && self
                .peek_nth(lookahead_for_indicator)
                .is_some_and(|(tok, _)| {
                    matches!(
                        tok,
                        Token::BlockSeqIndicator | Token::MappingKey | Token::Colon
                    )
                });
        if crossed_line_boundary && self.current_indent < min_indent && !at_block_indicator {
            return Some(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                anchor,
                tag,
                span: self.current_span(),
            });
        }

        // Dispatch based on current token
        match self.peek() {
            None => {
                // EOF - emit empty value / null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor,
                    tag,
                    span: self.current_span(),
                })
            }

            // Dedent token at the top level (before any block collection is started).
            // This can happen when leading comments are at a higher indent than the content.
            // E.g., `# Comment\n  # Indented comment\nkey: value`
            // The Dedent from indent 2 -> 0 needs to be consumed so we can parse `key: value`.
            Some((Token::Dedent, _)) => {
                self.advance();
                // Re-enter Value state to parse the actual value
                self.state_stack.push(ParseState::Value {
                    min_indent,
                    anchor,
                    tag,
                    is_key,
                    allow_implicit_mapping,
                });
                None
            }

            Some((Token::DocEnd | Token::DocStart, span)) => {
                if self.flow_depth > 0 {
                    // Document markers inside flow context are invalid.
                    // Report error and continue - the flow state machine will handle them.
                    // Don't emit a null here; let the caller re-dispatch.
                    let span = span;
                    self.error(ErrorKind::DocumentMarkerInFlow, span);
                    self.advance();
                    // Re-enter Value state to parse the actual value
                    self.state_stack.push(ParseState::Value {
                        min_indent,
                        anchor,
                        tag,
                        is_key,
                        allow_implicit_mapping, // Preserve the flag
                    });
                    None
                } else {
                    // Block context - document marker ends the value, emit null
                    Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        anchor,
                        tag,
                        span: self.current_span(),
                    })
                }
            }

            Some((Token::Alias(name), span)) => {
                // Copy values before advancing (to avoid borrow issues)
                let alias_name_str = *name;
                let alias_span = span;
                self.advance();
                self.skip_ws();

                // Error: Properties (anchor/tag) on alias are invalid
                if (anchor.is_some() || tag.is_some()) && !crossed_line_boundary {
                    self.error(ErrorKind::PropertiesOnAlias, alias_span);
                }

                // Error: Undefined alias
                if !self.anchors.contains(alias_name_str) {
                    self.error(ErrorKind::UndefinedAlias, alias_span);
                }

                let alias_name = Cow::Borrowed(alias_name_str);

                // Check if alias is a mapping key (followed by colon) after crossing line boundary
                // This can create a nested mapping even without outer anchor/tag
                if crossed_line_boundary && matches!(self.peek(), Some((Token::Colon, _))) {
                    // Alias is a mapping key - create a nested mapping
                    // The outer anchor/tag (if any) go on the MappingStart
                    let map_indent = self.current_indent;

                    // Push BlockMap state in AfterKey phase (we already have the key)
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::AfterKey {
                            is_implicit_scalar_key: false, // Alias, not a plain scalar
                        },
                        start_span: alias_span,
                        anchor: None,
                        tag: None,
                    });
                    // Push the alias as the key to emit
                    self.state_stack.push(ParseState::EmitAlias {
                        name: Cow::Owned(alias_name.into_owned()),
                        span: alias_span,
                    });

                    // Push indent level for orphan indent detection
                    self.push_indent(map_indent);
                    // Emit MappingStart with outer anchor/tag
                    return Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        anchor,
                        tag,
                        span: alias_span,
                    });
                }
                // Not a mapping key, just emit the alias
                Some(Event::Alias {
                    name: alias_name,
                    span: alias_span,
                })
            }

            Some((Token::BlockSeqIndicator, _)) => {
                let span = self.current_span();
                // Use column position of `-` indicator for indent tracking, not current_indent
                // This is crucial for nested sequences like `- - item`
                let seq_indent = self.column_of_position(span.start_usize());
                // Push block sequence state with collected anchor/tag
                self.state_stack.push(ParseState::BlockSeq {
                    indent: seq_indent,
                    phase: BlockSeqPhase::EmitStart,
                    start_span: span,
                    anchor,
                    tag,
                });
                // Re-process to emit SequenceStart
                self.process_state_stack()
            }

            Some((Token::MappingKey | Token::Colon, _)) => {
                // In flow context, a tag/anchor followed by colon means an empty tagged/anchored
                // scalar as the key - do NOT start a block mapping inside flow context.
                // E.g., `{ !!str : bar }` - the `!!str` is an empty key with tag
                if self.flow_depth > 0 {
                    return Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        anchor,
                        tag,
                        span: self.current_span(),
                    });
                }

                let span = self.current_span();
                // Use the LINE's indent level for mapping indent tracking, not the column
                // of the `?` or `:`. This ensures that the mapping ends correctly when we
                // encounter entries at the same line indent level.
                let map_indent = self.current_indent;

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
                let props_for_key = !prop_crossed_line
                    && (anchor.is_some() || tag.is_some())
                    && matches!(self.peek(), Some((Token::Colon, _)));

                if props_for_key {
                    // Properties belong to the implicit null key
                    // Emit MappingStart with no properties, then emit null scalar with properties
                    // Use current_indent for mapping indent (the line's indent level),
                    // not the colon's column position
                    self.state_stack.push(ParseState::BlockMap {
                        indent: self.current_indent,
                        phase: BlockMapPhase::AfterKey {
                            is_implicit_scalar_key: false, // Null key with properties, not plain scalar
                        },
                        start_span: span,
                        anchor: None,
                        tag: None,
                    });
                    // Push the null key with properties
                    let owned_anchor = anchor.map(|(name, sp)| (Cow::Owned(name.into_owned()), sp));
                    let owned_tag = tag.map(|(name, sp)| (Cow::Owned(name.into_owned()), sp));
                    self.state_stack.push(ParseState::EmitScalar {
                        value: Cow::Borrowed(""),
                        anchor: owned_anchor,
                        tag: owned_tag,
                        span,
                        style: ScalarStyle::Plain,
                    });
                    // Push indent level for orphan indent detection
                    self.push_indent(self.current_indent);
                    Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        anchor: None,
                        tag: None,
                        span,
                    })
                } else {
                    // Properties belong to the mapping
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::EmitStart,
                        start_span: span,
                        anchor,
                        tag,
                    });
                    self.process_state_stack()
                }
            }

            Some((Token::FlowSeqStart, _)) => {
                let span = self.current_span();

                // In block context, check if this flow sequence is a complex key
                if self.flow_depth == 0 && self.is_flow_seq_complex_key() {
                    // This is a block mapping with a flow sequence as key
                    let map_indent = self.current_indent;

                    self.advance();
                    self.enter_flow_collection(span.start_usize());

                    // Push BlockMap state for after the key
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::AfterKey {
                            is_implicit_scalar_key: false, // Flow sequence key, not plain scalar
                        },
                        start_span: span,
                        anchor: None,
                        tag: None,
                    });

                    // Push FlowSeq state with EmitStart phase to emit SequenceStart next
                    self.state_stack.push(ParseState::FlowSeq {
                        phase: FlowSeqPhase::EmitStart,
                        start_span: span,
                    });

                    // Push indent level for orphan indent detection
                    self.push_indent(map_indent);
                    // Emit MappingStart with the properties
                    return Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        anchor,
                        tag,
                        span,
                    });
                }

                self.advance();
                self.enter_flow_collection(span.start_usize());
                self.state_stack.push(ParseState::FlowSeq {
                    phase: FlowSeqPhase::BeforeEntry,
                    start_span: span,
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Flow,
                    anchor,
                    tag,
                    span,
                })
            }

            Some((Token::FlowMapStart, _)) => {
                let span = self.current_span();

                // In block context, check if this flow mapping is a complex key
                if self.flow_depth == 0 && self.is_flow_map_complex_key() {
                    // This is a block mapping with a flow mapping as key
                    let map_indent = self.current_indent;

                    self.advance();
                    self.enter_flow_collection(span.start_usize());

                    // Push BlockMap state for after the key
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::AfterKey {
                            is_implicit_scalar_key: false, // Flow mapping key, not plain scalar
                        },
                        start_span: span,
                        anchor: None,
                        tag: None,
                    });

                    // Push FlowMap state with EmitStart phase to emit MappingStart next
                    self.state_stack.push(ParseState::FlowMap {
                        phase: FlowMapPhase::EmitStart,
                        start_span: span,
                    });

                    // Push indent level for orphan indent detection
                    self.push_indent(map_indent);
                    // Emit outer block MappingStart with the properties
                    return Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        anchor,
                        tag,
                        span,
                    });
                }

                self.advance();
                self.enter_flow_collection(span.start_usize());
                self.state_stack.push(ParseState::FlowMap {
                    phase: FlowMapPhase::BeforeKey,
                    start_span: span,
                });
                Some(Event::MappingStart {
                    style: crate::event::CollectionStyle::Flow,
                    anchor,
                    tag,
                    span,
                })
            }

            Some((Token::LiteralBlockHeader(_) | Token::FoldedBlockHeader(_), _)) => {
                self.parse_block_scalar(min_indent, anchor, tag)
            }

            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
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
                self.parse_scalar_or_mapping(
                    min_indent,
                    anchor,
                    tag,
                    is_key,
                    prop_crossed_line,
                    effective_allow,
                )
            }

            Some((Token::Anchor(_) | Token::Tag(_), _)) => {
                // More properties after line boundary - indicates nested structure
                // Properties may span multiple lines (e.g., `&anchor\n!!str\nvalue`)
                //
                // Important distinction for complex key patterns (6BFJ):
                // `&mapping\n&key [...]: value`
                // Here `&mapping` belongs to the outer block mapping, `&key` to the sequence key
                //
                // Strategy: Collect properties, but track if we cross a line boundary to more
                // properties + complex key pattern. If so, outer props go on mapping.

                // The `anchor` and `tag` params already contain properties collected
                // BEFORE we entered this match arm. So they are the "outer" properties.
                // Now we're seeing MORE properties after a line boundary.
                let outer_anchor = anchor;
                let outer_tag = tag;

                // Collect the NEW properties at current position (don't pass outer ones
                // since collect_properties would overwrite them)
                let (mut inner_anchor, mut inner_tag, _crossed) =
                    self.collect_properties(None, None);
                self.skip_ws();

                // Check for line boundary followed by more content (third layer of props)
                if matches!(self.peek(), Some((Token::LineStart(_), _))) {
                    // Peek ahead to see what's after the line boundary
                    let mut peek_offset = 1;
                    // Skip past Indent/Dedent tokens
                    while let Some((tok, _)) = self.peek_nth(peek_offset) {
                        if matches!(tok, Token::Indent(_) | Token::Dedent) {
                            peek_offset += 1;
                        } else {
                            break;
                        }
                    }

                    // Check if next content is more properties
                    if let Some((tok, _)) = self.peek_nth(peek_offset) {
                        if matches!(tok, Token::Anchor(_) | Token::Tag(_)) {
                            // More properties on next line - merge with inner
                            self.skip_ws_and_newlines();
                            let (new_anchor, new_tag, _) = self.collect_properties(None, None);
                            // Prefer newer properties (or keep inner if new is None)
                            inner_anchor = new_anchor.or(inner_anchor);
                            inner_tag = new_tag.or(inner_tag);
                            self.skip_ws();

                            // If we see LineStart again, skip it
                            if matches!(self.peek(), Some((Token::LineStart(_), _))) {
                                self.skip_ws_and_newlines();
                            }
                        } else {
                            // Not more properties - skip to see actual content
                            self.skip_ws_and_newlines();
                        }
                    }
                }

                // Check if this is a complex key pattern (flow seq/map followed by :)
                // If we have separate outer and inner properties, and the value is a
                // flow collection that's a key, emit MappingStart with outer props
                if inner_anchor.is_some() || inner_tag.is_some() {
                    // We have two layers of properties - check for complex key
                    if let Some((Token::FlowSeqStart | Token::FlowMapStart, _)) = self.peek() {
                        if self.is_flow_seq_complex_key() {
                            // This is a block mapping with a complex key
                            let span = self.current_span();
                            let map_indent = self.current_indent;

                            // Advance into the flow collection
                            let is_seq = matches!(self.peek(), Some((Token::FlowSeqStart, _)));
                            self.advance();
                            self.enter_flow_collection(span.start_usize());

                            // Push BlockMap state for after the key
                            self.state_stack.push(ParseState::BlockMap {
                                indent: map_indent,
                                phase: BlockMapPhase::AfterKey {
                                    is_implicit_scalar_key: false, // Flow collection key
                                },
                                start_span: span,
                                anchor: None,
                                tag: None,
                            });

                            // Push flow collection to parse key content
                            if is_seq {
                                self.state_stack.push(ParseState::FlowSeq {
                                    phase: FlowSeqPhase::BeforeEntry,
                                    start_span: span,
                                });
                                // Emit seq start with inner props (will be emitted via state)
                                self.state_stack.push(ParseState::EmitSeqStart {
                                    anchor: inner_anchor,
                                    tag: inner_tag,
                                    span,
                                });
                            } else {
                                self.state_stack.push(ParseState::FlowMap {
                                    phase: FlowMapPhase::BeforeKey,
                                    start_span: span,
                                });
                                // Map start with inner props will be emitted directly below
                            }

                            // Push indent level for orphan indent detection
                            self.push_indent(map_indent);
                            // Emit MappingStart with outer props
                            return Some(Event::MappingStart {
                                style: crate::event::CollectionStyle::Block,
                                anchor: outer_anchor,
                                tag: outer_tag,
                                span,
                            });
                        }
                    }
                }

                // Now determine what kind of value follows
                match self.peek() {
                    Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                        // Check if it's an implicit mapping (scalar followed by colon)
                        if !is_key && self.flow_depth == 0 && self.is_implicit_key() {
                            // It's a nested mapping
                            // Property ownership:
                            // - outer properties (before line boundary) -> MappingStart
                            // - inner properties (same line as key) -> key scalar
                            let span = self.current_span();
                            let map_indent = self.current_indent;

                            // Parse the first key WITH inner properties
                            let key_event =
                                self.parse_plain_scalar(inner_anchor, inner_tag, min_indent);

                            // Use the full first key span for MappingStart (matches batch parser)
                            let map_start_span = if let Some(Event::Scalar {
                                value,
                                anchor: k_anchor,
                                tag: k_tag,
                                span: k_span,
                                style,
                            }) = key_event
                            {
                                // Check for multiline implicit key error
                                self.check_multiline_implicit_key(k_span);

                                self.state_stack.push(ParseState::BlockMap {
                                    indent: map_indent,
                                    phase: BlockMapPhase::AfterKey {
                                        is_implicit_scalar_key: true, // Plain scalar key
                                    },
                                    start_span: span,
                                    anchor: None,
                                    tag: None,
                                });
                                let owned_value = Cow::Owned(value.into_owned());
                                let owned_anchor =
                                    k_anchor.map(|(name, sp)| (Cow::Owned(name.into_owned()), sp));
                                let owned_tag =
                                    k_tag.map(|(name, sp)| (Cow::Owned(name.into_owned()), sp));
                                self.state_stack.push(ParseState::EmitScalar {
                                    value: owned_value,
                                    anchor: owned_anchor,
                                    tag: owned_tag,
                                    span: k_span,
                                    style,
                                });
                                k_span // Use full key span for MappingStart
                            } else {
                                span // Fallback (shouldn't happen)
                            };

                            // Push indent level for orphan indent detection
                            self.push_indent(map_indent);
                            // Outer anchor/tag go on the MappingStart
                            return Some(Event::MappingStart {
                                style: crate::event::CollectionStyle::Block,
                                anchor: outer_anchor,
                                tag: outer_tag,
                                span: map_start_span,
                            });
                        }
                        // Not an implicit mapping, just a scalar with properties.
                        // Outer properties that crossed a line boundary can only attach
                        // to collections, not scalars. So we use inner properties only.
                        // E.g., `&outer\n  &inner scalar` → &outer is lost, &inner on scalar
                        let result = self.parse_plain_scalar(inner_anchor, inner_tag, min_indent);
                        // If this is a mapping key, check for multiline implicit key error
                        if is_key {
                            if let Some(Event::Scalar { span, .. }) = &result {
                                self.check_multiline_implicit_key(*span);
                            }
                        }
                        result
                    }
                    Some((Token::BlockSeqIndicator, _)) => {
                        // Nested sequence; merge properties (outer takes precedence)
                        let merged_anchor = outer_anchor.or(inner_anchor);
                        let merged_tag = outer_tag.or(inner_tag);
                        let span = self.current_span();
                        let seq_indent = self.column_of_position(span.start_usize());
                        self.state_stack.push(ParseState::BlockSeq {
                            indent: seq_indent,
                            phase: BlockSeqPhase::EmitStart,
                            start_span: span,
                            anchor: merged_anchor,
                            tag: merged_tag,
                        });
                        self.process_state_stack()
                    }
                    Some((Token::FlowSeqStart, span)) => {
                        // Flow sequence; merge properties (outer takes precedence)
                        let merged_anchor = outer_anchor.or(inner_anchor);
                        let merged_tag = outer_tag.or(inner_tag);
                        let span = span;
                        self.advance();
                        self.enter_flow_collection(span.start_usize());
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::BeforeEntry,
                            start_span: span,
                        });
                        Some(Event::SequenceStart {
                            style: crate::event::CollectionStyle::Flow,
                            anchor: merged_anchor,
                            tag: merged_tag,
                            span,
                        })
                    }
                    Some((Token::FlowMapStart, span)) => {
                        // Flow mapping; merge properties (outer takes precedence)
                        let merged_anchor = outer_anchor.or(inner_anchor);
                        let merged_tag = outer_tag.or(inner_tag);
                        let span = span;
                        self.advance();
                        self.enter_flow_collection(span.start_usize());
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::BeforeKey,
                            start_span: span,
                        });
                        Some(Event::MappingStart {
                            style: crate::event::CollectionStyle::Flow,
                            anchor: merged_anchor,
                            tag: merged_tag,
                            span,
                        })
                    }
                    _ => {
                        // Emit scalar with merged properties (outer takes precedence)
                        let merged_anchor = outer_anchor.or(inner_anchor);
                        let merged_tag = outer_tag.or(inner_tag);
                        Some(Event::Scalar {
                            style: ScalarStyle::Plain,
                            value: Cow::Borrowed(""),
                            anchor: merged_anchor,
                            tag: merged_tag,
                            span: self.current_span(),
                        })
                    }
                }
            }

            _ => {
                // Default: emit null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor,
                    tag,
                    span: self.current_span(),
                })
            }
        }
    }

    /// Collect anchor and tag properties before a value.
    ///
    /// Returns `(anchor, tag, crossed_line_boundary)`:
    /// - `crossed_line_boundary` is true if we skipped a `LineStart` after collecting any property.
    ///   This is important for determining if properties belong to a collection vs its first item.
    ///
    /// This function collects all properties that belong to the same value, spanning multiple
    /// lines if needed. However, it stops when the next line contains a property that is
    /// itself followed by content that creates an implicit mapping key (like `&key [...]: val`).
    ///
    /// Examples where properties span lines and all apply to the same value:
    /// ```yaml
    /// &anchor
    /// !!tag
    /// value         # &anchor and !!tag both apply to "value"
    ///
    /// &anchor
    /// !!tag value   # &anchor and !!tag both apply to "value"
    /// ```
    ///
    /// Example where properties DON'T merge (first creates a parent mapping):
    /// ```yaml
    /// &mapping
    /// &key [...]: val  # &mapping applies to mapping, &key applies to [...]
    /// ```
    fn collect_properties(
        &mut self,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    ) -> (
        Option<(Cow<'static, str>, Span)>,
        Option<(Cow<'static, str>, Span)>,
        bool,
    ) {
        // Delegate to the version with min_indent = 0 (no indent restriction)
        self.collect_properties_with_min_indent(anchor, tag, 0)
    }

    /// Collect properties (anchor, tag) with a minimum indentation constraint.
    ///
    /// When `min_indent > 0`, properties on a new line at `indent < min_indent`
    /// are NOT collected - they belong to a parent context.
    fn collect_properties_with_min_indent(
        &mut self,
        mut anchor: Option<(Cow<'static, str>, Span)>,
        mut tag: Option<(Cow<'static, str>, Span)>,
        min_indent: IndentLevel,
    ) -> (
        Option<(Cow<'static, str>, Span)>,
        Option<(Cow<'static, str>, Span)>,
        bool,
    ) {
        let mut crossed_line_boundary = false;
        loop {
            // Peek and extract info before any mutation
            enum PropAction {
                Anchor {
                    name: String,
                    span: Span,
                },
                Tag {
                    tag_str: String,
                    span: Span,
                },
                Comment,
                LineStart {
                    next_indent: IndentLevel,
                    should_continue: bool,
                },
                Break,
            }

            let has_props = anchor.is_some() || tag.is_some();
            let action = match self.peek() {
                Some((Token::Anchor(name), span)) => PropAction::Anchor {
                    name: String::from(*name),
                    span,
                },
                Some((Token::Tag(tag_str), span)) => PropAction::Tag {
                    tag_str: tag_str.to_string(),
                    span,
                },
                Some((Token::Comment(_), _)) if has_props => PropAction::Comment,
                Some((Token::LineStart(indent), _)) if has_props => PropAction::LineStart {
                    next_indent: *indent,
                    should_continue: self.should_continue_collecting_properties(),
                },
                _ => PropAction::Break,
            };

            match action {
                PropAction::Anchor { name, span } => {
                    // Error: Duplicate anchor on same node
                    if anchor.is_some() {
                        self.error(ErrorKind::DuplicateAnchor, span);
                    }

                    // Register anchor (need to get reference from tokens for HashSet)
                    if let Some((Token::Anchor(name_ref), _)) = self.peek() {
                        self.anchors.insert(name_ref);
                    }
                    anchor = Some((Cow::Owned(name), span));
                    self.advance();
                    self.skip_ws();
                }
                PropAction::Tag { tag_str, span } => {
                    // Error: Duplicate tag on same node
                    if tag.is_some() {
                        self.error(ErrorKind::DuplicateTag, span);
                    }

                    // Expand tag handle
                    let expanded = self.expand_tag(&tag_str, span);
                    tag = Some((Cow::Owned(expanded), span));
                    self.advance();

                    // Check for content immediately after tag (no space)
                    // e.g., `!invalid{}tag` - the `{` is immediately after the tag
                    let tag_looks_legitimate = !tag_str.contains('"') && !tag_str.contains('`');
                    let tag_end = span.end_usize();
                    if tag_looks_legitimate {
                        if let Some((next_tok, next_span)) = self.peek() {
                            let is_content = matches!(
                                next_tok,
                                Token::Plain(_)
                                    | Token::StringStart(_)
                                    | Token::FlowSeqStart
                                    | Token::FlowMapStart
                                    | Token::BlockSeqIndicator
                            );
                            if is_content && next_span.start_usize() == tag_end {
                                self.error(ErrorKind::ContentOnSameLine, next_span);
                            }
                        }
                    }

                    self.skip_ws();
                }
                PropAction::Comment => {
                    // Skip comment - the line boundary after it will be handled next iteration
                    self.advance();
                }
                PropAction::LineStart {
                    next_indent,
                    should_continue,
                } => {
                    // We have collected at least one property and there's a line boundary.
                    // Check if the next line has more properties that we should collect.
                    //
                    // We should continue collecting if:
                    // 1. The next line's indent is >= min_indent (valid for this context)
                    // 2. AND the next line has properties that are NOT followed by implicit key
                    //
                    // We should STOP if:
                    // - The next line is at indent < min_indent (belongs to parent context)
                    // - OR the next line has properties followed by an implicit key
                    if next_indent < min_indent {
                        // Content at lower indent belongs to parent - stop collecting
                        crossed_line_boundary = true;
                        break;
                    }
                    if should_continue {
                        crossed_line_boundary = true;
                        self.advance(); // consume LineStart
                        // Skip Indent token if present after LineStart
                        if matches!(self.peek(), Some((Token::Indent(_), _))) {
                            self.advance();
                        }
                    } else {
                        // Stop - record that we saw a line boundary
                        crossed_line_boundary = true;
                        break;
                    }
                }
                PropAction::Break => break,
            }
        }
        (anchor, tag, crossed_line_boundary)
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

        // Skip Indent token if present (LineStart is often followed by Indent)
        if matches!(self.peek_nth(idx), Some((Token::Indent(_), _))) {
            idx += 1;
        }

        // First, check if there are any properties on the next line
        let mut found_property = false;
        loop {
            match self.peek_nth(idx) {
                Some((Token::Anchor(_) | Token::Tag(_), _)) => {
                    found_property = true;
                    idx += 1;
                }
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => {
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
        match self.peek_nth(idx) {
            Some((Token::LineStart(_), _)) => {
                // Property-only line - continue collecting
                true
            }
            Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                // Flow collection - check if it's a complex key
                // Look for matching close bracket, then colon
                let is_key = self.is_implicit_key_at_offset(idx);
                // If it's a key, DON'T continue (parent mapping gets first props)
                !is_key
            }
            Some((Token::Plain(_), _)) => {
                // Plain scalar - check if it's a key
                // Simple check: skip the Plain token and look for Colon
                let is_key = self.is_implicit_key_at_offset(idx);
                // If it's a key, DON'T continue
                !is_key
            }
            _ => {
                // Other content (quoted string, etc.) - continue collecting
                // These cases generally don't form implicit keys in this context
                true
            }
        }
    }

    /// Check if token at offset `start_idx` is an implicit mapping key.
    fn is_implicit_key_at_offset(&self, start_idx: usize) -> bool {
        let mut idx = start_idx;

        // Handle different token types
        match self.peek_nth(idx) {
            Some((Token::Plain(_), _)) => {
                idx += 1;
                // Skip whitespace
                while let Some((tok, _)) = self.peek_nth(idx) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => idx += 1,
                        _ => break,
                    }
                }
                matches!(self.peek_nth(idx), Some((Token::Colon, _)))
            }
            Some((Token::FlowSeqStart, _)) => {
                // Find matching ]
                idx += 1;
                let mut depth = 1;
                while let Some((tok, _)) = self.peek_nth(idx) {
                    match tok {
                        Token::FlowSeqStart | Token::FlowMapStart => depth += 1,
                        Token::FlowSeqEnd => {
                            depth -= 1;
                            if depth == 0 {
                                idx += 1;
                                break;
                            }
                        }
                        Token::FlowMapEnd => depth -= 1,
                        _ => {}
                    }
                    idx += 1;
                    if idx > 200 {
                        return false;
                    }
                }
                // Skip whitespace
                while let Some((tok, _)) = self.peek_nth(idx) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => idx += 1,
                        _ => break,
                    }
                }
                matches!(self.peek_nth(idx), Some((Token::Colon, _)))
            }
            _ => false,
        }
    }

    /// Expand a tag handle to its full form.
    ///
    /// The lexer transforms tags as follows:
    /// - `!!str` → `Tag("!str")` (secondary handle)
    /// - `!name!suffix` → `Tag("name!suffix")` (named handle)
    /// - `!<uri>` → `Tag("\0uri")` (verbatim, marked with NUL)
    /// - `!` alone → `Tag("")` (non-specific)
    #[allow(
        clippy::indexing_slicing,
        reason = "bang_pos is from find('!') so slicing up to it is safe"
    )]
    fn expand_tag(&mut self, tag_str: &str, span: Span) -> String {
        /// Decode percent-encoded characters in a tag suffix.
        /// E.g., `tag%21` → `tag!` (since %21 is '!')
        fn percent_decode(input: &str) -> String {
            let mut result = String::with_capacity(input.len());
            let mut chars = input.chars().peekable();
            while let Some(ch) = chars.next() {
                if ch == '%' {
                    // Try to read two hex digits
                    let hex: String = chars.by_ref().take(2).collect();
                    if hex.len() == 2 {
                        if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                            result.push(char::from(byte));
                            continue;
                        }
                    }
                    // Failed to decode, keep as-is
                    result.push('%');
                    result.push_str(&hex);
                } else {
                    result.push(ch);
                }
            }
            result
        }

        // Verbatim tag: marked with leading '\0' by lexer - return as-is (without marker)
        if let Some(verbatim) = tag_str.strip_prefix('\0') {
            return String::from(verbatim);
        }

        // Secondary handle: original !!type, stored as !type by lexer
        if let Some(suffix) = tag_str.strip_prefix('!') {
            let decoded_suffix = percent_decode(suffix);
            if let Some(prefix) = self.tag_handles.get("!!") {
                return format!("{prefix}{decoded_suffix}");
            }
            // Default secondary handle if not in tag_handles
            return format!("tag:yaml.org,2002:{decoded_suffix}");
        }

        // Empty tag (just `!`) - non-specific tag
        if tag_str.is_empty() {
            return String::from("!");
        }

        // Named handle: original !name!suffix, stored as name!suffix by lexer
        if let Some(bang_pos) = tag_str.find('!') {
            let handle = format!("!{}!", &tag_str[0..bang_pos]);
            let suffix = &tag_str[bang_pos + 1..];
            // Look up handle using as_str() since HashMap keys are &str
            if let Some(prefix) = self.tag_handles.get(handle.as_str()) {
                let decoded_suffix = percent_decode(suffix);
                return format!("{prefix}{decoded_suffix}");
            }
            // Handle not declared - emit error and return unexpanded
            self.error(ErrorKind::UndefinedTagHandle, span);
            return format!("!{tag_str}");
        }

        // Primary handle: original !type, stored as type (no '!' in the value)
        // Check if primary handle `!` has been redefined via %TAG ! prefix
        if let Some(prefix) = self.tag_handles.get("!") {
            let decoded_tag = percent_decode(tag_str);
            return format!("{prefix}{decoded_tag}");
        }

        // No redefinition - use as local tag with leading !
        format!("!{tag_str}")
    }

    // ─────────────────────────────────────────────────────────────
    // Block Sequence
    // ─────────────────────────────────────────────────────────────

    fn process_block_seq(
        &mut self,
        indent: IndentLevel,
        phase: BlockSeqPhase,
        start_span: Span,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    ) -> Option<Event<'input>> {
        match phase {
            BlockSeqPhase::EmitStart => {
                // Push indent level onto stack for orphan indent detection
                self.push_indent(indent);
                // Push state for first entry (anchor/tag already consumed)
                self.state_stack.push(ParseState::BlockSeq {
                    indent,
                    phase: BlockSeqPhase::BeforeEntry,
                    start_span,
                    anchor: None,
                    tag: None,
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Block,
                    anchor,
                    tag,
                    span: start_span,
                })
            }

            BlockSeqPhase::BeforeEntry => {
                self.skip_ws_and_newlines();

                // Check for `-` at the sequence indent
                match self.peek() {
                    Some((Token::BlockSeqIndicator, span)) => {
                        // Use column position of `-` for consistent indent tracking
                        let entry_indent = self.column_of_position(span.start_usize());
                        if entry_indent < indent {
                            // Dedented, end sequence
                            self.pop_indent();
                            return Some(Event::SequenceEnd {
                                span: self.collection_end_span(),
                            });
                        }

                        self.advance(); // consume `-`
                        self.skip_ws();
                        // Check for tabs after block sequence indicator
                        self.check_tabs_after_block_indicator();

                        // Push AfterEntry, then Value
                        self.state_stack.push(ParseState::BlockSeq {
                            indent,
                            phase: BlockSeqPhase::AfterEntry,
                            start_span,
                            anchor: None,
                            tag: None,
                        });
                        self.state_stack.push(ParseState::Value {
                            min_indent: entry_indent + 1,
                            anchor: None,
                            tag: None,
                            is_key: false,                // Sequence entry is a value
                            allow_implicit_mapping: true, // Sequence entries allow implicit mappings
                        });
                        None // Continue processing
                    }

                    Some((Token::DocEnd | Token::DocStart, _)) | None => {
                        // End of sequence
                        // Only check trailing content for root-level sequences (not nested)
                        // A root-level sequence has indent_stack == [0, 0] (base + seq)
                        if indent == 0 && self.indent_stack.len() == 2 {
                            self.check_trailing_content_at_root(0);
                        }
                        self.pop_indent();
                        Some(Event::SequenceEnd {
                            span: self.collection_end_span(),
                        })
                    }

                    Some((Token::LineStart(n), _)) => {
                        if *n < indent {
                            // Check for orphan indentation: n is not in the
                            // parser's indent stack (between valid levels)
                            if !self.is_valid_indent(*n) {
                                self.report_invalid_indent();
                            }
                            if indent == 0 && self.indent_stack.len() == 2 {
                                self.check_trailing_content_at_root(0);
                            }
                            self.pop_indent();
                            Some(Event::SequenceEnd {
                                span: self.collection_end_span(),
                            })
                        } else {
                            self.advance();
                            // Re-push state to check again
                            self.state_stack.push(ParseState::BlockSeq {
                                indent,
                                phase: BlockSeqPhase::BeforeEntry,
                                start_span,
                                anchor: None,
                                tag: None,
                            });
                            None
                        }
                    }

                    // Dedent token - only end if we're actually dedented below our indent
                    Some((Token::Dedent, _span)) => {
                        if self.current_indent < indent {
                            // Check for orphan indentation: current_indent is not in the
                            // parser's indent stack (between valid levels)
                            if !self.is_valid_indent(self.current_indent) {
                                self.report_invalid_indent();
                            }
                            // We're actually dedented - end the sequence
                            if indent == 0 && self.indent_stack.len() == 2 {
                                self.check_trailing_content_at_root(0);
                            }
                            self.advance();
                            self.pop_indent();
                            Some(Event::SequenceEnd {
                                span: self.collection_end_span(),
                            })
                        } else {
                            // Dedent from nested structure - consume and continue
                            self.advance();
                            self.state_stack.push(ParseState::BlockSeq {
                                indent,
                                phase: BlockSeqPhase::BeforeEntry,
                                start_span,
                                anchor: None,
                                tag: None,
                            });
                            None
                        }
                    }

                    _ => {
                        // No more entries - check for trailing content at seq_indent level
                        // Mimic batch parser's check_trailing_content logic in block.rs:137-162
                        if let Some((tok, span)) = self.peek() {
                            let col = self.column_of_position(span.start_usize());
                            // Only check at seq_indent level, not at lower indents
                            if col == indent {
                                let is_unexpected_content = match tok {
                                    Token::Plain(_) => {
                                        // Check if this might be a mapping key (followed by colon)
                                        // If so, it's a valid sibling mapping entry
                                        let next = self.peek_nth(1);
                                        !matches!(next, Some((Token::Colon, _)))
                                    }
                                    Token::StringStart(_) => {
                                        // Quoted strings could be mapping keys
                                        false
                                    }
                                    Token::Anchor(_) | Token::Alias(_) | Token::Tag(_) => {
                                        // Could be followed by content that makes a mapping entry
                                        false
                                    }
                                    _ => false,
                                };
                                if is_unexpected_content {
                                    self.error(ErrorKind::TrailingContent, span);
                                }
                            }
                        }
                        // Root-level check
                        if indent == 0 && self.indent_stack.len() == 2 {
                            self.check_trailing_content_at_root(0);
                        }
                        self.pop_indent();
                        Some(Event::SequenceEnd {
                            span: self.collection_end_span(),
                        })
                    }
                }
            }

            BlockSeqPhase::AfterEntry => {
                // After parsing entry value, continue with next entry
                self.state_stack.push(ParseState::BlockSeq {
                    indent,
                    phase: BlockSeqPhase::BeforeEntry,
                    start_span,
                    anchor: None,
                    tag: None,
                });
                None
            }
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Block Mapping
    // ─────────────────────────────────────────────────────────────

    fn process_block_map(
        &mut self,
        indent: IndentLevel,
        phase: BlockMapPhase,
        start_span: Span,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    ) -> Option<Event<'input>> {
        match phase {
            BlockMapPhase::EmitStart => {
                // Push indent level onto stack for orphan indent detection
                self.push_indent(indent);
                self.state_stack.push(ParseState::BlockMap {
                    indent,
                    phase: BlockMapPhase::BeforeKey {
                        require_line_boundary: false, // First key doesn't require line boundary
                    },
                    start_span,
                    anchor: None,
                    tag: None,
                });
                Some(Event::MappingStart {
                    style: crate::event::CollectionStyle::Block,
                    anchor,
                    tag,
                    span: start_span,
                })
            }

            BlockMapPhase::BeforeKey {
                require_line_boundary,
            } => {
                // Track if we cross a line boundary - in block context, new entries
                // after the first one must be on a new line. Same-line content like
                // `: c` after `a: b` is invalid.
                // Use specialized skip that reports InvalidIndentation for over-indented
                // orphan content, matching batch parser's advance_to_same_indent behavior.
                let crossed_line = self.skip_ws_and_newlines_for_mapping(indent);

                // If we require a line boundary (subsequent entry after a value) and
                // didn't cross one, end the mapping - we can't have same-line entries.
                if require_line_boundary && !crossed_line {
                    self.pop_indent();
                    return Some(Event::MappingEnd {
                        span: self.collection_end_span(),
                    });
                }

                // Check if we've moved to a lower indent level (mapping should end)
                // This catches cases where skip_ws_and_newlines consumed a LineStart
                // that would have triggered a dedent.
                // We also need to consume the corresponding Dedent token so the outer
                // mapping can continue at the new indent level.
                if self.current_indent < indent {
                    // Check for orphan indentation: current_indent is not in the
                    // parser's indent stack (between valid levels)
                    if !self.is_valid_indent(self.current_indent) {
                        self.report_invalid_indent();
                    }
                    // Consume one Dedent token if present (one per indent level exited)
                    if matches!(self.peek(), Some((Token::Dedent, _))) {
                        self.advance();
                    }
                    self.pop_indent();
                    return Some(Event::MappingEnd {
                        span: self.collection_end_span(),
                    });
                }

                match self.peek() {
                    Some((Token::MappingKey, _)) => {
                        self.advance();
                        self.skip_ws();
                        // Push AfterKey, then Value for key
                        self.state_stack.push(ParseState::BlockMap {
                            indent,
                            phase: BlockMapPhase::AfterKey {
                                is_implicit_scalar_key: false, // Explicit key (`?`)
                            },
                            start_span,
                            anchor: None,
                            tag: None,
                        });
                        // Use is_key: false because the VALUE after an explicit `?` can be
                        // any node, including a mapping. The `is_key: true` flag is only for
                        // implicit key scalars to prevent infinite nesting detection.
                        self.state_stack.push(ParseState::Value {
                            min_indent: indent + 1,
                            anchor: None,
                            tag: None,
                            is_key: false, // Explicit key value can be any node
                            allow_implicit_mapping: true, // Explicit key value on new line
                        });
                        None
                    }

                    Some((Token::Colon, _)) => {
                        // Implicit null key
                        self.state_stack.push(ParseState::BlockMap {
                            indent,
                            phase: BlockMapPhase::AfterKey {
                                is_implicit_scalar_key: false, // Null key, not plain scalar
                            },
                            start_span,
                            anchor: None,
                            tag: None,
                        });
                        Some(self.emit_null())
                    }

                    Some((Token::DocEnd | Token::DocStart, _)) | None => {
                        self.pop_indent();
                        Some(Event::MappingEnd {
                            span: self.collection_end_span(),
                        })
                    }

                    // Dedent token - but we only end if current_indent < our indent.
                    // A Dedent at our level just means a nested structure ended; we continue.
                    // E.g., for root mapping at indent 0: Dedent after inner mapping
                    // (indent 2 → 0) doesn't end us - we consume it and continue.
                    Some((Token::Dedent, _span)) => {
                        if self.current_indent < indent {
                            // Check for orphan indentation: current_indent is not in the
                            // parser's indent stack (between valid levels)
                            if !self.is_valid_indent(self.current_indent) {
                                self.report_invalid_indent();
                            }
                            // We're actually dedented - end the mapping
                            self.advance();
                            self.pop_indent();
                            Some(Event::MappingEnd {
                                span: self.collection_end_span(),
                            })
                        } else {
                            // Dedent from a nested structure - consume and continue
                            self.advance();
                            self.state_stack.push(ParseState::BlockMap {
                                indent,
                                phase: BlockMapPhase::BeforeKey {
                                    // We've crossed lines (dedent implies line boundary)
                                    require_line_boundary: false,
                                },
                                start_span,
                                anchor: None,
                                tag: None,
                            });
                            None
                        }
                    }

                    Some((Token::LineStart(n), _span)) => {
                        if *n < indent {
                            // Check for orphan indentation: n is not in the
                            // parser's indent stack (between valid levels)
                            if !self.is_valid_indent(*n) {
                                self.report_invalid_indent();
                            }
                            self.pop_indent();
                            Some(Event::MappingEnd {
                                span: self.collection_end_span(),
                            })
                        } else {
                            self.advance();
                            self.state_stack.push(ParseState::BlockMap {
                                indent,
                                phase: BlockMapPhase::BeforeKey {
                                    // We just saw a LineStart, so we've crossed a line boundary
                                    require_line_boundary: false,
                                },
                                start_span,
                                anchor: None,
                                tag: None,
                            });
                            None
                        }
                    }

                    _ => {
                        // Check for orphan indentation before trying to parse content.
                        // If current_indent is greater than mapping indent but not a valid
                        // level in the indent stack, we need to end this mapping.
                        //
                        // IMPORTANT: We should NOT report InvalidIndentation here.
                        // The batch parser's advance_to_same_indent only reports this error
                        // while actively iterating through LineStart tokens. If the LineStart
                        // was already consumed by a nested structure (like a sequence), the
                        // batch parser doesn't report the error from the outer mapping.
                        //
                        // For cases like DMG6 where the error IS reported, it comes from the
                        // inner mapping's handling (BlockMap::BeforeKey seeing LineStart), not
                        // from this catch-all branch.
                        if self.current_indent > indent
                            && !self.is_valid_indent(self.current_indent)
                        {
                            self.pop_indent();
                            return Some(Event::MappingEnd {
                                span: self.collection_end_span(),
                            });
                        }

                        // Check for implicit key (scalar followed by colon)
                        // At this point, if require_line_boundary was true and crossed_line
                        // was false, we would have already returned MappingEnd above.
                        // So either this is the first entry or we crossed a line.
                        if self.is_implicit_key() {
                            self.state_stack.push(ParseState::BlockMap {
                                indent,
                                phase: BlockMapPhase::AfterKey {
                                    is_implicit_scalar_key: true, // Implicit key detected
                                },
                                start_span,
                                anchor: None,
                                tag: None,
                            });
                            self.state_stack.push(ParseState::Value {
                                min_indent: indent,
                                anchor: None,
                                tag: None,
                                is_key: true,                 // This is a mapping key
                                allow_implicit_mapping: true, // Keys can be implicit mappings if on new line
                            });
                            None
                        } else {
                            // Check if there's a scalar at THIS MAPPING'S indent level
                            // that looks like a key but is missing a colon.
                            // Only report MissingColon if we're at the mapping's indent,
                            // not for scalars that are indented past this level.
                            if self.current_indent == indent {
                                if let Some(scalar_event) =
                                    self.check_missing_colon_in_mapping_with_event()
                                {
                                    // Emit the scalar as an orphaned key, then end mapping
                                    // Push MappingEnd onto stack so it's emitted after scalar
                                    self.state_stack.push(ParseState::EmitMapEnd {
                                        span: self.collection_end_span(),
                                    });
                                    return Some(scalar_event);
                                }
                            }
                            self.pop_indent();
                            Some(Event::MappingEnd {
                                span: self.collection_end_span(),
                            })
                        }
                    }
                }
            }

            BlockMapPhase::AfterKey {
                is_implicit_scalar_key,
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
                self.skip_ws_and_newlines_impl();

                // Expect colon
                if matches!(self.peek(), Some((Token::Colon, _))) {
                    self.advance();
                    self.skip_ws();
                    // Check for tabs after block mapping indicator (colon)
                    self.check_tabs_after_block_indicator();

                    // Check for block sequence indicator on same line as key - invalid in YAML
                    // Block sequences must start on a new line after the colon.
                    // This only applies to implicit scalar keys (like `key: - item`).
                    // For explicit keys (`? key : - item`) or null keys (`: - item`), this is valid.
                    if is_implicit_scalar_key {
                        if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                            self.error(ErrorKind::ContentOnSameLine, span);
                        }
                    }

                    // Reset crossed_line_boundary before parsing value.
                    // We only want to track line crossings DURING value parsing,
                    // not from earlier (e.g., initial LineStart at start of document).
                    self.crossed_line_boundary = false;

                    // Push AfterValue, then Value
                    self.state_stack.push(ParseState::BlockMap {
                        indent,
                        phase: BlockMapPhase::AfterValue,
                        start_span,
                        anchor: None,
                        tag: None,
                    });
                    self.state_stack.push(ParseState::Value {
                        min_indent: indent + 1,
                        anchor: None,
                        tag: None,
                        is_key: false, // This is the value after colon
                        // CRITICAL: Don't allow nested implicit mappings on the same line.
                        // `a: b: c` should NOT create nested mappings - need a line break.
                        // The allow_implicit_mapping will become true if parse_value crosses a line.
                        allow_implicit_mapping: false,
                    });
                    None
                } else {
                    // No colon, emit null value - use LineStart span to match batch behavior
                    self.state_stack.push(ParseState::BlockMap {
                        indent,
                        phase: BlockMapPhase::AfterValue,
                        start_span,
                        anchor: None,
                        tag: None,
                    });
                    // Use the LineStart span if we've crossed a line boundary (either before
                    // skip_ws_and_newlines_impl or during it). This correctly handles the case
                    // where a nested structure (like sequence) already consumed the LineStart.
                    let empty_span = if crossed_before || self.crossed_line_boundary {
                        // last_line_start_span is updated by advance() whenever a LineStart
                        // is consumed, so it reflects the most recent line break
                        self.last_line_start_span
                    } else {
                        self.current_span()
                    };
                    Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        anchor: None,
                        tag: None,
                        span: empty_span,
                    })
                }
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

                self.state_stack.push(ParseState::BlockMap {
                    indent,
                    phase: BlockMapPhase::BeforeKey {
                        require_line_boundary,
                    },
                    start_span,
                    anchor: None,
                    tag: None,
                });
                None
            }
        }
    }

    /// Check if we're at an implicit mapping key (value followed by colon).
    /// Handles properties (anchor/tag) that may span lines before the key.
    fn is_implicit_key(&self) -> bool {
        let mut i = 0;
        let mut seen_property = false;

        // First, skip any properties (anchors/tags) and their line continuations
        loop {
            match self.peek_nth(i) {
                Some((Token::Anchor(_) | Token::Tag(_), _)) => {
                    seen_property = true;
                    i += 1;
                }
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => {
                    i += 1;
                }
                Some((Token::LineStart(_), _)) if seen_property => {
                    // Property can be followed by LineStart before the actual key
                    i += 1;
                }
                _ => break,
            }
            if i > 20 {
                return false;
            }
        }

        // Now check what kind of value we have
        match self.peek_nth(i) {
            // Plain scalar: scan until colon or line boundary
            Some((Token::Plain(_), _)) => {
                i += 1;
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Colon => return true,
                        Token::LineStart(_) | Token::DocEnd | Token::DocStart => return false,
                        Token::FlowSeqStart | Token::FlowMapStart => return false,
                        _ => i += 1,
                    }
                    if i > 30 {
                        break;
                    }
                }
                false
            }

            // Quoted string: scan through to StringEnd, then check for colon
            // This handles multiline quoted keys like "a\nb": value
            Some((Token::StringStart(_), _)) => {
                i += 1;
                // Find matching StringEnd (handles nested strings if any)
                let mut depth = 1;
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::StringStart(_) => depth += 1,
                        Token::StringEnd(_) => {
                            depth -= 1;
                            if depth == 0 {
                                i += 1;
                                break;
                            }
                        }
                        _ => {}
                    }
                    i += 1;
                    if i > 200 {
                        return false;
                    }
                }
                // Skip whitespace after string
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => break,
                    }
                }
                // Check if followed by colon
                matches!(self.peek_nth(i), Some((Token::Colon, _)))
            }

            // Flow collections as keys - not implicit keys in block context
            Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => false,

            // Alias as key - check if followed by colon
            Some((Token::Alias(_), _)) => {
                i += 1;
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Colon => return true,
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => return false,
                    }
                    if i > 30 {
                        break;
                    }
                }
                false
            }

            _ => false,
        }
    }

    /// Check if there's a scalar at the current position that looks like a mapping key
    /// but is missing a colon. Reports `MissingColon` error if detected.
    ///
    /// This is called when `is_implicit_key()` returns false but we're at a scalar
    /// that could have been intended as a key. Cases like:
    /// ```yaml
    /// foo:
    ///   bar: baz
    /// invalid    # <- scalar at mapping indent, but no colon
    /// ```
    /// Check if there's a scalar at the current position that looks like a mapping key
    /// but is missing a colon. If found, reports `MissingColon` error and returns the
    /// scalar event to emit (as an orphaned key for error recovery).
    fn check_missing_colon_in_mapping_with_event(&mut self) -> Option<Event<'static>> {
        // Check if current token is a scalar-like token at this position
        match self.peek() {
            Some((Token::Plain(text), span)) => {
                // Plain scalar without a colon following - this is a MissingColon
                let span = span;
                let value: Cow<'static, str> = Cow::Owned(text.to_string());

                // Also check for InvalidDirective: `%` at column 0
                if text.starts_with('%') && self.column_of_position(span.start_usize()) == 0 {
                    self.error(ErrorKind::InvalidDirective, span);
                }

                self.error(ErrorKind::MissingColon, span);
                // Consume the plain scalar tokens
                self.skip_plain_scalar_tokens();
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value,
                    anchor: None,
                    tag: None,
                    span,
                })
            }
            Some((Token::StringStart(_), span)) => {
                // Quoted string without a colon following - this is a MissingColon
                let span = span;
                self.error(ErrorKind::MissingColon, span);
                // Parse the quoted string to get its value
                let (value, full_span) = self.parse_quoted_string_content();
                Some(Event::Scalar {
                    style: ScalarStyle::DoubleQuoted, // or SingleQuoted - doesn't matter for error recovery
                    value,
                    anchor: None,
                    tag: None,
                    span: full_span.unwrap_or(span),
                })
            }
            Some((Token::Alias(name), span)) => {
                // Alias without a colon following - this is a MissingColon
                let span = span;
                let alias_name = Cow::Owned((*name).to_owned());
                self.error(ErrorKind::MissingColon, span);
                // Skip the alias token
                self.advance();
                Some(Event::Alias {
                    name: alias_name,
                    span,
                })
            }
            _ => None,
        }
    }

    /// Parse a quoted string content, consuming tokens from `StringStart` through `StringEnd`.
    /// Returns the assembled string value and the span covering the entire quoted string.
    fn parse_quoted_string_content(&mut self) -> (Cow<'static, str>, Option<Span>) {
        let start_span = self.current_span();
        let mut content = String::new();

        // Skip StringStart
        if matches!(self.peek(), Some((Token::StringStart(_), _))) {
            self.advance();
        }

        // Collect content until StringEnd
        loop {
            match self.peek() {
                Some((Token::StringContent(text), _)) => {
                    content.push_str(text);
                    self.advance();
                }
                Some((Token::StringEnd(_), span)) => {
                    let end_span = span;
                    self.advance();
                    let full_span =
                        Span::from_usize_range(start_span.start_usize()..end_span.end_usize());
                    return (Cow::Owned(content), Some(full_span));
                }
                None => break,
                _ => {
                    self.advance();
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
    /// Note: The batch parser reports this error from each nested block level that
    /// encounters the orphan indentation. For parity, we do the same - each block
    /// that sees the invalid indent reports it.
    /// Uses `last_line_start_span` to ensure consistent spans matching the batch parser.
    fn report_invalid_indent(&mut self) {
        self.error(ErrorKind::InvalidIndentation, self.last_line_start_span);
    }

    /// Check if there's content on the same line (without any `LineStart` before it).
    /// Used by `AfterValue` to detect trailing content like `key: "val" trailing`.
    /// Returns true if the next non-whitespace token is content without a `LineStart`.
    fn has_content_without_line_boundary(&self) -> bool {
        let mut i = 0;
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                // Skip whitespace
                Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                // LineStart means we've moved to a new line - no same-line content
                Token::LineStart(_) => return false,
                // Content tokens on same line - this is trailing content
                Token::Plain(_)
                | Token::StringStart(_)
                | Token::MappingKey
                | Token::Colon
                | Token::Anchor(_)
                | Token::Tag(_)
                | Token::Alias(_)
                | Token::BlockSeqIndicator => return true,
                // End tokens - no content
                Token::DocEnd | Token::DocStart => return false,
                // Other tokens (Dedent, Indent, Comment, etc.) - skip
                _ => i += 1,
            }
        }
        false
    }

    /// Check if there's actual content at the current orphan indentation level.
    /// Used to determine whether to report `InvalidIndentation` when a mapping
    /// encounters an over-indented line (indent > mapping's indent but orphan).
    /// The batch parser's `advance_to_same_indent` only reports an error when
    /// there's content tokens (`Plain`, `StringStart`, `MappingKey`, `Colon`, `Anchor`, `Tag`),
    /// not for comments or empty lines.
    fn has_content_at_orphan_level(&self) -> bool {
        self.has_content_at_orphan_level_from(0)
    }

    /// Check if there's content at an orphan level, starting from peek offset `start_offset`.
    fn has_content_at_orphan_level_from(&self, start_offset: usize) -> bool {
        // Skip past any Dedent/Indent tokens from the given position
        let mut i = start_offset;
        while let Some((tok, _)) = self.peek_nth(i) {
            if matches!(tok, Token::Dedent | Token::Indent(_)) {
                i += 1;
            } else {
                break;
            }
        }
        // Check if the next token is content that would trigger an error
        if let Some((tok, _)) = self.peek_nth(i) {
            matches!(
                tok,
                Token::Plain(_)
                    | Token::StringStart(_)
                    | Token::MappingKey
                    | Token::Colon
                    | Token::Anchor(_)
                    | Token::Tag(_)
            )
        } else {
            false
        }
    }

    /// Skip whitespace and newlines for block mapping `BeforeKey` phase.
    /// This mimics the batch parser's `advance_to_same_indent` by reporting
    /// `InvalidIndentation` errors for over-indented content at orphan levels.
    /// Returns whether a line boundary was crossed.
    ///
    /// IMPORTANT: Does NOT consume `LineStart(n)` when `n < mapping_indent`.
    /// The caller should handle this case (end the mapping).
    ///
    /// Behavior for Comments:
    /// - Comments that appear BEFORE a matching `LineStart(n)` (where `n >= mapping_indent`)
    ///   are skipped. This handles inline comments on the same line as a value.
    /// - After finding a matching `LineStart(n)`, we stop. If the next non-ws token is a
    ///   Comment (standalone comment line), the caller will see it and end the mapping.
    ///   This matches the batch parser's behavior where `parse_implicit_mapping_entry`
    ///   fails when it sees a Comment.
    fn skip_ws_and_newlines_for_mapping(&mut self, mapping_indent: IndentLevel) -> bool {
        let mut crossed_line = false;
        let mut found_matching_line_start = false;

        loop {
            match self.peek() {
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => self.advance(),
                Some((Token::LineStart(n), span)) => {
                    let n = *n;
                    let span = span;

                    if n < mapping_indent {
                        // Dedented below mapping - don't consume, let caller handle
                        // Update current_indent so caller knows the level
                        self.current_indent = n;
                        self.last_line_start_span = span;

                        // Check for orphan indentation BEFORE breaking.
                        // The batch parser's advance_to_same_indent reports InvalidIndentation
                        // when n < target_indent and n is not a valid indent level.
                        // We need to do the same here, because the caller may not reach
                        // the orphan check (e.g., if require_line_boundary causes early exit).
                        if !self.is_valid_indent(n) && self.has_content_at_orphan_level_from(1) {
                            self.error(ErrorKind::InvalidIndentation, span);
                        }
                        break;
                    }

                    crossed_line = true;
                    self.last_line_start_span = span;
                    self.current_indent = n;

                    if n > mapping_indent && !self.is_valid_indent(n) {
                        // Over-indented orphan line - check for content
                        // Need to peek past Dedent/Indent tokens to see actual content
                        let has_content = self.has_content_at_orphan_level_from(1);
                        if has_content {
                            // Report InvalidIndentation at the LineStart span
                            self.error(ErrorKind::InvalidIndentation, span);
                        }
                    }

                    self.advance();

                    // Check for tabs as indentation after crossing a line boundary
                    // This must be done after consuming LineStart but before consuming
                    // the WhitespaceWithTabs, so we can detect invalid tab indentation.
                    if self.flow_context_columns.is_empty() {
                        self.check_tabs_as_indentation();
                    }

                    // After finding a LineStart at our indent level, we've "arrived"
                    // at the next line. Skip Indent/Dedent tokens, but stop at Comments.
                    if n == mapping_indent {
                        found_matching_line_start = true;
                    }
                }
                // Skip comments only if we haven't yet found a matching LineStart.
                // After finding one, a standalone comment line should end the mapping.
                Some((Token::Comment(_), _)) => {
                    if found_matching_line_start {
                        // Standalone comment after crossing to a valid line - stop here
                        // so the mapping's BeforeKey sees it and ends the mapping.
                        break;
                    }
                    // Inline comment before the next LineStart - skip it
                    self.advance();
                }
                Some((Token::Indent(_) | Token::Dedent, _)) => self.advance(),
                _ => break,
            }
        }
        crossed_line
    }

    /// Skip tokens comprising a plain scalar (Plain + whitespace on same line).
    fn skip_plain_scalar_tokens(&mut self) {
        while let Some((tok, _)) = self.peek() {
            match tok {
                Token::Plain(_) | Token::Whitespace | Token::WhitespaceWithTabs => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Skip tokens comprising a quoted string (`StringStart` through `StringEnd`).
    fn skip_quoted_string_tokens(&mut self) {
        // Skip StringStart
        if matches!(self.peek(), Some((Token::StringStart(_), _))) {
            self.advance();
        }
        // Skip until StringEnd
        loop {
            let is_end = matches!(self.peek(), Some((Token::StringEnd(_), _)));
            self.advance();
            if is_end {
                break;
            }
            if self.is_eof() {
                break;
            }
        }
    }

    /// Check if the current Plain token is followed by a Colon (making it a mapping key).
    ///
    /// This is used to stop plain scalar continuation when the next line
    /// looks like a mapping key (`key: value`). Mirrors the batch parser's
    /// `is_mapping_key_at_position` logic.
    fn is_plain_followed_by_colon(&self) -> bool {
        // Current token should be Plain - skip it, then look for Colon
        if !matches!(self.peek(), Some((Token::Plain(_), _))) {
            return false;
        }
        let mut i = 1;
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                Token::Colon => return true,
                Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                // Any other token means no colon follows on this line
                _ => return false,
            }
            if i > 10 {
                break;
            }
        }
        false
    }

    /// Check if the current `FlowSeqStart` token is part of a complex key pattern.
    ///
    /// This looks ahead through the flow sequence to find the closing `]`,
    /// then checks if it's followed by `:` (making it a mapping key).
    fn is_flow_seq_complex_key(&self) -> bool {
        // Current token should be FlowSeqStart
        if !matches!(self.peek(), Some((Token::FlowSeqStart, _))) {
            return false;
        }

        // Look ahead to find matching ] and check for :
        let mut depth = 0;
        let mut i = 0;
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                Token::FlowSeqStart | Token::FlowMapStart => depth += 1,
                Token::FlowSeqEnd => {
                    depth -= 1;
                    if depth == 0 {
                        // Found matching ] - check for :
                        i += 1;
                        // Skip whitespace
                        while let Some((ws_tok, _)) = self.peek_nth(i) {
                            if matches!(ws_tok, Token::Whitespace | Token::WhitespaceWithTabs) {
                                i += 1;
                            } else {
                                break;
                            }
                        }
                        return matches!(self.peek_nth(i), Some((Token::Colon, _)));
                    }
                }
                Token::FlowMapEnd => depth -= 1,
                Token::DocEnd | Token::DocStart => return false,
                _ => {}
            }
            i += 1;
            if i > 200 {
                // Safety limit
                break;
            }
        }
        false
    }

    /// Check if the current `FlowMapStart` token is part of a complex key pattern.
    ///
    /// This looks ahead through the flow mapping to find the closing `}`,
    /// then checks if it's followed by `:` (making it a mapping key).
    fn is_flow_map_complex_key(&self) -> bool {
        // Current token should be FlowMapStart
        if !matches!(self.peek(), Some((Token::FlowMapStart, _))) {
            return false;
        }

        // Look ahead to find matching } and check for :
        let mut depth = 0;
        let mut idx = 0;
        while let Some((tok, _)) = self.peek_nth(idx) {
            match tok {
                Token::FlowSeqStart | Token::FlowMapStart => depth += 1,
                Token::FlowMapEnd => {
                    depth -= 1;
                    if depth == 0 {
                        // Found matching } - check for :
                        idx += 1;
                        // Skip whitespace and newlines
                        while let Some((inner_tok, _)) = self.peek_nth(idx) {
                            if matches!(
                                inner_tok,
                                Token::Whitespace | Token::WhitespaceWithTabs | Token::LineStart(_)
                            ) {
                                idx += 1;
                            } else {
                                break;
                            }
                        }
                        return matches!(self.peek_nth(idx), Some((Token::Colon, _)));
                    }
                }
                Token::FlowSeqEnd => depth -= 1,
                Token::DocEnd | Token::DocStart => return false,
                _ => {}
            }
            idx += 1;
            if idx > 200 {
                // Safety limit
                break;
            }
        }
        false
    }

    /// Check if the current position is at an implicit flow mapping entry.
    ///
    /// Inside flow sequences, `[ key: value ]` creates an implicit mapping.
    /// This looks ahead to detect if the current entry is followed by a Colon.
    fn is_implicit_flow_mapping_entry(&self) -> bool {
        let mut i = 0;

        // Skip whitespace and newlines at the start
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                Token::Whitespace | Token::WhitespaceWithTabs | Token::LineStart(_) => i += 1,
                _ => break,
            }
        }

        // Skip properties (anchors/tags)
        while let Some((Token::Anchor(_) | Token::Tag(_), _)) = self.peek_nth(i) {
            i += 1;
            // Skip whitespace after property
            while let Some((tok, _)) = self.peek_nth(i) {
                match tok {
                    Token::Whitespace | Token::WhitespaceWithTabs | Token::LineStart(_) => {
                        i += 1;
                    }
                    _ => break,
                }
            }
        }

        // Now check what kind of value we have
        match self.peek_nth(i) {
            // Plain scalar: may span multiple lines in flow context
            Some((Token::Plain(_), _)) => {
                i += 1;
                // In flow context, plain scalars can continue across lines
                // Pattern: Plain + (LineStart + Plain)*
                loop {
                    // Skip whitespace
                    while let Some((tok, _)) = self.peek_nth(i) {
                        match tok {
                            Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                            _ => break,
                        }
                    }

                    // Check for continuation: LineStart followed by Plain
                    if let Some((Token::LineStart(_), _)) = self.peek_nth(i) {
                        if let Some((Token::Plain(_), _)) = self.peek_nth(i + 1) {
                            // This is a continuation line
                            i += 2; // Skip LineStart + Plain
                            continue;
                        }
                    }
                    break;
                }
                // Skip any remaining whitespace before colon check
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => break,
                    }
                }
                // Check if followed by colon
                matches!(self.peek_nth(i), Some((Token::Colon, _)))
            }

            // Quoted string: scan through content to StringEnd, then check for colon
            Some((Token::StringStart(_), _)) => {
                i += 1;
                // Find matching StringEnd
                let mut depth = 1;
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::StringStart(_) => depth += 1,
                        Token::StringEnd(_) => {
                            depth -= 1;
                            if depth == 0 {
                                i += 1;
                                break;
                            }
                        }
                        _ => {}
                    }
                    i += 1;
                    if i > 200 {
                        return false;
                    }
                }
                // Skip whitespace after string
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => break,
                    }
                }
                // Check if followed by colon
                matches!(self.peek_nth(i), Some((Token::Colon, _)))
            }

            // Flow sequence: find matching ], check for colon
            Some((Token::FlowSeqStart, _)) => {
                i += 1;
                let mut depth = 1;
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::FlowSeqStart | Token::FlowMapStart => depth += 1,
                        Token::FlowSeqEnd => {
                            depth -= 1;
                            if depth == 0 {
                                i += 1;
                                break;
                            }
                        }
                        Token::FlowMapEnd => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                    if i > 200 {
                        return false;
                    }
                }
                // Skip whitespace after ]
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => break,
                    }
                }
                matches!(self.peek_nth(i), Some((Token::Colon, _)))
            }

            // Flow mapping: find matching }, check for colon
            Some((Token::FlowMapStart, _)) => {
                i += 1;
                let mut depth = 1;
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::FlowSeqStart | Token::FlowMapStart => depth += 1,
                        Token::FlowMapEnd => {
                            depth -= 1;
                            if depth == 0 {
                                i += 1;
                                break;
                            }
                        }
                        Token::FlowSeqEnd => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                    if i > 200 {
                        return false;
                    }
                }
                // Skip whitespace after }
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => break,
                    }
                }
                matches!(self.peek_nth(i), Some((Token::Colon, _)))
            }

            // Delimiters mean no entry
            Some((Token::Comma | Token::FlowSeqEnd | Token::FlowMapEnd, _)) => false,

            // Alias followed by colon
            Some((Token::Alias(_), _)) => {
                i += 1;
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => break,
                    }
                }
                matches!(self.peek_nth(i), Some((Token::Colon, _)))
            }

            // Colon directly: empty implicit key (e.g., `[ : value ]`)
            Some((Token::Colon, _)) => true,

            _ => false,
        }
    }

    /// Skip indent-related tokens (`Indent`, `Dedent`, `Whitespace`, `WhitespaceWithTabs`).
    fn skip_indent_tokens(&mut self) {
        while matches!(
            self.peek(),
            Some((
                Token::Indent(_) | Token::Dedent | Token::Whitespace | Token::WhitespaceWithTabs,
                _
            ))
        ) {
            self.advance();
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
        min_indent: IndentLevel,
    ) -> bool {
        match self.peek() {
            Some((Token::Plain(next_text), next_span)) => {
                // Before consuming as continuation, check if it's a mapping key
                if self.is_plain_followed_by_colon() {
                    return false;
                }

                // Apply folding: single newline → space, multiple → n-1 newlines
                Self::append_folded_separator(content, *consecutive_newlines);
                content.push_str(next_text);
                *end_span = next_span;
                *consecutive_newlines = 0;
                self.advance();
                true
            }
            Some((Token::LineStart(_), _)) => {
                // Another newline, continue counting (caller will handle)
                true
            }
            // Handle BlockSeqIndicator that's inside content area (not a valid entry marker)
            // A `-` at column c is only a valid sequence entry if c < min_indent.
            // If c >= min_indent, it's within the content area and is plain text.
            Some((Token::BlockSeqIndicator, span)) => {
                let column = self.column_of_position(span.start_usize());
                if column < min_indent {
                    // Valid entry marker - end continuation
                    false
                } else {
                    // Inside content area - treat as plain text
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
        while let Some((tok, tok_span)) = self.peek() {
            if matches!(tok, Token::LineStart(_)) {
                break;
            }
            line_end = tok_span.end_usize();
            self.advance();
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
        let mut i = 1; // Start after current token
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                Token::Dedent | Token::Indent(_) => {
                    i += 1;
                }
                Token::LineStart(n) if *n >= min_indent => {
                    // Found a properly indented line after empty line(s)
                    return true;
                }
                Token::LineStart(_) => {
                    // Another low-indent line, keep looking
                    i += 1;
                }
                Token::Whitespace | Token::WhitespaceWithTabs => {
                    // Check if followed by content
                    i += 1;
                }
                Token::Plain(_) => {
                    // Content found - check if it's a mapping key
                    // If it's a mapping key, stop the scalar
                    return !self.is_mapping_key_at_offset(i);
                }
                _ => {
                    return false;
                }
            }
            if i > 20 {
                break;
            }
        }
        false
    }

    /// Check if the Plain token at offset `i` from current position is followed by Colon.
    fn is_mapping_key_at_offset(&self, offset: usize) -> bool {
        if !matches!(self.peek_nth(offset), Some((Token::Plain(_), _))) {
            return false;
        }
        let mut i = offset + 1;
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                Token::Colon => return true,
                Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                _ => return false,
            }
            if i > offset + 10 {
                break;
            }
        }
        false
    }

    /// Check if current Anchor/Tag starts a mapping key pattern.
    /// Pattern: (Anchor|Tag)+ (Whitespace|LineStart)* (Plain|Quoted) Whitespace* Colon
    /// This handles properties that span multiple lines, e.g.:
    /// ```yaml
    /// &m2
    /// key2: val2
    /// ```
    fn is_anchor_tag_mapping_key(&self) -> bool {
        let mut i = 0;
        // Skip anchor/tag tokens, whitespace, and LineStart tokens
        // LineStart can appear between properties and key (multiline properties)
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                Token::Anchor(_) | Token::Tag(_) => i += 1,
                Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                Token::LineStart(_) => i += 1,
                _ => break,
            }
        }
        // Check for scalar followed by colon
        match self.peek_nth(i) {
            Some((Token::Plain(_), _)) => {
                i += 1;
                // Skip whitespace
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => break,
                    }
                }
                matches!(self.peek_nth(i), Some((Token::Colon, _)))
            }
            Some((Token::StringStart(_), _)) => {
                // Skip through quoted string
                i += 1;
                let mut depth = 1;
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::StringStart(_) => depth += 1,
                        Token::StringEnd(_) => {
                            depth -= 1;
                            if depth == 0 {
                                i += 1;
                                break;
                            }
                        }
                        _ => {}
                    }
                    i += 1;
                    if i > 50 {
                        return false;
                    }
                }
                // Skip whitespace
                while let Some((tok, _)) = self.peek_nth(i) {
                    match tok {
                        Token::Whitespace | Token::WhitespaceWithTabs => i += 1,
                        _ => break,
                    }
                }
                matches!(self.peek_nth(i), Some((Token::Colon, _)))
            }
            _ => false,
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Flow Sequence
    // ─────────────────────────────────────────────────────────────

    fn process_flow_seq(&mut self, phase: FlowSeqPhase, start_span: Span) -> Option<Event<'input>> {
        match phase {
            FlowSeqPhase::EmitStart => {
                self.state_stack.push(ParseState::FlowSeq {
                    phase: FlowSeqPhase::BeforeEntry,
                    start_span,
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Flow,
                    anchor: None,
                    tag: None,
                    span: start_span,
                })
            }

            FlowSeqPhase::BeforeEntry => {
                self.skip_ws_and_newlines();

                match self.peek() {
                    Some((Token::FlowSeqEnd, span)) => {
                        let span = span;
                        let flow_end = span.end_usize();
                        self.advance();
                        self.exit_flow_collection();
                        // Check for content immediately after flow collection in block context
                        if self.flow_depth == 0 {
                            self.check_content_after_flow(flow_end);
                            // Check for multiline implicit key (flow collection spanning lines)
                            self.check_multiline_flow_key(start_span, span);
                        }
                        Some(Event::SequenceEnd { span })
                    }

                    Some((Token::Comma, comma_span)) => {
                        // Consecutive comma - report MissingSeparator
                        // (BeforeEntry is entered after `[` or after consuming a comma,
                        // so seeing another comma means consecutive commas)
                        self.error(ErrorKind::MissingSeparator, comma_span);
                        self.advance();
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::BeforeEntry,
                            start_span,
                        });
                        None // Don't emit null for consecutive commas
                    }

                    Some((Token::DocStart | Token::DocEnd, span)) => {
                        // Document markers inside flow context are invalid (already reported
                        // by lexer as DocumentMarkerInFlow). Ignore them and continue parsing.
                        let span = span;
                        self.error(ErrorKind::DocumentMarkerInFlow, span);
                        self.advance();
                        // Re-push state to continue parsing
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::BeforeEntry,
                            start_span,
                        });
                        None
                    }

                    None => {
                        // Unterminated
                        self.error(ErrorKind::UnexpectedEof, start_span);
                        self.exit_flow_collection();
                        Some(Event::SequenceEnd {
                            span: self.current_span(),
                        })
                    }

                    Some((Token::MappingKey, _)) => {
                        // Explicit key inside flow sequence: [ ? key : value ]
                        // This creates a flow mapping entry
                        let map_start_span = self.current_span();
                        self.advance(); // consume ?
                        self.skip_ws_and_newlines();

                        // Push states in reverse order:
                        // 1. After the mapping, continue with AfterEntry
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::AfterEntry,
                            start_span,
                        });
                        // 2. Emit MappingEnd after value
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::ImplicitMapEnd { map_start_span },
                            start_span,
                        });
                        // 3. Parse the value (after we see colon)
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::ImplicitMapValue { map_start_span },
                            start_span,
                        });
                        // 4. Parse the key
                        self.state_stack.push(ParseState::Value {
                            min_indent: 0,
                            anchor: None,
                            tag: None,
                            is_key: true,
                            allow_implicit_mapping: true, // Flow context - doesn't affect block mappings
                        });

                        // Emit MappingStart for the explicit mapping (flow style)
                        Some(Event::MappingStart {
                            style: crate::event::CollectionStyle::Flow,
                            anchor: None,
                            tag: None,
                            span: map_start_span,
                        })
                    }

                    _ => {
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
                                phase: FlowSeqPhase::ImplicitMapEnd { map_start_span },
                                start_span,
                            });
                            // 3. Parse the value (after we see colon)
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::ImplicitMapValue { map_start_span },
                                start_span,
                            });

                            // Check if this is an empty key (colon directly without key)
                            // Pattern: [ : value ] where colon is at current position
                            if matches!(self.peek(), Some((Token::Colon, _))) {
                                // 4. Emit null key then MappingStart
                                self.state_stack.push(ParseState::FlowSeq {
                                    phase: FlowSeqPhase::ImplicitMapEmptyKey { map_start_span },
                                    start_span,
                                });
                            } else {
                                // 4. Parse the key (non-empty)
                                self.state_stack.push(ParseState::Value {
                                    min_indent: 0,
                                    anchor: None,
                                    tag: None,
                                    is_key: true, // This is the key of the implicit mapping
                                    allow_implicit_mapping: true, // Flow context
                                });
                            }

                            // Emit MappingStart for the implicit mapping
                            Some(Event::MappingStart {
                                style: crate::event::CollectionStyle::Flow,
                                anchor: None,
                                tag: None,
                                span: map_start_span,
                            })
                        } else {
                            // Regular entry - parse as value
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::AfterEntry,
                                start_span,
                            });
                            self.state_stack.push(ParseState::Value {
                                min_indent: 0,
                                anchor: None,
                                tag: None,
                                is_key: false, // Sequence entry is a value
                                allow_implicit_mapping: true, // Flow context
                            });
                            None
                        }
                    }
                }
            }

            FlowSeqPhase::ImplicitMapEmptyKey { map_start_span } => {
                // Empty key case: emit null scalar for the key
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: None,
                    tag: None,
                    span: map_start_span,
                })
            }

            FlowSeqPhase::ImplicitMapValue { map_start_span } => {
                // After the key, expect Colon then parse value
                self.skip_ws_and_newlines();

                if matches!(self.peek(), Some((Token::Colon, _))) {
                    self.advance();
                    self.skip_ws_and_newlines();

                    // Check for empty value (followed by , or ])
                    if matches!(self.peek(), Some((Token::Comma | Token::FlowSeqEnd, _))) {
                        // Empty value - emit null
                        Some(self.emit_null())
                    } else {
                        // Parse the value
                        self.state_stack.push(ParseState::Value {
                            min_indent: 0,
                            anchor: None,
                            tag: None,
                            is_key: false,
                            allow_implicit_mapping: true, // Flow context
                        });
                        None
                    }
                } else {
                    // No colon - this shouldn't happen if is_implicit_flow_mapping_entry worked
                    // Emit null as value
                    Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        anchor: None,
                        tag: None,
                        span: map_start_span,
                    })
                }
            }

            FlowSeqPhase::ImplicitMapEnd { .. } => {
                // After the value, emit MappingEnd
                Some(Event::MappingEnd {
                    span: self.collection_end_span(),
                })
            }

            FlowSeqPhase::AfterEntry => {
                self.skip_ws_and_newlines();

                match self.peek() {
                    Some((Token::Comma, _)) => {
                        self.advance();
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::BeforeEntry,
                            start_span,
                        });
                        None
                    }

                    Some((Token::FlowSeqEnd, span)) => {
                        let span = span;
                        let flow_end = span.end_usize();
                        self.advance();
                        self.exit_flow_collection();
                        // Check for content immediately after flow collection in block context
                        if self.flow_depth == 0 {
                            self.check_content_after_flow(flow_end);
                            // Check for multiline implicit key (flow collection spanning lines)
                            self.check_multiline_flow_key(start_span, span);
                        }
                        Some(Event::SequenceEnd { span })
                    }

                    Some((Token::DocStart | Token::DocEnd, span)) => {
                        // Document markers inside flow context - ignore and continue
                        let span = span;
                        self.error(ErrorKind::DocumentMarkerInFlow, span);
                        self.advance();
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::AfterEntry,
                            start_span,
                        });
                        None
                    }

                    _ => {
                        // Missing comma or end
                        self.error(ErrorKind::UnexpectedEof, start_span);
                        self.exit_flow_collection();
                        Some(Event::SequenceEnd {
                            span: self.current_span(),
                        })
                    }
                }
            }
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Flow Mapping
    // ─────────────────────────────────────────────────────────────

    fn process_flow_map(&mut self, phase: FlowMapPhase, start_span: Span) -> Option<Event<'input>> {
        match phase {
            FlowMapPhase::EmitStart => {
                self.state_stack.push(ParseState::FlowMap {
                    phase: FlowMapPhase::BeforeKey,
                    start_span,
                });
                Some(Event::MappingStart {
                    style: crate::event::CollectionStyle::Flow,
                    anchor: None,
                    tag: None,
                    span: start_span,
                })
            }

            FlowMapPhase::BeforeKey => {
                self.skip_ws_and_newlines();

                match self.peek() {
                    Some((Token::FlowMapEnd, span)) => {
                        let span = span;
                        let flow_end = span.end_usize();
                        self.advance();
                        self.exit_flow_collection();
                        // Check for content immediately after flow collection in block context
                        if self.flow_depth == 0 {
                            self.check_content_after_flow(flow_end);
                            // Check for multiline implicit key (flow collection spanning lines)
                            self.check_multiline_flow_key(start_span, span);
                        }
                        Some(Event::MappingEnd { span })
                    }

                    Some((Token::Comma, comma_span)) => {
                        // Consecutive comma - report MissingSeparator
                        // (BeforeKey is entered after `{` or after consuming a comma,
                        // so seeing another comma means consecutive commas)
                        self.error(ErrorKind::MissingSeparator, comma_span);
                        self.advance();
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::BeforeKey,
                            start_span,
                        });
                        None // Don't emit null for consecutive commas
                    }

                    Some((Token::MappingKey, _)) => {
                        self.advance();
                        self.skip_ws();
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterKey,
                            start_span,
                        });
                        self.state_stack.push(ParseState::Value {
                            min_indent: 0,
                            anchor: None,
                            tag: None,
                            is_key: true,                 // Flow mapping key
                            allow_implicit_mapping: true, // Flow context
                        });
                        None
                    }

                    Some((Token::Colon, _)) => {
                        // Null key
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterKey,
                            start_span,
                        });
                        Some(self.emit_null())
                    }

                    Some((Token::DocStart | Token::DocEnd, span)) => {
                        // Document markers inside flow context are invalid.
                        // Ignore them and continue parsing.
                        let span = span;
                        self.error(ErrorKind::DocumentMarkerInFlow, span);
                        self.advance();
                        // Re-push state to continue parsing
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

                    _ => {
                        // Implicit key
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterKey,
                            start_span,
                        });
                        self.state_stack.push(ParseState::Value {
                            min_indent: 0,
                            anchor: None,
                            tag: None,
                            is_key: true,                 // Flow mapping key
                            allow_implicit_mapping: true, // Flow context
                        });
                        None
                    }
                }
            }

            FlowMapPhase::AfterKey => {
                self.skip_ws_and_newlines();

                if matches!(self.peek(), Some((Token::Colon, _))) {
                    self.advance();
                    self.skip_ws_and_newlines();

                    // Check for empty value
                    if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
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
                            min_indent: 0,
                            anchor: None,
                            tag: None,
                            is_key: false,                // Flow mapping value
                            allow_implicit_mapping: true, // Flow context
                        });
                        None
                    }
                } else {
                    // No colon, emit null value
                    self.state_stack.push(ParseState::FlowMap {
                        phase: FlowMapPhase::AfterValue,
                        start_span,
                    });
                    Some(self.emit_null())
                }
            }

            FlowMapPhase::AfterValue => {
                self.skip_ws_and_newlines();

                match self.peek() {
                    Some((Token::Comma, _)) => {
                        self.advance();
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::BeforeKey,
                            start_span,
                        });
                        None
                    }

                    Some((Token::FlowMapEnd, span)) => {
                        let span = span;
                        let flow_end = span.end_usize();
                        self.advance();
                        self.exit_flow_collection();
                        // Check for content immediately after flow collection in block context
                        if self.flow_depth == 0 {
                            self.check_content_after_flow(flow_end);
                            // Check for multiline implicit key (flow collection spanning lines)
                            self.check_multiline_flow_key(start_span, span);
                        }
                        Some(Event::MappingEnd { span })
                    }

                    Some((Token::DocStart | Token::DocEnd, span)) => {
                        // Document markers inside flow context - ignore and continue
                        let span = span;
                        self.error(ErrorKind::DocumentMarkerInFlow, span);
                        self.advance();
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::AfterValue,
                            start_span,
                        });
                        None
                    }

                    _ => {
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
    // Scalar Parsing (Simplified - will need enhancement)
    // ─────────────────────────────────────────────────────────────

    /// Parse a scalar or detect if it's actually a mapping key.
    ///
    /// If the scalar is followed by `:`, it's an implicit mapping key.
    /// In that case, emit `MappingStart` and set up states to parse the mapping.
    ///
    /// If `is_key` is true, we're already parsing a key, so we don't look for
    /// implicit mappings (to avoid infinite nesting).
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
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        is_key: bool,
        crossed_line_boundary: bool,
        allow_implicit_mapping: bool,
    ) -> Option<Event<'input>> {
        // Check if this is an implicit mapping key (scalar followed by colon)
        // Skip this check if:
        // - we're already parsing a key (avoid infinite recursion)
        // - we're in flow context (different rules apply)
        // - nested implicit mappings aren't allowed (same line as parent colon)
        if !is_key && self.flow_depth == 0 && allow_implicit_mapping && self.is_implicit_key() {
            // This is a block mapping with an implicit key
            let span = self.current_span();

            // Determine where anchor/tag belong based on crossed_line_boundary:
            // - If crossed_line_boundary: properties belong to MAPPING, key has none
            // - Otherwise: properties belong to KEY (same line), mapping has none
            let (map_anchor, map_tag, key_anchor, key_tag) = if crossed_line_boundary {
                // Properties crossed a line boundary, so they belong to the mapping
                (anchor, tag, None, None)
            } else {
                // Properties on same line as key, so they belong to the key
                // Also collect any additional properties before the key itself
                let (key_anchor, key_tag, _) = self.collect_properties(anchor, tag);
                (None, None, key_anchor, key_tag)
            };
            self.skip_ws();

            // Parse the key scalar with its properties
            let key_event = self.parse_plain_scalar(key_anchor, key_tag, min_indent);

            // Stack setup: (top to bottom)
            // 1. EmitScalar - emit the key we just parsed
            // 2. AfterKey - consume colon and parse value
            // MappingStart is returned immediately
            // Use full first key span for MappingStart to match batch parser behavior
            let map_start_span = if let Some(Event::Scalar {
                value,
                anchor: k_anchor,
                tag: k_tag,
                span: k_span,
                style,
            }) = key_event
            {
                // Check for multiline implicit key error
                self.check_multiline_implicit_key(k_span);
                // Determine mapping indent based on context:
                // - If there are properties (anchor or tag), use current_indent because
                //   the mapping's indent is the line's indent (e.g., `&a a: b` at root = indent 0)
                // - Otherwise (no properties, e.g., `- key: value`), use key's column position
                //   because that's where continuation lines need to align
                let has_properties = map_anchor.is_some()
                    || map_tag.is_some()
                    || k_anchor.is_some()
                    || k_tag.is_some();
                let map_indent = if crossed_line_boundary || has_properties {
                    self.current_indent
                } else {
                    self.column_of_position(k_span.start_usize())
                };
                self.state_stack.push(ParseState::BlockMap {
                    indent: map_indent,
                    phase: BlockMapPhase::AfterKey {
                        is_implicit_scalar_key: true, // Plain scalar followed by colon
                    },
                    start_span: span,
                    anchor: None,
                    tag: None,
                });
                // Convert all values to owned for storage in state (to satisfy 'static lifetime)
                let owned_value = Cow::Owned(value.into_owned());
                let owned_anchor = k_anchor.map(|(name, sp)| (Cow::Owned(name.into_owned()), sp));
                let owned_tag = k_tag.map(|(name, sp)| (Cow::Owned(name.into_owned()), sp));
                self.state_stack.push(ParseState::EmitScalar {
                    value: owned_value,
                    anchor: owned_anchor,
                    tag: owned_tag,
                    span: k_span,
                    style,
                });
                // Push indent level for orphan indent detection
                self.push_indent(map_indent);
                k_span // Use full first key span for MappingStart
            } else {
                // Fallback - shouldn't happen but use current_indent for stack consistency
                self.push_indent(self.current_indent);
                span // Fallback (shouldn't happen)
            };

            // Emit MappingStart with anchor/tag if they belong to the mapping
            return Some(Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                anchor: map_anchor,
                tag: map_tag,
                span: map_start_span,
            });
        }

        // Not a mapping key, just parse as a scalar
        let result = self.parse_plain_scalar(anchor, tag, min_indent);
        // If this is a mapping key, check for multiline implicit key error
        if is_key {
            if let Some(Event::Scalar { span, .. }) = &result {
                self.check_multiline_implicit_key(*span);
            }
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
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        min_indent: IndentLevel,
    ) -> Option<Event<'input>> {
        match self.peek() {
            Some((Token::Plain(text), span)) => {
                // Extract values before releasing the borrow
                let starts_with_percent = text.starts_with('%');
                let col = self.column_of_position(span.start_usize());
                let mut content = (*text).to_string();
                let start_span = span;
                let mut end_span = span;
                self.advance();

                // Check for reserved indicator `%` at column 0 starting a plain scalar.
                // Per YAML 1.2 spec production [22] c-indicator and [126] ns-plain-first,
                // `%` is a c-indicator and cannot start a plain scalar.
                // This is only an error at the START of a scalar, not in continuations.
                if starts_with_percent && col == 0 {
                    self.error(ErrorKind::InvalidDirective, start_span);
                }

                // Collect continuation lines for multiline plain scalars
                let mut consecutive_newlines = 0;

                if self.flow_depth > 0 {
                    // Flow context: plain scalars can span multiple lines.
                    // Newlines are folded to spaces. Terminate at flow indicators.
                    loop {
                        // Skip whitespace
                        while matches!(
                            self.peek(),
                            Some((Token::Whitespace | Token::WhitespaceWithTabs, _))
                        ) {
                            self.advance();
                        }

                        // Check for LineStart followed by Plain continuation
                        let Some((Token::LineStart(_), _)) = self.peek() else {
                            break;
                        };

                        // Check what follows the LineStart
                        let Some((next_tok, next_span)) = self.peek_nth(1) else {
                            break;
                        };

                        match next_tok {
                            Token::Plain(continuation) => {
                                // Fold newline to space
                                content.push(' ');
                                content.push_str(continuation);
                                end_span = next_span;
                                self.advance(); // consume LineStart
                                self.advance(); // consume Plain
                            }
                            _ => break,
                        }
                    }
                } else {
                    // Block context: continuation based on indentation
                    // Use the min_indent passed from caller, not current_indent
                    #[allow(clippy::while_let_loop, reason = "complex match with multiple arms")]
                    loop {
                        match self.peek() {
                            Some((Token::LineStart(n), line_span)) => {
                                let indent = *n;
                                let line_span = line_span;

                                // Check if this continues the scalar
                                if indent >= min_indent {
                                    // Normal continuation - properly indented
                                    self.advance();
                                    consecutive_newlines += 1;

                                    // Skip whitespace/indent tokens
                                    self.skip_indent_tokens();

                                    // Check for continuation content
                                    if !self.try_consume_plain_continuation(
                                        &mut content,
                                        &mut end_span,
                                        &mut consecutive_newlines,
                                        min_indent,
                                    ) {
                                        // Not a continuation. Check for orphan indentation.
                                        // The batch parser's advance_to_same_indent reports InvalidIndentation
                                        // when encountering an indent level not in the stack, even if the
                                        // indent is >= min_indent. This handles cases like EW3V where
                                        // a line at indent 1 is seen while the stack is [0, 0].
                                        if !self.is_valid_indent(indent)
                                            && self.has_content_at_orphan_level_from(1)
                                        {
                                            self.error(ErrorKind::InvalidIndentation, line_span);
                                        }
                                        break;
                                    }
                                } else {
                                    // Low indent - might be empty line, check for continuation
                                    if self.has_continuation_after_low_indent(min_indent) {
                                        // Empty line - consume and continue counting
                                        self.advance();
                                        consecutive_newlines += 1;
                                        // Skip any dedent tokens
                                        while matches!(self.peek(), Some((Token::Dedent, _))) {
                                            self.advance();
                                        }
                                    } else {
                                        // No continuation, end of scalar
                                        break;
                                    }
                                }
                            }
                            _ => break,
                        }
                    }
                }

                // Combine spans
                let final_span =
                    Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Owned(content),
                    anchor,
                    tag,
                    span: final_span,
                })
            }

            Some((Token::StringStart(quote_style), _)) => {
                self.parse_quoted_scalar(anchor, tag, *quote_style, min_indent)
            }

            _ => Some(self.emit_null()),
        }
    }

    /// Parse a quoted scalar.
    /// `min_indent` is used to validate that continuation lines have proper indentation.
    fn parse_quoted_scalar(
        &mut self,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        quote_style: crate::lexer::QuoteStyle,
        min_indent: IndentLevel,
    ) -> Option<Event<'input>> {
        let start_span = self.current_span();
        self.advance(); // consume StringStart

        let mut value = String::new();
        let mut end_span = start_span;
        let mut pending_newlines: usize = 0;

        loop {
            match self.peek() {
                Some((Token::StringContent(content), span)) => {
                    // Apply pending newlines before content
                    if pending_newlines > 0 {
                        if pending_newlines == 1 {
                            value.push(' ');
                        } else {
                            for _ in 1..pending_newlines {
                                value.push('\n');
                            }
                        }
                        pending_newlines = 0;
                    }
                    value.push_str(content);
                    end_span = span;
                    self.advance();
                }
                Some((Token::LineStart(indent), span)) => {
                    let indent = *indent;
                    let line_start_span = span;

                    // Check for InvalidIndentationContext error:
                    // If the next token is StringContent (a content line) and indent < min_indent,
                    // report the error.
                    let is_content_line =
                        matches!(self.peek_nth(1), Some((Token::StringContent(_), _)));
                    if is_content_line && indent < min_indent {
                        self.error(
                            ErrorKind::InvalidIndentationContext {
                                expected: min_indent,
                                found: indent,
                            },
                            line_start_span,
                        );
                    }

                    // Trim trailing spaces before newline
                    let trimmed_len = value.trim_end_matches(' ').len();
                    value.truncate(trimmed_len);
                    pending_newlines += 1;
                    end_span = line_start_span;
                    self.advance();
                }
                Some((Token::StringEnd(_), span)) => {
                    // Apply pending newlines at end
                    if pending_newlines == 1 {
                        value.push(' ');
                    } else if pending_newlines > 1 {
                        for _ in 1..pending_newlines {
                            value.push('\n');
                        }
                    }
                    end_span = span;
                    self.advance();
                    break;
                }
                _ => {
                    // Unterminated string
                    self.error(ErrorKind::UnterminatedString, start_span);
                    break;
                }
            }
        }

        let style = match quote_style {
            crate::lexer::QuoteStyle::Single => ScalarStyle::SingleQuoted,
            crate::lexer::QuoteStyle::Double => ScalarStyle::DoubleQuoted,
        };

        let full_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

        Some(Event::Scalar {
            style,
            value: Cow::Owned(value),
            anchor,
            tag,
            span: full_span,
        })
    }

    /// Parse a block scalar (literal or folded).
    ///
    /// This implementation tracks line types for proper folding:
    /// - Normal lines: fold single newlines to spaces (for folded style)
    /// - Empty lines: preserve as newlines
    /// - More-indented lines: preserve with newlines (don't fold)
    fn parse_block_scalar(
        &mut self,
        min_indent: IndentLevel,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    ) -> Option<Event<'input>> {
        let (header, start_span) = match self.peek() {
            Some((Token::LiteralBlockHeader(hdr), span)) => (hdr.clone(), span),
            Some((Token::FoldedBlockHeader(hdr), span)) => (hdr.clone(), span),
            _ => return Some(self.emit_null()),
        };

        let is_literal = matches!(self.peek(), Some((Token::LiteralBlockHeader(_), _)));
        self.advance(); // consume header

        // Track lines with their types
        #[derive(Clone, Copy, PartialEq, Debug)]
        #[allow(clippy::items_after_statements, reason = "local helper enum")]
        enum LineType {
            Normal,     // Regular content line
            Empty,      // No content on this line
            MoreIndent, // Line with extra indentation (preserve)
        }

        let mut lines: Vec<(String, LineType)> = Vec::new();
        let mut end_span = start_span;

        // Per YAML spec 8.1.1.1: content_indent = block_scalar_indent + explicit_indicator
        // The block_scalar_indent is min_indent - 1 (the parent block's indentation)
        // This matches the batch parser's logic in scalar.rs line 676-678.
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
        while matches!(
            self.peek(),
            Some((Token::Indent(_) | Token::Dedent | Token::Comment(_), _))
        ) {
            self.advance();
        }

        loop {
            // Expect LineStart to begin each line
            let Some((Token::LineStart(indent_level), ls_span)) = self.peek() else {
                break;
            };
            let n = usize::from(*indent_level);
            let line_start_span = ls_span;
            self.advance();

            // Skip Indent/Dedent tokens
            while matches!(self.peek(), Some((Token::Indent(_) | Token::Dedent, _))) {
                self.advance();
            }

            // Check if this line ends the block scalar
            let is_terminator = matches!(
                self.peek(),
                None | Some((Token::DocEnd | Token::DocStart, _))
            );
            if is_terminator && n == 0 {
                break;
            }

            // Check if line has content (for termination and indent detection)
            // Include flow indicators since they can be content in block scalars.
            // Also include block structure indicators (BlockSeqIndicator, MappingKey) since they
            // indicate sibling nodes and should trigger termination checks.
            let has_content = matches!(
                self.peek(),
                Some((
                    Token::Plain(_)
                        | Token::Whitespace
                        | Token::WhitespaceWithTabs
                        | Token::Comment(_)
                        | Token::FlowSeqStart
                        | Token::FlowSeqEnd
                        | Token::FlowMapStart
                        | Token::FlowMapEnd
                        | Token::Colon
                        | Token::Comma
                        | Token::BlockSeqIndicator
                        | Token::MappingKey,
                    _
                ))
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

            // Collect content on this line
            let mut line_content = String::new();
            let line_type;
            let mut line_end_span = line_start_span;

            // Check for extra indentation (more-indented)
            let extra_indent = content_indent.map(|ci| n.saturating_sub(ci)).unwrap_or(0);
            if extra_indent > 0 {
                // Add extra spaces for more-indented lines
                line_content.push_str(&" ".repeat(extra_indent));
            }

            while let Some((tok, span)) = self.peek() {
                match tok {
                    Token::Plain(text) => {
                        line_content.push_str(text);
                        line_end_span = span;
                        self.advance();
                    }
                    Token::Whitespace => {
                        line_content.push(' ');
                        self.advance();
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
                            line_content.push_str(ws);
                        }
                        line_end_span = span;
                        self.advance();
                    }
                    Token::Comment(text) => {
                        // In block scalars, # is NOT a comment indicator - it's literal content
                        // The lexer incorrectly tokenizes it as a comment, so we need to
                        // reconstruct the original text including the #
                        line_content.push('#');
                        line_content.push_str(text);
                        line_end_span = span;
                        self.advance();
                    }
                    // In block scalars, flow indicators are literal content, not structure.
                    // The lexer doesn't know we're in a block scalar context.
                    Token::FlowSeqStart => {
                        line_content.push('[');
                        line_end_span = span;
                        self.advance();
                    }
                    Token::FlowSeqEnd => {
                        line_content.push(']');
                        line_end_span = span;
                        self.advance();
                    }
                    Token::FlowMapStart => {
                        line_content.push('{');
                        line_end_span = span;
                        self.advance();
                    }
                    Token::FlowMapEnd => {
                        line_content.push('}');
                        line_end_span = span;
                        self.advance();
                    }
                    Token::Colon => {
                        // Colon is literal content in block scalars
                        line_content.push(':');
                        line_end_span = span;
                        self.advance();
                    }
                    Token::Comma => {
                        // Comma is literal content in block scalars
                        line_content.push(',');
                        line_end_span = span;
                        self.advance();
                    }
                    _ => break,
                }
            }

            // Recover trailing whitespace that the lexer didn't include in tokens.
            // The lexer creates Plain("foo") for "foo " (missing trailing space).
            // If the next token is a LineStart, any gap between line_end_span.end and
            // LineStart.start is trailing whitespace to preserve.
            #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
            if !line_content.is_empty() {
                if let Some((Token::LineStart(_), next_span)) = self.peek() {
                    let end_pos = line_end_span.end_usize();
                    let next_start = next_span.start_usize();
                    if next_start > end_pos {
                        // There's a gap - check if it's all whitespace
                        let trailing = &self.input[end_pos..next_start];
                        if trailing.chars().all(|ch| ch == ' ' || ch == '\t') {
                            line_content.push_str(trailing);
                        }
                    }
                }
            }

            // Determine line type
            if line_content.is_empty() || line_content.chars().all(|ch| ch == ' ' || ch == '\t') {
                if line_content.is_empty() {
                    line_type = LineType::Empty;
                } else {
                    // More-indented whitespace line
                    line_type = LineType::MoreIndent;
                }
            } else if extra_indent > 0 {
                line_type = LineType::MoreIndent;
            } else {
                line_type = LineType::Normal;
            }

            // Only update end_span if this line had actual content
            // This matches the batch parser behavior where empty lines don't extend the span
            if !line_content.is_empty() {
                end_span = line_end_span;
            }

            lines.push((line_content, line_type));
        }

        // For empty block scalars (no actual content), use just the header span
        // Otherwise, span from header start to end of last content
        // A scalar is empty if lines is empty OR all lines are empty/whitespace-only
        let is_empty_scalar = lines.is_empty() || lines.iter().all(|(line, _)| line.is_empty());

        // Build final content based on style
        let value = if is_literal {
            // Literal: preserve all newlines
            let mut result = String::new();
            for (i, (line, _)) in lines.iter().enumerate() {
                result.push_str(line);
                if i + 1 < lines.len() {
                    result.push('\n');
                }
            }
            if !lines.is_empty() {
                result.push('\n');
            }
            result
        } else {
            // Folded: fold normal lines, preserve empty/more-indented
            let string_lines: Vec<String> = lines.into_iter().map(|(line, _)| line).collect();
            self.join_folded_lines(&string_lines)
        };

        // Apply chomping
        let value = self.apply_chomping(&value, &header);

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

        Some(Event::Scalar {
            style,
            value: Cow::Owned(value),
            anchor,
            tag,
            span: full_span,
        })
    }

    /// Join lines for folded block scalar with proper folding rules.
    fn join_folded_lines(&self, lines: &[String]) -> String {
        #[derive(Clone, Copy, PartialEq)]
        enum LineType {
            Normal,
            Empty,
            MoreIndent,
        }

        // YAML 1.2 folding rules:
        // - Normal → Normal: fold (space)
        // - Empty: preserve newline
        // - MoreIndent: preserve newlines before/after

        if lines.is_empty() {
            return String::new();
        }

        let mut result = String::new();

        // Classify each line by type
        let typed_lines: Vec<(&str, LineType)> = lines
            .iter()
            .map(|line| {
                let line_str = line.as_str();
                let line_type = if line_str.is_empty() {
                    LineType::Empty
                } else if line_str.starts_with(' ') || line_str.starts_with('\t') {
                    LineType::MoreIndent
                } else {
                    LineType::Normal
                };
                (line_str, line_type)
            })
            .collect();

        let mut prev_type = LineType::Empty; // Start as if preceded by empty
        // Track the last non-empty line type to handle Empty→MoreIndent correctly.
        // When transitioning Empty→MoreIndent, we only add an extra newline if we
        // came from a Normal context (Normal→Empty→MoreIndent), not from MoreIndent
        // context (MoreIndent→Empty→MoreIndent).
        let mut last_content_type: Option<LineType> = None;

        for (content, line_type) in typed_lines {
            match line_type {
                LineType::Empty => {
                    // Empty line contributes newline
                    if prev_type == LineType::MoreIndent {
                        result.push('\n'); // Line break after more-indented
                    }
                    result.push('\n');
                }
                LineType::MoreIndent => {
                    // More-indented lines preserve the preceding newline, but we need
                    // to be careful about Empty→MoreIndent transitions
                    if !result.is_empty() {
                        match prev_type {
                            LineType::Normal | LineType::MoreIndent => {
                                // Line break before more-indented is NOT folded
                                result.push('\n');
                            }
                            LineType::Empty => {
                                // Empty already added its newline. We only add another
                                // if we came from Normal context through Empty.
                                // MoreIndent→Empty→MoreIndent: no extra newline needed
                                // Normal→Empty→MoreIndent: need extra newline
                                if last_content_type == Some(LineType::Normal) {
                                    result.push('\n');
                                }
                            }
                        }
                    }
                    result.push_str(content);
                    last_content_type = Some(LineType::MoreIndent);
                }
                LineType::Normal => {
                    if !result.is_empty() {
                        match prev_type {
                            LineType::Normal => {
                                result.push(' '); // Fold
                            }
                            LineType::MoreIndent => {
                                result.push('\n'); // Preserve
                            }
                            LineType::Empty => {
                                // Newlines already added
                            }
                        }
                    }
                    result.push_str(content);
                    last_content_type = Some(LineType::Normal);
                }
            }
            prev_type = line_type;
        }

        // Add final newline
        if !lines.is_empty() && prev_type != LineType::Empty {
            result.push('\n');
        }

        result
    }

    /// Apply chomping indicator to block scalar value.
    /// Matches the batch parser's logic in scalar.rs `apply_chomping()`.
    fn apply_chomping(&self, value: &str, header: &crate::lexer::BlockScalarHeader) -> String {
        use crate::lexer::Chomping;

        let mut result = String::from(value);

        match header.chomping {
            Chomping::Strip => {
                // Remove all trailing newlines
                let trimmed_len = result.trim_end_matches('\n').len();
                result.truncate(trimmed_len);
            }
            Chomping::Clip => {
                // Keep exactly one trailing newline, but only if there's actual content
                let trimmed_len = result.trim_end_matches('\n').len();
                result.truncate(trimmed_len);
                if !result.is_empty() {
                    result.push('\n');
                }
            }
            Chomping::Keep => {
                // Ensure at least one trailing newline if content exists
                if !result.is_empty() && !result.ends_with('\n') {
                    result.push('\n');
                }
            }
        }

        result
    }
}
