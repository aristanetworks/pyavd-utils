// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Streaming event emitter - a pure state-machine parser.
//!
//! This module provides `Emitter`, a YAML parser that produces events
//! one at a time using an explicit state stack instead of recursion.
//! It is designed from the ground up for true streaming operation.
//!
//! The `Emitter` is validated against the batch `Parser` - both must
//! produce identical event sequences for all inputs.

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
    AfterKey,
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
    /// After implicit mapping value, emit MappingEnd.
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
        /// Anchor/tag to attach to SequenceStart (only used in EmitStart phase).
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    },
    /// Block mapping parsing.
    BlockMap {
        indent: IndentLevel,
        phase: BlockMapPhase,
        start_span: Span,
        /// Anchor/tag to attach to MappingStart (only used in EmitStart phase).
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
    /// Emit `SequenceStart` (for complex key scenarios where MappingStart is emitted first).
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

/// A streaming YAML emitter using an explicit state machine.
///
/// This is an alternative to the recursive descent `Parser`, designed
/// for true streaming operation. It produces events one at a time
/// via the `Iterator` interface.
#[derive(Debug)]
pub struct Emitter<'tokens, 'input> {
    /// Token slice (we still use slice access for now, but the state machine
    /// design allows future migration to streaming token input).
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
    /// Last content span - used for MappingEnd/SequenceEnd to match batch parser behavior.
    /// Updated when emitting content events (scalars, aliases, nested collection ends).
    last_content_span: Option<Span>,
    /// Whether we've crossed a line boundary since the last time this flag was cleared.
    /// Set when consuming `LineStart` tokens. Used by `AfterValue` to determine if
    /// a nested structure crossed a line, even after those tokens were consumed.
    crossed_line_boundary: bool,
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

    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError {
            kind,
            span,
            span_offset: 0,
        });
    }

    /// Check if a span contains a newline (used for multiline implicit key detection).
    /// If the key spans multiple lines, it cannot be an implicit key.
    fn check_multiline_implicit_key(&mut self, key_span: Span) {
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

    /// Check for tabs used as indentation after a `LineStart` token.
    /// In block context, tabs used for indentation are invalid.
    fn check_tabs_as_indentation(&mut self) {
        // Only check in block context
        if self.flow_depth > 0 {
            return;
        }

        // Check if current token is WhitespaceWithTabs
        if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek() {
            // Look ahead to see what follows the whitespace
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
                Some((Token::LineStart(_), span)) => {
                    crossed_line = true;
                    last_linestart_span = Some(span);
                    self.advance();
                    // Check for tabs as indentation after crossing a line boundary
                    self.check_tabs_as_indentation();
                }
                Some((Token::Indent(_), _)) => self.advance(),
                Some((Token::Comment(_), _)) => self.advance(),
                Some((Token::Dedent, _)) => self.advance(),
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
        let span = self.current_span();
        if has_doc_end {
            self.advance();
            self.skip_ws_and_newlines();
        }

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
                    self.error(ErrorKind::TrailingContent, span);
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
                    return Some(Event::SequenceEnd { span });
                }

                ParseState::EmitMapEnd { span } => {
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

        // Check for properties (anchor, tag) before the value
        let (anchor, tag, prop_crossed_line) = self.collect_properties(anchor, tag);

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
        let at_block_indicator = !in_sequence_entry
            && matches!(
                self.peek(),
                Some((
                    Token::BlockSeqIndicator | Token::MappingKey | Token::Colon,
                    _
                ))
            );
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
                        phase: BlockMapPhase::AfterKey,
                        start_span: alias_span,
                        anchor: None,
                        tag: None,
                    });
                    // Push the alias as the key to emit
                    self.state_stack.push(ParseState::EmitAlias {
                        name: Cow::Owned(alias_name.into_owned()),
                        span: alias_span,
                    });

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
                        phase: BlockMapPhase::AfterKey,
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
                    self.flow_depth += 1;

                    // Push BlockMap state for after the key
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::AfterKey,
                        start_span: span,
                        anchor: None,
                        tag: None,
                    });

                    // Push FlowSeq state with EmitStart phase to emit SequenceStart next
                    self.state_stack.push(ParseState::FlowSeq {
                        phase: FlowSeqPhase::EmitStart,
                        start_span: span,
                    });

                    // Emit MappingStart with the properties
                    return Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        anchor,
                        tag,
                        span,
                    });
                }

                self.advance();
                self.flow_depth += 1;
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
                    self.flow_depth += 1;

                    // Push BlockMap state for after the key
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::AfterKey,
                        start_span: span,
                        anchor: None,
                        tag: None,
                    });

                    // Push FlowMap state with EmitStart phase to emit MappingStart next
                    self.state_stack.push(ParseState::FlowMap {
                        phase: FlowMapPhase::EmitStart,
                        start_span: span,
                    });

                    // Emit outer block MappingStart with the properties
                    return Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        anchor,
                        tag,
                        span,
                    });
                }

                self.advance();
                self.flow_depth += 1;
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
                            self.flow_depth += 1;

                            // Push BlockMap state for after the key
                            self.state_stack.push(ParseState::BlockMap {
                                indent: map_indent,
                                phase: BlockMapPhase::AfterKey,
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
                                // Emit map start with inner props via deferred state
                                // For now, emit directly - TODO: add EmitMapStart state
                            }

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
                                    phase: BlockMapPhase::AfterKey,
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
                        self.parse_plain_scalar(inner_anchor, inner_tag, min_indent)
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
                        self.flow_depth += 1;
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
                        self.flow_depth += 1;
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
        mut anchor: Option<(Cow<'static, str>, Span)>,
        mut tag: Option<(Cow<'static, str>, Span)>,
    ) -> (
        Option<(Cow<'static, str>, Span)>,
        Option<(Cow<'static, str>, Span)>,
        bool,
    ) {
        let mut crossed_line_boundary = false;
        loop {
            // Peek and extract info before any mutation
            enum PropAction {
                Anchor { name: String, span: Span },
                Tag { tag_str: String, span: Span },
                Comment,
                LineStart { should_continue: bool },
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
                Some((Token::LineStart(_), _)) if has_props => PropAction::LineStart {
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
                    let expanded = self.expand_tag(&tag_str);
                    tag = Some((Cow::Owned(expanded), span));
                    self.advance();
                    self.skip_ws();
                }
                PropAction::Comment => {
                    // Skip comment - the line boundary after it will be handled next iteration
                    self.advance();
                }
                PropAction::LineStart { should_continue } => {
                    // We have collected at least one property and there's a line boundary.
                    // Check if the next line has more properties that we should collect.
                    //
                    // We should continue collecting if:
                    // 1. Next line has properties that are NOT followed by implicit mapping key content
                    // 2. Or next line has no properties (we'll stop naturally)
                    //
                    // We should STOP if the next line has properties followed by an implicit key,
                    // because then the first properties go on the parent mapping.
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
    fn expand_tag(&self, tag_str: &str) -> String {
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
            // Handle not declared - return unexpanded (could emit error)
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
                        Some(Event::SequenceEnd {
                            span: self.collection_end_span(),
                        })
                    }

                    Some((Token::LineStart(n), _)) => {
                        if *n < indent {
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

                    _ => {
                        // No more entries
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
                let crossed_line = self.skip_ws_and_newlines_returns_crossed_line();

                // If we require a line boundary (subsequent entry after a value) and
                // didn't cross one, end the mapping - we can't have same-line entries.
                if require_line_boundary && !crossed_line {
                    return Some(Event::MappingEnd {
                        span: self.collection_end_span(),
                    });
                }

                // Check if we've moved to a lower indent level (mapping should end)
                // This catches cases where skip_ws_and_newlines consumed a LineStart
                // that would have triggered a dedent
                if self.current_indent < indent {
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
                            phase: BlockMapPhase::AfterKey,
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
                            phase: BlockMapPhase::AfterKey,
                            start_span,
                            anchor: None,
                            tag: None,
                        });
                        Some(self.emit_null())
                    }

                    Some((Token::DocEnd | Token::DocStart, _)) | None => Some(Event::MappingEnd {
                        span: self.collection_end_span(),
                    }),

                    Some((Token::LineStart(n), _)) => {
                        if *n < indent {
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
                        // Check for implicit key (scalar followed by colon)
                        // At this point, if require_line_boundary was true and crossed_line
                        // was false, we would have already returned MappingEnd above.
                        // So either this is the first entry or we crossed a line.
                        if self.is_implicit_key() {
                            self.state_stack.push(ParseState::BlockMap {
                                indent,
                                phase: BlockMapPhase::AfterKey,
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
                            Some(Event::MappingEnd {
                                span: self.collection_end_span(),
                            })
                        }
                    }
                }
            }

            BlockMapPhase::AfterKey => {
                // In block mappings, the colon can be on a following line after an explicit key
                // e.g., `? [ key ]\n: value` - need to skip the newline to find the colon

                // Track last LineStart span - for empty values, batch parser uses
                // the LineStart span (newline position), not the next token position
                let (_, last_linestart_span) = self.skip_ws_and_newlines_impl();

                // Expect colon
                if matches!(self.peek(), Some((Token::Colon, _))) {
                    self.advance();
                    self.skip_ws();
                    // Check for tabs after block mapping indicator (colon)
                    self.check_tabs_after_block_indicator();

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
                    // Use LineStart span if we crossed a line, else current span
                    let empty_span = last_linestart_span.unwrap_or_else(|| self.current_span());
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
                // Check if we've crossed a line boundary since parsing started.
                // This flag is set whenever we consume a LineStart token, including
                // when nested structures (like inner mappings) consumed them.
                //
                // If crossed_line_boundary is true, we've moved to a new line and
                // subsequent entries are valid. If false, we're on the same line
                // as the previous value (like `a: b: c`) and should prevent new entries.
                let already_crossed = self.crossed_line_boundary;

                // Reset the flag - we've consumed this information.
                // The next BeforeKey will re-check via skip_ws_and_newlines.
                self.crossed_line_boundary = false;

                self.state_stack.push(ParseState::BlockMap {
                    indent,
                    phase: BlockMapPhase::BeforeKey {
                        // Only require line boundary if we haven't already crossed one
                        require_line_boundary: !already_crossed,
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

    /// Check if the current FlowSeqStart token is part of a complex key pattern.
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
        loop {
            match self.peek_nth(i) {
                Some((Token::Anchor(_) | Token::Tag(_), _)) => {
                    i += 1;
                    // Skip whitespace after property
                    while let Some((tok, _)) = self.peek_nth(i) {
                        match tok {
                            Token::Whitespace | Token::WhitespaceWithTabs | Token::LineStart(_) => {
                                i += 1
                            }
                            _ => break,
                        }
                    }
                }
                _ => break,
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

    /// Skip indent-related tokens (Indent, Dedent, Whitespace, WhitespaceWithTabs).
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

    /// Consume all tokens until LineStart, returning the text as a slice.
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
    /// Looks ahead to see if there's a properly indented LineStart followed by content.
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
                        self.advance();
                        self.flow_depth = self.flow_depth.saturating_sub(1);
                        Some(Event::SequenceEnd { span })
                    }

                    Some((Token::Comma, _)) => {
                        // Empty entry
                        self.advance();
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::BeforeEntry,
                            start_span,
                        });
                        Some(self.emit_null())
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
                        self.flow_depth = self.flow_depth.saturating_sub(1);
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
                        self.advance();
                        self.flow_depth = self.flow_depth.saturating_sub(1);
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
                        self.flow_depth = self.flow_depth.saturating_sub(1);
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
                        self.advance();
                        self.flow_depth = self.flow_depth.saturating_sub(1);
                        Some(Event::MappingEnd { span })
                    }

                    Some((Token::Comma, _)) => {
                        // Empty entry - null key
                        self.advance();
                        self.state_stack.push(ParseState::FlowMap {
                            phase: FlowMapPhase::BeforeKey,
                            start_span,
                        });
                        // Emit null key and null value
                        Some(self.emit_null())
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
                        self.flow_depth = self.flow_depth.saturating_sub(1);
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
                        self.advance();
                        self.flow_depth = self.flow_depth.saturating_sub(1);
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
                        self.flow_depth = self.flow_depth.saturating_sub(1);
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
                    phase: BlockMapPhase::AfterKey,
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
                k_span // Use full first key span for MappingStart
            } else {
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
        self.parse_plain_scalar(anchor, tag, min_indent)
    }

    /// Parse a plain scalar (potentially multiline).
    ///
    /// Implements basic multiline plain scalar handling:
    /// - Single newline followed by content becomes a space
    /// - Multiple consecutive newlines preserve (n-1) newlines
    ///
    /// `min_indent` specifies the minimum indentation for continuation lines.
    /// Continuation lines must have indent >= min_indent to be considered part of the scalar.
    fn parse_plain_scalar(
        &mut self,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        min_indent: IndentLevel,
    ) -> Option<Event<'input>> {
        match self.peek() {
            Some((Token::Plain(text), span)) => {
                let mut content = (*text).to_string();
                let start_span = span;
                let mut end_span = span;
                self.advance();

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
                    loop {
                        match self.peek() {
                            Some((Token::LineStart(n), _)) => {
                                let indent = *n;

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
                self.parse_quoted_scalar(anchor, tag, *quote_style)
            }

            _ => Some(self.emit_null()),
        }
    }

    /// Parse a quoted scalar.
    fn parse_quoted_scalar(
        &mut self,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
        quote_style: crate::lexer::QuoteStyle,
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
                Some((Token::LineStart(_), span)) => {
                    // Trim trailing spaces before newline
                    let trimmed_len = value.trim_end_matches(' ').len();
                    value.truncate(trimmed_len);
                    pending_newlines += 1;
                    end_span = span;
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
        #[derive(Clone, Copy, PartialEq)]
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

            // Determine content indent from first non-empty line
            if content_indent.is_none() && has_content && n > 0 {
                content_indent = Some(n);
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

            end_span = line_start_span;

            // Collect content on this line
            let mut line_content = String::new();
            let line_type;

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
                        end_span = span;
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
                        end_span = span;
                        self.advance();
                    }
                    Token::Comment(text) => {
                        // In block scalars, # is NOT a comment indicator - it's literal content
                        // The lexer incorrectly tokenizes it as a comment, so we need to
                        // reconstruct the original text including the #
                        line_content.push('#');
                        line_content.push_str(text);
                        end_span = span;
                        self.advance();
                    }
                    // In block scalars, flow indicators are literal content, not structure.
                    // The lexer doesn't know we're in a block scalar context.
                    Token::FlowSeqStart => {
                        line_content.push('[');
                        end_span = span;
                        self.advance();
                    }
                    Token::FlowSeqEnd => {
                        line_content.push(']');
                        end_span = span;
                        self.advance();
                    }
                    Token::FlowMapStart => {
                        line_content.push('{');
                        end_span = span;
                        self.advance();
                    }
                    Token::FlowMapEnd => {
                        line_content.push('}');
                        end_span = span;
                        self.advance();
                    }
                    Token::Colon => {
                        // Colon is literal content in block scalars
                        line_content.push(':');
                        end_span = span;
                        self.advance();
                    }
                    Token::Comma => {
                        // Comma is literal content in block scalars
                        line_content.push(',');
                        end_span = span;
                        self.advance();
                    }
                    _ => break,
                }
            }

            // Recover trailing whitespace that the lexer didn't include in tokens.
            // The lexer creates Plain("foo") for "foo " (missing trailing space).
            // If the next token is a LineStart, any gap between end_span.end and
            // LineStart.start is trailing whitespace to preserve.
            #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
            if !line_content.is_empty() {
                if let Some((Token::LineStart(_), next_span)) = self.peek() {
                    let end_pos = end_span.end_usize();
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

            lines.push((line_content, line_type));
        }

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

        let full_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

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
