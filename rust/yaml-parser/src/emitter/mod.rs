// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML event emitter with error recovery.
//!
//! This module provides `Emitter`, a YAML parser that produces events
//! using an explicit state stack instead of recursion. It processes
//! a slice of tokens and emits events via the `Iterator` interface.
//!
//! The `Emitter` is validated against the YAML Test Suite to ensure
//! correct event sequences for all inputs.

mod cursor;
mod states;

use std::borrow::Cow;
use std::collections::HashSet;

use crate::error::{ErrorKind, ParseError};
use crate::event::{Event, Properties, Property, ScalarStyle};
use crate::lexer::{RichToken, Token};
use crate::span::{IndentLevel, Span, usize_to_indent};

use cursor::TokenCursor;
use states::{
    BlockMapPhase, BlockSeqPhase, DocState, FlowMapPhase, FlowSeqPhase, ParseState, ValueContext,
    ValueKind,
};

/// Type alias for property collection return type:
/// `(properties, crossed_property_line_boundary)`.
///
/// The boolean is `true` if a `LineStart` token was encountered while
/// collecting properties (i.e. properties span multiple lines).
type PropertyCollection<'input> = (Properties<'input>, bool);

/// Result of deciding what to do with collected properties at a dedented
/// indent: either emit an empty scalar now, or keep the properties attached to
/// the upcoming value.
#[derive(Debug)]
enum MaybeEmptyScalarDecision<'input> {
    /// Do not emit an empty scalar; continue parsing this value with the
    /// (possibly updated) properties.
    Continue { properties: Properties<'input> },
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

/// A YAML event emitter using an explicit state machine.
///
/// Processes tokens and produces YAML events via the `Iterator` interface.
/// Uses an explicit state stack instead of recursion for parsing.
///
/// ## Architecture Note
///
/// The emitter owns the tokens because YAML parsing fundamentally requires
/// lookahead to resolve ambiguities. This matches the approach used by `LibYAML`
/// and other production YAML parsers, which buffer tokens internally to support
/// lookahead operations.
#[derive(Debug)]
pub struct Emitter<'tokens, 'input> {
    /// Token cursor wrapper. Provides read-only access helpers over the token slice.
    ///
    /// NOTE: The raw slice is still stored separately during the initial refactor
    /// to avoid touching all direct `tokens` usages at once. Over time, new code
    /// should go through the cursor, and direct slice access can be phased out.
    tokens: &'tokens [RichToken<'input>],
    cursor: TokenCursor<'tokens, 'input>,
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
}

impl<'tokens, 'input> Emitter<'tokens, 'input> {
    /// Create a new emitter from tokens.
    ///
    /// Takes ownership of the tokens to enable zero-copy parsing where events
    /// borrow directly from the input string.
    #[must_use]
    pub fn new(tokens: &'tokens [RichToken<'input>], input: &'input str) -> Self {
        Self {
            tokens,
            cursor: TokenCursor::new(tokens),
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

    /// Take collected errors.
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.errors)
    }

    // ─────────────────────────────────────────────────────────────
    // Token access helpers
    // ─────────────────────────────────────────────────────────────

    fn peek(&self) -> Option<(&Token<'input>, Span)> {
        self.cursor.peek(self.pos)
    }

    fn peek_nth(&self, n: usize) -> Option<(&Token<'input>, Span)> {
        self.cursor.peek_nth(self.pos, n)
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
        self.cursor.is_eof(self.pos)
    }

    fn current_span(&self) -> Span {
        self.cursor.current_span(self.pos)
    }

    /// Get span for collection end events (MappingEnd/SequenceEnd).
    /// Uses the last content span if available, otherwise falls back to current span.
    /// End spans point to the end of the last content rather than the next token
    /// (like a newline or dedent).
    fn collection_end_span(&self) -> Span {
        if let Some(span) = self.last_content_span {
            // Use end position of last content as a point span
            Span::new(span.end..span.end)
        } else if self.pos > 0 {
            // Fallback: use end of previous token
            self.tokens.get(self.pos - 1).map_or_else(
                || self.current_span(),
                |rt| Span::from_usize_range(rt.span.end_usize()..rt.span.end_usize()),
            )
        } else {
            self.current_span()
        }
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
        #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
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
                Some((Token::LineStart(indent), span)) => {
                    let indent = *indent;
                    crossed_line = true;
                    last_linestart_span = Some(span);
                    self.advance();

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
                            let content_col = self.column_of_position(content_span.start_usize());
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

                    // Check for tabs as indentation after crossing a line boundary
                    // In flow context, only tabs at column 0 are invalid
                    if self.flow_context_columns.is_empty() {
                        self.check_tabs_as_indentation();
                    } else {
                        self.check_tabs_at_column_zero_in_flow();
                    }
                }
                Some((
                    Token::Whitespace
                    | Token::WhitespaceWithTabs
                    | Token::Indent(_)
                    | Token::Comment(_),
                    _,
                )) => self.advance(),
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

        // Reset document-level state (anchors, tag handles, etc.).
        self.reset_document_state();

        // Track directives for "directive without document" error
        let mut has_directive = false;
        let mut first_directive_span = Span::from_usize_range(0..0);

        // Skip directive tokens
        while let Some((tok, span)) = self.peek() {
            match tok {
                Token::YamlDirective(_) | Token::TagDirective(..) | Token::ReservedDirective(_) => {
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

        // Skip any Dedent tokens that may have been generated from indented comments.
        // These can appear after directives or at the start of a comment-only stream.
        // Dedent tokens should not trigger an implicit document start.
        while matches!(self.peek(), Some((Token::Dedent, _))) {
            self.advance();
        }

        // After skipping dedents, skip any remaining whitespace/newlines
        // (e.g., trailing blank lines after indented comments)
        self.skip_ws_and_newlines();

        // Check for "directive without document" error
        if has_directive {
            let at_end = self.is_eof() || matches!(self.peek(), Some((Token::DocEnd, _)));
            if at_end {
                self.error(ErrorKind::TrailingContent, first_directive_span);
                return None;
            }
        }

        // After skipping dedents and whitespace, check if we're at EOF
        // (e.g., comment-only input with indented comments)
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
            if content_on_line && !self.is_eof() && self.check_block_mapping_on_start_line() {
                self.error(ErrorKind::ContentOnSameLine, self.current_span());
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
                Token::Colon | Token::MappingKey => return true,
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

        // Scan for TagDirective tokens and insert their handle/prefix pairs.
        let mut idx = self.pos;
        while idx < self.tokens.len() {
            let Some(rich_token) = self.tokens.get(idx) else {
                break;
            };
            match &rich_token.token {
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
                    Token::YamlDirective(_) | Token::TagDirective(..) | Token::ReservedDirective(_),
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
            properties: Properties::default(),
            span: self.current_span(),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════
// Iterator Implementation - The State Machine
// ═══════════════════════════════════════════════════════════════════

impl<'input> Iterator for Emitter<'_, 'input> {
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
                    // Push initial value parse state (top-level document value)
                    self.state_stack.push(ParseState::Value {
                        ctx: ValueContext {
                            min_indent: 0,
                            kind: ValueKind::TopLevelValue,
                            allow_implicit_mapping: true, // Document root allows implicit mappings
                        },
                        properties: Properties::default(),
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

#[allow(
    clippy::too_many_lines,
    reason = "Complex state machine with value dispatch logic"
)]
impl<'input> Emitter<'_, 'input> {
    /// Process the state stack and return the next event, if any.
    ///
    /// Returns `None` when the stack is empty (document content complete).
    fn process_state_stack(&mut self) -> Option<Event<'input>> {
        loop {
            let state = self.state_stack.pop()?;

            match state {
                ParseState::Value { ctx, properties } => {
                    // Parse the value context and properties and push state accordingly
                    self.parse_value(ctx, properties);
                }
                ParseState::ValueAfterProperties {
                    ctx,
                    properties,
                    initial_crossed_line,
                    prop_crossed_line,
                    property_indent,
                } => {
                    if let Some(event) = self.process_value_after_properties(
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
                ParseState::AdditionalPropertiesValue { ctx, outer } => {
                    if let Some(event) = self.process_additional_properties_value_state(ctx, outer)
                    {
                        return Some(event);
                    }
                    // No value produced, continue with next state
                }
                ParseState::FlowCollectionValue {
                    is_map,
                    span,
                    properties,
                } => {
                    return Some(
                        self.process_flow_collection_value_state(is_map, span, properties),
                    );
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

                ParseState::EmitMapEnd { span } => {
                    self.pop_indent();
                    return Some(Event::MappingEnd { span });
                }

                ParseState::EmitScalar {
                    value,
                    properties,
                    span,
                    style,
                } => {
                    return Some(Event::Scalar {
                        style,
                        value,
                        properties,
                        span,
                    });
                }

                ParseState::EmitAlias { name, span } => {
                    return Some(Event::Alias { name, span });
                }

                ParseState::EmitSeqStart { properties, span } => {
                    return Some(Event::SequenceStart {
                        style: crate::event::CollectionStyle::Flow,
                        properties,
                        span,
                    });
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
                    phase: BlockSeqPhase::AfterEntry,
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
        properties: Properties<'input>,
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
        let Some((Token::LineStart(next_indent), _)) = self.peek() else {
            return MaybeEmptyScalarDecision::Continue { properties };
        };
        if *next_indent >= min_indent {
            return MaybeEmptyScalarDecision::Continue { properties };
        }

        // Check if we're in a sequence entry context.
        let in_sequence_entry = self.in_sequence_entry_context();

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
        let properties_at_invalid_indent =
            initial_crossed_line && property_indent.is_some_and(|prop_ind| prop_ind < min_indent);

        // Even if not too dedented, we should NOT bridge to a sibling mapping key.
        // Look ahead past the LineStart and Dedent tokens to see what follows.
        // Only bridge if the next content token is a block collection indicator (- or ?).
        let mut lookahead_for_indicator = 1;
        while let Some((tok, _)) = self.peek_nth(lookahead_for_indicator) {
            if matches!(tok, Token::Dedent | Token::Indent(_)) {
                lookahead_for_indicator += 1;
            } else {
                break;
            }
        }

        let can_bridge = !in_sequence_entry
            && !too_dedented
            && !properties_at_invalid_indent
            && self
                .peek_nth(lookahead_for_indicator)
                .is_some_and(|(token, _)| {
                    matches!(token, Token::BlockSeqIndicator | Token::MappingKey)
                });

        if !can_bridge {
            // Can't bridge - emit empty scalar.
            // If properties were collected at an invalid indent (below min_indent),
            // drop them (e.g. `seq:\n&anchor\n- a` where &anchor is at indent 0
            // < min_indent 1).
            let event_properties = if properties_at_invalid_indent {
                Properties::default() // Drop properties collected at invalid indent
            } else {
                properties
            };
            return MaybeEmptyScalarDecision::EmitEmptyScalar {
                event: Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties: event_properties,
                    span: self.current_span(),
                },
            };
        }

        // Bridging to block collection at valid dedent level.
        MaybeEmptyScalarDecision::Continue { properties }
    }

    /// Check if a dedent after crossing a line boundary indicates an empty
    /// value (the dedented content belongs to a parent context).
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
    fn maybe_emit_empty_due_to_dedent(
        &mut self,
        ctx: ValueContext,
        crossed_line_after_properties: bool,
        properties: &Properties<'input>,
    ) -> Option<Event<'input>> {
        let min_indent = ctx.min_indent;

        // Only dedent-based emptiness if we crossed a line.
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

        // Look past Dedent/Indent tokens to find the actual content token.
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

        if self.current_indent < min_indent && !at_block_indicator {
            return Some(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                properties: properties.clone(),
                span: self.current_span(),
            });
        }

        None
    }

    /// Merge additional "inner" properties that may appear after a line
    /// boundary when processing `AdditionalPropertiesValue`.
    ///
    /// This handles patterns like:
    ///
    /// ```yaml
    /// - &outer
    ///   !!null
    ///   &inner !!tag
    ///   key: value
    /// ```
    ///
    /// where `inner` properties can be defined on a later line and should
    /// override earlier inner properties.
    fn collect_additional_inner_properties(
        &mut self,
        mut inner_properties: Properties<'input>,
    ) -> Properties<'input> {
        // Check for line boundary followed by more content (third layer of
        // properties).
        if matches!(self.peek(), Some((Token::LineStart(_), _))) {
            // Peek ahead to see what's after the line boundary.
            let mut peek_offset = 1;
            // Skip past Indent/Dedent tokens.
            while let Some((tok, _)) = self.peek_nth(peek_offset) {
                if matches!(tok, Token::Indent(_) | Token::Dedent) {
                    peek_offset += 1;
                } else {
                    break;
                }
            }

            // Check if next content is more properties.
            if let Some((tok, _)) = self.peek_nth(peek_offset) {
                if matches!(tok, Token::Anchor(_) | Token::Tag(_)) {
                    // More properties on next line - merge with inner.
                    self.skip_ws_and_newlines();
                    let (new_props, _) = self.collect_properties(Properties::default());
                    // Prefer newer properties (or keep inner if new is None).
                    inner_properties.update(new_props);
                    self.skip_ws();

                    // If we see LineStart again, skip it.
                    if matches!(self.peek(), Some((Token::LineStart(_), _))) {
                        self.skip_ws_and_newlines();
                    }
                } else {
                    // Not more properties - skip to see actual content.
                    self.skip_ws_and_newlines();
                }
            }
        }

        inner_properties
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
        properties: &Properties<'input>,
    ) -> bool {
        !prop_crossed_line
            && (properties.anchor.is_some() || properties.tag.is_some())
            && matches!(self.peek(), Some((Token::Colon, _)))
    }

    /// Common helper for creating a block mapping from a scalar key that has
    /// already been parsed.
    ///
    /// This sets up the `BlockMap` and `EmitScalar` states and returns the
    /// `MappingStart` event. The `start_span` is the span of the indicator that
    /// triggered implicit mapping detection (used as `start_span` for
    /// `BlockMap`), while the returned event's span is the full key span.
    fn build_block_mapping_from_scalar_key(
        &mut self,
        map_indent: IndentLevel,
        start_span: Span,
        map_properties: Properties<'input>,
        key_event: Event<'input>,
    ) -> Event<'input> {
        if let Event::Scalar {
            value,
            properties:
                Properties {
                    anchor: k_anchor,
                    tag: k_tag,
                },
            span: k_span,
            style,
        } = key_event
        {
            // Check for multiline implicit key error.
            self.check_multiline_implicit_key(k_span);

            // Stack setup: emit the key first, then parse the value after colon.
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: true, // Plain scalar key
                },
                start_span,
                properties: Properties::default(),
            });
            self.state_stack.push(ParseState::EmitScalar {
                value,
                properties: Properties {
                    anchor: k_anchor,
                    tag: k_tag,
                },
                span: k_span,
                style,
            });
            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);

            Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: map_properties,
                span: k_span, // Use full key span for MappingStart
            }
        } else {
            // Fallback - shouldn't happen, but keep stack consistent.
            self.push_indent(map_indent);
            Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: map_properties,
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
            if let Some((tok, span)) = self.peek() {
                if matches!(tok, Token::Anchor(_) | Token::Tag(_)) {
                    // Properties at invalid indent get both InvalidIndentation and OrphanedProperties
                    self.error(ErrorKind::InvalidIndentation, span);
                    self.error(ErrorKind::OrphanedProperties, span);
                } else if matches!(tok, Token::Plain(_) | Token::StringStart(_)) {
                    // Don't report InvalidIndentation if this is a mapping key (followed by colon).
                    // E.g., `: # comment\n"key":` - the "key": is a new mapping entry, not a value.
                    let is_mapping_key = if matches!(tok, Token::Plain(_)) {
                        self.is_plain_followed_by_colon()
                    } else {
                        // StringStart - check if followed by StringContent, StringEnd, then Colon
                        let mut i = 1;
                        // Skip StringContent tokens
                        while matches!(self.peek_nth(i), Some((Token::StringContent(_), _))) {
                            i += 1;
                        }
                        // Check for StringEnd
                        if matches!(self.peek_nth(i), Some((Token::StringEnd(_), _))) {
                            i += 1;
                            // Skip whitespace
                            while matches!(
                                self.peek_nth(i),
                                Some((Token::Whitespace | Token::WhitespaceWithTabs, _))
                            ) {
                                i += 1;
                            }
                            // Check for Colon
                            matches!(self.peek_nth(i), Some((Token::Colon, _)))
                        } else {
                            false
                        }
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
            && let Some((Token::LineStart(indent), _)) = self.peek()
            && *indent < min_indent
        {
            // Look for property tokens after the LineStart
            let mut lookahead = 1;
            if matches!(self.peek_nth(lookahead), Some((Token::Indent(_), _))) {
                lookahead += 1;
            }
            while let Some((Token::Anchor(_) | Token::Tag(_), span)) = self.peek_nth(lookahead) {
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

    /// Handle a flow sequence start token as a value, including the case where
    /// the flow sequence acts as a complex mapping key in block context.
    /// Handle a flow collection start token (`[` or `{`) as a value, including
    /// the case where the flow collection acts as a complex mapping key in
    /// block context.
    fn process_flow_collection_value_state(
        &mut self,
        is_map: bool,
        span: Span,
        properties: Properties<'input>,
    ) -> Event<'input> {
        // In block context, check if this flow collection is a complex key.
        let is_complex_key = if is_map {
            self.flow_depth == 0 && self.is_flow_map_complex_key()
        } else {
            self.flow_depth == 0 && self.is_flow_seq_complex_key()
        };
        if is_complex_key {
            // This is a block mapping with a flow collection as key.
            let map_indent = self.current_indent;

            self.advance();
            self.enter_flow_collection(span.start_usize());

            // Push BlockMap state for after the key.
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: false, // Flow collection key, not plain scalar
                },
                start_span: span,
                properties: Properties::default(),
            });

            // Push flow collection state with EmitStart phase to emit
            // SequenceStart/MappingStart next.
            if is_map {
                self.state_stack.push(ParseState::FlowMap {
                    phase: FlowMapPhase::EmitStart,
                    start_span: span,
                });
            } else {
                self.state_stack.push(ParseState::FlowSeq {
                    phase: FlowSeqPhase::EmitStart,
                    start_span: span,
                });
            }

            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);
            // Emit MappingStart with the properties.
            return Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties,
                span,
            };
        }

        // Not a complex key: treat as regular flow collection value.
        self.advance();
        self.enter_flow_collection(span.start_usize());
        if is_map {
            self.state_stack.push(ParseState::FlowMap {
                phase: FlowMapPhase::BeforeKey,
                start_span: span,
            });
            Event::MappingStart {
                style: crate::event::CollectionStyle::Flow,
                properties,
                span,
            }
        } else {
            self.state_stack.push(ParseState::FlowSeq {
                phase: FlowSeqPhase::BeforeEntry,
                start_span: span,
            });
            Event::SequenceStart {
                style: crate::event::CollectionStyle::Flow,
                properties,
                span,
            }
        }
    }

    /// Continue parsing after encountering additional properties (anchor/tag)
    /// that appear after a line boundary before a value, including complex
    /// key patterns.
    ///
    /// This is the state-driven version of the old
    /// `handle_additional_properties_value` helper.
    fn process_additional_properties_value_state(
        &mut self,
        ctx: ValueContext,
        outer: Properties<'input>,
    ) -> Option<Event<'input>> {
        let Properties {
            anchor: outer_anchor,
            tag: outer_tag,
        } = outer;
        let min_indent = ctx.min_indent;
        let is_key = matches!(ctx.kind, ValueKind::Key);

        // Collect the NEW properties at current position (don't pass outer ones
        // since collect_properties would overwrite them), then merge any
        // additional properties that may appear after a line break.
        let inner: Properties<'input> = {
            let (props, _crossed) = self.collect_properties(Properties::default());
            self.skip_ws();
            self.collect_additional_inner_properties(props)
        };
        // Check if this is a complex key pattern (flow seq/map followed by :).
        // If we have separate outer and inner properties, and the value is a
        // flow collection that's a key, emit MappingStart with outer props.
        if !inner.is_empty()
            && let Some((Token::FlowSeqStart | Token::FlowMapStart, _)) = self.peek()
            && self.is_flow_seq_complex_key()
        {
            // This is a block mapping with a complex key.
            let span = self.current_span();
            let map_indent = self.current_indent;

            // Advance into the flow collection.
            let is_seq = matches!(self.peek(), Some((Token::FlowSeqStart, _)));
            self.advance();
            self.enter_flow_collection(span.start_usize());

            // Push BlockMap state for after the key.
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: false, // Flow collection key
                },
                start_span: span,
                properties: Properties::default(),
            });

            // Push flow collection to parse key content.
            if is_seq {
                self.state_stack.push(ParseState::FlowSeq {
                    phase: FlowSeqPhase::BeforeEntry,
                    start_span: span,
                });
                // Emit seq start with inner props (via EmitSeqStart).
                self.state_stack.push(ParseState::EmitSeqStart {
                    properties: inner.clone(),
                    span,
                });
            } else {
                self.state_stack.push(ParseState::FlowMap {
                    phase: FlowMapPhase::BeforeKey,
                    start_span: span,
                });
                // Map start with inner props will be emitted directly
                // by FlowMap handling.
            }

            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);
            // Emit MappingStart with outer props.
            return Some(Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: Properties {
                    anchor: outer_anchor,
                    tag: outer_tag,
                },
                span,
            });
        }

        // Now determine what kind of value follows.
        match self.peek() {
            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                // Check if it's an implicit mapping (scalar followed by colon).
                if !is_key && self.flow_depth == 0 && self.is_implicit_key() {
                    // It's a nested mapping.
                    // Property ownership:
                    // - outer properties (before line boundary) -> MappingStart
                    // - inner properties (same line as key) -> key scalar
                    let span = self.current_span();
                    let map_indent = self.current_indent;

                    // Parse the first key WITH inner properties.
                    let key_event = self.parse_plain_scalar(inner.clone(), min_indent);

                    // Build the mapping from the parsed key and outer properties.
                    let mapping_start = self.build_block_mapping_from_scalar_key(
                        map_indent,
                        span,
                        Properties {
                            anchor: outer_anchor,
                            tag: outer_tag,
                        },
                        key_event,
                    );
                    return Some(mapping_start);
                }
                // Not an implicit mapping, just a scalar with properties.
                // Outer properties that crossed a line boundary can only attach
                // to collections, not scalars. So we use inner properties only.
                // E.g., `&outer\n  &inner scalar` → &outer is lost, &inner on scalar.
                let result = self.parse_plain_scalar(inner.clone(), min_indent);
                // If this is a mapping key, check for multiline implicit key error.
                if is_key && let Event::Scalar { span, .. } = &result {
                    self.check_multiline_implicit_key(*span);
                }
                Some(result)
            }
            Some((Token::BlockSeqIndicator, _)) => {
                // Nested sequence; merge properties (outer takes precedence).
                let merged = inner.updated(Properties {
                    anchor: outer_anchor,
                    tag: outer_tag,
                });
                let span = self.current_span();
                let seq_indent = self.column_of_position(span.start_usize());
                self.state_stack.push(ParseState::BlockSeq {
                    indent: seq_indent,
                    phase: BlockSeqPhase::EmitStart,
                    start_span: span,
                    properties: merged,
                });
                self.process_state_stack()
            }
            Some((Token::FlowSeqStart, span)) => {
                // Flow sequence; merge properties (outer takes precedence).
                let merged = inner.updated(Properties {
                    anchor: outer_anchor,
                    tag: outer_tag,
                });
                self.advance();
                self.enter_flow_collection(span.start_usize());
                self.state_stack.push(ParseState::FlowSeq {
                    phase: FlowSeqPhase::BeforeEntry,
                    start_span: span,
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Flow,
                    properties: merged,
                    span,
                })
            }
            Some((Token::FlowMapStart, span)) => {
                // Flow mapping; merge properties (outer takes precedence).
                let merged = inner.updated(Properties {
                    anchor: outer_anchor,
                    tag: outer_tag,
                });
                self.advance();
                self.enter_flow_collection(span.start_usize());
                self.state_stack.push(ParseState::FlowMap {
                    phase: FlowMapPhase::BeforeKey,
                    start_span: span,
                });
                Some(Event::MappingStart {
                    style: crate::event::CollectionStyle::Flow,
                    properties: merged,
                    span,
                })
            }
            _ => {
                // Emit scalar with merged properties (outer takes precedence).
                let merged = inner.updated(Properties {
                    anchor: outer_anchor,
                    tag: outer_tag,
                });
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties: merged,
                    span: self.current_span(),
                })
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
        properties: Properties<'input>,
        crossed_line_boundary: bool,
    ) -> Event<'input> {
        // Advance past the alias token and skip any immediate whitespace.
        self.advance();
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
        if crossed_line_boundary && matches!(self.peek(), Some((Token::Colon, _))) {
            let map_indent = self.current_indent;

            // Push BlockMap state in AfterKey phase (we already have the key).
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: false, // Alias, not a plain scalar
                },
                start_span: alias_span,
                properties: Properties::default(),
            });
            // The alias itself will be emitted as the key.
            self.state_stack.push(ParseState::EmitAlias {
                name: alias_name,
                span: alias_span,
            });

            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);
            // Emit MappingStart with any outer anchor/tag.
            return Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties,
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
    /// This is the first phase of value parsing, responsible for whitespace
    /// skipping, indentation validation, and property collection. After
    /// properties have been collected, control is transferred to
    /// `ValueAfterProperties`, which performs the main dispatch and
    /// bridging/empty-value decisions.
    fn parse_value(&mut self, ctx: ValueContext, properties: Properties<'input>) {
        let min_indent = ctx.min_indent;

        // Track if we see a LineStart before value content
        // IMPORTANT: We must always call skip_ws_and_newlines_returns_crossed_line() to consume
        // the LineStart token, even if we detect it with matches!. Otherwise, collect_properties
        // won't see the anchor/tag tokens that follow.
        let has_leading_linestart = matches!(self.peek(), Some((Token::LineStart(_), _)));
        let skipped_crossed_line = self.skip_ws_and_newlines_returns_crossed_line();
        let initial_crossed_line = has_leading_linestart || skipped_crossed_line;

        // Check if we crossed a line and landed at an invalid indentation level.
        self.handle_invalid_indent_after_line_cross(min_indent, initial_crossed_line);

        // Check for properties (anchor, tag) before the value
        // Use min_indent to prevent collecting properties that are at invalid indentation
        let (valid_properties, prop_crossed_line) =
            self.collect_properties_with_min_indent(properties, min_indent);

        self.report_orphaned_properties_after_invalid_indent(min_indent, prop_crossed_line);

        // Capture the indent where properties were collected (if any)
        let property_indent = (!valid_properties.is_empty()).then_some(self.current_indent);

        // Hand off to the `ValueAfterProperties` state, which will perform the
        // main dispatch, bridging decisions, and empty-value handling.
        self.state_stack.push(ParseState::ValueAfterProperties {
            ctx,
            properties: valid_properties,
            initial_crossed_line,
            prop_crossed_line,
            property_indent,
        });
    }

    /// Continue parsing a value after properties have been collected.
    ///
    /// This function hosts the main dispatch logic that was previously the
    /// second half of `parse_value`, including:
    /// - Deciding whether properties at a dedented indent should bridge to a
    ///   block collection or emit an empty scalar.
    /// - Handling dedented empty values.
    /// - Dispatching to scalars, block/flow collections, aliases, and
    ///   additional property handling.
    #[allow(clippy::too_many_lines, reason = "Complex value dispatch logic")]
    fn process_value_after_properties(
        &mut self,
        ctx: ValueContext,
        mut properties: Properties<'input>,
        initial_crossed_line: bool,
        prop_crossed_line: bool,
        property_indent: Option<IndentLevel>,
    ) -> Option<Event<'input>> {
        let min_indent = ctx.min_indent;
        let is_key = matches!(ctx.kind, ValueKind::Key);
        let mut allow_implicit_mapping = ctx.allow_implicit_mapping;

        // Track line crossing from either initial skip or property collection.
        // This is local to value parsing and distinct from the emitter-wide
        // `crossed_line_boundary` flag used by block mappings.
        let mut crossed_line_after_properties = initial_crossed_line || prop_crossed_line;

        // If we crossed a line boundary, implicit mappings are now allowed
        // (even if they weren't before, a line break resets the context).
        allow_implicit_mapping = allow_implicit_mapping || crossed_line_after_properties;

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

        // Skip any more whitespace/newlines after properties and record if we
        // cross additional line boundaries.
        crossed_line_after_properties |= self.skip_ws_and_newlines_returns_crossed_line();

        // Dedent-based empty value check. This is only meaningful when we've
        // actually crossed a line boundary; avoid calling the helper at all on
        // the very common single-line value paths.
        if crossed_line_after_properties
            && let Some(event) =
                self.maybe_emit_empty_due_to_dedent(ctx, crossed_line_after_properties, &properties)
        {
            return Some(event);
        }

        // Dispatch based on current token
        match self.peek() {
            None => {
                // EOF - emit empty value / null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties,
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
                    ctx: ValueContext {
                        min_indent,
                        kind: ctx.kind,
                        allow_implicit_mapping,
                    },
                    properties,
                });
                None
            }

            Some((Token::DocEnd | Token::DocStart, span)) => {
                if self.flow_depth > 0 {
                    // Document markers inside flow context are invalid.
                    // Report error and continue - the flow state machine will handle them.
                    // Don't emit a null here; let the caller re-dispatch.
                    self.error(ErrorKind::DocumentMarkerInFlow, span);
                    self.advance();
                    // Re-enter Value state to parse the actual value
                    self.state_stack.push(ParseState::Value {
                        ctx: ValueContext {
                            min_indent,
                            kind: ctx.kind,
                            allow_implicit_mapping,
                        },
                        properties,
                    });
                    None
                } else {
                    // Block context - document marker ends the value, emit null
                    Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties,
                        span: self.current_span(),
                    })
                }
            }

            Some((Token::Alias(name), span)) => {
                // Defer alias handling to the `AliasValue` state so that
                // complex-key behaviour is driven by the state machine.
                let alias_name = Cow::Borrowed(*name);
                let alias_span = span;
                self.state_stack.push(ParseState::AliasValue {
                    name: alias_name,
                    span: alias_span,
                    properties,
                    crossed_line_after_properties,
                });
                None
            }

            Some((Token::BlockSeqIndicator, _)) => {
                let span = self.current_span();

                // YAML spec: Anchors/tags on the same line as a block sequence indicator
                // are ambiguous and disallowed. They must be on a separate line.
                // E.g., `&anchor - item` is invalid, but `&anchor\n- item` is valid.
                //
                // Check if properties are on the same line as the block indicator.
                // We can't rely on crossed_line_boundary because it may be reset when
                // we consume Dedent tokens and re-enter parse_value.
                // Instead, check if there's a LineStart token between the property and the indicator.
                let properties_on_same_line = {
                    // Check if the property and the indicator are on the same line
                    // by checking if there's a LineStart token between them.
                    // We can do this by checking if the property's end position is on the
                    // same line as the indicator's start position.
                    // A simple heuristic: if the indicator's start is immediately after
                    // the property's end (with only whitespace in between), they're on the same line.
                    // But this doesn't work if there's a newline.
                    // Better: check if we've seen a LineStart token since the property.
                    // We can use last_line_start_span for this.
                    let prop_end = match &properties {
                        Properties {
                            anchor: Some(anchor),
                            ..
                        } => anchor.span.end_usize(),
                        Properties { tag: Some(tag), .. } => tag.span.end_usize(),
                        _ => 0,
                    };
                    prop_end > self.last_line_start_span.end_usize()
                };

                if properties_on_same_line {
                    // Properties on same line as block sequence indicator - error
                    self.error(ErrorKind::ContentOnSameLine, span);
                    // Continue parsing to provide better error recovery
                }

                // Use column position of `-` indicator for indent tracking, not current_indent
                // This is crucial for nested sequences like `- - item`
                let seq_indent = self.column_of_position(span.start_usize());
                // Push block sequence state with collected anchor/tag
                self.state_stack.push(ParseState::BlockSeq {
                    indent: seq_indent,
                    phase: BlockSeqPhase::EmitStart,
                    start_span: span,
                    properties,
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
                        properties,
                        span: self.current_span(),
                    });
                }

                let span = self.current_span();
                // For compact mappings (where the mapping starts on the same line as an
                // indicator like `?` or `-`), use the column of the first token.
                // For regular mappings, use the line's indent level.
                // This ensures that compact mappings like `- ? earth: blue` correctly
                // track the mapping at column 4 (where `earth` starts), not at indent 0.
                let map_indent = if self.current_indent < min_indent {
                    // Compact notation: use the column of the current token
                    let column = span
                        .start_usize()
                        .saturating_sub(self.last_line_start_span.end_usize());
                    // YAML indentation is limited to reasonable values (< 65535)
                    usize_to_indent(column)
                } else {
                    // Regular notation: use the line's indent
                    self.current_indent
                };

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
                        },
                        start_span: span,
                        properties: Properties::default(),
                    });
                    // Push the null key with properties
                    self.state_stack.push(ParseState::EmitScalar {
                        value: Cow::Borrowed(""),
                        properties,
                        span,
                        style: ScalarStyle::Plain,
                    });
                    // Push indent level for orphan indent detection
                    self.push_indent(self.current_indent);
                    Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        properties: Properties::default(),
                        span,
                    })
                } else {
                    // Properties belong to the mapping
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::EmitStart,
                        start_span: span,
                        properties,
                    });
                    self.process_state_stack()
                }
            }

            Some((Token::FlowSeqStart, _)) => {
                let span = self.current_span();
                self.state_stack.push(ParseState::FlowCollectionValue {
                    is_map: false,
                    span,
                    properties,
                });
                None
            }

            Some((Token::FlowMapStart, _)) => {
                let span = self.current_span();
                self.state_stack.push(ParseState::FlowCollectionValue {
                    is_map: true,
                    span,
                    properties,
                });
                None
            }

            Some((Token::LiteralBlockHeader(_) | Token::FoldedBlockHeader(_), _)) => {
                Some(self.parse_block_scalar(min_indent, properties))
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
                Some(self.parse_scalar_or_mapping(
                    min_indent,
                    properties,
                    is_key,
                    prop_crossed_line,
                    effective_allow,
                ))
            }

            Some((Token::Anchor(_) | Token::Tag(_), _)) => {
                // More properties after line boundary - indicates nested structure.
                // Properties may span multiple lines (e.g., `&anchor\n!!str\nvalue`).
                // Defer to the `AdditionalPropertiesValue` state so complex
                // key behaviour is driven by the state machine.
                self.state_stack
                    .push(ParseState::AdditionalPropertiesValue {
                        ctx,
                        outer: properties,
                    });
                None
            }

            _ => {
                // Default: emit null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties,
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
    fn collect_properties(&mut self, properties: Properties<'input>) -> PropertyCollection<'input> {
        // Delegate to the version with min_indent = 0 (no indent restriction)
        self.collect_properties_with_min_indent(properties, 0)
    }

    /// Collect properties (anchor, tag) with a minimum indentation constraint.
    ///
    /// When `min_indent > 0`, properties on a new line at `indent < min_indent`
    /// are NOT collected - they belong to a parent context.
    fn collect_properties_with_min_indent(
        &mut self,
        mut properties: Properties<'input>,
        min_indent: IndentLevel,
    ) -> PropertyCollection<'input> {
        // Track whether we crossed a line boundary while collecting properties.
        // This is local to property collection and distinct from the emitter-wide
        // `crossed_line_boundary` flag used by block mappings.
        let mut crossed_property_line_boundary = false;
        loop {
            let has_props = !properties.is_empty();

            match self.peek() {
                Some((Token::Anchor(name_ref), span)) => {
                    // Extract data we need before any mutations
                    let name_str = *name_ref;

                    // Error: Duplicate anchor on same node
                    if properties.anchor.is_some() {
                        self.error(ErrorKind::DuplicateAnchor, span);
                    }

                    // Register anchor and store it (zero-copy)
                    self.anchors.insert(name_str);
                    properties.anchor = Some(Property {
                        value: Cow::Borrowed(name_str),
                        span,
                    });
                    self.advance();
                    self.skip_ws();
                }

                Some((Token::Tag(tag_cow), span)) => {
                    // Extract data we need before any mutations
                    let tag_str = tag_cow.as_ref();
                    let tag_looks_legitimate = !tag_str.contains('"') && !tag_str.contains('`');
                    let tag_cow_clone = tag_cow.clone(); // Clone the Cow (cheap if Borrowed)

                    // Error: Duplicate tag on same node
                    if properties.tag.is_some() {
                        self.error(ErrorKind::DuplicateTag, span);
                    }

                    // Expand tag handle (returns Cow, so zero-copy when possible)
                    let expanded = self.expand_tag(tag_cow_clone, span);
                    properties.tag = Some(Property {
                        value: expanded,
                        span,
                    });
                    self.advance();

                    // Check for content immediately after tag (no space)
                    // e.g., `!invalid{}tag` - the `{` is immediately after the tag
                    let tag_end = span.end_usize();
                    if tag_looks_legitimate && let Some((next_tok, next_span)) = self.peek() {
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

                    self.skip_ws();
                }

                Some((Token::Comment(_), _)) if has_props => {
                    // Skip comment - the line boundary after it will be handled next iteration
                    self.advance();
                }

                Some((Token::LineStart(indent), _)) if has_props => {
                    // Extract data we need before any mutations
                    let next_indent = *indent;
                    let should_continue = self.should_continue_collecting_properties();

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
                        crossed_property_line_boundary = true;
                        break;
                    }
                    if should_continue {
                        crossed_property_line_boundary = true;
                        self.advance(); // consume LineStart
                        // Skip Indent token if present after LineStart
                        if matches!(self.peek(), Some((Token::Indent(_), _))) {
                            self.advance();
                        }
                    } else {
                        // Stop - record that we saw a line boundary
                        crossed_property_line_boundary = true;
                        break;
                    }
                }

                _ => break,
            }
        }
        (properties, crossed_property_line_boundary)
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
                // Property-only line - continue collecting
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

        let tag_str = tag_cow.as_ref();

        // Verbatim tag: marked with leading '\0' by lexer - return as-is (without marker)
        if let Some(verbatim) = tag_str.strip_prefix('\0') {
            return Cow::Owned(String::from(verbatim));
        }

        // Now tags include the '!' prefix from the lexer
        // Empty tag (just `!`) - non-specific tag
        if tag_str == "!" {
            // Can return the borrowed tag directly!
            return tag_cow;
        }

        // Secondary handle: !!type
        if let Some(suffix) = tag_str.strip_prefix("!!") {
            if let Some(prefix) = self.tag_handles.get("!!") {
                let decoded_suffix = percent_decode(suffix).unwrap_or_else(|| suffix.to_owned());
                return Cow::Owned(format!("{prefix}{decoded_suffix}"));
            }
            // Default secondary handle if not in tag_handles
            let decoded_suffix = percent_decode(suffix).unwrap_or_else(|| suffix.to_owned());
            return Cow::Owned(format!("tag:yaml.org,2002:{decoded_suffix}"));
        }

        // Named handle: !name!suffix
        #[allow(
            clippy::string_slice,
            reason = "Slicing at positions returned by find('!'), which are guaranteed UTF-8 boundaries"
        )]
        if let Some(first_bang) = tag_str.find('!')
            && let Some(second_bang) = tag_str[first_bang + 1..].find('!')
        {
            let second_bang_pos = first_bang + 1 + second_bang;
            let handle = &tag_str[0..=second_bang_pos]; // e.g., "!yaml!"
            let suffix = &tag_str[second_bang_pos + 1..];
            // Look up handle using as_str() since HashMap keys are &str
            if let Some(prefix) = self.tag_handles.get(handle) {
                let decoded_suffix = percent_decode(suffix).unwrap_or_else(|| suffix.to_owned());
                return Cow::Owned(format!("{prefix}{decoded_suffix}"));
            }
            // Handle not declared - emit error and return unexpanded
            self.error(ErrorKind::UndefinedTagHandle, span);
            return tag_cow; // Return as-is
        }

        // Primary handle: !type
        if let Some(suffix) = tag_str.strip_prefix('!') {
            // Check if primary handle `!` has been redefined via %TAG ! prefix
            if let Some(prefix) = self.tag_handles.get("!") {
                let decoded_suffix = percent_decode(suffix).unwrap_or_else(|| suffix.to_owned());
                return Cow::Owned(format!("{prefix}{decoded_suffix}"));
            }
            // No redefinition - if no percent-encoding, return as-is (zero-copy!)
            if percent_decode(suffix).is_none() {
                return tag_cow;
            }
            // Has percent-encoding - need to decode
            let decoded_suffix = percent_decode(suffix).unwrap();
            return Cow::Owned(format!("!{decoded_suffix}"));
        }

        // Shouldn't reach here, but return as-is
        tag_cow
    }

    // ─────────────────────────────────────────────────────────────
    // Block Sequence
    // ─────────────────────────────────────────────────────────────

    fn process_block_seq(
        &mut self,
        indent: IndentLevel,
        phase: BlockSeqPhase,
        start_span: Span,
        properties: Properties<'input>,
    ) -> Option<Event<'input>> {
        match phase {
            BlockSeqPhase::EmitStart => {
                // Push indent level onto stack for orphan indent detection
                self.push_indent(indent);
                // Push state for first entry (properties already consumed by SequenceStart)
                self.state_stack.push(ParseState::BlockSeq {
                    indent,
                    phase: BlockSeqPhase::BeforeEntry,
                    start_span,
                    properties: Properties::default(),
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Block,
                    properties,
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
                        // Check for tabs after block sequence indicator (before consuming whitespace)
                        self.check_tabs_after_block_indicator();
                        self.skip_ws();

                        // Push AfterEntry, then Value
                        self.state_stack.push(ParseState::BlockSeq {
                            indent,
                            phase: BlockSeqPhase::AfterEntry,
                            start_span,
                            properties: Properties::default(),
                        });
                        self.state_stack.push(ParseState::Value {
                            ctx: ValueContext {
                                min_indent: entry_indent + 1,
                                kind: ValueKind::SeqEntryValue,
                                allow_implicit_mapping: true, // Sequence entries allow implicit mappings
                            },
                            properties: Properties::default(),
                        });
                        None // Continue processing
                    }

                    Some((Token::DocEnd | Token::DocStart, _)) | None => {
                        // End of sequence
                        // Only check trailing content for root-level sequences (not nested)
                        if self.is_root_level_sequence(indent) {
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
                            if self.is_root_level_sequence(indent) {
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
                                properties: Properties::default(),
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
                            if self.is_root_level_sequence(indent) {
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
                                properties: Properties::default(),
                            });
                            None
                        }
                    }

                    _ => {
                        // No more entries - check for trailing content at seq_indent level
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
                                    _ => false,
                                };
                                if is_unexpected_content {
                                    self.error(ErrorKind::TrailingContent, span);
                                }
                            }
                        }
                        // Root-level check
                        if self.is_root_level_sequence(indent) {
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
                    properties: Properties::default(),
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
        properties: Properties<'input>,
    ) -> Option<Event<'input>> {
        match phase {
            BlockMapPhase::EmitStart => {
                // Push indent level onto stack for orphan indent detection
                self.push_indent(indent);
                // Reset crossed_line_boundary when starting a new mapping.
                // We only want to track line crossings WITHIN this mapping,
                // not from earlier processing (e.g., the parent sequence or mapping).
                self.crossed_line_boundary = false;
                self.state_stack.push(ParseState::BlockMap {
                    indent,
                    phase: BlockMapPhase::BeforeKey {
                        require_line_boundary: false, // First key doesn't require line boundary
                    },
                    start_span,
                    properties: Properties::default(),
                });
                Some(Event::MappingStart {
                    style: crate::event::CollectionStyle::Block,
                    properties,
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
                // orphan content.
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
                // Only check this if we crossed a line boundary - for compact mappings
                // on the same line as a parent indicator, current_indent may be less than
                // the mapping indent, but that's expected.
                // We check BOTH crossed_line (from skip_ws_and_newlines_for_mapping) AND
                // self.crossed_line_boundary (which may have been set earlier, e.g., in AfterKey
                // when looking for a colon after an explicit key).
                if (crossed_line || self.crossed_line_boundary) && self.current_indent < indent {
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
                        // Check for tabs after mapping key indicator (before consuming whitespace)
                        self.check_tabs_after_block_indicator();
                        self.skip_ws();
                        // Push AfterKey, then Value for key
                        self.state_stack.push(ParseState::BlockMap {
                            indent,
                            phase: BlockMapPhase::AfterKey {
                                is_implicit_scalar_key: false, // Explicit key (`?`)
                            },
                            start_span,
                            properties: Properties::default(),
                        });
                        // Use kind: MappingValue because the VALUE after an explicit `?`
                        // can be any node, including a mapping.
                        self.state_stack.push(ParseState::Value {
                            ctx: ValueContext {
                                min_indent: indent + 1,
                                kind: ValueKind::MappingValue,
                                allow_implicit_mapping: true, // Explicit key value on new line
                            },
                            properties: Properties::default(),
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
                            properties: Properties::default(),
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
                                properties: Properties::default(),
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
                                properties: Properties::default(),
                            });
                            None
                        }
                    }

                    // Standalone comment at the mapping's indent should not end the
                    // mapping. Treat it as whitespace: consume it and stay in
                    // BeforeKey so that subsequent lines (including the next real
                    // key) are still part of this mapping.
                    Some((Token::Comment(_), _)) => {
                        self.advance();
                        self.state_stack.push(ParseState::BlockMap {
                            indent,
                            phase: BlockMapPhase::BeforeKey {
                                // We've already crossed a line boundary to reach
                                // this comment, so we no longer require an
                                // additional line boundary for the next entry.
                                require_line_boundary: false,
                            },
                            start_span,
                            properties: Properties::default(),
                        });
                        None
                    }

                    _ => {
                        // Check for orphan indentation before trying to parse content.
                        // If current_indent is greater than mapping indent but not a valid
                        // level in the indent stack, we need to end this mapping.
                        //
                        // IMPORTANT: We should NOT report InvalidIndentation here. That error
                        // is only emitted while actively iterating through LineStart tokens at
                        // this block level. If the LineStart was already consumed by a nested
                        // structure (like a sequence), we don't report the error from the
                        // outer mapping.
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
                                properties: Properties::default(),
                            });
                            self.state_stack.push(ParseState::Value {
                                ctx: ValueContext {
                                    min_indent: indent,
                                    kind: ValueKind::Key, // This is a mapping key
                                    allow_implicit_mapping: true, // Keys can be implicit mappings if on new line
                                },
                                properties: Properties::default(),
                            });
                            None
                        } else {
                            // Check if there's a scalar at THIS MAPPING'S indent level
                            // that looks like a key but is missing a colon.
                            // Only report MissingColon if we're at the mapping's indent,
                            // not for scalars that are indented past this level.
                            if self.current_indent == indent
                                && let Some(scalar_event) =
                                    self.check_missing_colon_in_mapping_with_event()
                            {
                                // Emit the scalar as an orphaned key, then end mapping
                                // Push MappingEnd onto stack so it's emitted after scalar
                                self.state_stack.push(ParseState::EmitMapEnd {
                                    span: self.collection_end_span(),
                                });
                                return Some(scalar_event);
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

                // Skip Dedent tokens that may appear between an explicit key and its colon.
                // For example: `? a\n  true\n: null` - after parsing the multiline key "a true",
                // we encounter a Dedent before the colon. We need to skip it to find the colon.
                // This is safe because we're looking for a colon at the SAME indent level as
                // the mapping, so any Dedent tokens here are just returning to that level.
                while matches!(self.peek(), Some((Token::Dedent, _))) {
                    self.advance();
                }

                // Expect colon
                if matches!(self.peek(), Some((Token::Colon, _))) {
                    self.advance();
                    // Check for tabs after block mapping indicator (colon, before consuming whitespace)
                    self.check_tabs_after_block_indicator();
                    self.skip_ws();

                    // Check for block sequence indicator on same line as key - invalid in YAML
                    // Block sequences must start on a new line after the colon.
                    // This only applies to implicit scalar keys (like `key: - item`).
                    // For explicit keys (`? key : - item`) or null keys (`: - item`), this is valid.
                    if is_implicit_scalar_key
                        && let Some((Token::BlockSeqIndicator, span)) = self.peek()
                    {
                        self.error(ErrorKind::ContentOnSameLine, span);
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
                        properties: Properties::default(),
                    });
                    self.state_stack.push(ParseState::Value {
                        ctx: ValueContext {
                            min_indent: indent + 1,
                            kind: ValueKind::MappingValue,
                            // CRITICAL: For implicit scalar keys, don't allow nested implicit mappings
                            // on the same line. `a: b: c` should NOT create nested mappings - need a line break.
                            // But for explicit keys (`? key : value`), allow implicit mappings even on
                            // the same line, because the explicit indicators make the structure unambiguous.
                            // The allow_implicit_mapping will become true if parse_value crosses a line.
                            allow_implicit_mapping: !is_implicit_scalar_key,
                        },
                        properties: Properties::default(),
                    });
                    None
                } else {
                    // No colon, emit null value - use LineStart span to match batch behavior
                    self.state_stack.push(ParseState::BlockMap {
                        indent,
                        phase: BlockMapPhase::AfterValue,
                        start_span,
                        properties: Properties::default(),
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
                        properties: Properties::default(),
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
                    properties: Properties::default(),
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
                        Token::LineStart(_)
                        | Token::DocEnd
                        | Token::DocStart
                        | Token::FlowSeqStart
                        | Token::FlowMapStart => return false,
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
    fn check_missing_colon_in_mapping_with_event(&mut self) -> Option<Event<'input>> {
        // Check if current token is a scalar-like token at this position
        match self.peek() {
            Some((Token::Plain(text), span)) => {
                // Plain scalar without a colon following - this is a MissingColon
                let value: Cow<'input, str> = text.clone(); // Clone the Cow (cheap if Borrowed)

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
                    properties: Properties::default(),
                    span,
                })
            }
            Some((Token::StringStart(_), span)) => {
                // Quoted string without a colon following - this is a MissingColon
                self.error(ErrorKind::MissingColon, span);
                // Parse the quoted string to get its value
                let (value, full_span) = self.parse_quoted_string_content();
                Some(Event::Scalar {
                    style: ScalarStyle::DoubleQuoted, // or SingleQuoted - doesn't matter for error recovery
                    value,
                    properties: Properties::default(),
                    span: full_span.unwrap_or(span),
                })
            }
            Some((Token::Alias(name), span)) => {
                // Alias without a colon following - this is a MissingColon
                let alias_name = Cow::Borrowed(*name); // Borrow directly from input
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
    fn parse_quoted_string_content(&mut self) -> (Cow<'input, str>, Option<Span>) {
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
    /// Uses `last_line_start_span` so that spans are consistent across nested blocks.
    fn report_invalid_indent(&mut self) {
        self.error(ErrorKind::InvalidIndentation, self.last_line_start_span);
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
    /// Reports `InvalidIndentation` errors for over-indented content at orphan levels.
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
    fn skip_ws_and_newlines_for_mapping(&mut self, mapping_indent: IndentLevel) -> bool {
        let mut crossed_line = false;
        let mut found_matching_line_start = false;

        loop {
            match self.peek() {
                Some((Token::LineStart(n), span)) => {
                    let n = *n;

                    if n < mapping_indent {
                        // Potentially dedented below mapping. However, blank lines inside a
                        // mapping (lines with no content before the next LineStart/Doc marker)
                        // are allowed and MUST NOT end the mapping or trigger
                        // InvalidIndentation.
                        let mut i = 1;
                        let mut is_blank_line = false;
                        loop {
                            match self.peek_nth(i) {
                                // Skip structural spacing tokens when looking ahead
                                Some((
                                    Token::Indent(_)
                                    | Token::Dedent
                                    | Token::Whitespace
                                    | Token::WhitespaceWithTabs,
                                    _,
                                )) => {
                                    i += 1;
                                }
                                // Comment-only line at a lower indent is treated as real
                                // content outside the mapping; let the existing
                                // dedent-handling logic below deal with it.
                                Some((Token::Comment(_), _)) => {
                                    break;
                                }
                                // Next token is another LineStart, a document marker, or EOF
                                // with no intervening content: this line is effectively blank.
                                Some((
                                    Token::LineStart(_) | Token::DocStart | Token::DocEnd,
                                    _,
                                ))
                                | None => {
                                    is_blank_line = true;
                                    break;
                                }
                                // Any other token means there is content at this lower indent;
                                // fall through to the original dedent handling below.
                                Some(_) => {
                                    break;
                                }
                            }
                        }

                        if is_blank_line {
                            // Consume the LineStart and continue scanning without ending
                            // the mapping. Do not update current_indent / last_line_start_span:
                            // they should continue to refer to the last meaningful line in
                            // this mapping.
                            crossed_line = true;
                            self.advance();
                            continue;
                        }

                        // Dedented below mapping with actual content - don't consume, let
                        // caller handle. Update current_indent so caller knows the level.
                        crossed_line = true; // We DID cross a line, even though we're breaking
                        self.current_indent = n;
                        self.last_line_start_span = span;

                        // Check for orphan indentation BEFORE breaking.
                        // When `n < target_indent` and `n` is not a valid indent level we
                        // still need to report `InvalidIndentation`, even if the caller
                        // exits early (e.g., because `require_line_boundary` is set).
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
                Some((
                    Token::Whitespace
                    | Token::WhitespaceWithTabs
                    | Token::Indent(_)
                    | Token::Dedent,
                    _,
                )) => self.advance(),
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

    /// Check if the current Plain token is followed by a Colon (making it a mapping key).
    ///
    /// This is used to stop plain scalar continuation when the next line
    /// looks like a mapping key (`key: value`).
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
                    if let Some((Token::LineStart(_), _)) = self.peek_nth(i)
                        && let Some((Token::Plain(_), _)) = self.peek_nth(i + 1)
                    {
                        // This is a continuation line
                        i += 2; // Skip LineStart + Plain
                        continue;
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
                Token::Anchor(_)
                | Token::Tag(_)
                | Token::Whitespace
                | Token::WhitespaceWithTabs
                | Token::LineStart(_) => i += 1,
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

    /// Handle `---` / `...` inside a flow sequence phase.
    ///
    /// This is a tiny helper used by the individual `FlowSeqPhase` arms so the
    /// error and recovery behaviour for `DocumentMarkerInFlow` is centralized:
    /// - Record the error
    /// - Consume the marker token
    /// - Re-push the current `FlowSeq` state to continue parsing.
    fn handle_doc_marker_in_flow_seq(
        &mut self,
        start_span: Span,
        doc_span: Span,
        phase: FlowSeqPhase,
    ) -> Option<Event<'input>> {
        self.error(ErrorKind::DocumentMarkerInFlow, doc_span);
        self.advance();
        self.state_stack
            .push(ParseState::FlowSeq { phase, start_span });
        None
    }

    fn process_flow_seq(&mut self, phase: FlowSeqPhase, start_span: Span) -> Option<Event<'input>> {
        match phase {
            FlowSeqPhase::EmitStart => {
                self.state_stack.push(ParseState::FlowSeq {
                    phase: FlowSeqPhase::BeforeEntry,
                    start_span,
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Flow,
                    properties: Properties::default(),
                    span: start_span,
                })
            }

            FlowSeqPhase::BeforeEntry => {
                self.skip_ws_and_newlines();

                match self.peek() {
                    Some((Token::FlowSeqEnd, span)) => {
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
                        // Document markers inside flow context are invalid
                        // (already reported by lexer as DocumentMarkerInFlow).
                        // Use a tiny helper to keep error+recovery behaviour
                        // consistent across `FlowSeq` phases.
                        self.handle_doc_marker_in_flow_seq(
                            start_span,
                            span,
                            FlowSeqPhase::BeforeEntry,
                        )
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
                            phase: FlowSeqPhase::ImplicitMapEnd,
                            start_span,
                        });
                        // 3. Parse the value (after we see colon)
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::ImplicitMapValue { map_start_span },
                            start_span,
                        });
                        // 4. Parse the key
                        self.state_stack.push(ParseState::Value {
                            ctx: ValueContext {
                                min_indent: 0,
                                kind: ValueKind::Key,
                                allow_implicit_mapping: true, // Flow context - doesn't affect block mappings
                            },
                            properties: Properties::default(),
                        });

                        // Emit MappingStart for the explicit mapping (flow style)
                        Some(Event::MappingStart {
                            style: crate::event::CollectionStyle::Flow,
                            properties: Properties::default(),
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
                                phase: FlowSeqPhase::ImplicitMapEnd,
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
                                    ctx: ValueContext {
                                        min_indent: 0,
                                        kind: ValueKind::Key, // This is the key of the implicit mapping
                                        allow_implicit_mapping: true, // Flow context
                                    },
                                    properties: Properties::default(),
                                });
                            }

                            // Emit MappingStart for the implicit mapping
                            Some(Event::MappingStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: Properties::default(),
                                span: map_start_span,
                            })
                        } else {
                            // Regular entry - parse as value
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::AfterEntry,
                                start_span,
                            });
                            self.state_stack.push(ParseState::Value {
                                ctx: ValueContext {
                                    min_indent: 0,
                                    kind: ValueKind::SeqEntryValue, // Sequence entry is a value
                                    allow_implicit_mapping: true,   // Flow context
                                },
                                properties: Properties::default(),
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
                    properties: Properties::default(),
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
                            ctx: ValueContext {
                                min_indent: 0,
                                kind: ValueKind::MappingValue,
                                allow_implicit_mapping: true, // Flow context
                            },
                            properties: Properties::default(),
                        });
                        None
                    }
                } else {
                    // No colon - this shouldn't happen if is_implicit_flow_mapping_entry worked
                    // Emit null as value
                    Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: Properties::default(),
                        span: map_start_span,
                    })
                }
            }

            FlowSeqPhase::ImplicitMapEnd => {
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
                        self.handle_doc_marker_in_flow_seq(
                            start_span,
                            span,
                            FlowSeqPhase::AfterEntry,
                        )
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
        self.advance();
        self.state_stack
            .push(ParseState::FlowMap { phase, start_span });
        None
    }

    fn process_flow_map(&mut self, phase: FlowMapPhase, start_span: Span) -> Option<Event<'input>> {
        match phase {
            FlowMapPhase::EmitStart => {
                self.state_stack.push(ParseState::FlowMap {
                    phase: FlowMapPhase::BeforeKey,
                    start_span,
                });
                Some(Event::MappingStart {
                    style: crate::event::CollectionStyle::Flow,
                    properties: Properties::default(),
                    span: start_span,
                })
            }

            FlowMapPhase::BeforeKey => {
                self.skip_ws_and_newlines();

                match self.peek() {
                    Some((Token::FlowMapEnd, span)) => {
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
                            ctx: ValueContext {
                                min_indent: 0,
                                kind: ValueKind::Key,         // Flow mapping key
                                allow_implicit_mapping: true, // Flow context
                            },
                            properties: Properties::default(),
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
                        // Ignore them and continue parsing via a tiny helper.
                        self.handle_doc_marker_in_flow_map(
                            start_span,
                            span,
                            FlowMapPhase::BeforeKey,
                        )
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
                            ctx: ValueContext {
                                min_indent: 0,
                                kind: ValueKind::Key,         // Flow mapping key
                                allow_implicit_mapping: true, // Flow context
                            },
                            properties: Properties::default(),
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
                            ctx: ValueContext {
                                min_indent: 0,
                                kind: ValueKind::MappingValue, // Flow mapping value
                                allow_implicit_mapping: true,  // Flow context
                            },
                            properties: Properties::default(),
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
                        self.handle_doc_marker_in_flow_map(
                            start_span,
                            span,
                            FlowMapPhase::AfterValue,
                        )
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
    // Scalar Parsing
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
        properties: Properties<'input>,
        is_key: bool,
        crossed_line_boundary: bool,
        allow_implicit_mapping: bool,
    ) -> Event<'input> {
        // Check if this is an implicit mapping key (scalar followed by colon)
        // Skip this check if:
        // - we're already parsing a key (avoid infinite recursion)
        // - we're in flow context (different rules apply)
        // - nested implicit mappings aren't allowed (same line as parent colon)
        if !is_key && self.flow_depth == 0 && allow_implicit_mapping && self.is_implicit_key() {
            // This is a block mapping with an implicit key
            let span = self.current_span();

            // Determine where properties belong based on crossed_line_boundary:
            // - If crossed_line_boundary: properties belong to MAPPING, key has none
            // - Otherwise: properties belong to KEY (same line), mapping has none
            let (map_props, key_props) = if crossed_line_boundary {
                // Properties crossed a line boundary, so they belong to the mapping
                (properties, Properties::default())
            } else {
                // Properties on same line as key, so they belong to the key
                // Also collect any additional properties before the key itself
                let (key_props, _crossed) = self.collect_properties(properties);
                (Properties::default(), key_props)
            };
            self.skip_ws();

            // Parse the key scalar with its properties
            let key_event = self.parse_plain_scalar(key_props.clone(), min_indent);

            // Determine mapping indent based on context:
            // - If there are properties (anchor or tag), use current_indent because
            //   the mapping's indent is the line's indent (e.g., `&a a: b` at root = indent 0)
            // - Otherwise (no properties, e.g., `- key: value`), use key's column position
            //   because that's where continuation lines need to align
            let has_properties = !map_props.is_empty()
                || matches!(
                    key_event,
                    Event::Scalar {
                        properties: Properties {
                            anchor: Some(_),
                            ..
                        },
                        ..
                    }
                )
                || matches!(
                    key_event,
                    Event::Scalar {
                        properties: Properties { tag: Some(_), .. },
                        ..
                    }
                );
            let map_indent = if crossed_line_boundary || has_properties {
                self.current_indent
            } else if let Event::Scalar { span: k_span, .. } = key_event {
                self.column_of_position(k_span.start_usize())
            } else {
                self.current_indent
            };

            let mapping_start =
                self.build_block_mapping_from_scalar_key(map_indent, span, map_props, key_event);
            return mapping_start;
        }

        // Not a mapping key, just parse as a scalar
        let result = self.parse_plain_scalar(properties, min_indent);
        // If this is a mapping key, check for multiline implicit key error
        if is_key && let Event::Scalar { span, .. } = &result {
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
        properties: Properties<'input>,
        min_indent: IndentLevel,
    ) -> Event<'input> {
        match self.peek() {
            Some((Token::Plain(text), span)) => {
                // Extract values before releasing the borrow
                let starts_with_percent = text.starts_with('%');
                let col = self.column_of_position(span.start_usize());
                let first_line = text.clone(); // Clone the Cow (cheap if Borrowed)
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
                let mut has_continuation = false;

                // Build content string - only allocate if we have continuations
                let mut content: Option<String> = None;

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
                                // We have a continuation - need to allocate
                                has_continuation = true;
                                let content_str =
                                    content.get_or_insert_with(|| first_line.clone().into_owned());
                                content_str.push(' ');
                                content_str.push_str(continuation);
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
                    while let Some((Token::LineStart(n), line_span)) = self.peek() {
                        let indent = *n;

                        // Check if this continues the scalar
                        if indent >= min_indent {
                            // Normal continuation - properly indented
                            self.advance();
                            consecutive_newlines += 1;

                            // Skip whitespace/indent tokens
                            self.skip_indent_tokens();

                            // Check for continuation content
                            // Allocate content string on first continuation
                            let content_str =
                                content.get_or_insert_with(|| first_line.clone().into_owned());

                            if self.try_consume_plain_continuation(
                                content_str,
                                &mut end_span,
                                &mut consecutive_newlines,
                                min_indent,
                            ) {
                                has_continuation = true;
                            } else {
                                // Not a continuation. Check for orphan indentation.
                                // We report `InvalidIndentation` when encountering an
                                // indent level not in the stack, even if the indent is
                                // >= min_indent. This handles cases like EW3V where a line
                                // at indent 1 is seen while the stack is [0, 0].
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
                }

                // Combine spans
                let final_span =
                    Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

                // Use the content string if we had continuations, otherwise use first_line (zero-copy!)
                let value = if has_continuation {
                    Cow::Owned(content.unwrap_or_else(|| first_line.into_owned()))
                } else {
                    first_line
                };

                Event::Scalar {
                    style: ScalarStyle::Plain,
                    value,
                    properties,
                    span: final_span,
                }
            }

            Some((Token::StringStart(quote_style), _)) => {
                self.parse_quoted_scalar(properties, *quote_style, min_indent)
            }

            _ => self.emit_null(),
        }
    }

    /// Parse a quoted scalar.
    /// `min_indent` is used to validate that continuation lines have proper indentation.
    fn parse_quoted_scalar(
        &mut self,
        properties: Properties<'input>,
        quote_style: crate::lexer::QuoteStyle,
        min_indent: IndentLevel,
    ) -> Event<'input> {
        let start_span = self.current_span();
        self.advance(); // consume StringStart

        // Try to optimize for single-content-token case (most common)
        // Check if we have: StringContent followed immediately by StringEnd
        let (single_token_value, first_content) =
            if let Some((Token::StringContent(content), content_span)) = self.peek() {
                let content_cow = content.clone();
                self.advance();

                // Check if next is StringEnd
                if matches!(self.peek(), Some((Token::StringEnd(_), _))) {
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
            let end_span = if let Some((Token::StringEnd(_), span)) = self.peek() {
                self.advance();
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
                properties,
                span: full_span,
            };
        }

        // Slow path: multiple tokens, collect parts to minimize allocations
        let mut parts: Vec<Cow<'input, str>> = Vec::new();

        // If we already consumed the first StringContent in the fast-path check, add it now
        if let Some(first) = first_content {
            parts.push(first);
        }
        let mut end_span = start_span;
        let mut pending_newlines: usize = 0;
        let mut needs_trim = false;

        loop {
            match self.peek() {
                Some((Token::StringContent(content), span)) => {
                    // If we need to trim, do it BEFORE applying pending newlines
                    // This trims trailing spaces from the previous line
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

                    // Apply pending newlines before content (after trimming!)
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

                    // Mark that we need to trim trailing spaces before the next content
                    needs_trim = true;
                    pending_newlines += 1;
                    end_span = line_start_span;
                    self.advance();
                }
                Some((Token::StringEnd(_), span)) => {
                    // If we need to trim, do it BEFORE applying pending newlines at end
                    // This handles trailing spaces on the last line before StringEnd
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
                    // needs_trim is not read after this point, so no need to set to false

                    // Apply pending newlines at end (after trimming!)
                    if pending_newlines == 1 {
                        parts.push(Cow::Borrowed(" "));
                    } else if pending_newlines > 1 {
                        for _ in 1..pending_newlines {
                            parts.push(Cow::Borrowed("\n"));
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

        // Join parts into final string
        let value: String = parts.iter().map(std::convert::AsRef::as_ref).collect();

        let style = match quote_style {
            crate::lexer::QuoteStyle::Single => ScalarStyle::SingleQuoted,
            crate::lexer::QuoteStyle::Double => ScalarStyle::DoubleQuoted,
        };

        let full_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

        Event::Scalar {
            style,
            value: Cow::Owned(value),
            properties,
            span: full_span,
        }
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
        properties: Properties<'input>,
    ) -> Event<'input> {
        let (header, start_span) = match self.peek() {
            Some((Token::LiteralBlockHeader(hdr) | Token::FoldedBlockHeader(hdr), span)) => {
                (hdr.clone(), span)
            }
            _ => return self.emit_null(),
        };

        let is_literal = matches!(self.peek(), Some((Token::LiteralBlockHeader(_), _)));
        self.advance(); // consume header
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
        while matches!(
            self.peek(),
            Some((Token::Indent(_) | Token::Dedent | Token::Comment(_), _))
        ) {
            self.advance();
        }

        // Expect LineStart to begin each line
        while let Some((Token::LineStart(indent_level), ls_span)) = self.peek() {
            let n = usize::from(*indent_level);
            let line_start_span = ls_span;
            self.advance();

            // Skip Indent/Dedent tokens
            while matches!(self.peek(), Some((Token::Indent(_) | Token::Dedent, _))) {
                self.advance();
            }

            // Check for tabs used as indentation in block scalar content.
            // Per YAML spec, tabs are allowed as content but not as indentation.
            // Tabs are invalid if they appear BEFORE reaching the content indent level.
            // For example, in `foo: |\n\t\tbar`, if content_indent is 2, the tabs at column 0
            // are indentation (invalid). But in `foo: |\n  \tbar`, the tab at column 2 is
            // content (valid).
            if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek() {
                let tab_col = usize::from(self.column_of_position(tab_span.start_usize()));
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
                line_parts.push(Cow::Owned(" ".repeat(extra_indent)));
            }

            while let Some((tok, span)) = self.peek() {
                match tok {
                    Token::Plain(text) => {
                        line_parts.push(text.clone());
                        // `Plain` tokens always contain some non-whitespace content;
                        // the lexer emits dedicated whitespace tokens.
                        has_non_whitespace = true;
                        line_end_span = span;
                        self.advance();
                    }
                    Token::Whitespace => {
                        line_parts.push(Cow::Borrowed(" "));
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
                        self.advance();
                    }
                    Token::Comment(text) => {
                        // In block scalars, # is NOT a comment indicator - it's literal content
                        // The lexer incorrectly tokenizes it as a comment, so we need to
                        // reconstruct the original text including the #
                        line_parts.push(Cow::Owned(format!("#{text}")));
                        has_non_whitespace = true;
                        line_end_span = span;
                        self.advance();
                    }
                    // In block scalars, flow indicators are literal content, not structure.
                    // The lexer doesn't know we're in a block scalar context.
                    Token::FlowSeqStart => {
                        line_parts.push(Cow::Borrowed("["));
                        has_non_whitespace = true;
                        line_end_span = span;
                        self.advance();
                    }
                    Token::FlowSeqEnd => {
                        line_parts.push(Cow::Borrowed("]"));
                        has_non_whitespace = true;
                        line_end_span = span;
                        self.advance();
                    }
                    Token::FlowMapStart => {
                        line_parts.push(Cow::Borrowed("{"));
                        has_non_whitespace = true;
                        line_end_span = span;
                        self.advance();
                    }
                    Token::FlowMapEnd => {
                        line_parts.push(Cow::Borrowed("}"));
                        has_non_whitespace = true;
                        line_end_span = span;
                        self.advance();
                    }
                    Token::Colon => {
                        // Colon is literal content in block scalars
                        line_parts.push(Cow::Borrowed(":"));
                        has_non_whitespace = true;
                        line_end_span = span;
                        self.advance();
                    }
                    Token::Comma => {
                        // Comma is literal content in block scalars
                        line_parts.push(Cow::Borrowed(","));
                        has_non_whitespace = true;
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
            if !line_parts.is_empty()
                && let Some((Token::LineStart(_), next_span)) = self.peek()
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
        let chomped_value = Self::apply_chomping(&value, &header, is_empty_scalar);

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
            properties,
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
        header: &crate::lexer::BlockScalarHeader,
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
        use crate::event::{CollectionStyle, Event, Properties, Property, ScalarStyle};

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
            let (events, errors) = crate::emit_events(input);
            let events: Vec<Event<'static>> = events.into_iter().map(Event::into_owned).collect();

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
                    Event::Scalar {
                        properties: Properties {
                            anchor: Some(Property { value, .. }),
                            ..
                        },
                        ..
                    } if value == "anchor"
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
                    Event::Scalar {
                        properties: Properties {
                            tag: Some(Property { value, .. }),
                            ..
                        },
                        ..
                    } if value == "tag:yaml.org,2002:str"
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
