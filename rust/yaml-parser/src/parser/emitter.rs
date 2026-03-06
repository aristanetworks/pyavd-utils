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
use crate::span::{IndentLevel, Span};

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
    BeforeKey,
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
    },
    /// Block sequence parsing.
    BlockSeq {
        indent: IndentLevel,
        phase: BlockSeqPhase,
        start_span: Span,
    },
    /// Block mapping parsing.
    BlockMap {
        indent: IndentLevel,
        phase: BlockMapPhase,
        start_span: Span,
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
                // At EOF, return span at end of last token
                self.tokens
                    .last()
                    .map(|rt| Span::from_usize_range(rt.span.end_usize()..rt.span.end_usize()))
                    .unwrap_or_else(|| Span::from_usize_range(0..0))
            })
    }

    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError {
            kind,
            span,
            span_offset: 0,
        });
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
        loop {
            match self.peek() {
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => self.advance(),
                Some((Token::LineStart(_), _)) => self.advance(),
                Some((Token::Indent(_), _)) => self.advance(),
                Some((Token::Comment(_), _)) => self.advance(),
                Some((Token::Dedent, _)) => self.advance(),
                _ => break,
            }
        }
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
                    });
                    self.doc_state = DocState::Content;
                    return Some(event);
                }

                DocState::Content => {
                    // Process state stack
                    if let Some(event) = self.process_state_stack() {
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
                } => {
                    if let Some(event) = self.parse_value(min_indent, anchor, tag) {
                        return Some(event);
                    }
                    // No value produced, continue with next state
                }

                ParseState::BlockSeq {
                    indent,
                    phase,
                    start_span,
                } => {
                    if let Some(event) = self.process_block_seq(indent, phase, start_span) {
                        return Some(event);
                    }
                }

                ParseState::BlockMap {
                    indent,
                    phase,
                    start_span,
                } => {
                    if let Some(event) = self.process_block_map(indent, phase, start_span) {
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
    #[allow(clippy::too_many_lines, reason = "Complex value dispatch logic")]
    fn parse_value(
        &mut self,
        min_indent: IndentLevel,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    ) -> Option<Event<'input>> {
        self.skip_ws_and_newlines();

        // Check for properties (anchor, tag) before the value
        let (anchor, tag) = self.collect_properties(anchor, tag);

        self.skip_ws_and_newlines();

        // Dispatch based on current token
        match self.peek() {
            None | Some((Token::DocEnd | Token::DocStart, _)) => {
                // Empty value / null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor,
                    tag,
                    span: self.current_span(),
                })
            }

            Some((Token::Alias(name), span)) => {
                let name = Cow::Borrowed(*name);
                let span = span;
                self.advance();
                // TODO: Check for alias as mapping key (colon follows)
                Some(Event::Alias { name, span })
            }

            Some((Token::BlockSeqIndicator, _)) => {
                let span = self.current_span();
                // Push block sequence state
                self.state_stack.push(ParseState::BlockSeq {
                    indent: self.current_indent,
                    phase: BlockSeqPhase::EmitStart,
                    start_span: span,
                });
                // Re-process to emit SequenceStart
                self.process_state_stack()
            }

            Some((Token::MappingKey | Token::Colon, _)) => {
                let span = self.current_span();
                // Push block mapping state
                self.state_stack.push(ParseState::BlockMap {
                    indent: self.current_indent,
                    phase: BlockMapPhase::EmitStart,
                    start_span: span,
                });
                self.process_state_stack()
            }

            Some((Token::FlowSeqStart, _)) => {
                let span = self.current_span();
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
                self.parse_block_scalar(anchor, tag)
            }

            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                // Could be scalar or start of block mapping
                self.parse_scalar_or_mapping(min_indent, anchor, tag)
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
    fn collect_properties(
        &mut self,
        mut anchor: Option<(Cow<'static, str>, Span)>,
        mut tag: Option<(Cow<'static, str>, Span)>,
    ) -> (
        Option<(Cow<'static, str>, Span)>,
        Option<(Cow<'static, str>, Span)>,
    ) {
        loop {
            match self.peek() {
                Some((Token::Anchor(name), span)) => {
                    let name_owned = Cow::Owned(String::from(*name));
                    // Register anchor
                    self.anchors.insert(name);
                    anchor = Some((name_owned, span));
                    self.advance();
                    self.skip_ws();
                }
                Some((Token::Tag(tag_str), span)) => {
                    // Expand tag handle
                    let expanded = self.expand_tag(tag_str);
                    tag = Some((Cow::Owned(expanded), span));
                    self.advance();
                    self.skip_ws();
                }
                _ => break,
            }
        }
        (anchor, tag)
    }

    /// Expand a tag handle to its full form.
    fn expand_tag(&self, tag_str: &str) -> String {
        // Handle verbatim tags
        if tag_str.starts_with("!<") && tag_str.ends_with('>') {
            return String::from(&tag_str[2..tag_str.len() - 1]);
        }

        // Handle shorthand tags
        if let Some(rest) = tag_str.strip_prefix("!!") {
            if let Some(prefix) = self.tag_handles.get("!!") {
                return format!("{prefix}{rest}");
            }
        }

        // Primary tag handle
        if let Some(rest) = tag_str.strip_prefix('!') {
            if rest.is_empty() {
                return String::from("!");
            }
            // Check for named handle (!name!)
            if let Some(end) = rest.find('!') {
                let handle = &tag_str[..end + 2]; // includes leading ! and trailing !
                let suffix = &rest[end + 1..];
                if let Some(prefix) = self.tag_handles.get(handle) {
                    return format!("{prefix}{suffix}");
                }
            }
            // Local tag
            return String::from(tag_str);
        }

        String::from(tag_str)
    }

    // ─────────────────────────────────────────────────────────────
    // Block Sequence
    // ─────────────────────────────────────────────────────────────

    fn process_block_seq(
        &mut self,
        indent: IndentLevel,
        phase: BlockSeqPhase,
        start_span: Span,
    ) -> Option<Event<'input>> {
        match phase {
            BlockSeqPhase::EmitStart => {
                // Push state for first entry
                self.state_stack.push(ParseState::BlockSeq {
                    indent,
                    phase: BlockSeqPhase::BeforeEntry,
                    start_span,
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Block,
                    anchor: None,
                    tag: None,
                    span: start_span,
                })
            }

            BlockSeqPhase::BeforeEntry => {
                self.skip_ws_and_newlines();

                // Check for `-` at the sequence indent
                match self.peek() {
                    Some((Token::BlockSeqIndicator, _)) => {
                        let entry_indent = self.current_indent;
                        if entry_indent < indent {
                            // Dedented, end sequence
                            return Some(Event::SequenceEnd {
                                span: self.current_span(),
                            });
                        }

                        self.advance(); // consume `-`
                        self.skip_ws();

                        // Push AfterEntry, then Value
                        self.state_stack.push(ParseState::BlockSeq {
                            indent,
                            phase: BlockSeqPhase::AfterEntry,
                            start_span,
                        });
                        self.state_stack.push(ParseState::Value {
                            min_indent: entry_indent + 1,
                            anchor: None,
                            tag: None,
                        });
                        None // Continue processing
                    }

                    Some((Token::DocEnd | Token::DocStart, _)) | None => {
                        // End of sequence
                        Some(Event::SequenceEnd {
                            span: self.current_span(),
                        })
                    }

                    Some((Token::LineStart(n), _)) => {
                        if *n < indent {
                            Some(Event::SequenceEnd {
                                span: self.current_span(),
                            })
                        } else {
                            self.advance();
                            // Re-push state to check again
                            self.state_stack.push(ParseState::BlockSeq {
                                indent,
                                phase: BlockSeqPhase::BeforeEntry,
                                start_span,
                            });
                            None
                        }
                    }

                    _ => {
                        // No more entries
                        Some(Event::SequenceEnd {
                            span: self.current_span(),
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
    ) -> Option<Event<'input>> {
        match phase {
            BlockMapPhase::EmitStart => {
                self.state_stack.push(ParseState::BlockMap {
                    indent,
                    phase: BlockMapPhase::BeforeKey,
                    start_span,
                });
                Some(Event::MappingStart {
                    style: crate::event::CollectionStyle::Block,
                    anchor: None,
                    tag: None,
                    span: start_span,
                })
            }

            BlockMapPhase::BeforeKey => {
                self.skip_ws_and_newlines();

                match self.peek() {
                    Some((Token::MappingKey, _)) => {
                        self.advance();
                        self.skip_ws();
                        // Push AfterKey, then Value for key
                        self.state_stack.push(ParseState::BlockMap {
                            indent,
                            phase: BlockMapPhase::AfterKey,
                            start_span,
                        });
                        self.state_stack.push(ParseState::Value {
                            min_indent: indent + 1,
                            anchor: None,
                            tag: None,
                        });
                        None
                    }

                    Some((Token::Colon, _)) => {
                        // Implicit null key
                        self.state_stack.push(ParseState::BlockMap {
                            indent,
                            phase: BlockMapPhase::AfterKey,
                            start_span,
                        });
                        Some(self.emit_null())
                    }

                    Some((Token::DocEnd | Token::DocStart, _)) | None => Some(Event::MappingEnd {
                        span: self.current_span(),
                    }),

                    Some((Token::LineStart(n), _)) => {
                        if *n < indent {
                            Some(Event::MappingEnd {
                                span: self.current_span(),
                            })
                        } else {
                            self.advance();
                            self.state_stack.push(ParseState::BlockMap {
                                indent,
                                phase: BlockMapPhase::BeforeKey,
                                start_span,
                            });
                            None
                        }
                    }

                    _ => {
                        // Check for implicit key (scalar followed by colon)
                        if self.is_implicit_key() {
                            self.state_stack.push(ParseState::BlockMap {
                                indent,
                                phase: BlockMapPhase::AfterKey,
                                start_span,
                            });
                            self.state_stack.push(ParseState::Value {
                                min_indent: indent,
                                anchor: None,
                                tag: None,
                            });
                            None
                        } else {
                            Some(Event::MappingEnd {
                                span: self.current_span(),
                            })
                        }
                    }
                }
            }

            BlockMapPhase::AfterKey => {
                self.skip_ws();

                // Expect colon
                if matches!(self.peek(), Some((Token::Colon, _))) {
                    self.advance();
                    self.skip_ws();

                    // Push AfterValue, then Value
                    self.state_stack.push(ParseState::BlockMap {
                        indent,
                        phase: BlockMapPhase::AfterValue,
                        start_span,
                    });
                    self.state_stack.push(ParseState::Value {
                        min_indent: indent + 1,
                        anchor: None,
                        tag: None,
                    });
                    None
                } else {
                    // No colon, emit null value
                    self.state_stack.push(ParseState::BlockMap {
                        indent,
                        phase: BlockMapPhase::AfterValue,
                        start_span,
                    });
                    Some(self.emit_null())
                }
            }

            BlockMapPhase::AfterValue => {
                // Continue with next key
                self.state_stack.push(ParseState::BlockMap {
                    indent,
                    phase: BlockMapPhase::BeforeKey,
                    start_span,
                });
                None
            }
        }
    }

    /// Check if we're at an implicit mapping key (value followed by colon).
    fn is_implicit_key(&self) -> bool {
        // Look ahead for colon
        let mut i = 0;
        while let Some((tok, _)) = self.peek_nth(i) {
            match tok {
                Token::Colon => return true,
                Token::LineStart(_) | Token::DocEnd | Token::DocStart => return false,
                Token::FlowSeqStart | Token::FlowMapStart => return false,
                _ => i += 1,
            }
            if i > 20 {
                break;
            }
        }
        false
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

                    None => {
                        // Unterminated
                        self.error(ErrorKind::UnexpectedEof, start_span);
                        self.flow_depth = self.flow_depth.saturating_sub(1);
                        Some(Event::SequenceEnd {
                            span: self.current_span(),
                        })
                    }

                    _ => {
                        // Parse entry value
                        self.state_stack.push(ParseState::FlowSeq {
                            phase: FlowSeqPhase::AfterEntry,
                            start_span,
                        });
                        self.state_stack.push(ParseState::Value {
                            min_indent: 0,
                            anchor: None,
                            tag: None,
                        });
                        None
                    }
                }
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
    fn parse_scalar_or_mapping(
        &mut self,
        _min_indent: IndentLevel,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    ) -> Option<Event<'input>> {
        // For now, just parse as a plain scalar
        // TODO: Implement mapping key detection
        self.parse_plain_scalar(anchor, tag)
    }

    /// Parse a plain scalar.
    fn parse_plain_scalar(
        &mut self,
        anchor: Option<(Cow<'static, str>, Span)>,
        tag: Option<(Cow<'static, str>, Span)>,
    ) -> Option<Event<'input>> {
        match self.peek() {
            Some((Token::Plain(text), span)) => {
                let value = Cow::Owned((*text).to_string());
                let span = span;
                self.advance();
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value,
                    anchor,
                    tag,
                    span,
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
    /// This is a simplified implementation - block scalars are complex.
    fn parse_block_scalar(
        &mut self,
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

        // For now, collect content using Plain tokens and LineStart
        // This is simplified - full implementation needs indent tracking
        let mut lines: Vec<String> = Vec::new();
        let mut current_line = String::new();
        let mut end_span = start_span;

        // Skip initial newline
        self.skip_ws_and_newlines();

        // Collect content
        while let Some((tok, span)) = self.peek() {
            match tok {
                Token::Plain(text) => {
                    current_line.push_str(text);
                    end_span = span;
                    self.advance();
                }
                Token::Whitespace | Token::WhitespaceWithTabs => {
                    current_line.push(' ');
                    self.advance();
                }
                Token::LineStart(_) => {
                    if !current_line.is_empty() || !lines.is_empty() {
                        lines.push(std::mem::take(&mut current_line));
                    }
                    end_span = span;
                    self.advance();
                }
                Token::Indent(_) | Token::Dedent => {
                    self.advance();
                }
                _ => break,
            }
        }

        // Push final line if any
        if !current_line.is_empty() {
            lines.push(current_line);
        }

        // Join lines based on style
        let value = if is_literal {
            lines.join("\n")
        } else {
            // Folded: single newlines become spaces
            lines.join(" ")
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

    /// Apply chomping indicator to block scalar value.
    fn apply_chomping(&self, value: &str, header: &crate::lexer::BlockScalarHeader) -> String {
        use crate::lexer::Chomping;

        let mut result = String::from(value);

        match header.chomping {
            Chomping::Strip => {
                // Remove all trailing newlines
                while result.ends_with('\n') {
                    result.pop();
                }
            }
            Chomping::Clip => {
                // Keep single trailing newline
                while result.ends_with("\n\n") {
                    result.pop();
                }
                if !result.ends_with('\n') && !result.is_empty() {
                    result.push('\n');
                }
            }
            Chomping::Keep => {
                // Keep all trailing newlines (no change)
            }
        }

        result
    }
}
