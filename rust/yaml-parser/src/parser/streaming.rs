// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Streaming parser that implements `Iterator<Item = Event>`.
//!
//! This module provides `StreamingParser`, which wraps the batch `Parser`
//! and exposes events through an iterator interface. This is Step 5 of the
//! streaming parser transformation (see `REFACTORING_STREAMING_PARSER.md`).
//!
//! Step 5: Document-level states are now handled by an explicit state machine.
//! `DocumentStart` and `DocumentEnd` events are emitted directly by `next()`.
//! Content parsing still delegates to the batch parser.

use std::collections::VecDeque;

use crate::error::ParseError;
use crate::event::Event;
use crate::lexer::{RichToken, Token};
use crate::span::Span;

use super::Parser;

/// State machine for document-level parsing.
///
/// This enum tracks where we are in the document lifecycle:
/// - `Ready`: Ready to start a new document (or emit `StreamEnd` if at EOF)
/// - `EmitDocStart`: About to emit `DocumentStart` event
/// - `ParseDocContent`: Parsing document content (dispatches to value states)
/// - `DrainBuffer`: Draining events from the buffer after content parsing
/// - `EmitDocEnd`: About to emit `DocumentEnd` event
/// - `Done`: Stream has ended
#[derive(Debug, Clone)]
enum StreamState {
    /// Ready to start processing the next document.
    Ready,
    /// About to emit `DocumentStart`.
    EmitDocStart { explicit: bool, span: Span },
    /// Parsing document content - dispatches to appropriate value parser.
    ParseDocContent { has_explicit_start: bool },
    /// Draining buffered events from content parsing.
    DrainBuffer,
    /// About to emit `DocumentEnd`.
    EmitDocEnd { explicit: bool, span: Span },
    /// Stream has ended.
    Done,
}

/// Value type dispatch for root-level content.
///
/// Step 6: This determines which parsing path to take based on the first token.
/// Each variant currently delegates to the batch parser's corresponding method.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueKind {
    /// Block sequence starting with `-`
    BlockSequence,
    /// Block mapping (starting with key or `?` or `:`)
    BlockMapping,
    /// Flow sequence `[...]`
    FlowSequence,
    /// Flow mapping `{...}`
    FlowMapping,
    /// Scalar (plain or quoted)
    Scalar,
    /// Block literal scalar `|`
    LiteralBlockScalar,
    /// Block folded scalar `>`
    FoldedBlockScalar,
    /// Alias `*name`
    Alias,
    /// Empty document (no content)
    Empty,
}

/// Parsing context stack entry.
///
/// Step 7: This tracks where we are in nested structures.
/// Currently used for observation only - actual parsing still delegates to batch parser.
/// Future steps will use this to drive state-machine parsing.
#[derive(Debug, Clone)]
#[allow(
    dead_code,
    reason = "Fields will be used in future state-machine steps"
)]
enum ParseContext {
    /// Root document level
    Document,
    /// Inside a block sequence at given indent level
    BlockSequence { indent: crate::span::IndentLevel },
    /// Inside a block mapping at given indent level
    BlockMapping { indent: crate::span::IndentLevel },
    /// Inside a flow sequence at given depth
    FlowSequence { depth: usize },
    /// Inside a flow mapping at given depth
    FlowMapping { depth: usize },
}

/// A streaming YAML parser that implements `Iterator<Item = Event>`.
///
/// This parser yields events one at a time, enabling incremental processing
/// of YAML documents without loading the entire event stream into memory.
///
/// # Current Implementation (Step 7)
///
/// Document boundaries (`DocumentStart`/`DocumentEnd`) are state-driven.
/// Value dispatch classifies by `ValueKind`. Context stack tracks nesting.
/// Content parsing still delegates to the batch parser.
///
/// # Future Implementation (Steps 8+)
///
/// Will progressively convert content parsing to true state-driven emission.
#[derive(Debug)]
pub struct StreamingParser<'tokens, 'input> {
    /// The underlying batch parser.
    parser: Parser<'tokens, 'input>,
    /// Buffer of events to drain (used during content parsing).
    buffer: VecDeque<Event<'input>>,
    /// Current state in the document lifecycle.
    state: StreamState,
    /// Whether we've emitted `StreamStart`.
    emitted_stream_start: bool,
    /// Stack tracking nested parsing contexts.
    /// Step 7: Currently used to track document context; will drive nested parsing in future.
    context_stack: Vec<ParseContext>,
}

impl<'tokens: 'input, 'input> StreamingParser<'tokens, 'input> {
    /// Create a new streaming parser from a token slice.
    #[must_use]
    pub fn new(tokens: &'tokens [RichToken<'input>], input: &'input str) -> Self {
        Self {
            parser: Parser::new(tokens, input),
            buffer: VecDeque::new(),
            state: StreamState::Ready,
            emitted_stream_start: false,
            context_stack: Vec::new(),
        }
    }

    /// Get any parse errors encountered so far.
    #[must_use]
    pub fn errors(&self) -> &[ParseError] {
        &self.parser.errors
    }

    /// Take the collected errors, leaving an empty vector.
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.parser.errors)
    }

    /// Skip whitespace, orphan doc-end markers, and directives.
    /// Returns `Some((explicit, span))` if a document should be started,
    /// or `None` if at EOF (or directive without document error).
    fn prepare_document(&mut self) -> Option<(bool, Span)> {
        // Skip initial whitespace/newlines
        self.parser.skip_ws_and_newlines();

        if self.parser.is_eof() {
            return None;
        }

        // Skip any orphan DocEnd markers
        while matches!(self.parser.peek(), Some((Token::DocEnd, _))) {
            self.parser.advance();
            self.parser.skip_ws_and_newlines();
        }

        if self.parser.is_eof() {
            return None;
        }

        // Populate tag handles for this document
        self.parser.populate_tag_handles_from_tokens();

        // Track directives for "directive without document" error
        let mut has_directive_in_prologue = false;
        let mut first_directive_span = Span::from_usize_range(0..0);

        // Skip directive tokens
        while let Some((tok, span)) = self.parser.peek() {
            match tok {
                Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_) => {
                    if !has_directive_in_prologue {
                        first_directive_span = span;
                    }
                    has_directive_in_prologue = true;
                    self.parser.advance();
                    self.parser.skip_ws_and_newlines();
                }
                _ => break,
            }
        }

        // Check for "directive without document" error
        if has_directive_in_prologue {
            let at_doc_end_or_eof =
                self.parser.is_eof() || matches!(self.parser.peek(), Some((Token::DocEnd, _)));
            if at_doc_end_or_eof {
                self.parser.error(
                    crate::error::ErrorKind::TrailingContent,
                    first_directive_span,
                );
                return None;
            }
        }

        if self.parser.is_eof() {
            return None;
        }

        // Check for explicit document start marker `---`
        let has_doc_start = matches!(self.parser.peek(), Some((Token::DocStart, _)));
        let doc_start_span = self.parser.current_span();

        if has_doc_start {
            self.parser.advance(); // consume DocStart

            // Skip whitespace after `---` (but NOT newlines yet)
            while matches!(
                self.parser.peek(),
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _))
            ) {
                self.parser.advance();
            }

            // Check if there's content on the same line as `---`
            let content_on_start_line = !self.parser.is_eof()
                && !matches!(
                    self.parser.peek(),
                    Some((Token::LineStart(_) | Token::DocEnd, _))
                );

            // Skip newlines after `---`
            self.parser.skip_ws_and_newlines();

            // Check for block mapping error on start line
            if content_on_start_line && !self.parser.is_eof() {
                let has_block_mapping_on_start_line =
                    self.parser.check_block_mapping_on_start_line();
                if has_block_mapping_on_start_line {
                    self.parser.error(
                        crate::error::ErrorKind::ContentOnSameLine,
                        self.parser.current_span(),
                    );
                }
            }
        }

        Some((has_doc_start, doc_start_span))
    }

    /// Determine the value kind based on the current token.
    ///
    /// Step 6: This is the dispatch logic that decides which parsing path to take.
    fn classify_value(&self) -> ValueKind {
        match self.parser.peek() {
            None | Some((Token::DocEnd | Token::DocStart, _)) => ValueKind::Empty,
            Some((Token::BlockSeqIndicator, _)) => ValueKind::BlockSequence,
            Some((Token::MappingKey | Token::Colon, _)) => ValueKind::BlockMapping,
            Some((Token::FlowSeqStart, _)) => ValueKind::FlowSequence,
            Some((Token::FlowMapStart, _)) => ValueKind::FlowMapping,
            Some((Token::LiteralBlockHeader(_), _)) => ValueKind::LiteralBlockScalar,
            Some((Token::FoldedBlockHeader(_), _)) => ValueKind::FoldedBlockScalar,
            Some((Token::Alias(_), _)) => ValueKind::Alias,
            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                // Could be a scalar or the start of a block mapping
                // The batch parser handles this detection internally
                ValueKind::Scalar
            }
            Some((Token::Anchor(_) | Token::Tag(_), _)) => {
                // Properties before a value - delegate to batch parser which handles this
                ValueKind::Scalar
            }
            Some((Token::LineStart(_) | Token::Indent(_) | Token::Comment(_), _)) => {
                // Skip these and re-classify - but for now delegate to batch parser
                ValueKind::Scalar
            }
            _ => ValueKind::Scalar, // Default to scalar for unknown tokens
        }
    }

    /// Dispatch to the appropriate parser based on value kind and parse content into buffer.
    ///
    /// Step 7: Tracks parsing context via `context_stack` while still delegating to batch parser.
    /// Future steps will use the context stack to drive state-machine parsing.
    fn dispatch_and_parse_content(&mut self, has_explicit_start: bool) {
        use crate::event::ScalarStyle;
        use std::borrow::Cow;

        let kind = self.classify_value();

        // Push document context
        self.context_stack.push(ParseContext::Document);

        match kind {
            ValueKind::Empty => {
                // Empty document after explicit ---
                if has_explicit_start {
                    let null_span = Span::from_usize_range(0..0);
                    self.buffer.push_back(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        anchor: None,
                        tag: None,
                        span: null_span,
                    });
                }
            }
            ValueKind::BlockSequence
            | ValueKind::BlockMapping
            | ValueKind::FlowSequence
            | ValueKind::FlowMapping
            | ValueKind::LiteralBlockScalar
            | ValueKind::FoldedBlockScalar
            | ValueKind::Alias
            | ValueKind::Scalar => {
                // All value kinds currently delegate to parse_value(0)
                // which handles the full dispatch internally
                let _ = self.parser.parse_value(0);

                // Move events from parser to our buffer
                for event in self.parser.take_events() {
                    self.buffer.push_back(event);
                }
            }
        }

        // Pop document context
        self.context_stack.pop();
    }

    /// Finish parsing the document and determine if it ends explicitly.
    /// Returns `(explicit, span)` for the `DocumentEnd` event.
    fn finish_document(&mut self) -> (bool, Span) {
        // After parsing, skip remaining whitespace and Dedent tokens
        loop {
            self.parser.skip_ws_and_newlines();
            if matches!(self.parser.peek(), Some((Token::Dedent, _))) {
                self.parser.advance();
            } else {
                break;
            }
        }

        // Consume any remaining content that belongs to this document
        self.consume_trailing_content();

        // Check for document end marker `...`
        let has_doc_end = matches!(self.parser.peek(), Some((Token::DocEnd, _)));
        let doc_end_span = self.parser.current_span();
        if has_doc_end {
            self.parser.advance(); // consume DocEnd
            self.parser.skip_ws_and_newlines();
        }

        // Skip trailing Dedent tokens
        while matches!(self.parser.peek(), Some((Token::Dedent, _))) {
            self.parser.advance();
        }

        (has_doc_end, doc_end_span)
    }

    /// Consume trailing content before the next document marker.
    fn consume_trailing_content(&mut self) {
        while !self.parser.is_eof() {
            if matches!(self.parser.peek(), Some((Token::Dedent, _))) {
                self.parser.advance();
                continue;
            }
            // Stop at document markers
            if matches!(
                self.parser.peek(),
                Some((Token::DocStart | Token::DocEnd, _))
            ) {
                break;
            }
            // Stop at directives
            if matches!(
                self.parser.peek(),
                Some((
                    Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_),
                    _
                ))
            ) {
                break;
            }

            // Check for orphan content
            if let Some((token, span)) = self.parser.peek() {
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
                    self.parser
                        .error(crate::error::ErrorKind::TrailingContent, span);
                }
            }
            self.parser.advance();
            self.parser.skip_ws_and_newlines();
        }
    }
}

impl<'tokens: 'input, 'input> Iterator for StreamingParser<'tokens, 'input> {
    type Item = Event<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        // First, emit StreamStart
        if !self.emitted_stream_start {
            self.emitted_stream_start = true;
            return Some(Event::StreamStart);
        }

        loop {
            match &self.state {
                StreamState::Ready => {
                    // Try to prepare the next document
                    if let Some((explicit, span)) = self.prepare_document() {
                        self.state = StreamState::EmitDocStart { explicit, span };
                    } else {
                        // No more documents
                        self.state = StreamState::Done;
                        return Some(Event::StreamEnd);
                    }
                }

                StreamState::EmitDocStart { explicit, span } => {
                    let event = Event::DocumentStart {
                        explicit: *explicit,
                        span: *span,
                    };
                    let has_explicit = *explicit;
                    self.state = StreamState::ParseDocContent {
                        has_explicit_start: has_explicit,
                    };
                    return Some(event);
                }

                StreamState::ParseDocContent { has_explicit_start } => {
                    let has_explicit = *has_explicit_start;
                    // Dispatch based on value kind and parse content
                    self.dispatch_and_parse_content(has_explicit);
                    self.state = StreamState::DrainBuffer;
                }

                StreamState::DrainBuffer => {
                    if let Some(event) = self.buffer.pop_front() {
                        return Some(event);
                    }
                    // Buffer empty, finish document
                    let (explicit, span) = self.finish_document();
                    self.state = StreamState::EmitDocEnd { explicit, span };
                }

                StreamState::EmitDocEnd { explicit, span } => {
                    let event = Event::DocumentEnd {
                        explicit: *explicit,
                        span: *span,
                    };
                    self.state = StreamState::Ready;
                    return Some(event);
                }

                StreamState::Done => {
                    return None;
                }
            }
        }
    }
}
