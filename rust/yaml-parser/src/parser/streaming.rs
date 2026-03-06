// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Streaming parser that implements `Iterator<Item = Event>`.
//!
//! This module provides `StreamingParser`, which wraps the batch `Parser`
//! and exposes events through an iterator interface. This is Step 4 of the
//! streaming parser transformation (see `REFACTORING_STREAMING_PARSER.md`).
//!
//! Initially, this delegates to the batch parser and drains from an internal buffer.
//! Future steps will convert this to a true state-machine-based streaming parser.

use std::collections::VecDeque;

use crate::error::ParseError;
use crate::event::Event;
use crate::lexer::RichToken;

use super::Parser;

/// A streaming YAML parser that implements `Iterator<Item = Event>`.
///
/// This parser yields events one at a time, enabling incremental processing
/// of YAML documents without loading the entire event stream into memory.
///
/// # Current Implementation (Step 4)
///
/// Currently, this wraps the batch `Parser` and drains events from an internal
/// buffer. Documents are parsed one at a time when the buffer is exhausted.
///
/// # Future Implementation (Steps 5+)
///
/// Will be converted to a true state-machine-based parser that only advances
/// token consumption when `next()` is called.
#[derive(Debug)]
pub struct StreamingParser<'tokens, 'input> {
    /// The underlying batch parser.
    parser: Parser<'tokens, 'input>,
    /// Buffer of events to drain.
    buffer: VecDeque<Event<'input>>,
    /// Whether we've emitted StreamStart.
    emitted_stream_start: bool,
    /// Whether we've emitted StreamEnd.
    emitted_stream_end: bool,
}

impl<'tokens: 'input, 'input> StreamingParser<'tokens, 'input> {
    /// Create a new streaming parser from a token slice.
    pub fn new(tokens: &'tokens [RichToken<'input>], input: &'input str) -> Self {
        Self {
            parser: Parser::new(tokens, input),
            buffer: VecDeque::new(),
            emitted_stream_start: false,
            emitted_stream_end: false,
        }
    }

    /// Get any parse errors encountered so far.
    pub fn errors(&self) -> &[ParseError] {
        &self.parser.errors
    }

    /// Take the collected errors, leaving an empty vector.
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.parser.errors)
    }

    /// Fill the buffer by parsing the next document.
    /// Returns true if events were added, false if at EOF.
    fn fill_buffer(&mut self) -> bool {
        // Skip initial whitespace/newlines
        self.parser.skip_ws_and_newlines();

        if self.parser.is_eof() {
            return false;
        }

        // Skip any orphan DocEnd markers
        while matches!(self.parser.peek(), Some((crate::lexer::Token::DocEnd, _))) {
            self.parser.advance();
            self.parser.skip_ws_and_newlines();
        }

        if self.parser.is_eof() {
            return false;
        }

        // Populate tag handles for this document
        self.parser.populate_tag_handles_from_tokens();

        // Track directives for "directive without document" error
        let mut has_directive_in_prologue = false;
        let mut first_directive_span = crate::span::Span::from_usize_range(0..0);

        // Skip directive tokens
        while let Some((tok, span)) = self.parser.peek() {
            match tok {
                crate::lexer::Token::YamlDirective(_)
                | crate::lexer::Token::TagDirective(_)
                | crate::lexer::Token::ReservedDirective(_) => {
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

        // Check for "directive without document" error:
        // If we saw directives but now hit EOF or `...` (doc end), that's an error
        if has_directive_in_prologue {
            let at_doc_end_or_eof = self.parser.is_eof()
                || matches!(self.parser.peek(), Some((crate::lexer::Token::DocEnd, _)));
            if at_doc_end_or_eof {
                self.parser.error(
                    crate::error::ErrorKind::TrailingContent,
                    first_directive_span,
                );
                // Return false to skip to next iteration / EOF check
                return false;
            }
        }

        if self.parser.is_eof() {
            return false;
        }

        // Parse one document
        self.parser.parse_document_events();
        self.parser.skip_ws_and_newlines();

        // Move events from parser to our buffer
        for event in self.parser.take_events() {
            self.buffer.push_back(event);
        }

        !self.buffer.is_empty()
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

        // If buffer is empty, try to fill it
        if self.buffer.is_empty() && !self.fill_buffer() {
            // No more documents, emit StreamEnd if we haven't
            if !self.emitted_stream_end {
                self.emitted_stream_end = true;
                return Some(Event::StreamEnd);
            }
            return None;
        }

        // Drain from buffer
        self.buffer.pop_front()
    }
}
