// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::Parser;
use crate::ast_event::AstEvent;
use crate::error::ErrorKind;
use crate::error::ParseError;
use crate::event::Event;
use crate::span::Span;
use crate::value::Node;

impl<'input, I> Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Create a new parser from an event iterator.
    #[must_use]
    pub(crate) fn new(events: I) -> Self {
        Self {
            events,
            peeked: None,
            events_consumed: 0,
            errors: Vec::new(),
            anchors: std::collections::HashSet::new(),
            anchor_nodes: std::collections::HashMap::new(),
        }
    }

    /// Take collected errors.
    #[must_use]
    pub(crate) fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.errors)
    }

    /// Parse all documents from the event stream.
    #[must_use]
    pub(crate) fn parse(&mut self) -> Vec<Node<'input>> {
        let mut documents = Vec::new();
        while let Some(node) = self.parse_next_document() {
            documents.push(node);
        }
        documents
    }

    /// Parse the next document from the event stream, if any.
    ///
    /// This is a streaming-friendly variant that consumes at most one document
    /// worth of events and leaves the parser positioned at the start of the
    /// next document (or end-of-stream). Anchors are scoped per document and
    /// cleared after each call.
    pub(crate) fn parse_next_document(&mut self) -> Option<Node<'input>> {
        loop {
            let event = self.peek().cloned()?;
            match event {
                AstEvent::Event(Event::StreamStart) => {
                    // Skip the stream start marker.
                    self.advance();
                }
                AstEvent::Event(Event::StreamEnd) => {
                    // Consume the stream end marker and signal EOF.
                    self.advance();
                    return None;
                }
                AstEvent::Event(Event::DocumentStart { .. })
                | AstEvent::RichEvent {
                    event: Event::DocumentStart { .. },
                    ..
                } => {
                    // Explicit document: consume the start marker, parse the
                    // root node, optionally consume a trailing DocumentEnd,
                    // then clear anchors for the next document.
                    self.advance();
                    let parsed_root = self.parse_node();
                    if matches!(
                        self.peek(),
                        Some(
                            AstEvent::Event(Event::DocumentEnd { .. })
                                | AstEvent::RichEvent {
                                    event: Event::DocumentEnd { .. },
                                    ..
                                }
                        )
                    ) {
                        self.advance();
                    }
                    // Anchors are scoped to a single document.
                    self.anchors.clear();
                    self.anchor_nodes.clear();
                    if let Some(root_node) = parsed_root {
                        return Some(root_node);
                    }
                }
                AstEvent::Event(
                    Event::MappingEnd { .. }
                    | Event::SequenceEnd { .. }
                    | Event::InvalidatePair { .. },
                )
                | AstEvent::RichEvent {
                    event: Event::MappingEnd { .. } | Event::SequenceEnd { .. },
                    ..
                } => {
                    // Stray end markers - skip them to avoid infinite loop.
                    self.advance();
                }
                _ => {
                    // Content without explicit document start - treat as an
                    // implicit document.
                    let consumed_before = self.events_consumed;
                    if let Some(parsed_node) = self.parse_node() {
                        // Anchors are scoped to a single document.
                        self.anchors.clear();
                        self.anchor_nodes.clear();
                        return Some(parsed_node);
                    }
                    // `parse_node` returned None without consuming - skip the
                    // current event to avoid an infinite loop.
                    if self.events_consumed == consumed_before {
                        self.advance();
                    }
                }
            }
        }
    }

    /// Peek at the current event, using an internal one-element buffer.
    pub(super) fn peek(&mut self) -> Option<&AstEvent<'input>> {
        if self.peeked.is_none() {
            self.peeked = self.events.next().map(Into::into);
        }
        self.peeked.as_ref()
    }

    /// Advance to the next event.
    ///
    /// This logically consumes the current event (including any buffered by
    /// `peek()`) and increments `events_consumed` for progress tracking.
    pub(super) fn advance(&mut self) {
        if self.peeked.is_some() {
            self.peeked = None;
            self.events_consumed += 1;
        } else if self.events.next().is_some() {
            self.events_consumed += 1;
        }
    }

    /// Consume and return the current event by ownership.
    pub(super) fn next_event(&mut self) -> Option<AstEvent<'input>> {
        let event = if let Some(event) = self.peeked.take() {
            Some(event)
        } else {
            self.events.next().map(Into::into)
        };
        if event.is_some() {
            self.events_consumed += 1;
        }
        event
    }

    /// Record an error.
    pub(super) fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError::new(kind, span));
    }
}
