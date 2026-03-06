// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Event-based YAML parser.
//!
//! This module implements a simplified parser that consumes events from the
//! [`EventEmitter`](crate::event::EventEmitter) and builds the AST (`Node`/`Value`).
//!
//! # Architecture
//!
//! ```text
//! Tokens → EventEmitter → EventParser → AST (Vec<Node>)
//! ```
//!
//! The event-based parser is much simpler than token-based parsing because
//! structural detection (indentation, flow/block contexts) is handled by the
//! event emitter. The parser just needs to:
//! 1. Match start/end event pairs (mapping, sequence, document)
//! 2. Apply type inference to scalars
//! 3. Track anchors for alias validation

use std::borrow::Cow;
use std::collections::HashMap;

use crate::error::{ErrorKind, ParseError};
use crate::event::{Event, ScalarStyle};
use crate::span::Span;
use crate::value::{Node, Properties, Value};

/// Parser that builds AST from events.
///
/// This is a simplified parser that consumes events and builds the AST.
/// Structural complexity is handled by the event emitter.
pub struct EventParser<'events, 'input> {
    /// Event stream to consume
    events: &'events [Event<'input>],
    /// Current position in event stream
    pos: usize,
    /// Collected errors
    errors: Vec<ParseError>,
    /// Map of anchor names to their nodes (for validation)
    anchors: HashMap<&'input str, ()>,
}

impl<'events, 'input> EventParser<'events, 'input> {
    /// Create a new event parser.
    #[must_use]
    pub fn new(events: &'events [Event<'input>]) -> Self {
        Self {
            events,
            pos: 0,
            errors: Vec::new(),
            anchors: HashMap::new(),
        }
    }

    /// Take collected errors.
    #[must_use]
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.errors)
    }

    /// Parse all documents from the event stream.
    #[must_use]
    pub fn parse(&mut self) -> Vec<Node<'input>> {
        let mut documents = Vec::new();

        while let Some(event) = self.peek() {
            match event {
                Event::StreamStart => {
                    self.advance();
                }
                Event::StreamEnd => {
                    self.advance();
                    break;
                }
                Event::DocumentStart { .. } => {
                    self.advance();
                    if let Some(node) = self.parse_node() {
                        documents.push(node);
                    }
                    // Consume DocumentEnd if present
                    if matches!(self.peek(), Some(Event::DocumentEnd { .. })) {
                        self.advance();
                    }
                }
                Event::MappingEnd { .. } | Event::SequenceEnd { .. } => {
                    // Stray end markers - skip them to avoid infinite loop
                    self.advance();
                }
                _ => {
                    // Content without explicit document start - treat as implicit document
                    if let Some(node) = self.parse_node() {
                        documents.push(node);
                    } else {
                        // parse_node returned None without consuming - skip to avoid infinite loop
                        self.advance();
                    }
                }
            }
        }

        documents
    }

    /// Peek at the current event.
    fn peek(&self) -> Option<&Event<'input>> {
        self.events.get(self.pos)
    }

    /// Advance to the next event.
    fn advance(&mut self) {
        if self.pos < self.events.len() {
            self.pos += 1;
        }
    }

    /// Record an error.
    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError::new(kind, span));
    }

    /// Parse a single node from events.
    fn parse_node(&mut self) -> Option<Node<'input>> {
        let event = self.peek()?.clone();
        match event {
            Event::MappingStart {
                anchor, tag, span, ..
            } => {
                self.advance();
                Some(self.parse_mapping(anchor, tag, span))
            }
            Event::SequenceStart {
                anchor, tag, span, ..
            } => {
                self.advance();
                Some(self.parse_sequence(anchor, tag, span))
            }
            Event::Scalar {
                style,
                value,
                anchor,
                tag,
                span,
            } => {
                self.advance();
                Some(self.build_scalar(style, value, anchor, tag, span))
            }
            Event::Alias { name, span } => {
                self.advance();
                self.build_alias(name, span)
            }
            // Skip document markers, stream markers
            Event::StreamStart
            | Event::StreamEnd
            | Event::DocumentStart { .. }
            | Event::DocumentEnd { .. }
            | Event::MappingEnd { .. }
            | Event::SequenceEnd { .. } => None,
        }
    }

    /// Parse a mapping node.
    fn parse_mapping(
        &mut self,
        anchor: Option<Cow<'input, str>>,
        tag: Option<Cow<'input, str>>,
        start_span: Span,
    ) -> Node<'input> {
        // Register anchor if present
        if let Some(ref anc) = anchor {
            if let Cow::Borrowed(s) = anc {
                self.anchors.insert(s, ());
            }
        }

        let mut pairs: Vec<(Node<'input>, Node<'input>)> = Vec::new();
        let mut end_span = start_span;

        loop {
            match self.peek() {
                Some(Event::MappingEnd { span }) => {
                    end_span = *span;
                    self.advance();
                    break;
                }
                Some(Event::SequenceEnd { .. }) => {
                    // Mismatched end marker - break to avoid infinite loop
                    break;
                }
                Some(_) => {
                    // Parse key
                    let pos_before = self.pos;
                    let key = self.parse_node().unwrap_or_else(|| Node::null(start_span));
                    // Parse value
                    let value = self.parse_node().unwrap_or_else(|| Node::null(start_span));
                    pairs.push((key, value));
                    // Ensure we made progress to avoid infinite loop
                    if self.pos == pos_before {
                        self.advance();
                    }
                }
                None => break,
            }
        }

        let span = start_span.union(end_span);
        let node = Node::new(Value::Mapping(pairs), span);
        self.apply_properties(node, anchor, tag)
    }

    /// Parse a sequence node.
    fn parse_sequence(
        &mut self,
        anchor: Option<Cow<'input, str>>,
        tag: Option<Cow<'input, str>>,
        start_span: Span,
    ) -> Node<'input> {
        // Register anchor if present
        if let Some(ref anc) = anchor {
            if let Cow::Borrowed(s) = anc {
                self.anchors.insert(s, ());
            }
        }

        let mut items: Vec<Node<'input>> = Vec::new();
        let mut end_span = start_span;

        loop {
            match self.peek() {
                Some(Event::SequenceEnd { span }) => {
                    end_span = *span;
                    self.advance();
                    break;
                }
                Some(Event::MappingEnd { .. }) => {
                    // Mismatched end marker - break to avoid infinite loop
                    break;
                }
                Some(_) => {
                    let pos_before = self.pos;
                    if let Some(item) = self.parse_node() {
                        items.push(item);
                    }
                    // Ensure we made progress to avoid infinite loop
                    if self.pos == pos_before {
                        self.advance();
                    }
                }
                None => break,
            }
        }

        let span = start_span.union(end_span);
        let node = Node::new(Value::Sequence(items), span);
        self.apply_properties(node, anchor, tag)
    }

    /// Build a scalar node with type inference.
    fn build_scalar(
        &mut self,
        style: ScalarStyle,
        value: Cow<'input, str>,
        anchor: Option<Cow<'input, str>>,
        tag: Option<Cow<'input, str>>,
        span: Span,
    ) -> Node<'input> {
        // Register anchor if present
        if let Some(ref anc) = anchor {
            if let Cow::Borrowed(s) = anc {
                self.anchors.insert(s, ());
            }
        }

        // Type inference only applies to plain scalars without explicit tags
        let typed_value = if style == ScalarStyle::Plain && tag.is_none() {
            Self::infer_type(&value)
        } else {
            // Quoted/block scalars or tagged scalars are strings
            Value::String(value)
        };

        let node = Node::new(typed_value, span);
        self.apply_properties(node, anchor, tag)
    }

    /// Build an alias node.
    fn build_alias(&mut self, name: Cow<'input, str>, span: Span) -> Option<Node<'input>> {
        // Validate that the anchor exists
        let name_str: &str = &name;
        if !self.anchors.contains_key(name_str) {
            self.error(ErrorKind::UndefinedAlias, span);
        }
        Some(Node::new(Value::Alias(name), span))
    }

    /// Apply anchor and tag properties to a node.
    fn apply_properties(
        &self,
        mut node: Node<'input>,
        anchor: Option<Cow<'input, str>>,
        tag: Option<Cow<'input, str>>,
    ) -> Node<'input> {
        if anchor.is_some() || tag.is_some() {
            node.properties = Some(Box::new(Properties { anchor, tag }));
        }
        node
    }

    /// Infer the type of a plain scalar value.
    ///
    /// Per YAML 1.2 Core Schema:
    /// - null: null, Null, NULL, ~, empty
    /// - bool: true, True, TRUE, false, False, FALSE
    /// - int: decimal integers
    /// - float: decimal floats, .inf, -.inf, .nan
    /// - everything else is a string
    fn infer_type(value: &str) -> Value<'input> {
        match value {
            "null" | "Null" | "NULL" | "~" | "" => Value::Null,
            "true" | "True" | "TRUE" => Value::Bool(true),
            "false" | "False" | "FALSE" => Value::Bool(false),
            _ => {
                // Try integer
                if let Ok(int) = value.parse::<i64>() {
                    return Value::Int(int);
                }
                // Try float
                if let Ok(float) = value.parse::<f64>() {
                    return Value::Float(float);
                }
                // Special float values
                match value {
                    ".inf" | ".Inf" | ".INF" => return Value::Float(f64::INFINITY),
                    "-.inf" | "-.Inf" | "-.INF" => return Value::Float(f64::NEG_INFINITY),
                    ".nan" | ".NaN" | ".NAN" => return Value::Float(f64::NAN),
                    _ => {}
                }
                // Default to string
                Value::String(Cow::Owned(value.to_owned()))
            }
        }
    }
}
