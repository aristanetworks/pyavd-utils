// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Event-to-AST parser.
//!
//! This module implements the `Parser` that consumes events emitted by the
//! YAML emitter and builds the AST (`Node` / `Value`).
//!
//! # Architecture
//!
//! ```text
//! Lexer -> Emitter (Event stream) -> Parser -> AST
//! ```
//!
//! The `Parser` is much simpler than a token-based parser because
//! structural detection (indentation, flow/block contexts) is handled by the
//! `Emitter`. The `Parser` just needs to:
//! 1. Match start/end event pairs (mapping, sequence, document)
//! 2. Apply type inference to scalars
//! 3. Track anchors for alias validation

use std::borrow::Cow;
use std::collections::HashSet;

use crate::ast_event::AstEvent;
use crate::error::{ErrorKind, ParseError};
use crate::event::{Event, Property as EventProperty, ScalarStyle};
use crate::span::Span;
use crate::value::{Integer, MappingPair, Node, Properties as NodeProperties, SequenceItem, Value};

/// Parser that builds AST from a streaming source of events.
///
/// This parser consumes events and builds the AST.
/// Structural complexity such as indentation and block/flow handling is
/// resolved earlier by the emitter.
///
/// The parser operates over any `Iterator<Item = Event<'input>>`, using an
/// internal one-element lookahead buffer.
pub struct Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Underlying event iterator.
    events: I,
    /// Buffered lookahead event (result of the most recent `peek()`).
    peeked: Option<AstEvent<'input>>,
    /// Count of events that have been logically consumed via `advance()`.
    /// Used for progress tracking in recovery paths.
    events_consumed: usize,
    /// Collected errors
    errors: Vec<ParseError>,
    /// Set of registered anchor names (for alias validation)
    /// Uses owned strings because events may contain `Cow::Owned` values
    anchors: HashSet<String>,
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Create a new parser from an event iterator.
    #[must_use]
    pub fn new(events: I) -> Self {
        Self {
            events,
            peeked: None,
            events_consumed: 0,
            errors: Vec::new(),
            anchors: HashSet::new(),
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
                AstEvent::Event(Event::DocumentStart { .. }) => {
                    // Explicit document: consume the start marker, parse the
                    // root node, optionally consume a trailing DocumentEnd,
                    // then clear anchors for the next document.
                    self.advance();
                    let node = self.parse_node();
                    if matches!(
                        self.peek(),
                        Some(AstEvent::Event(Event::DocumentEnd { .. }))
                    ) {
                        self.advance();
                    }
                    // Anchors are scoped to a single document.
                    self.anchors.clear();
                    return node;
                }
                AstEvent::Event(Event::MappingEnd { .. } | Event::SequenceEnd { .. }) => {
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
    fn peek(&mut self) -> Option<&AstEvent<'input>> {
        if self.peeked.is_none() {
            self.peeked = self.events.next().map(Into::into);
        }
        self.peeked.as_ref()
    }

    /// Advance to the next event.
    ///
    /// This logically consumes the current event (including any buffered by
    /// `peek()`) and increments `events_consumed` for progress tracking.
    fn advance(&mut self) {
        if self.peeked.is_some() {
            self.peeked = None;
            self.events_consumed += 1;
        } else if self.events.next().is_some() {
            self.events_consumed += 1;
        }
    }

    /// Consume and return the current event by ownership.
    fn next_event(&mut self) -> Option<AstEvent<'input>> {
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
    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError::new(kind, span));
    }

    /// Parse a single node from events.
    fn parse_node(&mut self) -> Option<Node<'input>> {
        self.next_event()
            .and_then(|event| self.parse_node_from_ast_event(event))
    }

    fn parse_node_from_ast_event(&mut self, event: AstEvent<'input>) -> Option<Node<'input>> {
        match event {
            AstEvent::SequenceItem {
                event: inner_event, ..
            }
            | AstEvent::MappingKey {
                key_event: inner_event,
                ..
            }
            | AstEvent::Event(inner_event) => self.parse_node_from_event(inner_event),
        }
    }

    fn parse_node_from_event(&mut self, event: Event<'input>) -> Option<Node<'input>> {
        match event {
            Event::MappingStart {
                properties, span, ..
            } => Some(self.parse_mapping(properties, span)),
            Event::SequenceStart {
                properties, span, ..
            } => Some(self.parse_sequence(properties, span)),
            Event::Scalar {
                style,
                value,
                properties,
                span,
            } => Some(self.build_scalar(style, value, properties, span)),
            Event::Alias { name, span } => Some(self.build_alias(name, span)),
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
    ///
    /// Span policy:
    /// - block mappings use `start..last_value.end`
    /// - flow mappings use `start..closing_brace.end`
    fn parse_mapping(
        &mut self,
        props: Option<Box<NodeProperties<'input>>>,
        start_span: Span,
    ) -> Node<'input> {
        self.register_anchor(
            props
                .as_ref()
                .and_then(|event_props| event_props.anchor.as_ref()),
        );

        let mut pairs: Vec<MappingPair<'input>> = Vec::with_capacity(8);
        let mut end_span = start_span;

        loop {
            match self.peek() {
                Some(AstEvent::Event(Event::MappingEnd { span })) => {
                    end_span = *span;
                    self.advance();
                    break;
                }
                Some(AstEvent::Event(Event::SequenceEnd { .. })) => {
                    // Mismatched end marker - break to avoid infinite loop
                    break;
                }
                Some(AstEvent::MappingKey { .. }) => {
                    // Parse key
                    let consumed_before = self.events_consumed;
                    let Some(AstEvent::MappingKey {
                        pair_start,
                        key_event,
                    }) = self.next_event()
                    else {
                        debug_assert!(false, "peeked MappingKey event disappeared");
                        break;
                    };
                    let key = self
                        .parse_node_from_event(key_event)
                        .unwrap_or_else(|| Node::null(start_span));
                    // Parse value
                    let value = self.parse_node().unwrap_or_else(|| Node::null(start_span));
                    let pair_span = Span::new(pair_start..value.span.end);
                    pairs.push(MappingPair::new(pair_span, key, value));
                    // Ensure we made progress to avoid infinite loop
                    if self.events_consumed == consumed_before {
                        self.advance();
                    }
                }
                Some(_) => {
                    let consumed_before = self.events_consumed;
                    let key = self.parse_node().unwrap_or_else(|| Node::null(start_span));
                    let value = self.parse_node().unwrap_or_else(|| Node::null(start_span));
                    let pair_span =
                        Span::from_usize_range(key.span.start_usize()..value.span.end_usize());
                    pairs.push(MappingPair::new(pair_span, key, value));
                    if self.events_consumed == consumed_before {
                        self.advance();
                    }
                }
                None => break,
            }
        }

        // Flow collections end at the closing brace. Block collections end at
        // the last successfully parsed value.
        let start = start_span.start_usize();
        let end = if end_span.start == end_span.end {
            // Block: use last value's end
            pairs
                .last()
                .map_or(start, |pair| pair.value.span.end_usize())
        } else {
            // Flow: end_span covers the closing brace
            end_span.end_usize()
        };
        let span = Span::from_usize_range(start..end);

        let node = Node::new(Value::Mapping(pairs), span);
        Self::apply_properties(node, props)
    }

    /// Parse a sequence node.
    ///
    /// Span policy:
    /// - block sequences use `start..last_item.end`
    /// - flow sequences use `start..closing_bracket.end`
    fn parse_sequence(
        &mut self,
        props: Option<Box<NodeProperties<'input>>>,
        start_span: Span,
    ) -> Node<'input> {
        self.register_anchor(
            props
                .as_ref()
                .and_then(|event_props| event_props.anchor.as_ref()),
        );

        let mut items: Vec<SequenceItem<'input>> = Vec::with_capacity(16);
        let mut end_span = start_span;

        loop {
            match self.peek() {
                Some(AstEvent::Event(Event::SequenceEnd { span })) => {
                    end_span = *span;
                    self.advance();
                    break;
                }
                Some(AstEvent::Event(Event::MappingEnd { .. })) => {
                    // Mismatched end marker - break to avoid infinite loop
                    break;
                }
                Some(AstEvent::SequenceItem { .. }) => {
                    let consumed_before = self.events_consumed;
                    let Some(AstEvent::SequenceItem { item_start, event }) = self.next_event()
                    else {
                        debug_assert!(false, "peeked SequenceItem event disappeared");
                        break;
                    };
                    if let Some(node) = self.parse_node_from_event(event) {
                        let item_span = Span::new(item_start..node.span.end);
                        items.push(SequenceItem::new(item_span, node));
                    }
                    // Ensure we made progress to avoid infinite loop
                    if self.events_consumed == consumed_before {
                        self.advance();
                    }
                }
                Some(_) => {
                    let consumed_before = self.events_consumed;
                    if let Some(node) = self.parse_node() {
                        let item_span = node.span;
                        items.push(SequenceItem::new(item_span, node));
                    }
                    if self.events_consumed == consumed_before {
                        self.advance();
                    }
                }
                None => break,
            }
        }

        // Flow collections end at the closing bracket. Block collections end
        // at the last successfully parsed item.
        let start = start_span.start_usize();
        let end = if end_span.start == end_span.end {
            // Block: use last item's end
            items
                .last()
                .map_or(start, |item| item.node.span.end_usize())
        } else {
            // Flow: end_span covers the closing bracket
            end_span.end_usize()
        };
        let span = Span::from_usize_range(start..end);

        let node = Node::new(Value::Sequence(items), span);
        Self::apply_properties(node, props)
    }

    /// Build a scalar node with type inference.
    ///
    /// Plain scalars are inferred before explicit tags are interpreted as
    /// annotations, so `!!str 42` currently becomes `Int(42)` with a `!!str`
    /// tag attached to the node.
    fn build_scalar(
        &mut self,
        style: ScalarStyle,
        value: Cow<'input, str>,
        props: Option<Box<NodeProperties<'input>>>,
        span: Span,
    ) -> Node<'input> {
        self.register_anchor(
            props
                .as_ref()
                .and_then(|event_props| event_props.anchor.as_ref()),
        );

        let invalid_explicit_null = style == ScalarStyle::Plain
            && props
                .as_ref()
                .and_then(|event_props| event_props.tag.as_ref())
                .is_some_and(|tag| {
                    tag.value == "tag:yaml.org,2002:null"
                        && !value.is_empty()
                        && value.as_ref() != "null"
                });

        // Type inference applies to plain scalars regardless of tag. We may
        // still record additional errors later if an explicit tag is
        // incompatible with the scalar's textual representation.
        let typed_value = if style == ScalarStyle::Plain {
            infer_scalar_type(value)
        } else {
            // Quoted/block scalars are always strings
            Value::String(value)
        };

        let base_node = Node::new(typed_value, span);
        let node = Self::apply_properties(base_node, props);

        // Validate core-schema tags against the scalar's textual form.
        // For now we are conservative and only enforce additional rules for
        // the `!!null` tag. Other core tags currently continue to follow the
        // "type inference first, tags as annotations" behaviour.
        //
        // NOTE: We adopt a *mostly strict* policy for explicit `!!null` to
        // match the behaviour of `serde_yaml` and `saphyr` while still staying
        // compatible with the YAML test suite:
        // - For plain scalars tagged `!!null`, we accept either the canonical
        //   textual form `null` or an *empty* scalar as valid representations
        //   of `tag:yaml.org,2002:null`.
        // - Any other textual form (e.g. `Null`, `NULL`, `~`, `str`, `0`, ...)
        //   is treated as an invalid use of the null tag and recorded as a
        //   parse error.
        //
        // This keeps `!!null str` and similar cases aligned with
        // `serde_yaml`/`saphyr`, but allows "tags on empty scalars" such as
        // those in the FH7J YAML test to remain error-free.
        if invalid_explicit_null {
            // Recognised null tag whose textual content is neither
            // empty nor the canonical "null" is treated as an
            // invalid null scalar. We still keep the inferred
            // `Value::Null` in the AST so error-recovery consumers can
            // inspect partial data, but callers like `parse_ok` and
            // `serde::from_str` will see this as a parse error.
            self.error(ErrorKind::InvalidValue, span);
        }

        node
    }

    /// Build an alias node.
    fn build_alias(&mut self, name: Cow<'input, str>, span: Span) -> Node<'input> {
        // Validate that the anchor exists
        if !self.anchor_is_defined(name.as_ref()) {
            self.error(ErrorKind::UndefinedAlias, span);
        }
        Node::new(Value::Alias(name), span)
    }

    /// Register an anchor in the anchor tracking set.
    fn register_anchor(&mut self, anchor: Option<&EventProperty<'input>>) {
        if let Some(prop) = anchor {
            // Use as_ref() to avoid cloning if already owned, or convert borrowed to owned
            self.anchors.insert(prop.value.as_ref().to_owned());
        }
    }

    /// Check if an anchor is defined.
    fn anchor_is_defined(&self, name: &str) -> bool {
        self.anchors.contains(name)
    }

    /// Apply anchor and tag properties to a node.
    ///
    /// Note: We intentionally do NOT extend the node span to include property spans.
    /// The node span should cover only the value itself, so that span-based extraction
    /// (used by tests) returns just the value text without anchor/tag syntax.
    fn apply_properties(
        mut node: Node<'input>,
        props: Option<Box<NodeProperties<'input>>>,
    ) -> Node<'input> {
        node.properties = props;
        node
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NumericKind {
    Integer,
    Float,
    NotNumeric,
}

/// Classify whether a scalar is a plausible integer, a plausible float, or
/// definitely not numeric using a single byte scan.
#[inline]
fn classify_numeric(input: &str) -> NumericKind {
    let bytes = input.as_bytes();
    if bytes.is_empty() {
        return NumericKind::NotNumeric;
    }

    let mut idx = 0;
    if matches!(bytes.get(idx), Some(b'+' | b'-')) {
        idx += 1;
        if idx == bytes.len() {
            return NumericKind::NotNumeric;
        }
    }

    let mut saw_digit = false;
    let mut saw_dot = false;
    let mut saw_exp = false;
    let mut expect_exp_digit = false;

    while idx < bytes.len() {
        match bytes.get(idx) {
            Some(b'0'..=b'9') => {
                saw_digit = true;
                expect_exp_digit = false;
            }
            Some(b'.') if !saw_dot && !saw_exp => {
                saw_dot = true;
            }
            Some(b'e' | b'E') if saw_digit && !saw_exp => {
                saw_exp = true;
                expect_exp_digit = true;
                saw_digit = false;
                if matches!(bytes.get(idx + 1), Some(b'+' | b'-')) {
                    idx += 1;
                }
            }
            _ => return NumericKind::NotNumeric,
        }
        idx += 1;
    }

    if !saw_digit || expect_exp_digit {
        return NumericKind::NotNumeric;
    }
    if saw_dot || saw_exp {
        NumericKind::Float
    } else {
        NumericKind::Integer
    }
}

/// Infer the type of a plain scalar value.
///
/// Per YAML 1.2 Core Schema:
/// - null: null, Null, NULL, ~, empty
/// - bool: true, True, TRUE, false, False, FALSE
/// - int: decimal integers
/// - float: decimal floats, .inf, -.inf, .nan
/// - everything else is a string
///
/// Takes a `Cow<'input, str>` to avoid unnecessary allocations when the value
/// is inferred as a string (can return the Cow as-is).
pub(crate) fn infer_scalar_type(value: Cow<'_, str>) -> Value<'_> {
    let text = value.as_ref();
    match text.as_bytes().first() {
        None => Value::Null,
        Some(b'~') if text.len() == 1 => Value::Null,
        Some(b'n') if matches!(text, "null") => Value::Null,
        Some(b'N') if matches!(text, "Null" | "NULL") => Value::Null,
        Some(b't') if text == "true" => Value::Bool(true),
        Some(b'T') if matches!(text, "True" | "TRUE") => Value::Bool(true),
        Some(b'f') if text == "false" => Value::Bool(false),
        Some(b'F') if matches!(text, "False" | "FALSE") => Value::Bool(false),
        Some(b'0'..=b'9' | b'+' | b'-' | b'.') => {
            match text {
                ".inf" | ".Inf" | ".INF" => return Value::Float(f64::INFINITY),
                "-.inf" | "-.Inf" | "-.INF" => return Value::Float(f64::NEG_INFINITY),
                ".nan" | ".NaN" | ".NAN" => return Value::Float(f64::NAN),
                _ => {}
            }

            match classify_numeric(text) {
                NumericKind::Float => {
                    if let Ok(float) = text.parse::<f64>() {
                        return Value::Float(float);
                    }
                }
                NumericKind::Integer => {
                    if let Ok(int) = text.parse::<i64>() {
                        return Value::Int(Integer::I64(int));
                    }
                    if let Ok(int) = text.parse::<i128>() {
                        return Value::Int(Integer::I128(int));
                    }
                    if let Ok(uint) = text.parse::<u128>() {
                        return Value::Int(Integer::U128(uint));
                    }
                    return Value::Int(Integer::BigIntStr(value));
                }
                NumericKind::NotNumeric => {}
            }

            Value::String(value)
        }
        _ => Value::String(value),
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::indexing_slicing, reason = "panics are acceptable in tests")]
    #![allow(clippy::panic, reason = "panic is acceptable in tests")]
    #![allow(
        clippy::min_ident_chars,
        reason = "single-char closure params are fine in tests"
    )]
    #![allow(clippy::type_complexity, reason = "complex types are fine in tests")]
    #![allow(
        clippy::approx_constant,
        reason = "test values don't need to use consts"
    )]
    #![allow(clippy::float_cmp, reason = "exact equality is fine for these tests")]

    use super::*;
    use crate::{Stream, error::ParseError};

    /// Use the standard parse function for tests.
    fn parse(input: &str) -> (Stream<'static>, Vec<ParseError>) {
        let (nodes, errors) = crate::parse(input);
        (nodes.into_iter().map(Node::into_owned).collect(), errors)
    }

    fn nodes_equal_ignoring_structural_spans(left: &Node<'_>, right: &Node<'_>) -> bool {
        left.properties == right.properties
            && left.span == right.span
            && values_equal_ignoring_structural_spans(&left.value, &right.value)
    }

    fn values_equal_ignoring_structural_spans(left: &Value<'_>, right: &Value<'_>) -> bool {
        match (left, right) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(lb), Value::Bool(rb)) => lb == rb,
            (Value::Int(li), Value::Int(ri)) => li == ri,
            (Value::Float(lf), Value::Float(rf)) => lf == rf,
            (Value::String(ls), Value::String(rs)) => ls == rs,
            (Value::Alias(la), Value::Alias(ra)) => la == ra,
            (Value::Sequence(left_items), Value::Sequence(right_items)) => {
                left_items.len() == right_items.len()
                    && left_items
                        .iter()
                        .zip(right_items.iter())
                        .all(|(left_item, right_item)| {
                            nodes_equal_ignoring_structural_spans(&left_item.node, &right_item.node)
                        })
            }
            (Value::Mapping(left_pairs), Value::Mapping(right_pairs)) => {
                left_pairs.len() == right_pairs.len()
                    && left_pairs
                        .iter()
                        .zip(right_pairs.iter())
                        .all(|(left_pair, right_pair)| {
                            nodes_equal_ignoring_structural_spans(&left_pair.key, &right_pair.key)
                                && nodes_equal_ignoring_structural_spans(
                                    &left_pair.value,
                                    &right_pair.value,
                                )
                        })
            }
            _ => false,
        }
    }

    #[test]
    fn test_parse_simple_scalar() {
        let (docs, errors) = parse("hello");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        assert!(matches!(&docs.first().unwrap().value, Value::String(string) if string == "hello"));
    }

    #[test]
    fn test_parse_simple_mapping() {
        let (docs, errors) = parse("key: value");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(value, Value::Mapping(_)));
        if let Value::Mapping(pairs) = value {
            assert_eq!(pairs.len(), 1);
            let pair = pairs.first().unwrap();
            assert!(matches!(&pair.key.value, Value::String(string) if string == "key"));
            assert!(matches!(&pair.value.value, Value::String(string) if string == "value"));
        }
    }

    #[test]
    fn test_parse_flow_mapping() {
        let (docs, errors) = parse("{a: 1, b: 2}");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(value, Value::Mapping(_)));
        if let Value::Mapping(pairs) = value {
            assert_eq!(pairs.len(), 2);
        }
    }

    #[test]
    fn test_parse_flow_sequence() {
        let (docs, errors) = parse("[1, 2, 3]");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(value, Value::Sequence(_)));
        if let Value::Sequence(items) = value {
            assert_eq!(items.len(), 3);
        }
    }

    #[test]
    fn test_parse_block_sequence() {
        let (docs, errors) = parse("- a\n- b\n- c");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(value, Value::Sequence(_)));
        if let Value::Sequence(items) = value {
            assert_eq!(items.len(), 3);
        }
    }

    #[test]
    fn test_parse_null_values() {
        let (docs, errors) = parse("~");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        assert!(matches!(&docs.first().unwrap().value, Value::Null));
    }

    #[test]
    fn test_parse_boolean_values() {
        let (docs, errors) = parse("true");
        assert!(errors.is_empty());
        assert!(matches!(&docs.first().unwrap().value, Value::Bool(true)));
    }

    #[test]
    fn test_parse_number_values() {
        let (docs, errors) = parse("42");
        assert!(errors.is_empty());
        assert!(matches!(
            &docs.first().unwrap().value,
            Value::Int(Integer::I64(42))
        ));

        let (docs_, errors_) = parse("3.45");
        assert!(errors_.is_empty());
        assert_eq!(docs_.len(), 1);
        assert_eq!(docs_.first().unwrap().value, Value::Float(3.45));
    }

    #[test]
    fn test_parse_multi_document() {
        let (docs, _) = parse("---\na\n---\nb");
        assert!(docs.len() >= 2);
    }

    #[test]
    fn test_parse_anchor_alias() {
        let (docs, errors) = parse("a: &anchor 1\nb: *anchor");
        assert!(errors.is_empty(), "errors: {errors:?}");
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(&docs.first().unwrap().value, Value::Mapping(_)));
        if let Value::Mapping(pairs) = value {
            assert_eq!(pairs.len(), 2, "expected 2 pairs but got {pairs:?}");
            // Check first pair's value has anchor
            let first_value = &pairs.first().unwrap().value;
            assert_eq!(
                first_value.anchor(),
                Some("anchor"),
                "First value should have anchor 'anchor'"
            );
            let alias_value = &pairs.last().unwrap().value.value;
            assert!(
                matches!(alias_value, Value::Alias(name) if name.as_ref() == "anchor"),
                "Expected Alias(\"anchor\"), got {alias_value:?}"
            );
        }
    }

    #[test]
    fn test_multiline_quoted_key_error() {
        let input = "\"c\n d\": 1";
        let (_, errors) = parse(input);
        assert!(!errors.is_empty());
    }

    /// Parse input through the full event pipeline and return nodes.
    fn parse_via_events(input: &str) -> Vec<crate::value::Node<'static>> {
        let (events, _errors) = crate::emit_events(input);
        let mut parser = Parser::new(events.into_iter());
        parser
            .parse()
            .into_iter()
            .map(crate::value::Node::into_owned)
            .collect()
    }

    /// Test that parsing through collected events matches the public AST path.
    #[test]
    #[allow(clippy::print_stderr, reason = "Debug output on failure")]
    fn test_event_parser_matches_public_parse_pipeline() {
        let test_cases = [
            // Simple scalars
            "hello",
            "42",
            "3.14",
            "true",
            "null",
            "~",
            "",
            // Quoted strings
            "'single quoted'",
            "\"double quoted\"",
            "\"with\\nescape\"",
            // Block scalars
            "|\n  literal\n  block",
            ">\n  folded\n  block",
            // Simple collections
            "a: 1",
            "- item",
            "a: 1\nb: 2",
            "- a\n- b\n- c",
            // Flow collections
            "{a: 1, b: 2}",
            "[1, 2, 3]",
            "{a: [1, 2], b: {c: 3}}",
            // Nested block
            "outer:\n  inner: value",
            "- - nested\n  - items",
            "- a: 1\n  b: 2",
            // Anchors and aliases
            "&anchor value",
            "- &a 1\n- *a",
            // Tags
            "!!str 42",
            "!custom tagged",
            // Multi-document
            "---\nfirst\n---\nsecond",
            // Complex cases
            "key: |\n  multi\n  line",
            "list:\n  - a\n  - b",
            "mixed: [1, {a: b}]",
            // Empty values
            "key:",
            "- \n- value",
            // Explicit keys
            "? explicit\n: value",
        ];

        let mut failures = Vec::new();

        for input in test_cases {
            let (parsed_nodes, _parse_errors) = crate::parse(input);
            let via_events_nodes = parse_via_events(input);

            // Compare node counts
            if parsed_nodes.len() != via_events_nodes.len() {
                failures.push(format!(
                    "Input: {input:?}\n  Node count mismatch: parse={}, via_events={}",
                    parsed_nodes.len(),
                    via_events_nodes.len()
                ));
                continue;
            }

            // Compare each node
            for (i, (parsed, via_events)) in
                parsed_nodes.iter().zip(via_events_nodes.iter()).enumerate()
            {
                if !nodes_equal_ignoring_structural_spans(parsed, via_events) {
                    failures.push(format!(
                        "Input: {input:?}\n  Document {i} mismatch:\n    parse:      {parsed:?}\n    via_events: {via_events:?}"
                    ));
                }
            }

            // We rely on the shared implementation of `parse` and the event-based
            // pipeline to keep error reporting behaviour aligned; this test
            // focuses on node equivalence.
        }

        if !failures.is_empty() {
            eprintln!("\n=== Parse vs Event-Pipeline Mismatches ===");
            for failure in &failures {
                eprintln!("{failure}\n");
            }
            panic!(
                "{} test case(s) failed - parse() output differs from the event pipeline",
                failures.len()
            );
        }
    }

    #[test]
    fn test_parser_simple_scalar() {
        let nodes = parse_via_events("hello");
        assert_eq!(nodes.len(), 1);
        assert!(matches!(&nodes[0].value, Value::String(s) if s == "hello"));
    }

    #[test]
    fn test_event_parser_typed_scalars() {
        // Test type inference for plain scalars
        let test_cases: &[(&str, fn(&Value) -> bool)] = &[
            ("true", |v| matches!(v, Value::Bool(true))),
            ("false", |v| matches!(v, Value::Bool(false))),
            ("null", |v| matches!(v, Value::Null)),
            ("42", |v| matches!(v, Value::Int(Integer::I64(42)))),
            (
                "3.14",
                |v| matches!(v, Value::Float(f) if (*f - 3.14).abs() < 0.001),
            ),
        ];

        for (input, check) in test_cases {
            let nodes = parse_via_events(input);
            assert_eq!(nodes.len(), 1, "Input: {input}");
            assert!(
                check(&nodes[0].value),
                "Input: {input}, got: {:?}",
                nodes[0].value
            );
        }
    }

    #[test]
    fn test_event_parser_simple_mapping() {
        let nodes = parse_via_events("a: 1\nb: 2");
        assert_eq!(nodes.len(), 1);

        if let Value::Mapping(pairs) = &nodes[0].value {
            assert_eq!(pairs.len(), 2);
            // First pair
            assert!(matches!(&pairs[0].key.value, Value::String(s) if s == "a"));
            assert!(matches!(&pairs[0].value.value, Value::Int(Integer::I64(1))));
            // Second pair
            assert!(matches!(&pairs[1].key.value, Value::String(s) if s == "b"));
            assert!(matches!(&pairs[1].value.value, Value::Int(Integer::I64(2))));
        } else {
            panic!("Expected mapping, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_simple_sequence() {
        let nodes = parse_via_events("- a\n- b\n- c");
        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 3);
            assert!(matches!(&items[0].node.value, Value::String(s) if s == "a"));
            assert!(matches!(&items[1].node.value, Value::String(s) if s == "b"));
            assert!(matches!(&items[2].node.value, Value::String(s) if s == "c"));
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_flow_mapping() {
        let nodes = parse_via_events("{a: 1, b: 2}");
        assert_eq!(nodes.len(), 1);

        if let Value::Mapping(pairs) = &nodes[0].value {
            assert_eq!(pairs.len(), 2);
        } else {
            panic!("Expected mapping, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_flow_sequence() {
        let nodes = parse_via_events("[1, 2, 3]");
        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 3);
            assert!(matches!(&items[0].node.value, Value::Int(Integer::I64(1))));
            assert!(matches!(&items[1].node.value, Value::Int(Integer::I64(2))));
            assert!(matches!(&items[2].node.value, Value::Int(Integer::I64(3))));
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_anchor_and_alias() {
        let nodes = parse_via_events("- &anchor value\n- *anchor");
        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 2);
            // First item has anchor
            assert_eq!(items[0].node.anchor(), Some("anchor"));
            assert!(matches!(&items[0].node.value, Value::String(s) if s == "value"));
            // Second item is alias
            assert!(matches!(&items[1].node.value, Value::Alias(s) if s == "anchor"));
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_nested_mapping() {
        let nodes = parse_via_events("outer:\n  inner: value");
        assert_eq!(nodes.len(), 1);

        if let Value::Mapping(pairs) = &nodes[0].value {
            assert_eq!(pairs.len(), 1);
            // Check outer key
            assert!(matches!(&pairs[0].key.value, Value::String(s) if s == "outer"));
            // Check inner mapping
            if let Value::Mapping(inner_pairs) = &pairs[0].value.value {
                assert_eq!(inner_pairs.len(), 1);
                assert!(matches!(&inner_pairs[0].key.value, Value::String(s) if s == "inner"));
                assert!(matches!(&inner_pairs[0].value.value, Value::String(s) if s == "value"));
            } else {
                panic!("Expected inner mapping, got: {:?}", pairs[0].value.value);
            }
        } else {
            panic!("Expected mapping, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_nested_sequence() {
        let input = "- - a\n  - b\n- c";
        let nodes = parse_via_events(input);

        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 2);
            // First item is nested sequence
            if let Value::Sequence(nested) = &items[0].node.value {
                assert_eq!(nested.len(), 2);
                assert!(matches!(&nested[0].node.value, Value::String(s) if s == "a"));
                assert!(matches!(&nested[1].node.value, Value::String(s) if s == "b"));
            } else {
                panic!("Expected nested sequence, got: {:?}", items[0].node.value);
            }
            // Second item is scalar
            assert!(matches!(&items[1].node.value, Value::String(s) if s == "c"));
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_sequence_of_mappings() {
        let nodes = parse_via_events("- a: 1\n- b: 2");
        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 2);
            // First item is mapping
            if let Value::Mapping(pairs) = &items[0].node.value {
                assert!(matches!(&pairs[0].key.value, Value::String(s) if s == "a"));
                assert!(matches!(&pairs[0].value.value, Value::Int(Integer::I64(1))));
            } else {
                panic!("Expected mapping, got: {:?}", items[0].node.value);
            }
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }
}
