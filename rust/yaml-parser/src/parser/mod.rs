// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Event-to-AST parser.
//!
//! This module implements the `Parser` that consumes events emitted by the
//! [`Emitter`](crate::emitter::Emitter) and builds the AST (`Node`/`Value`).
//!
//! # Architecture
//!
//! ```text
//! Tokens → Emitter (emits Events) → Parser → AST (Vec<Node>)
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

use crate::error::{ErrorKind, ParseError};
use crate::event::{Event, Properties as EventProperties, Property as EventProperty, ScalarStyle};
use crate::span::Span;
use crate::value::{Node, Properties as NodeProperties, Value};

/// Parser that builds AST from events.
///
/// This is a simplified parser that consumes events and builds the AST.
/// Structural complexity is handled by the `Emitter`.
pub struct Parser<'events, 'input> {
    /// Event stream to consume
    events: &'events [Event<'input>],
    /// Current position in event stream
    pos: usize,
    /// Collected errors
    errors: Vec<ParseError>,
    /// Set of registered anchor names (for alias validation)
    /// Uses owned strings because events may contain `Cow::Owned` values
    anchors: HashSet<String>,
}

impl<'events, 'input> Parser<'events, 'input> {
    /// Create a new parser.
    #[must_use]
    pub fn new(events: &'events [Event<'input>]) -> Self {
        Self {
            events,
            pos: 0,
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
                properties, span, ..
            } => {
                self.advance();
                Some(self.parse_mapping(properties, span))
            }
            Event::SequenceStart {
                properties, span, ..
            } => {
                self.advance();
                Some(self.parse_sequence(properties, span))
            }
            Event::Scalar {
                style,
                value,
                properties,
                span,
            } => {
                self.advance();
                Some(self.build_scalar(style, value, properties, span))
            }
            Event::Alias { name, span } => {
                self.advance();
                Some(self.build_alias(name, span))
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
    ///
    /// Note: To match the hybrid parser, span calculation differs by style:
    /// - Block mappings: `start..last_value.end`
    /// - Flow mappings: `start..closing_brace.end`
    fn parse_mapping(&mut self, props: EventProperties<'input>, start_span: Span) -> Node<'input> {
        // Register anchor if present
        self.register_anchor(props.anchor.as_ref());

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

        // Span calculation to match hybrid parser:
        // - Flow collections: end_span has non-zero length (closing brace), use it
        // - Block collections: end_span has zero length, use last_value.end
        let start = start_span.start_usize();
        let end = if end_span.start == end_span.end {
            // Block: use last value's end
            pairs.last().map_or(start, |(_, val)| val.span.end_usize())
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
    /// Note: To match the hybrid parser, span calculation differs by style:
    /// - Block sequences: `start..last_item.end`
    /// - Flow sequences: `start..closing_bracket.end`
    fn parse_sequence(&mut self, props: EventProperties<'input>, start_span: Span) -> Node<'input> {
        // Register anchor if present
        self.register_anchor(props.anchor.as_ref());

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

        // Span calculation to match hybrid parser:
        // - Flow collections: end_span has non-zero length (closing bracket), use it
        // - Block collections: end_span has zero length, use last_item.end
        let start = start_span.start_usize();
        let end = if end_span.start == end_span.end {
            // Block: use last item's end
            items.last().map_or(start, |node| node.span.end_usize())
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
    /// Note: To match the hybrid parser, type inference is done for plain scalars
    /// BEFORE considering the tag. The hybrid parser does `parse_scalar()` (which
    /// calls `scalar_to_value`) first, then applies properties including tags.
    /// This means `!!str 42` results in `Int(42)` with a `!!str` tag.
    fn build_scalar(
        &mut self,
        style: ScalarStyle,
        value: Cow<'input, str>,
        props: EventProperties<'input>,
        span: Span,
    ) -> Node<'input> {
        // Register anchor if present
        self.register_anchor(props.anchor.as_ref());

        // Type inference applies to plain scalars (regardless of tag, to match hybrid parser)
        let typed_value = if style == ScalarStyle::Plain {
            Self::infer_type(value)
        } else {
            // Quoted/block scalars are always strings
            Value::String(value)
        };

        let node = Node::new(typed_value, span);
        Self::apply_properties(node, props)
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
    fn apply_properties(mut node: Node<'input>, props: EventProperties<'input>) -> Node<'input> {
        if props.anchor.is_some() || props.tag.is_some() {
            // Store just the values (without spans) in the node properties
            node.properties = Some(Box::new(NodeProperties {
                anchor: props.anchor.map(|prop| prop.value),
                tag: props.tag.map(|prop| prop.value),
            }));
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
    ///
    /// Takes a `Cow<'input, str>` to avoid unnecessary allocations when the value
    /// is inferred as a string (can return the Cow as-is).
    fn infer_type(value: Cow<'input, str>) -> Value<'input> {
        match value.as_ref() {
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
                match value.as_ref() {
                    ".inf" | ".Inf" | ".INF" => return Value::Float(f64::INFINITY),
                    "-.inf" | "-.Inf" | "-.INF" => return Value::Float(f64::NEG_INFINITY),
                    ".nan" | ".NaN" | ".NAN" => return Value::Float(f64::NAN),
                    _ => {}
                }
                // Default to string - return the Cow as-is (zero-copy if borrowed!)
                Value::String(value)
            }
        }
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

    use super::*;
    use crate::{Stream, error::ParseError};

    /// Use the standard parse function for tests.
    fn parse(input: &str) -> (Stream<'static>, Vec<ParseError>) {
        let (nodes, errors) = crate::parse(input);
        (nodes.into_iter().map(Node::into_owned).collect(), errors)
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
            assert!(matches!(&pair.0.value, Value::String(string) if string == "key"));
            assert!(matches!(&pair.1.value, Value::String(string) if string == "value"));
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
        assert!(matches!(&docs.first().unwrap().value, Value::Int(42)));

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
            let first_value = &pairs.first().unwrap().1;
            assert_eq!(
                first_value.anchor(),
                Some("anchor"),
                "First value should have anchor 'anchor'"
            );
            let alias_value = &pairs.last().unwrap().1.value;
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
        let mut parser = Parser::new(&events);
        parser
            .parse()
            .into_iter()
            .map(crate::value::Node::into_owned)
            .collect()
    }

    /// Test that Parser produces identical output to the hybrid parser.
    /// This is the key test ensuring we can remove node-building from the hybrid parser.
    #[test]
    #[allow(clippy::print_stderr, reason = "Debug output on failure")]
    fn test_event_parser_matches_hybrid_parser() {
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
            let (hybrid_nodes, _hybrid_errors) = crate::parse(input);
            let via_events_nodes = parse_via_events(input);

            // Compare node counts
            if hybrid_nodes.len() != via_events_nodes.len() {
                failures.push(format!(
                    "Input: {input:?}\n  Node count mismatch: hybrid={}, via_events={}",
                    hybrid_nodes.len(),
                    via_events_nodes.len()
                ));
                continue;
            }

            // Compare each node
            for (i, (hybrid, via_events)) in
                hybrid_nodes.iter().zip(via_events_nodes.iter()).enumerate()
            {
                if hybrid != via_events {
                    failures.push(format!(
                        "Input: {input:?}\n  Document {i} mismatch:\n    hybrid:     {hybrid:?}\n    via_events: {via_events:?}"
                    ));
                }
            }

            // We rely on the shared implementation of `parse` and the event-based
            // pipeline to keep error reporting behaviour aligned; this test
            // focuses on node equivalence.
        }

        if !failures.is_empty() {
            eprintln!("\n=== Parser vs Hybrid Parser Mismatches ===");
            for failure in &failures {
                eprintln!("{failure}\n");
            }
            panic!(
                "{} test case(s) failed - Parser output differs from hybrid parser",
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
            ("42", |v| matches!(v, Value::Int(42))),
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
            assert!(matches!(&pairs[0].0.value, Value::String(s) if s == "a"));
            assert!(matches!(&pairs[0].1.value, Value::Int(1)));
            // Second pair
            assert!(matches!(&pairs[1].0.value, Value::String(s) if s == "b"));
            assert!(matches!(&pairs[1].1.value, Value::Int(2)));
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
            assert!(matches!(&items[0].value, Value::String(s) if s == "a"));
            assert!(matches!(&items[1].value, Value::String(s) if s == "b"));
            assert!(matches!(&items[2].value, Value::String(s) if s == "c"));
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
            assert!(matches!(&items[0].value, Value::Int(1)));
            assert!(matches!(&items[1].value, Value::Int(2)));
            assert!(matches!(&items[2].value, Value::Int(3)));
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
            assert_eq!(items[0].anchor(), Some("anchor"));
            assert!(matches!(&items[0].value, Value::String(s) if s == "value"));
            // Second item is alias
            assert!(matches!(&items[1].value, Value::Alias(s) if s == "anchor"));
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
            assert!(matches!(&pairs[0].0.value, Value::String(s) if s == "outer"));
            // Check inner mapping
            if let Value::Mapping(inner_pairs) = &pairs[0].1.value {
                assert_eq!(inner_pairs.len(), 1);
                assert!(matches!(&inner_pairs[0].0.value, Value::String(s) if s == "inner"));
                assert!(matches!(&inner_pairs[0].1.value, Value::String(s) if s == "value"));
            } else {
                panic!("Expected inner mapping, got: {:?}", pairs[0].1.value);
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
            if let Value::Sequence(nested) = &items[0].value {
                assert_eq!(nested.len(), 2);
                assert!(matches!(&nested[0].value, Value::String(s) if s == "a"));
                assert!(matches!(&nested[1].value, Value::String(s) if s == "b"));
            } else {
                panic!("Expected nested sequence, got: {:?}", items[0].value);
            }
            // Second item is scalar
            assert!(matches!(&items[1].value, Value::String(s) if s == "c"));
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
            if let Value::Mapping(pairs) = &items[0].value {
                assert!(matches!(&pairs[0].0.value, Value::String(s) if s == "a"));
                assert!(matches!(&pairs[0].1.value, Value::Int(1)));
            } else {
                panic!("Expected mapping, got: {:?}", items[0].value);
            }
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }
}
