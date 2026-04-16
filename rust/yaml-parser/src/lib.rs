// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! A YAML 1.2 parser with error recovery, span tracking, and optional serde support.
//!
//! `yaml-parser` is designed for tooling and configuration workloads where
//! callers need more than a simple "parse or fail" interface. The crate
//! exposes three public layers:
//!
//! - [`parse`] builds an owned AST with spans, comments, anchors, and tags.
//! - [`emit_events`] exposes a lower-level event stream for SAX-style consumers.
//! - [`writer`] and the optional [`serde`] module provide YAML output.
//!
//! # What This Crate Guarantees
//!
//! - Parsing is recovery-oriented: a call may return both parsed documents and
//!   errors.
//! - Top-level results are returned as owned [`Node<'static>`][Node] values, so
//!   they can outlive the input string.
//! - Every public AST node and event carries span information.
//! - Comments, anchors, and tags are preserved where they have a stable public
//!   representation.
//! - Empty input, whitespace-only input, and comments-only input produce zero
//!   documents and zero parse errors.
//! - Multi-document input is represented as one top-level [`Node`] per YAML
//!   document.
//!
//! # Choosing an API
//!
//! Use [`parse`] when you need:
//!
//! - the AST,
//! - spans and comments,
//! - anchors and tags on nodes,
//! - or partial results from malformed input.
//!
//! Use [`emit_events`] when you need:
//!
//! - event-stream access,
//! - scalar presentation style,
//! - flow vs block collection style,
//! - or explicit alias events.
//!
//! Use [`writer::write_yaml_from_events`] when you already have events and want
//! to emit valid YAML text.
//!
//! Enable the `serde` feature and use [`serde`] when you want direct
//! `T: DeserializeOwned` / `T: Serialize` integration.
//!
//! # High-Level Parsing
//!
//! ```
//! use yaml_parser::{Value, parse};
//!
//! let input = "name: jane\nactive: true\n";
//! let (docs, errors) = parse(input);
//!
//! assert!(errors.is_empty());
//! assert_eq!(docs.len(), 1);
//!
//! match &docs[0].value {
//!     Value::Mapping(pairs) => assert_eq!(pairs.len(), 2),
//!     other => panic!("expected mapping, got {other:?}"),
//! }
//! ```
//!
//! [`parse`] returns a [`Stream<'static>`][Stream], which is an alias for
//! `Vec<Node<'static>>`.
//!
//! Important behavior:
//!
//! - Errors do not imply an empty result.
//! - Root-level unresolved aliases are dropped from the AST.
//! - Unresolved aliases inside sequences or mappings cause only the affected
//!   item or mapping pair to be dropped while surrounding content is retained.
//!
//! # AST Model
//!
//! The AST is centered around [`Node`] and [`Value`].
//!
//! - [`Node`] stores a semantic value, a [`Span`], optional [`Properties`], and
//!   an optional trailing same-line [`Comment`].
//! - [`Value`] is one of null, bool, integer, float, string, sequence, or
//!   mapping.
//! - [`Integer`] preserves large integers without forcing numeric truncation.
//!   Values that do not fit supported integer widths are represented as
//!   [`Integer::BigIntStr`].
//! - [`SequenceItem`] and [`MappingPair`] expose both semantic nodes and
//!   structural spans.
//!
//! Comments are preserved in the AST in two places:
//!
//! - [`Node::trailing_comment`] for same-line comments such as
//!   `key: value # trailing`
//! - [`SequenceItem::header_comment`] and [`MappingPair::header_comment`] for
//!   comments attached to a header line before a nested block value
//!
//! Anchors and tags are modeled as node properties, not as wrapper nodes.
//! Aliases are resolved eagerly in the AST, so there is no `Value::Alias`
//! variant in the high-level tree.
//!
//! # Spans and Source Locations
//!
//! Source locations are byte-based.
//!
//! - [`Span`] is a half-open byte range `[start, end)`.
//! - [`Position`] uses 1-based line and column numbers.
//! - [`SourceMap`] converts byte offsets into line/column positions.
//!
//! ```
//! use yaml_parser::{SourceMap, parse};
//!
//! let input = "key: value\n";
//! let (docs, errors) = parse(input);
//! assert!(errors.is_empty());
//!
//! let map = SourceMap::new(input);
//! let pos = map.position(docs[0].span.start_usize());
//! assert_eq!((pos.line, pos.column), (1, 1));
//! ```
//!
//! # Event API
//!
//! [`emit_events`] exposes the lower-level event stream:
//!
//! ```
//! use yaml_parser::{Event, emit_events};
//!
//! let (events, errors) = emit_events("a: [1, 2]\n");
//! assert!(errors.is_empty());
//! assert!(matches!(events.first(), Some(Event::StreamStart)));
//! ```
//!
//! This layer preserves presentation details such as [`ScalarStyle`],
//! [`CollectionStyle`], anchors, tags, and aliases. On malformed input,
//! [`Event::InvalidatePair`] may appear as a recovery sentinel for
//! event-stream consumers.
//!
//! # Errors
//!
//! Parse failures are returned as [`ParseError`] values.
//!
//! - [`ParseError::kind`] identifies the failure category.
//! - [`ParseError::span`] points at the relevant source region.
//! - [`ErrorKind::suggestion`] provides a short fix-up hint for many error kinds.
//!
//! # Optional Serde Support
//!
//! Enable the `serde` feature to use [`serde::from_str`],
//! [`serde::from_reader`], [`serde::stream_from_str_docs`],
//! [`serde::to_string`], and [`serde::to_writer`].
//!
//! - `from_str` expects exactly one document.
//! - `stream_from_str_docs` is the multi-document API.
//! - Anchor state is document-scoped when streaming across documents.
//! - `from_reader` currently reads the full input into memory before parsing.
//!
//! # See Also
//!
//! - `README.md` for a short crate overview
//! - `ARCHITECTURE.md` for implementation details

mod ast_event;
#[cfg(feature = "avdschema")]
mod avdschema_data_impl;
mod emitter;
mod error;
mod event;
mod lexer;
mod parser;
mod span;
mod stream;
mod value;

#[cfg(feature = "serde")]
mod ast_events;

pub mod writer;

#[cfg(feature = "serde")]
pub mod serde;

// Public API: high-level parsing, AST, spans, and errors.
pub use error::{ErrorKind, ParseError};
pub use event::{CollectionStyle, Event, ScalarStyle};
pub use span::{
    BytePosition, IndentLevel, Position, SourceMap, Span, Spanned, pos_to_usize, usize_to_indent,
    usize_to_pos,
};
pub use stream::Stream;
pub use value::{Comment, Integer, MappingPair, Node, Properties, Property, SequenceItem, Value};

/// Parse YAML input and return the parsed documents and any errors encountered.
///
/// This function implements error recovery, so it may return partial values
/// even when errors are present. Each top-level item in the returned stream is
/// a separate YAML document represented as a [`Node`].
///
/// This function returns owned data (`Node<'static>`) for convenience. The
/// returned nodes can outlive the input string. If you need event-level access
/// without building the AST, use [`emit_events`]. If you need serde-driven
/// deserialization, use [`serde::from_str`] when the `serde` feature is enabled.
///
/// # Arguments
///
/// * `input` - The YAML source code to parse
///
/// # Returns
///
/// A tuple of:
/// - `Stream<'static>` (`Vec<Node<'static>>`) containing the parsed documents
/// - `Vec<ParseError>` containing any errors reported by the lexer, emitter, or AST parser
///
/// # Architecture
///
/// Internally this function uses the three-layer architecture described in
/// `ARCHITECTURE.md`:
/// 1. Tokenize input with the unified lexer
/// 2. Emit events with the internal event emitter
/// 3. Build the AST with the event-to-AST parser
///
/// # Example
///
/// ```
/// # use yaml_parser::parse;
/// let input = "key: value";
/// let (nodes, errors) = parse(input);
/// // nodes can outlive input
/// drop(input);
/// assert_eq!(nodes.len(), 1);
/// ```
pub fn parse(input: &str) -> (Stream<'static>, Vec<ParseError>) {
    // 1. Create emitter with streaming lexer (lexes on demand, no pre-buffering).
    let mut emitter = emitter::Emitter::new(input);

    // 2. Feed events from emitter directly into the event-to-AST parser.
    //    This avoids building an intermediate Vec<Event>.
    let mut all_errors = Vec::new();
    let nodes = {
        let mut ast_emitter = emitter.ast_events();
        let mut parser = parser::Parser::new(&mut ast_emitter);
        let nodes = parser.parse();
        all_errors.extend(parser.take_errors());
        nodes
    };
    all_errors.extend(emitter.take_errors());

    // 3. Convert to owned so callers get `Node<'static>` values that can
    //    outlive the input string.
    let all_docs: Stream<'static> = nodes.into_iter().map(Node::into_owned).collect();

    (all_docs, all_errors)
}

/// Emit raw YAML events from input without building an AST.
///
/// This is an advanced API intended primarily for tests and tooling.
/// Typical library users should prefer [`parse`], which builds a typed AST.
/// Use `emit_events` when you need:
/// - Direct access to the YAML event stream (SAX-style processing)
/// - Integration with the YAML Test Suite event format
/// - Custom tooling for round-tripping or formatting based on events
///
/// The event stream follows the YAML Test Suite format:
/// `StreamStart`, `DocumentStart`, content events, `DocumentEnd`, `StreamEnd`
///
/// When the input contains parse errors, [`Event::InvalidatePair`] may also
/// appear in the stream. It is not part of the test-suite format; consumers
/// should skip it and treat the preceding key event as orphaned.
///
/// This function performs zero-copy parsing where possible: event payloads
/// borrow string data directly from the input. Escaped strings and processed
/// block scalars allocate only when transformation is required.
///
/// # Returns
///
/// A tuple of:
/// - `Vec<Event<'_>>` - The emitted events (borrowing from input)
/// - `Vec<ParseError>` - Any errors encountered during lexing/parsing
#[must_use]
pub fn emit_events(input: &str) -> (Vec<Event<'_>>, Vec<ParseError>) {
    // Create emitter with streaming lexer (lexes on demand, no pre-buffering).
    let mut emitter = emitter::Emitter::new(input);

    // Collect events and errors
    let events: Vec<Event<'_>> = emitter.by_ref().collect();
    let all_errors = emitter.take_errors();

    (events, all_errors)
}

#[cfg(test)]
mod tests;
