// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! A YAML 1.2 parser with error recovery, span tracking, and optional serde support.
//!
//! This crate provides a YAML parser that:
//! - Recovers from syntax errors and continues parsing
//! - Reports multiple errors in a single pass
//! - Tracks source spans for all values
//! - Targets broad YAML 1.2 compatibility for parsing, tooling, and serde use
//!
//! # Example
//!
//! ```
//! use yaml_parser::{parse, Node, Value};
//!
//! let input = r#"
//! name: John
//! age: 30
//! "#;
//!
//! let (docs, errors) = parse(input);
//!
//! if errors.is_empty() {
//!     println!("Parsed {} documents", docs.len());
//!     for node in &docs {
//!         println!("Document at {:?}: {:?}", node.span, node.value);
//!     }
//! } else {
//!     for error in &errors {
//!         eprintln!("Error: {:?}", error);
//!     }
//!     // Note: docs may still contain partial data
//! }
//! ```

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
pub use span::{Position, SourceMap, Span, Spanned};
pub use stream::Stream;
pub use value::{Node, Number, Properties, Value};

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
        let mut parser = parser::Parser::new(&mut emitter);
        let nodes = parser.parse();
        all_errors.extend(parser.take_errors());
        nodes
    };
    all_errors.extend(emitter.take_errors());

    // 3. Convert to owned so callers get `Node<'static>` values that can
    //    outlive the input string.
    let all_docs: Stream<'static> = nodes.into_iter().map(value::Node::into_owned).collect();

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
