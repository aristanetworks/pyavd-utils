// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! A YAML 1.2 parser with error recovery.
//!
//! This crate provides a YAML parser that:
//! - Recovers from syntax errors and continues parsing
//! - Reports multiple errors in a single pass
//! - Tracks source spans for all values
//! - Aims for full YAML 1.2 compliance
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
mod value;

pub use emitter::{Emitter, Stream};
pub use error::{ErrorKind, ParseError};
pub use event::{CollectionStyle, Event, ScalarStyle};
pub use lexer::{Lexer, RichToken, Token, tokenize_document};
pub use parser::Parser;
pub use span::{
    BytePosition, IndentLevel, Position, SourceMap, Span, Spanned, indent_to_usize, pos_to_usize,
    usize_to_indent, usize_to_pos,
};
pub use value::{Node, Properties, Value};

/// Parse YAML input and return the parsed documents and any errors encountered.
///
/// This function implements error recovery, so it may return partial values
/// even when errors are present. Each document in the stream is a separate
/// `Spanned<Value>`.
///
/// This function returns owned data (`Node<'static>`) for convenience. The returned
/// nodes can outlive the input string. For zero-copy parsing, use the lower-level
/// API: [`tokenize_document`], [`Emitter`], and [`Parser`] directly.
///
/// # Arguments
///
/// * `input` - The YAML source code to parse
///
/// # Returns
///
/// A tuple of:
/// - `Stream<'static>` (Vec<Node<'static>>) - The parsed documents with owned data
/// - `Vec<ParseError>` - Any errors encountered during parsing (from both lexer and parser)
///
/// # Architecture
///
/// This function uses the event-based architecture:
/// 1. Tokenize input with [`Lexer`]
/// 2. Emit events with [`Emitter`]
/// 3. Build AST with [`Parser`]
///
/// See `DESIGN_EVENT_LAYER.md` for details.
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
    // Get events from the emitter
    let (events, mut all_errors) = emit_events(input);

    // Parse events into AST using Parser
    let mut parser = Parser::new(&events);
    let nodes = parser.parse();
    all_errors.extend(parser.take_errors());

    // Convert to owned
    let all_docs: Stream<'static> = nodes.into_iter().map(value::Node::into_owned).collect();

    (all_docs, all_errors)
}

/// Emit raw YAML events from input without building an AST.
///
/// This function tokenizes the input and runs the parser in event-emitting mode
/// to produce a stream of events. This is useful for:
/// - Testing event emission against the YAML test suite
/// - Streaming/SAX-style processing
/// - Round-tripping YAML documents
///
/// The event stream follows the YAML Test Suite format:
/// `StreamStart`, `DocumentStart`, content events, `DocumentEnd`, `StreamEnd`
///
/// This function performs zero-copy parsing where possible - events borrow string
/// data directly from the input. Escaped strings and processed block scalars will
/// be allocated as needed.
///
/// # Returns
///
/// A tuple of:
/// - `Vec<Event<'_>>` - The emitted events (borrowing from input)
/// - `Vec<ParseError>` - Any errors encountered during lexing/parsing
#[must_use]
pub fn emit_events(input: &str) -> (Vec<Event<'_>>, Vec<ParseError>) {
    // Unified single-pass lexing: Lexer handles directives, document
    // markers, and content in a single pass over the input.
    let mut lexer = lexer::Lexer::new(input);
    let tokens: Vec<lexer::RichToken<'_>> = lexer.by_ref().collect();

    // Collect errors from lexer
    let mut all_errors = lexer.take_errors();

    // Parse the token stream into events using the emitter
    let mut emitter = emitter::Emitter::new(&tokens, input);
    let events: Vec<Event<'_>> = emitter.by_ref().collect();
    all_errors.extend(emitter.take_errors());

    (events, all_errors)
}

#[cfg(test)]
mod tests;
