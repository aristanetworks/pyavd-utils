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

mod error;
mod event;
mod event_parser;
mod lexer;
mod parser;
mod span;
mod value;

pub use error::{ErrorKind, ParseError};
pub use event::{CollectionStyle, Event, ScalarStyle};
pub use lexer::{RichToken, Token, tokenize_document};
pub use parser::{Stream, parse_single_document, parse_tokens};
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
/// This function returns owned data (`Node<'static>`) for convenience. If you need
/// zero-copy parsing to avoid allocations, use `parse_single_document` directly
/// with your own token storage.
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
/// # Owned vs Zero-Copy
///
/// This function returns **owned data** (`Node<'static>`) for convenience and ease of use.
/// All string data in the returned nodes is `Cow::Owned`, meaning there are no borrows
/// from the input string. This allows the returned `Stream` to outlive the input.
///
/// ## Zero-Copy Alternative
///
/// For zero-copy parsing where nodes borrow directly from the input string (avoiding
/// allocations for simple scalars), use [`parse_single_document`] with tokens from
/// [`tokenize_document`]. This returns `Node<'input>` with `Cow::Borrowed`
/// for scalars that don't require transformation.
///
/// See `test_zero_copy_parsing` in the test module for a usage example.
pub fn parse(input: &str) -> (Stream<'static>, Vec<ParseError>) {
    // Phase 3: Use event-based parsing exclusively
    // The parser emits events, and EventParser reconstructs the AST from them.
    // This is now the canonical path - see DESIGN_EVENT_LAYER.md
    parse_via_events(input)
}

/// Parse YAML using the event-based architecture.
///
/// This function uses the [`EventParser`] to build AST from the events emitted
/// by the parser. It has the same interface as [`parse`].
///
/// Note: The regular [`parse`] function also emits events now, so this function
/// primarily exists for testing the [`EventParser`] reconstruction path.
#[must_use]
pub fn parse_via_events(input: &str) -> (Stream<'static>, Vec<ParseError>) {
    use event_parser::EventParser;

    // Get events from the parser
    let (events, mut all_errors) = emit_events(input);

    // Parse events into AST using EventParser
    let mut event_parser = EventParser::new(&events);
    let nodes = event_parser.parse();
    all_errors.extend(event_parser.take_errors());

    // Convert to owned
    let all_docs: Stream<'static> = nodes.into_iter().map(|n| n.into_owned()).collect();

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
/// # Returns
///
/// A tuple of:
/// - `Vec<Event<'static>>` - The emitted events (owned)
/// - `Vec<ParseError>` - Any errors encountered during lexing/parsing
#[must_use]
pub fn emit_events(input: &str) -> (Vec<Event<'static>>, Vec<ParseError>) {
    // Unified single-pass lexing: DocumentLexer handles directives, document
    // markers, and content in a single pass over the input.
    let lexer = lexer::DocumentLexer::new(input);
    let tokens: Vec<lexer::RichToken<'_>> = lexer.collect();

    // Extract errors from tokens
    let mut all_errors: Vec<ParseError> = tokens
        .iter()
        .filter_map(|rich_token| {
            rich_token
                .error
                .clone()
                .map(|kind| error::ParseError::new(kind, rich_token.span))
        })
        .collect();

    // Parse the token stream into events
    let (events, parser_errors) = parser::parse_stream_from_tokens(&tokens, input);
    all_errors.extend(parser_errors);

    // Convert to owned events
    let owned_events: Vec<Event<'static>> = events.into_iter().map(Event::into_owned).collect();

    (owned_events, all_errors)
}

#[cfg(test)]
mod tests;
