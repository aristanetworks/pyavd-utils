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

mod context_lexer;
mod error;
mod lexer;
mod parser;
mod span;
mod stream_lexer;
mod token;
mod trivia;
mod value;

pub use context_lexer::tokenize_document;
pub use error::{ErrorKind, ParseError};
pub use lexer::{Token, tokenize};
pub use parser::{Stream, parse_single_document, parse_tokens};
pub use span::{Position, SourceMap, Span, Spanned};
pub use trivia::{RichToken, Trivia, TriviaKind};
pub use value::{Node, Value};

/// Parse YAML input and return the parsed documents and any errors encountered.
///
/// This function implements error recovery, so it may return partial values
/// even when errors are present. Each document in the stream is a separate
/// `Spanned<Value>`.
///
/// # Arguments
///
/// * `input` - The YAML source code to parse
///
/// # Returns
///
/// A tuple of:
/// - `Stream` (Vec<Spanned<Value>>) - The parsed documents with spans
/// - `Vec<ParseError>` - Any errors encountered during parsing (from both lexer and parser)
pub fn parse(input: &str) -> (Stream, Vec<ParseError>) {
    // Use layered parsing (context-aware lexing)
    parse_layered(input)
}

/// Parse YAML using the legacy single-pass lexer.
///
/// This uses the chumsky-based lexer which doesn't track flow context.
#[allow(dead_code, reason = "Kept for comparison and fallback")]
pub fn parse_legacy(input: &str) -> (Stream, Vec<ParseError>) {
    // Lexer phase
    let (tokens, mut errors) = tokenize(input);

    // Convert legacy tokens to RichToken format
    let rich_tokens: Vec<trivia::RichToken> = tokens
        .into_iter()
        .map(|(token, span)| trivia::RichToken {
            token,
            span,
            leading_trivia: Vec::new(),
            trailing_trivia: Vec::new(),
        })
        .collect();

    // Parser phase
    let (stream, parser_errors) = parse_tokens(&rich_tokens, input);
    errors.extend(parser_errors);

    (stream, errors)
}

/// Parse YAML using the layered architecture with context-aware lexing.
///
/// This separates stream-level parsing (document markers, directives) from
/// document-level parsing (context-aware tokenization based on flow depth).
pub fn parse_layered(input: &str) -> (Stream, Vec<ParseError>) {
    use chumsky::span::Span as _;

    let mut all_docs: Stream = Vec::new();
    let mut all_errors = Vec::new();

    // Layer 1: Parse stream into raw documents
    let (raw_docs, stream_errors) = stream_lexer::parse_stream(input);
    all_errors.extend(stream_errors);

    for raw_doc in raw_docs {
        // Layer 2: Tokenize document content with context awareness
        let (tokens, lexer_errors) = context_lexer::tokenize_document(&raw_doc.content);
        all_errors.extend(lexer_errors);

        // Layer 3: Parse tokens into a single document
        // Each raw document from the stream lexer should produce exactly one parsed document.
        // The stream lexer already handles document boundaries (--- and ...).
        // Pass the directives so the parser can validate tag handles.
        let (doc, errors) = parse_single_document(&tokens, &raw_doc.content, &raw_doc.directives);
        all_errors.extend(errors);

        // Check if document has explicit markers (--- or ...) in the content
        let has_explicit_marker = raw_doc.content.trim_start().starts_with("---")
            || raw_doc.content.trim_end().ends_with("...");

        if let Some(doc_) = doc {
            all_docs.push(doc_);
        } else if has_explicit_marker {
            // Empty explicit document -> produce null
            all_docs.push(Node::null(Span::new((), 0..0)));
        }
    }

    (all_docs, all_errors)
}

#[cfg(test)]
mod tests;
