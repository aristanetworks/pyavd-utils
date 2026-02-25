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
mod rich_token;
mod span;
mod stream_lexer;
mod token;
mod value;

pub use context_lexer::tokenize_document;
pub use error::{ErrorKind, ParseError};
pub use lexer::{Token, tokenize};
pub use parser::{Stream, parse_single_document, parse_tokens};
pub use rich_token::RichToken;
pub use span::{Position, SourceMap, Span, Spanned};
pub use value::{Node, Value};

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
/// [`context_lexer::tokenize_document`]. This returns `Node<'input>` with `Cow::Borrowed`
/// for scalars that don't require transformation.
///
/// Example of zero-copy usage:
/// ```ignore
/// let (raw_docs, _) = stream_lexer::parse_stream(input);
/// for raw_doc in raw_docs {
///     let (tokens, _) = context_lexer::tokenize_document(raw_doc.content);
///     let (node, _) = parse_single_document(&tokens, raw_doc.content, &raw_doc.directives);
///     // node borrows from raw_doc.content (which borrows from input)
///     // Call node.into_owned() when you need owned data
/// }
/// ```
pub fn parse(input: &str) -> (Stream<'static>, Vec<ParseError>) {
    use chumsky::span::Span as _;

    let mut all_docs: Stream<'static> = Vec::new();
    let mut all_errors = Vec::new();

    // Step 1: Parse stream into raw documents
    let (raw_docs, stream_errors) = stream_lexer::parse_stream(input);
    all_errors.extend(stream_errors);

    for raw_doc in raw_docs {
        // Step 2: Tokenize document content with context awareness
        let (tokens, lexer_errors) = context_lexer::tokenize_document(raw_doc.content);

        // Adjust error spans from local (relative to raw_doc.content) to global (relative to input)
        let doc_offset = raw_doc.content_span.start;
        all_errors.extend(lexer_errors.into_iter().map(|mut err| {
            err.span = Span::new((), err.span.start + doc_offset..err.span.end + doc_offset);
            err
        }));

        // Step 3: Parse tokens into a single document
        // Each raw document from the stream lexer should produce exactly one parsed document.
        // The stream lexer already handles document boundaries (--- and ...).
        // Pass the directives so the parser can validate tag handles.
        let (doc, errors) = parse_single_document(&tokens, raw_doc.content, &raw_doc.directives);
        all_errors.extend(errors.into_iter().map(|mut err| {
            err.span = Span::new((), err.span.start + doc_offset..err.span.end + doc_offset);
            err
        }));

        // Check if document has explicit markers (--- or ...) in the content
        let has_explicit_marker = raw_doc.content.trim_start().starts_with("---")
            || raw_doc.content.trim_end().ends_with("...");

        if let Some(doc_) = doc {
            // Convert to owned data since tokens are dropped at end of loop iteration
            all_docs.push(doc_.into_owned());
        } else if has_explicit_marker {
            // Empty explicit document -> produce null
            all_docs.push(Node::null(Span::new((), 0..0)));
        }
    }

    (all_docs, all_errors)
}

#[cfg(test)]
mod tests;
