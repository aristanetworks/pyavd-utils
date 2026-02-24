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
/// - `Stream` (Vec<Node>) - The parsed documents with owned data
/// - `Vec<ParseError>` - Any errors encountered during parsing (from both lexer and parser)
///
/// # Owned vs Zero-Copy
///
/// This function returns owned data (`Node<'static>`) for convenience.
/// For zero-copy parsing where nodes borrow directly from the input,
/// use [`parse_single_document`] with tokens from [`tokenize_document`].
///
/// The nodes contain `Cow<'static, str>` where:
/// - Simple scalars that required no transformation are `Cow::Owned` (converted from borrowed)
/// - Scalars that needed transformation (escapes, multiline) are `Cow::Owned`
///
/// To convert individual nodes to owned, use [`Node::into_owned()`].
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
        all_errors.extend(lexer_errors);

        // Step 3: Parse tokens into a single document
        // Each raw document from the stream lexer should produce exactly one parsed document.
        // The stream lexer already handles document boundaries (--- and ...).
        // Pass the directives so the parser can validate tag handles.
        let (doc, errors) = parse_single_document(&tokens, raw_doc.content, &raw_doc.directives);
        all_errors.extend(errors);

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
