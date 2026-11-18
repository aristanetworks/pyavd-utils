// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Self-contained input text and borrowing AST storage.

#![allow(
    clippy::mem_forget,
    reason = "self_cell internally uses mem::forget to safely construct its self-referential owner"
)]

use std::fmt;
use std::sync::Arc;

use self_cell::self_cell;

use crate::Node;
use crate::ParseError;
use crate::Stream;

struct ParsedStream<'input> {
    documents: Stream<'input>,
    errors: Vec<ParseError>,
}

self_cell!(
    struct ParsedYamlCell {
        owner: String,

        #[covariant]
        dependent: ParsedStream,
    }
);

/// A parsed YAML stream owning its input text, AST, and parse errors.
///
/// AST strings borrow directly from the retained input where possible. Text
/// requiring transformation, such as escaped strings and folded block scalars,
/// is owned by the AST. One object holds all documents in a YAML stream.
///
/// Parsing performs error recovery: documents and errors may both be present.
/// Access is immutable, and references to the AST cannot outlive this object.
/// The object itself can be moved without invalidating its internal borrows.
///
/// ```
/// use yaml_parser::ParsedYaml;
///
/// let yaml = String::from("name: jane\n---\nactive: true\n");
/// let parsed = ParsedYaml::parse(yaml);
/// assert!(parsed.errors().is_empty());
/// assert_eq!(parsed.documents().len(), 2);
/// assert!(parsed.input().starts_with("name: jane"));
/// ```
///
/// Borrowed nodes must not outlive their owning parsed YAML:
///
/// ```compile_fail
/// use yaml_parser::ParsedYaml;
///
/// let document = {
///     let parsed = ParsedYaml::parse(String::from("hello"));
///     &parsed.documents()[0]
/// };
/// println!("{document:?}");
/// ```
pub struct ParsedYaml {
    cell: ParsedYamlCell,
}

impl fmt::Debug for ParsedYaml {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParsedYaml")
            .field("input", &self.input())
            .field("documents", &self.documents())
            .field("errors", &self.errors())
            .finish()
    }
}

impl ParsedYaml {
    /// Parse owned YAML text without copying its input buffer.
    ///
    /// Returns partial documents alongside any parse errors. Empty,
    /// whitespace-only, or comments-only input produces no documents or errors.
    pub fn parse(input: String) -> Self {
        Self::parse_with_source(input, None)
    }

    /// Parse owned YAML text and attach a shared source identifier to every node.
    ///
    /// The identifier is diagnostic metadata, distinct from the YAML text
    /// returned by [`Self::input`]. It is shared with mapping keys as well as
    /// values, and is not emitted when serializing nodes.
    pub fn parse_with_source(input: String, source: Option<Arc<str>>) -> Self {
        Self {
            cell: ParsedYamlCell::new(input, |input| {
                let (documents, errors) = crate::parse_borrowed(input, source);
                ParsedStream { documents, errors }
            }),
        }
    }

    /// Return the original YAML text, retained for the lifetime of this object.
    pub fn input(&self) -> &str {
        self.cell.borrow_owner()
    }

    /// Return the parsed documents, whose string content may borrow from the input.
    pub fn documents(&self) -> &[Node<'_>] {
        &self.cell.borrow_dependent().documents
    }

    /// Return parse errors with byte spans relative to [`Self::input`].
    pub fn errors(&self) -> &[ParseError] {
        &self.cell.borrow_dependent().errors
    }
}

#[cfg(test)]
mod tests;
