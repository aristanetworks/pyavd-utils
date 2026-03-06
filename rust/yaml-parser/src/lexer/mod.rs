// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Lexer components for YAML parsing.
//!
//! This module contains the unified streaming lexer architecture:
//!
//! - **Document lexer** (`document`): Tokenizes YAML content including
//!   directives (`%YAML`, `%TAG`), document markers (`---`, `...`), and
//!   all document content. Tracks flow depth to distinguish block vs flow context.
//!
//! Also includes token definitions used by the lexer.

mod document;
mod rich_token;
mod token;

// Re-export main types and functions
pub use document::{DocumentLexer, tokenize_document};
pub use rich_token::RichToken;
pub use token::{BlockScalarHeader, Chomping, QuoteStyle, Token};
