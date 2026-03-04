// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Lexer components for YAML parsing.
//!
//! This module contains the two-layer lexer architecture:
//!
//! - **Stream lexer** (`stream`): Splits input into raw documents based on
//!   directives (`%YAML`, `%TAG`) and document markers (`---`, `...`).
//!
//! - **Context lexer** (`context`): Performs context-aware tokenization,
//!   tracking flow depth to distinguish block vs flow context.
//!
//! Also includes token definitions used by the lexers.

mod context;
mod rich_token;
mod stream;
mod token;

// Re-export main types and functions
pub use context::tokenize_document;
pub use rich_token::RichToken;
pub use stream::{Directive, tokenize_stream};
pub use token::{BlockScalarHeader, Chomping, QuoteStyle, Token};
