// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token wrapper.
//!
//! This module provides [`RichToken`] which wraps tokens with span information.

use crate::span::Span;
use crate::token::Token;

/// A token with its associated Span.
///
/// The lifetime `'input` refers to the input string being tokenized.
#[derive(Debug, Clone, PartialEq)]
pub struct RichToken<'input> {
    /// The actual token.
    pub token: Token<'input>,
    /// The source location of the token.
    pub span: Span,
}

impl<'input> RichToken<'input> {
    /// Create a new rich token.
    #[must_use]
    pub fn new(token: Token<'input>, span: Span) -> Self {
        Self { token, span }
    }
}

impl std::fmt::Display for RichToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.token.fmt(f)
    }
}
