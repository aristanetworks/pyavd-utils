// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token wrapper.
//!
//! This module provides [`RichToken`] which wraps tokens with span information.

use crate::error::ErrorKind;
use crate::span::Span;

use super::token::Token;

/// A token with its associated Span and optional error.
///
/// The lifetime `'input` refers to the input string being tokenized.
///
/// When an error occurs during lexing, the lexer attempts to recover and
/// produce a token anyway. The error is attached to the token via the `error`
/// field, allowing consumers to handle errors inline without interrupting
/// the token stream.
#[derive(Debug, Clone, PartialEq)]
pub struct RichToken<'input> {
    /// The actual token.
    pub token: Token<'input>,
    /// The source location of the token.
    pub span: Span,
    /// Optional error associated with this token.
    /// When present, indicates the token was produced via error recovery.
    /// The error's span is the same as the token's span.
    pub error: Option<ErrorKind>,
}

impl<'input> RichToken<'input> {
    /// Create a new rich token without an error.
    #[must_use]
    pub fn new(token: Token<'input>, span: Span) -> Self {
        Self {
            token,
            span,
            error: None,
        }
    }

    /// Create a new rich token with an associated error.
    #[must_use]
    pub fn with_error(token: Token<'input>, span: Span, error: ErrorKind) -> Self {
        Self {
            token,
            span,
            error: Some(error),
        }
    }

    /// Check if this token has an associated error.
    #[must_use]
    pub fn has_error(&self) -> bool {
        self.error.is_some()
    }
}

impl std::fmt::Display for RichToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.token.fmt(f)
    }
}
