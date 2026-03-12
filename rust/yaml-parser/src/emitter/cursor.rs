// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token cursor utilities for the YAML emitter.
//!
//! This module provides a small helper type that encapsulates read-only
//! navigation over a slice of [`RichToken`] values. The goal is to keep
//! low-level token access logic (peek, lookahead, EOF checks, span
//! computation) separate from the higher-level emitter state machine.
//!
//! The cursor is intentionally minimal and does **not** own any parsing
//! state (indentation, flow depth, etc.). It is purely a thin wrapper
//! around the token slice and is used by `Emitter` to implement its
//! token-access helpers.

use crate::lexer::{RichToken, Token};
use crate::span::Span;

/// Read-only view over the token stream used by the emitter.
#[derive(Debug)]
pub(crate) struct TokenCursor<'tokens, 'input> {
    tokens: &'tokens [RichToken<'input>],
}

impl<'tokens, 'input> TokenCursor<'tokens, 'input> {
    /// Create a new cursor over the given token slice.
    #[must_use]
    pub(crate) fn new(tokens: &'tokens [RichToken<'input>]) -> Self {
        Self { tokens }
    }

    /// Peek at the current token at `pos` without advancing.
    #[must_use]
    pub(crate) fn peek(&self, pos: usize) -> Option<(&Token<'input>, Span)> {
        self.tokens.get(pos).map(|rt| (&rt.token, rt.span))
    }

    /// Peek `n` tokens ahead (0-based offset from `pos`).
    #[must_use]
    pub(crate) fn peek_nth(&self, pos: usize, n: usize) -> Option<(&Token<'input>, Span)> {
        self.tokens.get(pos + n).map(|rt| (&rt.token, rt.span))
    }

    /// Return `true` if `pos` is at or past the end of the token stream.
    #[must_use]
    pub(crate) fn is_eof(&self, pos: usize) -> bool {
        pos >= self.tokens.len()
    }

    /// Get the span for the token at `pos`, or a zero-width span at the end
    /// of the last token if `pos` is at EOF.
    #[must_use]
    pub(crate) fn current_span(&self, pos: usize) -> Span {
        if let Some(rt) = self.tokens.get(pos) {
            return rt.span;
        }

        // At EOF, return span at end of last token.
        self.tokens
            .last()
            .map_or(Span::from_usize_range(0..0), |rt| {
                Span::from_usize_range(rt.span.end_usize()..rt.span.end_usize())
            })
    }
}
