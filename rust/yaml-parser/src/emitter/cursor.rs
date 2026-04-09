// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Token cursor utilities for the YAML emitter.
//!
//! This module provides a small helper type that encapsulates navigation
//! over the token stream used by the emitter. The goal is to keep
//! low-level token access logic (peek, lookahead, EOF checks, span
//! computation) separate from the higher-level emitter state machine.
//!
//! In the streaming design, the cursor owns a [`Lexer`] and pulls
//! [`RichToken`] values on demand, buffering them as needed to satisfy
//! lookahead.
//!
//! ## Interior Mutability
//!
//! The cursor uses `RefCell` for interior mutability, allowing shared
//! (`&self`) access to peek methods. This is necessary because the emitter
//! frequently needs to peek while holding other references to self (e.g.,
//! `self.error(..., self.current_span())`).

use std::cell::{Cell, RefCell};

use crate::error::ParseError;
use crate::lexer::{Lexer, RichToken, Token, TokenKind};
use crate::span::Span;

/// Borrowed lookahead window over buffered tokens.
///
/// This lets hot emitter scanners inspect a small token run while paying the
/// `RefCell` borrow cost only once for the whole scan instead of once per
/// `peek_kind_nth()` / `peek_nth_with()` step.
pub(crate) struct LookaheadWindow<'a, 'input> {
    tokens: &'a [RichToken<'input>],
}

impl<'input> LookaheadWindow<'_, 'input> {
    #[inline]
    #[must_use]
    pub(crate) fn kind(&self, offset: usize) -> Option<TokenKind> {
        self.tokens.get(offset).map(|rt| TokenKind::from(&rt.token))
    }

    #[inline]
    #[must_use]
    pub(crate) fn token(&self, offset: usize) -> Option<&Token<'input>> {
        self.tokens.get(offset).map(|rt| &rt.token)
    }
}

/// Streaming view over the token stream used by the emitter.
pub(crate) struct TokenCursor<'input> {
    lexer: RefCell<Lexer<'input>>,
    buffer: RefCell<Vec<RichToken<'input>>>,
    eof: Cell<bool>,
}

impl<'input> TokenCursor<'input> {
    /// Create a new cursor from the raw input string.
    #[must_use]
    pub(crate) fn new(input: &'input str) -> Self {
        Self {
            lexer: RefCell::new(Lexer::new(input)),
            buffer: RefCell::new(Vec::new()),
            eof: Cell::new(false),
        }
    }

    /// Ensure that the buffer contains a token at `index`, if possible.
    fn ensure_available(&self, index: usize) {
        if self.eof.get() {
            return;
        }

        loop {
            {
                let buffer = self.buffer.borrow();
                if buffer.len() > index {
                    return;
                }
            }

            let mut lexer = self.lexer.borrow_mut();
            if let Some(rt) = lexer.next() {
                drop(lexer);
                self.buffer.borrow_mut().push(rt);
            } else {
                self.eof.set(true);
                return;
            }
        }
    }

    /// Drain the lexer to EOF, buffering all remaining tokens.
    fn drain_to_end(&self) {
        if self.eof.get() {
            return;
        }

        loop {
            let mut lexer = self.lexer.borrow_mut();
            if let Some(rt) = lexer.next() {
                drop(lexer);
                self.buffer.borrow_mut().push(rt);
            } else {
                self.eof.set(true);
                return;
            }
        }
    }

    /// Peek at the current token at `pos` without advancing.
    #[inline]
    #[must_use]
    pub(crate) fn peek(&self, pos: usize) -> Option<(Token<'input>, Span)> {
        self.ensure_available(pos);
        let buffer = self.buffer.borrow();
        buffer.get(pos).map(|rt| (rt.token.clone(), rt.span))
    }

    /// Borrow a buffered lookahead window starting at `pos`.
    ///
    /// Callers provide the furthest offset they may inspect so the cursor can
    /// fill the buffer before taking a shared borrow across the window.
    #[inline]
    pub(crate) fn with_window<F, R>(&self, pos: usize, max_offset: usize, func: F) -> R
    where
        F: FnOnce(LookaheadWindow<'_, 'input>) -> R,
    {
        self.ensure_available(pos.saturating_add(max_offset));
        let buffer = self.buffer.borrow();
        let start = pos.min(buffer.len());
        func(LookaheadWindow {
            tokens: buffer.get(start..).unwrap_or(&[]),
        })
    }

    /// Take ownership of the token at `pos`, replacing it with a dummy.
    ///
    /// This is more efficient than `peek()` when you're consuming the token
    /// and won't need it again. The token is replaced with `Token::Whitespace`
    /// as a cheap sentinel to avoid leaving uninitialized memory.
    ///
    /// # Panics
    /// Panics if `pos` is out of bounds.
    #[inline]
    pub(crate) fn take(&self, pos: usize) -> Option<(Token<'input>, Span)> {
        self.ensure_available(pos);
        let mut buffer = self.buffer.borrow_mut();
        buffer.get_mut(pos).map(|rt| {
            // Replace with a zero-size sentinel token
            let token = std::mem::replace(&mut rt.token, Token::Whitespace);
            (token, rt.span)
        })
    }

    /// Peek at the current token at `pos` and apply a function to it.
    ///
    /// This is more efficient than `peek()` when you don't need to keep
    /// the token, as it avoids cloning `Cow<str>` data.
    #[inline]
    pub(crate) fn peek_with<F, R>(&self, pos: usize, func: F) -> Option<R>
    where
        F: FnOnce(&Token<'input>, Span) -> R,
    {
        self.ensure_available(pos);
        let buffer = self.buffer.borrow();
        buffer.get(pos).map(|rt| func(&rt.token, rt.span))
    }

    /// Peek at the token `n` ahead and apply a function to it.
    #[inline]
    pub(crate) fn peek_nth_with<F, R>(&self, pos: usize, n: usize, func: F) -> Option<R>
    where
        F: FnOnce(&Token<'input>, Span) -> R,
    {
        let index = pos + n;
        self.ensure_available(index);
        let buffer = self.buffer.borrow();
        buffer.get(index).map(|rt| func(&rt.token, rt.span))
    }

    /// Peek at the token kind at `pos` without cloning the token.
    ///
    /// This is more efficient than `peek()` when you only need to check
    /// the token discriminant (e.g., for `matches!` patterns).
    #[inline]
    #[must_use]
    pub(crate) fn peek_kind(&self, pos: usize) -> Option<TokenKind> {
        self.ensure_available(pos);
        let buffer = self.buffer.borrow();
        buffer.get(pos).map(|rt| TokenKind::from(&rt.token))
    }

    /// Peek at the token kind `n` tokens ahead without cloning.
    #[inline]
    #[must_use]
    pub(crate) fn peek_kind_nth(&self, pos: usize, n: usize) -> Option<TokenKind> {
        let index = pos + n;
        self.ensure_available(index);
        let buffer = self.buffer.borrow();
        buffer.get(index).map(|rt| TokenKind::from(&rt.token))
    }

    /// Peek `n` tokens ahead (0-based offset from `pos`).
    #[inline]
    #[must_use]
    pub(crate) fn peek_nth(&self, pos: usize, n: usize) -> Option<(Token<'input>, Span)> {
        let index = pos + n;
        self.ensure_available(index);
        let buffer = self.buffer.borrow();
        buffer.get(index).map(|rt| (rt.token.clone(), rt.span))
    }

    /// Return `true` if `pos` is at or past the end of the token stream.
    #[inline]
    #[must_use]
    pub(crate) fn is_eof(&self, pos: usize) -> bool {
        self.ensure_available(pos);
        let buffer_len = self.buffer.borrow().len();
        pos >= buffer_len
    }

    /// Get the span for the token at `pos`, or a zero-width span at the end
    /// of the last token if `pos` is at EOF.
    #[inline]
    #[must_use]
    pub(crate) fn current_span(&self, pos: usize) -> Span {
        self.ensure_available(pos);
        let buffer = self.buffer.borrow();
        if let Some(rt) = buffer.get(pos) {
            return rt.span;
        }

        // At EOF, return span at end of last token.
        buffer.last().map_or(Span::from_usize_range(0..0), |rt| {
            Span::from_usize_range(rt.span.end_usize()..rt.span.end_usize())
        })
    }

    /// Take collected lexer errors, draining the lexer to EOF first so that
    /// all pending errors are reported.
    #[must_use]
    pub(crate) fn take_lexer_errors(&self) -> Vec<ParseError> {
        self.drain_to_end();
        self.lexer.borrow_mut().take_errors()
    }
}
