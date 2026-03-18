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
use crate::lexer::{Lexer, RichToken, Token};
use crate::span::Span;

/// Token discriminant for cheap pattern matching without cloning.
///
/// This enum mirrors the variants of `Token` but contains no data,
/// allowing O(1) comparisons without allocating or cloning strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenKind {
    BlockSeqIndicator,
    MappingKey,
    Colon,
    FlowMapStart,
    FlowMapEnd,
    FlowSeqStart,
    FlowSeqEnd,
    Comma,
    DocStart,
    DocEnd,
    Plain,
    StringStart,
    StringEnd,
    StringContent,
    LiteralBlockHeader,
    FoldedBlockHeader,
    Anchor,
    Alias,
    Tag,
    YamlDirective,
    TagDirective,
    ReservedDirective,
    LineStart,
    Whitespace,
    WhitespaceWithTabs,
    Comment,
    Indent,
    Dedent,
}

impl<'input> From<&Token<'input>> for TokenKind {
    #[inline]
    fn from(token: &Token<'input>) -> Self {
        match token {
            Token::BlockSeqIndicator => Self::BlockSeqIndicator,
            Token::MappingKey => Self::MappingKey,
            Token::Colon => Self::Colon,
            Token::FlowMapStart => Self::FlowMapStart,
            Token::FlowMapEnd => Self::FlowMapEnd,
            Token::FlowSeqStart => Self::FlowSeqStart,
            Token::FlowSeqEnd => Self::FlowSeqEnd,
            Token::Comma => Self::Comma,
            Token::DocStart => Self::DocStart,
            Token::DocEnd => Self::DocEnd,
            Token::Plain(_) => Self::Plain,
            Token::StringStart(_) => Self::StringStart,
            Token::StringEnd(_) => Self::StringEnd,
            Token::StringContent(_) => Self::StringContent,
            Token::LiteralBlockHeader(_) => Self::LiteralBlockHeader,
            Token::FoldedBlockHeader(_) => Self::FoldedBlockHeader,
            Token::Anchor(_) => Self::Anchor,
            Token::Alias(_) => Self::Alias,
            Token::Tag(_) => Self::Tag,
            Token::YamlDirective(_) => Self::YamlDirective,
            Token::TagDirective(_, _) => Self::TagDirective,
            Token::ReservedDirective(_) => Self::ReservedDirective,
            Token::LineStart(_) => Self::LineStart,
            Token::Whitespace => Self::Whitespace,
            Token::WhitespaceWithTabs => Self::WhitespaceWithTabs,
            Token::Comment(_) => Self::Comment,
            Token::Indent(_) => Self::Indent,
            Token::Dedent => Self::Dedent,
        }
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
            match lexer.next() {
                Some(rt) => {
                    drop(lexer);
                    self.buffer.borrow_mut().push(rt);
                }
                None => {
                    self.eof.set(true);
                    return;
                }
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
            match lexer.next() {
                Some(rt) => {
                    drop(lexer);
                    self.buffer.borrow_mut().push(rt);
                }
                None => {
                    self.eof.set(true);
                    return;
                }
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

    /// Take ownership of the token at `pos`, replacing it with a dummy.
    ///
    /// This is more efficient than `peek()` when you're consuming the token
    /// and won't need it again. The token is replaced with `Token::Dedent`
    /// (a zero-size sentinel) to avoid leaving uninitialized memory.
    ///
    /// # Panics
    /// Panics if `pos` is out of bounds.
    #[inline]
    pub(crate) fn take(&self, pos: usize) -> Option<(Token<'input>, Span)> {
        self.ensure_available(pos);
        let mut buffer = self.buffer.borrow_mut();
        buffer.get_mut(pos).map(|rt| {
            // Replace with a zero-size sentinel token
            let token = std::mem::replace(&mut rt.token, Token::Dedent);
            (token, rt.span)
        })
    }

    /// Peek at the current token at `pos` and apply a function to it.
    ///
    /// This is more efficient than `peek()` when you don't need to keep
    /// the token, as it avoids cloning `Cow<str>` data.
    #[inline]
    pub(crate) fn peek_with<F, R>(&self, pos: usize, f: F) -> Option<R>
    where
        F: FnOnce(&Token<'input>, Span) -> R,
    {
        self.ensure_available(pos);
        let buffer = self.buffer.borrow();
        buffer.get(pos).map(|rt| f(&rt.token, rt.span))
    }

    /// Peek at the token `n` ahead and apply a function to it.
    #[inline]
    pub(crate) fn peek_nth_with<F, R>(&self, pos: usize, n: usize, f: F) -> Option<R>
    where
        F: FnOnce(&Token<'input>, Span) -> R,
    {
        let index = pos + n;
        self.ensure_available(index);
        let buffer = self.buffer.borrow();
        buffer.get(index).map(|rt| f(&rt.token, rt.span))
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
