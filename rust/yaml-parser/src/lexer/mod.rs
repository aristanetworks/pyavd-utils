// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Lexer components for YAML parsing.
//!
//! This lexer tokenizes YAML streams, including multi-document streams with
//! directives (`%YAML`, `%TAG`) and document markers (`---`, `...`).
//!
//! It is context-aware, tracking flow depth to properly tokenize characters
//! that have different meanings:
//! - In **block context** (`flow_depth` = 0): `,[]{}` are valid in plain scalars
//! - In **flow context** (`flow_depth` > 0): `,[]{}` are delimiters
//!
//! Uses `Cow<'input, str>` for zero-copy tokenization where possible.
#![allow(
    clippy::multiple_inherent_impl,
    reason = "the lexer state machine is intentionally split across multiple files without changing the owning type"
)]

mod block_scalars;
mod directives;
mod indicators;
mod plain;
mod properties;
mod quoted;
mod rich_token;
mod token;
mod trivia;

#[cfg(test)]
#[allow(
    clippy::indexing_slicing,
    clippy::min_ident_chars,
    clippy::shadow_reuse,
    reason = "Tests benefit from direct indexing, short identifiers, and variable shadowing for readability"
)]
mod tests;

// Re-export main types
pub(crate) use rich_token::RichToken;
pub(crate) use rich_token::TokenKind;
pub(crate) use token::{
    BlockScalarHeader, Chomping, PlainScalarMeta, PlainScalarToken, QuoteStyle, Token,
};

use std::borrow::Cow;
use std::collections::VecDeque;

use crate::error::{ErrorKind, ParseError};
use crate::span::{Span, Spanned};

/// Check if a character is valid in an anchor/alias name.
/// Per YAML 1.2 spec, ns-anchor-char is any non-whitespace char
/// except c-flow-indicator: `[`, `]`, `{`, `}`, `,`
fn is_anchor_char(ch: char) -> bool {
    !ch.is_whitespace() && !matches!(ch, '[' | ']' | '{' | '}' | ',')
}

/// Lexer mode based on flow depth.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LexMode {
    /// Block context - flow indicators are part of plain scalars
    Block,
    /// Flow context - flow indicators are delimiters
    Flow,
}

/// Iterator phase for the lexer state machine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IteratorPhase {
    /// Haven't emitted the initial LineStart(0) yet
    Initial,
    /// Normal tokenization
    Running,
    /// Iterator exhausted
    Done,
}

/// Lexer phase for multi-document stream handling.
///
/// Tracks where we are in the YAML stream structure to determine
/// what tokens are valid (e.g., directives only in prologue).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerPhase {
    /// Before any document content - directives are valid here.
    /// This is either at the start of the stream or after `...` (document end).
    DirectivePrologue,
    /// Inside a document - directives are NOT valid here.
    /// `%` at column 0 is plain scalar content.
    InDocument,
}

/// YAML lexer state.
///
/// The lifetime `'input` refers to the input string being tokenized.
///
/// Implements `Iterator<Item = RichToken<'input>>` for streaming tokenization.
/// Errors are collected internally and retrieved via [`Lexer::take_errors`].
#[allow(
    clippy::struct_excessive_bools,
    reason = "state machine requires multiple boolean flags"
)]
pub(crate) struct Lexer<'input> {
    input: &'input str,
    /// Byte offset of current position in the input string.
    byte_pos: usize,
    /// Current flow depth (number of unclosed `{` or `[`)
    flow_depth: usize,
    /// Whether the previous token was a "JSON-like" value
    /// (quoted string, alias, flow end). After these, `:` is always
    /// a mapping indicator in flow context.
    prev_was_json_like: bool,
    /// Track if previous token was whitespace or line start - for comment validation.
    /// A `#` can only start a comment if preceded by whitespace or at line start.
    prev_was_separator: bool,
    /// Pending tokens to be returned (used for multi-token constructs like quoted strings)
    pending_tokens: VecDeque<RichToken<'input>>,
    /// Whether we're currently inside a quoted string (between `StringStart` and `StringEnd`).
    /// Kept so the lexer can preserve quoted-string tokenization rules.
    in_quoted_string: bool,
    /// Current phase of the iterator state machine
    phase: IteratorPhase,
    /// Collected errors during lexing
    errors: Vec<ParseError>,
    /// Current phase for multi-document stream handling.
    /// Determines whether directives are valid (in prologue) or not (in document).
    phase_state: LexerPhase,
    /// Whether we've seen a %YAML directive in the current document's prologue.
    /// Reset to false when entering a new document (on `DocEnd` or `DocStart` after content).
    has_yaml_directive: bool,
    /// Whether we've seen any directive in the current prologue (for "directive without document" error).
    has_directive_in_prologue: bool,
    /// Span of the first directive in the current prologue (for error reporting).
    first_directive_span: Option<Span>,
}

impl<'input> Lexer<'input> {
    pub(crate) fn new(input: &'input str) -> Self {
        Self {
            input,
            byte_pos: 0,
            flow_depth: 0,
            prev_was_json_like: false,
            prev_was_separator: true, // At start, we're at "line start"
            pending_tokens: VecDeque::new(),
            in_quoted_string: false,
            phase: IteratorPhase::Initial,
            errors: Vec::new(),
            phase_state: LexerPhase::DirectivePrologue, // Start in directive prologue
            has_yaml_directive: false,
            has_directive_in_prologue: false,
            first_directive_span: None,
        }
    }

    /// Take collected errors.
    pub(crate) fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.errors)
    }

    /// Record an error during lexing.
    fn add_error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError::new(kind, span));
    }

    #[inline]
    fn mode(&self) -> LexMode {
        if self.flow_depth > 0 {
            LexMode::Flow
        } else {
            LexMode::Block
        }
    }

    /// Peek the current character without advancing.
    #[inline]
    fn peek(&self) -> Option<char> {
        self.input.get(self.byte_pos..)?.chars().next()
    }

    /// Peek `n` characters ahead (0 = current character).
    #[inline]
    fn peek_n(&self, n: usize) -> Option<char> {
        self.input.get(self.byte_pos..)?.chars().nth(n)
    }

    /// Peek an ASCII byte at a small fixed offset from the current position.
    ///
    /// This is intended for structural ASCII probes like `---` / `...`, where
    /// repeatedly re-walking `chars().nth(n)` would touch the same input bytes
    /// multiple times.
    #[inline]
    fn peek_ascii_byte(&self, offset: usize) -> Option<u8> {
        self.input.as_bytes().get(self.byte_pos + offset).copied()
    }

    #[inline]
    fn starts_with_bytes_at_current(&self, prefix: &[u8]) -> bool {
        self.input
            .as_bytes()
            .get(self.byte_pos..)
            .is_some_and(|tail| tail.starts_with(prefix))
    }

    /// Peek the next Unicode scalar after the current ASCII punctuation.
    ///
    /// This avoids re-walking the current byte via `chars().nth(1)` in hot
    /// punctuation-driven lexer branches.
    #[inline]
    fn peek_char_after_current_ascii(&self) -> Option<char> {
        self.input.get(self.byte_pos + 1..)?.chars().next()
    }

    /// Advance to the next character and return the current one.
    #[inline]
    fn advance(&mut self) -> Option<char> {
        let ch = self.input.get(self.byte_pos..)?.chars().next()?;
        self.byte_pos += ch.len_utf8();
        Some(ch)
    }

    #[inline]
    fn current_span(&self, start: usize) -> Span {
        Span::from_usize_range(start..self.byte_pos)
    }

    #[inline]
    fn marker_followed_by_break_or_eof(&self, offset: usize) -> bool {
        match self.peek_ascii_byte(offset) {
            None | Some(b' ' | b'\t' | b'\n' | b'\r') => true,
            Some(_) => self
                .input
                .get(self.byte_pos + offset..)
                .and_then(|tail| tail.chars().next())
                .is_some_and(Self::is_newline),
        }
    }

    #[inline]
    fn matches_ascii_document_marker(&self, marker: [u8; 3]) -> bool {
        self.starts_with_bytes_at_current(&marker) && self.marker_followed_by_break_or_eof(3)
    }

    /// Skip inline whitespace and return whether any tabs were found.
    fn skip_inline_whitespace_detecting_tabs(&mut self) -> bool {
        let mut has_tabs = false;
        while let Some(ch) = self.peek() {
            match ch {
                ' ' => {
                    self.advance();
                }
                '\t' => {
                    has_tabs = true;
                    self.advance();
                }
                _ => break,
            }
        }
        has_tabs
    }

    fn is_newline(ch: char) -> bool {
        matches!(ch, '\n' | '\r' | '\u{0085}' | '\u{2028}' | '\u{2029}')
    }

    fn is_flow_indicator(ch: char) -> bool {
        matches!(ch, ',' | '[' | ']' | '{' | '}')
    }

    /// Process a token after it's been produced: update lexer state.
    ///
    /// This handles flow depth tracking, quoted string context, and JSON-like detection.
    fn process_token(&mut self, token: &Token<'input>) {
        // Track flow depth
        match token {
            Token::FlowMapStart | Token::FlowSeqStart => {
                self.flow_depth += 1;
            }
            Token::FlowMapEnd | Token::FlowSeqEnd => {
                self.flow_depth = self.flow_depth.saturating_sub(1);
            }
            _ => {}
        }

        // Track quoted string context (between StringStart and StringEnd).
        match token {
            Token::StringStart(_) => {
                self.in_quoted_string = true;
            }
            Token::StringEnd(_) => {
                self.in_quoted_string = false;
            }
            _ => {}
        }

        // Track if this token is "JSON-like" for colon indicator detection.
        // Whitespace, LineStart, and Comment tokens don't reset the flag -
        // they act as separators that preserve the "just saw JSON value" state.
        match token {
            Token::Whitespace
            | Token::WhitespaceWithTabs
            | Token::LineStart(_)
            | Token::Comment(_)
            | Token::StringStart(_)
            | Token::StringContent(_)
            | Token::YamlDirective(_)
            | Token::TagDirective(..)
            | Token::ReservedDirective(_) => {
                // Don't change prev_was_json_like
                // These are separators/structure tokens that allow comments to follow
                self.prev_was_separator = true;
            }
            Token::StringEnd(_) | Token::Alias(_) | Token::FlowMapEnd | Token::FlowSeqEnd => {
                self.prev_was_json_like = true;
                self.prev_was_separator = false;
            }
            _ => {
                self.prev_was_json_like = false;
                self.prev_was_separator = false;
            }
        }

        // Track lexer phase for multi-document streams.
        // Phase transitions:
        // - DirectivePrologue -> InDocument: when we see `---` or content
        // - InDocument -> DirectivePrologue: when we see `...` (document end)
        match self.phase_state {
            LexerPhase::DirectivePrologue => {
                match token {
                    Token::DocStart => {
                        // `---` starts a document - reset directive tracking for this doc
                        self.phase_state = LexerPhase::InDocument;
                        self.has_yaml_directive = false;
                        self.has_directive_in_prologue = false;
                        self.first_directive_span = None;
                    }
                    Token::YamlDirective(_)
                    | Token::TagDirective(..)
                    | Token::ReservedDirective(_)
                    | Token::Comment(_)
                    | Token::Whitespace
                    | Token::WhitespaceWithTabs
                    | Token::LineStart(_) => {
                        // These don't end the prologue
                    }
                    _ => {
                        // Any other content token starts an implicit document
                        // Reset directive tracking for this doc
                        self.phase_state = LexerPhase::InDocument;
                        self.has_yaml_directive = false;
                        self.has_directive_in_prologue = false;
                        self.first_directive_span = None;
                    }
                }
            }
            LexerPhase::InDocument => {
                if matches!(token, Token::DocEnd) {
                    // `...` ends the document, back to directive prologue
                    // Reset for the next document's prologue
                    self.phase_state = LexerPhase::DirectivePrologue;
                    self.has_yaml_directive = false;
                    self.has_directive_in_prologue = false;
                    self.first_directive_span = None;
                }
            }
        }
    }

    /// Produce the next raw token.
    fn produce_next_token(&mut self) -> Option<RichToken<'input>> {
        let (token, span) = self.next_token()?;
        Some(RichToken::new(token, span))
    }

    // ========================================================================
    // Token lexing helpers - extracted from next_token() for clarity
    // ========================================================================

    /// Get the next token.
    ///
    /// This method dispatches to specialized helper methods for each token type.
    fn next_token(&mut self) -> Option<Spanned<Token<'input>>> {
        let start = self.byte_pos;
        let ch = self.peek()?;

        // Try each token type in order of precedence
        if let Some(token) = self.try_lex_document_marker(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_directive(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_newline(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_whitespace(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_comment(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_flow_indicator(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_block_indicator(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_colon(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_anchor_or_alias(start, ch) {
            return Some(token);
        }

        // Tags: !, !!type, !<uri>
        if ch == '!' {
            return Some(self.consume_tag(start));
        }

        if let Some(token) = self.try_lex_block_scalar_header(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_quoted_scalar(start, ch) {
            return Some(token);
        }

        // In directive prologue, content (other than directives/doc markers) is only valid at column 0.
        // Content NOT at column 0 is invalid trailing content after `...` on the same line.
        if self.phase_state == LexerPhase::DirectivePrologue && !self.is_at_column_zero() {
            // This is invalid trailing content after document end marker (e.g., "... invalid")
            // Consume to end of line and emit error
            while let Some(peek_ch) = self.peek() {
                if Self::is_newline(peek_ch) {
                    break;
                }
                self.advance();
            }
            let span = self.current_span(start);
            self.add_error(ErrorKind::TrailingContent, span);
            // Return a plain scalar token (with error attached) so parsing can continue
            #[allow(
                clippy::string_slice,
                reason = "byte positions are guaranteed to be on char boundaries"
            )]
            let content = &self.input[start..self.byte_pos];
            return Some((
                Token::Plain(PlainScalarToken::new(
                    Cow::Borrowed(content),
                    PlainScalarMeta::default(),
                )),
                span,
            ));
        }

        // Default: plain scalar
        Some(self.consume_plain_scalar(start))
    }
}

/// Iterator implementation for streaming tokenization.
///
/// The lexer yields tokens one at a time. Errors are collected internally
/// and can be retrieved via [`Lexer::take_errors`]. This enables streaming
/// processing without buffering all tokens in memory.
impl<'input> Iterator for Lexer<'input> {
    type Item = RichToken<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.phase {
                IteratorPhase::Initial => {
                    // Emit initial LineStart(0)
                    self.phase = IteratorPhase::Running;
                    return Some(RichToken::new(
                        Token::LineStart(0),
                        Span::from_usize_range(0..0),
                    ));
                }

                IteratorPhase::Running => {
                    // Check pending tokens first (DEDENT, string parts)
                    if let Some(pending) = self.pending_tokens.pop_front() {
                        // Process state for pending tokens too (e.g., StringEnd sets prev_was_json_like)
                        self.process_token(&pending.token);
                        return Some(pending);
                    }

                    // Try to produce a new token
                    if let Some(rich_token) = self.produce_next_token() {
                        // Update state based on token
                        self.process_token(&rich_token.token);

                        return Some(rich_token);
                    }

                    // No more tokens from input: all done.
                    self.phase = IteratorPhase::Done;
                }

                IteratorPhase::Done => {
                    return None;
                }
            }
        }
    }
}

/// Tokenize document content with context awareness.
///
/// Returns `RichToken`s wrapping each token. All tokens (including `Whitespace`,
/// `WhitespaceWithTabs`, `Comment`) are kept as real tokens in the stream.
///
/// Errors are collected internally by the lexer and returned separately.
///
/// For streaming usage, iterate directly over `Lexer::new(input)`.
#[cfg(test)]
pub(crate) fn tokenize_document(input: &str) -> (Vec<RichToken<'_>>, Vec<ParseError>) {
    let mut lexer = Lexer::new(input);
    let tokens: Vec<RichToken<'_>> = lexer.by_ref().collect();
    let errors = lexer.take_errors();

    (tokens, errors)
}
