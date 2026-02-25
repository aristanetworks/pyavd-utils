// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Context-aware document lexer for YAML.
//!
//! This lexer tracks flow depth to properly tokenize characters that have
//! different meanings in block vs flow context:
//! - In **block context** (`flow_depth` = 0): `,[]{}` are valid in plain scalars
//! - In **flow context** (`flow_depth` > 0): `,[]{}` are delimiters
//!
//! This is Step 2 of the parser architecture.
//!
//! Uses `Cow<'input, str>` for zero-copy tokenization where possible.

use std::borrow::Cow;
use std::collections::VecDeque;

use crate::error::{ErrorKind, ParseError};
use crate::rich_token::RichToken;
use crate::span::{Span, Spanned};
use crate::token::{BlockScalarHeader, Chomping, QuoteStyle, Token};

use chumsky::span::Span as _;

/// Check if a character is valid in an anchor/alias name.
/// Per YAML 1.2 spec, ns-anchor-char is any non-whitespace char
/// except c-flow-indicator: `[`, `]`, `{`, `}`, `,`
fn is_anchor_char(ch: char) -> bool {
    !ch.is_whitespace() && !matches!(ch, '[' | ']' | '{' | '}' | ',')
}

/// Lexer mode based on flow depth.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexMode {
    /// Block context - flow indicators are part of plain scalars
    Block,
    /// Flow context - flow indicators are delimiters
    Flow,
}

/// Context-aware lexer state.
///
/// The lifetime `'input` refers to the input string being tokenized.
pub struct ContextLexer<'input> {
    input: &'input str,
    /// Byte offset of current position in the input string.
    /// This replaces the previous `chars: Vec<char>` approach for zero-allocation iteration.
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
    /// Indentation stack for INDENT/DEDENT tokens (like Python).
    /// Starts with [0] representing the base indentation level.
    indent_stack: Vec<usize>,
    /// Collected errors during lexing
    errors: Vec<ParseError>,
    /// Pending tokens to be returned (used for multi-token constructs like quoted strings)
    pending_tokens: VecDeque<Spanned<Token<'input>>>,
    /// Whether we're currently inside a quoted string (between `StringStart` and `StringEnd`).
    /// When true, we suppress INDENT/DEDENT emission for `LineStart` tokens.
    in_quoted_string: bool,
}

impl<'input> ContextLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            byte_pos: 0,
            flow_depth: 0,
            prev_was_json_like: false,
            prev_was_separator: true, // At start, we're at "line start"
            indent_stack: vec![0],    // Base indentation level
            errors: Vec::new(),
            pending_tokens: VecDeque::new(),
            in_quoted_string: false,
        }
    }

    /// Add an error at the given span
    fn add_error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError::new(kind, span));
    }

    fn mode(&self) -> LexMode {
        if self.flow_depth > 0 {
            LexMode::Flow
        } else {
            LexMode::Block
        }
    }

    /// Returns true if we've reached the end of input.
    fn is_eof(&self) -> bool {
        self.byte_pos >= self.input.len()
    }

    /// Peek the current character without advancing.
    fn peek(&self) -> Option<char> {
        self.input.get(self.byte_pos..)?.chars().next()
    }

    /// Peek `n` characters ahead (0 = current character).
    fn peek_n(&self, n: usize) -> Option<char> {
        self.input.get(self.byte_pos..)?.chars().nth(n)
    }

    /// Advance to the next character and return the current one.
    fn advance(&mut self) -> Option<char> {
        let ch = self.input.get(self.byte_pos..)?.chars().next()?;
        self.byte_pos += ch.len_utf8();
        Some(ch)
    }

    fn current_span(&self, start: usize) -> Span {
        Span::new((), start..self.byte_pos)
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

    /// Tokenize the document and return `RichToken`s directly.
    ///
    /// All tokens (including `Whitespace`, `WhitespaceWithTabs`, `Comment`) are
    /// kept as real tokens in the stream because:
    /// - Comments have semantic meaning (they terminate plain scalars)
    /// - Whitespace carries indentation information for block structure
    /// - Tab detection requires seeing `WhitespaceWithTabs` tokens
    pub fn tokenize(mut self) -> (Vec<RichToken<'input>>, Vec<ParseError>) {
        let tokens = self.collect_tokens();
        (tokens, self.errors)
    }

    /// Collect all tokens as `RichToken`s directly (no intermediate allocation).
    fn collect_tokens(&mut self) -> Vec<RichToken<'input>> {
        let mut tokens = Vec::new();

        // Start with LineStart(0) for initial indentation
        tokens.push(RichToken::new(Token::LineStart(0), Span::new((), 0..0)));

        while !self.is_eof() || !self.pending_tokens.is_empty() {
            // Check pending tokens first (from multi-token constructs like quoted strings)
            let maybe_token = if let Some(pending) = self.pending_tokens.pop_front() {
                Some(pending)
            } else {
                self.next_token()
            };

            if let Some((token, span)) = maybe_token {
                // Track flow depth
                match &token {
                    Token::FlowMapStart | Token::FlowSeqStart => {
                        self.flow_depth += 1;
                    }
                    Token::FlowMapEnd | Token::FlowSeqEnd => {
                        self.flow_depth = self.flow_depth.saturating_sub(1);
                    }
                    _ => {}
                }

                // Track quoted string context (between StringStart and StringEnd).
                // Inside quoted strings, we don't emit INDENT/DEDENT tokens.
                match &token {
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
                // This allows for multiline flow mappings like:
                //   { "foo"
                //     :bar }
                match &token {
                    Token::Whitespace
                    | Token::WhitespaceWithTabs
                    | Token::LineStart(_)
                    | Token::Comment(_)
                    | Token::StringStart(_)
                    | Token::StringContent(_) => {
                        // Don't change prev_was_json_like
                        // These are separators that allow comments to follow
                        // StringStart/StringContent are part of a string - wait for StringEnd
                        self.prev_was_separator = true;
                    }
                    Token::StringEnd(_)
                    | Token::Alias(_)
                    | Token::FlowMapEnd
                    | Token::FlowSeqEnd => {
                        self.prev_was_json_like = true;
                        self.prev_was_separator = false;
                    }
                    _ => {
                        self.prev_was_json_like = false;
                        self.prev_was_separator = false;
                    }
                }

                // Emit INDENT/DEDENT tokens after LineStart in block context
                // but NOT inside quoted strings
                if let Token::LineStart(n) = &token {
                    tokens.push(RichToken::new(token.clone(), span));

                    // Only emit INDENT/DEDENT in block context (not inside flow collections or strings)
                    if self.flow_depth == 0 && !self.in_quoted_string {
                        self.emit_indent_dedent_tokens(*n, span, &mut tokens);
                    }
                } else {
                    tokens.push(RichToken::new(token, span));
                }
            }
        }

        // At end of input, emit Dedent for remaining stack levels (except base 0)
        if self.flow_depth == 0 {
            let end_span = Span::new((), self.byte_pos..self.byte_pos);
            while self.indent_stack.len() > 1 {
                self.indent_stack.pop();
                tokens.push(RichToken::new(Token::Dedent, end_span));
            }
        }

        tokens
    }

    /// Emit INDENT/DEDENT tokens based on indentation change.
    /// Called after a LineStart(n) token is produced.
    ///
    /// Note: We don't emit indentation errors here because:
    /// 1. Block scalar content has irregular indentation that's context-dependent
    /// 2. The parser has better context to determine if indentation is valid
    fn emit_indent_dedent_tokens(
        &mut self,
        new_indent: usize,
        span: Span,
        tokens: &mut Vec<RichToken<'input>>,
    ) {
        let current_indent = *self.indent_stack.last().unwrap_or(&0);

        if new_indent > current_indent {
            // Indent increased - push new level
            self.indent_stack.push(new_indent);
            tokens.push(RichToken::new(Token::Indent(new_indent), span));
        } else if new_indent < current_indent {
            // Indent decreased - pop levels and emit Dedent for each
            while let Some(&top) = self.indent_stack.last() {
                if top <= new_indent {
                    break;
                }
                self.indent_stack.pop();
                tokens.push(RichToken::new(Token::Dedent, span));
            }

            // If new_indent doesn't match any level, push it as a new level
            // This handles cases like block scalar content returning to parent level
            let final_indent = *self.indent_stack.last().unwrap_or(&0);
            if new_indent != final_indent && new_indent > 0 {
                // Push this as a new level (allows irregular indentation like in block scalars)
                self.indent_stack.push(new_indent);
                // Don't emit Indent here - this is just stack management
            }
        }
        // If new_indent == current_indent, no INDENT/DEDENT needed
    }

    /// Check if we're at column 0 (start of input or right after a newline).
    fn is_at_column_zero(&self) -> bool {
        if self.byte_pos == 0 {
            return true;
        }
        // Check if previous byte was a newline
        let prev_byte_pos = self.byte_pos.saturating_sub(1);
        if let Some(ch) = self.input.as_bytes().get(prev_byte_pos) {
            *ch == b'\n' || *ch == b'\r'
        } else {
            false
        }
    }

    // ========================================================================
    // Token lexing helpers - extracted from next_token() for clarity
    // ========================================================================

    /// Try to lex a document marker (`---` or `...`) at column 0.
    fn try_lex_document_marker(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if !self.is_at_column_zero() {
            return None;
        }

        // Check for `---` followed by whitespace/newline/EOF
        if ch == '-' && self.peek_n(1) == Some('-') && self.peek_n(2) == Some('-') {
            let after = self.peek_n(3);
            if after.is_none()
                || after == Some(' ')
                || after == Some('\t')
                || Self::is_newline(after.unwrap_or('\n'))
            {
                self.advance(); // -
                self.advance(); // -
                self.advance(); // -
                return Some((Token::DocStart, self.current_span(start)));
            }
        }

        // Check for `...` followed by whitespace/newline/EOF
        if ch == '.' && self.peek_n(1) == Some('.') && self.peek_n(2) == Some('.') {
            let after = self.peek_n(3);
            if after.is_none()
                || after == Some(' ')
                || after == Some('\t')
                || Self::is_newline(after.unwrap_or('\n'))
            {
                self.advance(); // .
                self.advance(); // .
                self.advance(); // .
                return Some((Token::DocEnd, self.current_span(start)));
            }
        }

        None
    }

    /// Try to lex a newline and subsequent indentation.
    fn try_lex_newline(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if !Self::is_newline(ch) {
            return None;
        }

        self.advance();
        // Handle \r\n
        if ch == '\r' && self.peek() == Some('\n') {
            self.advance();
        }

        // Count indentation spaces only (tabs are NOT valid for indentation in YAML)
        let mut indent = 0;
        while self.peek() == Some(' ') {
            self.advance();
            indent += 1;
        }

        Some((Token::LineStart(indent), self.current_span(start)))
    }

    /// Try to lex inline whitespace.
    /// Returns `Token::Whitespace` for spaces only, or `Token::WhitespaceWithTabs` if tabs are present.
    fn try_lex_whitespace(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if ch != ' ' && ch != '\t' {
            return None;
        }
        // Check if first char is a tab
        let first_is_tab = ch == '\t';
        // Continue consuming whitespace, detecting tabs
        let rest_has_tabs = self.skip_inline_whitespace_detecting_tabs();
        let has_tabs = first_is_tab || rest_has_tabs;
        let token = if has_tabs {
            Token::WhitespaceWithTabs
        } else {
            Token::Whitespace
        };
        Some((token, self.current_span(start)))
    }

    /// Try to lex a comment.
    fn try_lex_comment(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if ch != '#' {
            return None;
        }

        if !self.prev_was_separator {
            // `#` without preceding whitespace is invalid - report error
            self.advance();
            let span = self.current_span(start);
            self.add_error(ErrorKind::UnexpectedToken, span);
            // Try to recover by consuming rest of line as if it were a comment
            while let Some(peek_ch) = self.peek() {
                if Self::is_newline(peek_ch) {
                    break;
                }
                self.advance();
            }
            return Some((Token::Comment(Cow::Borrowed("")), self.current_span(start)));
        }

        self.advance(); // consume #
        let content_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if Self::is_newline(peek_ch) {
                break;
            }
            self.advance();
        }
        // Borrow directly from input (zero-copy)
        // Byte positions are always at UTF-8 boundaries (maintained by advance())
        let content = self
            .input
            .get(content_start..self.byte_pos)
            .unwrap_or_default();
        Some((
            Token::Comment(Cow::Borrowed(content)),
            self.current_span(start),
        ))
    }

    /// Try to lex a flow indicator (`{}[],`).
    fn try_lex_flow_indicator(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        match ch {
            '{' => {
                self.advance();
                Some((Token::FlowMapStart, self.current_span(start)))
            }
            '}' => {
                self.advance();
                Some((Token::FlowMapEnd, self.current_span(start)))
            }
            '[' => {
                self.advance();
                Some((Token::FlowSeqStart, self.current_span(start)))
            }
            ']' => {
                self.advance();
                Some((Token::FlowSeqEnd, self.current_span(start)))
            }
            ',' if self.mode() == LexMode::Flow => {
                self.advance();
                Some((Token::Comma, self.current_span(start)))
            }
            _ => None,
        }
    }

    /// Try to lex a block indicator (`-` or `?` followed by whitespace).
    fn try_lex_block_indicator(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        // Block sequence indicator: - followed by whitespace/newline
        if ch == '-' {
            if let Some(next) = self.peek_n(1) {
                if next == ' ' || next == '\t' || Self::is_newline(next) {
                    self.advance();
                    return Some((Token::BlockSeqIndicator, self.current_span(start)));
                }
            } else {
                // - at EOF
                self.advance();
                return Some((Token::BlockSeqIndicator, self.current_span(start)));
            }
        }

        // Explicit key indicator: ? followed by whitespace/newline
        if ch == '?' {
            if let Some(next) = self.peek_n(1) {
                if next == ' ' || next == '\t' || Self::is_newline(next) {
                    self.advance();
                    return Some((Token::MappingKey, self.current_span(start)));
                }
            } else {
                self.advance();
                return Some((Token::MappingKey, self.current_span(start)));
            }
        }

        None
    }

    /// Try to lex a colon as a mapping value indicator.
    fn try_lex_colon(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if ch != ':' {
            return None;
        }

        let next = self.peek_n(1);
        let is_indicator = if self.prev_was_json_like && self.mode() == LexMode::Flow {
            // After a JSON-like value, : is ALWAYS an indicator in flow context
            true
        } else {
            match self.mode() {
                LexMode::Flow => {
                    // In flow context, : is indicator if followed by:
                    // - whitespace, newline, EOF, or flow indicator
                    next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                        || next.is_some_and(Self::is_flow_indicator)
                }
                LexMode::Block => {
                    // In block context, : is indicator only if followed by whitespace/newline/EOF
                    next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                }
            }
        };

        // Note: : starts a plain scalar if not an indicator - let caller handle it
        is_indicator.then(|| {
            self.advance();
            (Token::Colon, self.current_span(start))
        })
    }

    /// Try to lex an anchor (`&name`) or alias (`*name`).
    fn try_lex_anchor_or_alias(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        // Anchors: &name
        if ch == '&'
            && let Some(next) = self.peek_n(1)
            && is_anchor_char(next)
        {
            self.advance(); // consume &
            let name = self.consume_anchor_name();
            return Some((Token::Anchor(name), self.current_span(start)));
        }

        // Aliases: *name
        if ch == '*'
            && let Some(next) = self.peek_n(1)
            && is_anchor_char(next)
        {
            self.advance(); // consume *
            let name = self.consume_anchor_name();
            return Some((Token::Alias(name), self.current_span(start)));
        }

        None
    }

    /// Try to lex a block scalar header (`|` or `>`).
    fn try_lex_block_scalar_header(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if ch == '|' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::LiteralBlockHeader(header), self.current_span(start)));
        }
        if ch == '>' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::FoldedBlockHeader(header), self.current_span(start)));
        }
        None
    }

    /// Try to lex a quoted scalar (`'...'` or `"..."`).
    fn try_lex_quoted_scalar(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if ch == '\'' {
            return Some(self.consume_single_quoted(start));
        }
        if ch == '"' {
            return Some(self.consume_double_quoted(start));
        }
        None
    }

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

        // Default: plain scalar
        Some(self.consume_plain_scalar(start))
    }

    /// Consume an anchor name, borrowing directly from input (zero-copy).
    fn consume_anchor_name(&mut self) -> Cow<'input, str> {
        // According to YAML 1.2 spec, anchor names (ns-anchor-char+) can contain:
        // - Any non-whitespace character except c-flow-indicator ([]{},)
        // This includes colons and other special characters!
        let name_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if is_anchor_char(peek_ch) {
                self.advance();
            } else {
                break;
            }
        }
        // Borrow directly from input (zero-copy)
        // Byte positions are always at UTF-8 boundaries (maintained by advance())
        Cow::Borrowed(
            self.input
                .get(name_start..self.byte_pos)
                .unwrap_or_default(),
        )
    }

    fn consume_tag(&mut self, start: usize) -> Spanned<Token<'input>> {
        let mut tag = String::new();
        self.advance(); // consume !

        // Check for !! or !<
        match self.peek() {
            Some('!') => {
                tag.push('!');
                self.advance();
                // Named tag like !!str
                while let Some(peek_ch) = self.peek() {
                    if peek_ch.is_whitespace() || Self::is_flow_indicator(peek_ch) {
                        break;
                    }
                    tag.push(peek_ch);
                    self.advance();
                }
            }
            Some('<') => {
                self.advance();
                // Verbatim tag !<uri>
                while let Some(peek_ch) = self.peek() {
                    if peek_ch == '>' {
                        self.advance();
                        break;
                    }
                    tag.push(peek_ch);
                    self.advance();
                }
            }
            Some(ch) if Self::is_valid_tag_start_char(ch) => {
                // Regular tag !name
                while let Some(peek_ch) = self.peek() {
                    if peek_ch.is_whitespace() || Self::is_flow_indicator(peek_ch) {
                        break;
                    }
                    tag.push(peek_ch);
                    self.advance();
                }
            }
            // ! followed by whitespace, EOF, or flow indicator is the non-specific tag
            Some(ch) if ch.is_whitespace() || Self::is_flow_indicator(ch) => {
                // Empty tag (non-specific tag `!`)
                // tag stays empty, which represents `!`
            }
            None => {
                // ! at end of input is also a valid non-specific tag
                // tag stays empty
            }
            _ => {
                // `!` followed by non-tag character (like `!"#$%...`)
                // This is NOT a tag - treat the `!` and following content as plain scalar
                // Note: # is only a comment if preceded by whitespace, so we include it here
                let mut plain = String::from("!");
                let in_flow = self.mode() == LexMode::Flow;
                while let Some(peek_ch) = self.peek() {
                    // Stop at whitespace
                    if peek_ch.is_whitespace() {
                        break;
                    }
                    // Flow indicators only terminate in flow mode
                    if in_flow && Self::is_flow_indicator(peek_ch) {
                        break;
                    }
                    // Colon ends plain scalar if followed by whitespace (or EOF)
                    if peek_ch == ':' {
                        let next = self.peek_n(1);
                        if next.is_none() || next.is_some_and(char::is_whitespace) {
                            break;
                        }
                    }
                    plain.push(peek_ch);
                    self.advance();
                }
                // Tag fallback to plain scalar requires owned string
                return (Token::Plain(Cow::Owned(plain)), self.current_span(start));
            }
        }

        // Tag content uses owned string (constructed from pieces like !!str)
        (Token::Tag(Cow::Owned(tag)), self.current_span(start))
    }

    /// Check if a character is valid at the start of a tag name.
    /// Valid tag characters are URI characters: alphanumerics and certain punctuation.
    fn is_valid_tag_start_char(ch: char) -> bool {
        ch.is_alphanumeric()
            || matches!(
                ch,
                '-' | '_' | '.' | '~' | '%' | '/' | ':' | '@' | '&' | '=' | '+' | '$' | ',' | ';'
            )
    }

    fn consume_block_header(&mut self) -> BlockScalarHeader {
        let mut indent = None;
        let mut chomping = Chomping::Clip;

        // Parse indent and chomping indicators (can be in any order)
        for _ in 0..2 {
            match self.peek() {
                Some('+') => {
                    chomping = Chomping::Keep;
                    self.advance();
                }
                Some('-') => {
                    chomping = Chomping::Strip;
                    self.advance();
                }
                Some(ch) if ch.is_ascii_digit() && ch != '0' => {
                    // ch is guaranteed to be ASCII digit 1-9, so to_digit is safe
                    indent = ch.to_digit(10).and_then(|digit| u8::try_from(digit).ok());
                    self.advance();
                }
                _ => break,
            }
        }

        // After block header, only whitespace and comments are allowed on the same line
        // Any other content is invalid (e.g., `> first line` is invalid)
        // Comments require preceding whitespace (e.g., `># comment` is invalid)
        let error_start = self.byte_pos;
        let mut has_invalid_content = false;
        let mut saw_whitespace = false;
        while let Some(peek_ch) = self.peek() {
            if Self::is_newline(peek_ch) {
                break;
            }
            if peek_ch == ' ' || peek_ch == '\t' {
                saw_whitespace = true;
                self.advance();
                continue;
            }
            if peek_ch == '#' {
                if !saw_whitespace {
                    // Comment without preceding whitespace is invalid
                    has_invalid_content = true;
                }
                // Consume comment regardless (for error recovery)
                while let Some(ch) = self.peek() {
                    if Self::is_newline(ch) {
                        break;
                    }
                    self.advance();
                }
                break;
            }
            // Invalid content on same line as block scalar indicator
            has_invalid_content = true;
            self.advance();
        }

        if has_invalid_content {
            let span = Span::new((), error_start..self.byte_pos);
            self.add_error(ErrorKind::UnexpectedToken, span);
        }

        BlockScalarHeader { indent, chomping }
    }

    /// Handle a newline within a quoted string.
    ///
    /// Emits the current content as a `StringContent` token (if non-empty),
    /// consumes the newline and indentation, and emits a `LineStart` token.
    /// Returns the new `content_start` position.
    fn handle_quoted_newline(&mut self, content: &mut String, content_start: usize) -> usize {
        // Emit current content before newline
        if !content.is_empty() {
            let content_span = Span::new((), content_start..self.byte_pos);
            // Quoted strings always use Cow::Owned (escape processing)
            self.pending_tokens.push_back((
                Token::StringContent(Cow::Owned(std::mem::take(content))),
                content_span,
            ));
        }

        // Consume newline
        let newline_start = self.byte_pos;
        let ch = self.advance().unwrap();
        if ch == '\r' && self.peek() == Some('\n') {
            self.advance();
        }

        // Count indentation
        let mut indent = 0;
        while self.peek() == Some(' ') {
            self.advance();
            indent += 1;
        }

        // Emit LineStart token
        let line_span = Span::new((), newline_start..self.byte_pos);
        self.pending_tokens
            .push_back((Token::LineStart(indent), line_span));

        self.byte_pos
    }

    /// Emit final string tokens after the main loop.
    ///
    /// Emits the remaining content (if any) and the `StringEnd` token.
    /// Reports an error if the string was not terminated.
    fn finalize_quoted_string(
        &mut self,
        start: usize,
        content: String,
        content_start: usize,
        terminated: bool,
        style: QuoteStyle,
    ) {
        // Emit final content segment if any
        if !content.is_empty() {
            let content_span = Span::new((), content_start..self.byte_pos);
            // Quoted strings always use Cow::Owned (escape processing)
            self.pending_tokens
                .push_back((Token::StringContent(Cow::Owned(content)), content_span));
        }

        // Emit StringEnd
        let end_span = Span::new((), self.byte_pos.saturating_sub(1)..self.byte_pos);
        if !terminated {
            let full_span = self.current_span(start);
            self.add_error(ErrorKind::UnterminatedString, full_span);
        }
        self.pending_tokens
            .push_back((Token::StringEnd(style), end_span));
    }

    /// Consume a single-quoted string, emitting `StringStart`, `StringContent`, `LineStart` and `StringEnd` tokens.
    /// Pushes tokens to `pending_tokens` and returns the first token.
    fn consume_single_quoted(&mut self, start: usize) -> Spanned<Token<'input>> {
        let start_span = Span::new((), start..start + 1);
        self.advance(); // consume opening '

        let mut content = String::new();
        let mut content_start = self.byte_pos;
        let mut terminated = false;

        loop {
            match self.peek() {
                None => break, // Unterminated
                Some('\'') => {
                    self.advance();
                    // Check for escaped quote ''
                    if self.peek() == Some('\'') {
                        content.push('\'');
                        self.advance();
                    } else {
                        terminated = true;
                        break;
                    }
                }
                Some('\n' | '\r') => {
                    content_start = self.handle_quoted_newline(&mut content, content_start);
                }
                Some(ch) => {
                    content.push(ch);
                    self.advance();
                }
            }
        }

        self.finalize_quoted_string(
            start,
            content,
            content_start,
            terminated,
            QuoteStyle::Single,
        );

        // Return StringStart as the immediate token
        (Token::StringStart(QuoteStyle::Single), start_span)
    }

    /// Consume a double-quoted string, emitting `StringStart`, `StringContent`, `LineStart` and `StringEnd` tokens.
    /// Pushes tokens to `pending_tokens` and returns the first token.
    fn consume_double_quoted(&mut self, start: usize) -> Spanned<Token<'input>> {
        let start_span = Span::new((), start..start + 1);
        self.advance(); // consume opening "

        let mut content = String::new();
        let mut content_start = self.byte_pos;
        let mut terminated = false;

        loop {
            match self.peek() {
                None => break,
                Some('"') => {
                    self.advance();
                    terminated = true;
                    break;
                }
                Some('\\') => {
                    let escape_start = self.byte_pos;
                    self.advance();
                    if let Some(escaped) = self.consume_escape_sequence(escape_start) {
                        content.push_str(&escaped);
                    }
                }
                Some('\n' | '\r') => {
                    content_start = self.handle_quoted_newline(&mut content, content_start);
                }
                Some(ch) => {
                    content.push(ch);
                    self.advance();
                }
            }
        }

        self.finalize_quoted_string(
            start,
            content,
            content_start,
            terminated,
            QuoteStyle::Double,
        );

        // Return StringStart as the immediate token
        (Token::StringStart(QuoteStyle::Double), start_span)
    }

    fn consume_escape_sequence(&mut self, start_byte_pos: usize) -> Option<String> {
        let ch = self.advance()?;
        let result = match ch {
            '0' => "\0".to_owned(),
            'a' => "\x07".to_owned(),
            'b' => "\x08".to_owned(),
            't' | '\t' => "\t".to_owned(),
            'n' => "\n".to_owned(),
            'v' => "\x0B".to_owned(),
            'f' => "\x0C".to_owned(),
            'r' => "\r".to_owned(),
            'e' => "\x1B".to_owned(),
            ' ' => " ".to_owned(),
            '"' => "\"".to_owned(),
            '/' => "/".to_owned(),
            '\\' => "\\".to_owned(),
            'N' => "\u{0085}".to_owned(),
            '_' => "\u{00A0}".to_owned(),
            'L' => "\u{2028}".to_owned(),
            'P' => "\u{2029}".to_owned(),
            'x' => self.consume_hex_escape(2),
            'u' => self.consume_hex_escape(4),
            'U' => self.consume_hex_escape(8),
            '\n' | '\r' => {
                // Line continuation - skip whitespace on next line
                while matches!(self.peek(), Some(' ' | '\t' | '\n' | '\r')) {
                    self.advance();
                }
                String::new()
            }
            _ => {
                // Invalid escape sequence - report error
                let span = Span::new((), start_byte_pos..self.byte_pos);
                self.add_error(ErrorKind::InvalidEscape(ch), span);
                // Still return the escaped char for error recovery
                ch.to_string()
            }
        };
        Some(result)
    }

    fn consume_hex_escape(&mut self, digits: usize) -> String {
        let mut hex = String::new();
        for _ in 0..digits {
            if let Some(peek_ch) = self.peek() {
                if peek_ch.is_ascii_hexdigit() {
                    hex.push(peek_ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }
        if let Ok(code) = u32::from_str_radix(&hex, 16)
            && let Some(ch) = char::from_u32(code)
        {
            return ch.to_string();
        }
        format!("\\x{hex}") // Invalid escape - keep as-is
    }

    /// Consume a plain scalar, respecting the current mode.
    fn consume_plain_scalar(&mut self, start: usize) -> Spanned<Token<'input>> {
        let mut content = String::new();
        let mut at_start = true;

        while let Some(ch) = self.peek() {
            // Always stop at newlines
            if Self::is_newline(ch) {
                break;
            }

            // Handle special indicators at start: -, ?, :
            // These can only start a plain scalar if followed by a "safe" character
            // In flow context, flow indicators are not safe
            if at_start && (ch == '-' || ch == '?' || ch == ':') {
                let next = self.peek_n(1);
                // Check if next is "safe" for plain scalar start
                let is_safe = match next {
                    None => false, // EOF not safe
                    Some(n) => {
                        !(n.is_whitespace()
                            || Self::is_newline(n)
                            || (self.mode() == LexMode::Flow && Self::is_flow_indicator(n)))
                    }
                };
                if !is_safe {
                    // Cannot start plain scalar with this character - emit error and skip it
                    let span = self.current_span(start);
                    self.add_error(ErrorKind::UnexpectedToken, span);
                    self.advance(); // Consume the invalid character to avoid infinite loop
                    return (Token::Plain(Cow::Borrowed("")), self.current_span(start));
                }
            }

            // Handle colon - behavior differs between block and flow mode
            if ch == ':' {
                if self.mode() == LexMode::Flow {
                    // In flow mode:
                    // - At the start of plain scalar, : can be included if followed by
                    //   non-whitespace and non-flow-indicator (e.g., :x, ://, etc.)
                    // - In the middle, : terminates if followed by whitespace/flow-indicator/EOF
                    let next = self.peek_n(1);
                    let colon_terminates = next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                        || next.is_some_and(Self::is_flow_indicator);

                    if colon_terminates && !at_start {
                        // In the middle and : is followed by terminator -> stop
                        break;
                    } else if colon_terminates && at_start {
                        // At start but : is followed by terminator -> empty scalar, stop
                        break;
                    }
                    // Otherwise, : is followed by non-terminator, consume it
                } else {
                    // In block mode, only stop if followed by whitespace/newline/EOF
                    if let Some(next) = self.peek_n(1) {
                        if next == ' ' || next == '\t' || Self::is_newline(next) {
                            break;
                        }
                    } else {
                        // : at EOF
                        break;
                    }
                }
            }

            // Space followed by # is a comment start
            if (ch == ' ' || ch == '\t') && self.peek_n(1) == Some('#') {
                break;
            }

            // Flow indicators - depends on mode
            if Self::is_flow_indicator(ch) && self.mode() == LexMode::Flow {
                break;
            }
            // In block mode, flow indicators are part of plain scalar

            content.push(ch);
            self.advance();
            at_start = false;
        }

        // Plain scalars need Cow::Owned because we trim trailing whitespace
        let trimmed = content.trim_end();
        (
            Token::Plain(Cow::Owned(trimmed.to_owned())),
            self.current_span(start),
        )
    }
}

/// Tokenize document content with context awareness.
///
/// Returns `RichToken`s wrapping each token. All tokens (including `Whitespace`,
/// `WhitespaceWithTabs`, `Comment`) are kept as real tokens in the stream because
/// comments have semantic meaning in YAML (they terminate plain scalars).
///
/// The `RichToken` wrapper supports future IDE features but currently has empty
/// leading/trailing trivia vectors.
pub fn tokenize_document(input: &str) -> (Vec<RichToken<'_>>, Vec<ParseError>) {
    ContextLexer::new(input).tokenize()
}

#[cfg(test)]
#[allow(
    clippy::indexing_slicing,
    clippy::min_ident_chars,
    clippy::shadow_reuse,
    reason = "Tests benefit from direct indexing, short identifiers, and variable shadowing for readability"
)]
mod tests {
    use super::*;

    fn get_tokens(input: &str) -> Vec<Token<'_>> {
        let (tokens, _errors) = tokenize_document(input);
        tokens
            .into_iter()
            .filter(|rt| {
                !matches!(
                    rt.token,
                    Token::Whitespace | Token::WhitespaceWithTabs | Token::LineStart(_)
                )
            })
            .map(|rt| rt.token)
            .collect()
    }

    #[test]
    fn test_colon_in_block_plain_scalar() {
        // In block context, :foo is a plain scalar
        let tokens = get_tokens(":foo");
        assert_eq!(tokens, vec![Token::Plain(":foo".into())]);
    }

    #[test]
    fn test_colon_in_flow_context() {
        // In flow context, key: value is a mapping. key:value (no space) is also
        // parsed as key + : + value because : followed by a flow indicator or
        // end of content terminates the key.
        // But key:value where : is followed by non-whitespace non-flow-indicator
        // is the key as the whole "key:value" plain scalar!
        // Actually, let's check: {key:value} - the colon is followed by 'v' which
        // is not whitespace/flow-indicator, so "key:value" is ONE plain scalar.
        let tokens = get_tokens("{key:value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Plain("key:value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_colon_with_space_in_flow() {
        // With a space after colon, it's a mapping
        let tokens = get_tokens("{key: value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Plain("key".into()),
                Token::Colon,
                Token::Plain("value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_colon_adjacent_in_flow() {
        // {:value} - the colon is followed by 'v', not whitespace, so :value is
        // a plain scalar starting with colon
        let tokens = get_tokens("{:value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Plain(":value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_colon_as_indicator_in_flow() {
        // {: value} - colon followed by space is an indicator (empty key)
        let tokens = get_tokens("{: value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Colon,
                Token::Plain("value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_quoted_key_adjacent_value() {
        // After quoted key, :value should be : + value (not :value as scalar)
        // This is test C2DT from yaml-test-suite
        let tokens = get_tokens("{\"adjacent\":value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::StringStart(QuoteStyle::Double),
                Token::StringContent("adjacent".into()),
                Token::StringEnd(QuoteStyle::Double),
                Token::Colon,
                Token::Plain("value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_comma_in_block_plain_scalar() {
        // In block context, comma is part of plain scalar
        let tokens = get_tokens("a,b,c");
        assert_eq!(tokens, vec![Token::Plain("a,b,c".into())]);
    }

    #[test]
    fn test_comma_in_flow_context() {
        // In flow context, comma is separator
        let tokens = get_tokens("[a,b,c]");
        assert_eq!(
            tokens,
            vec![
                Token::FlowSeqStart,
                Token::Plain("a".into()),
                Token::Comma,
                Token::Plain("b".into()),
                Token::Comma,
                Token::Plain("c".into()),
                Token::FlowSeqEnd,
            ]
        );
    }

    #[test]
    fn test_brackets_in_block_plain_scalar() {
        // In block context, [] are part of plain scalar...
        // Actually, no - [ and ] start/end flow sequences even in block context.
        // The difference is that , is only a separator in flow context.
        let tokens = get_tokens("key: [value]");
        // This should still recognize [] as flow sequence
        assert!(tokens.contains(&Token::FlowSeqStart));
    }

    #[test]
    fn test_percent_in_document() {
        // % in document content is just a regular character
        let tokens = get_tokens("foo%bar");
        assert_eq!(tokens, vec![Token::Plain("foo%bar".into())]);
    }

    #[test]
    fn test_percent_at_line_start_in_flow() {
        // % at line start inside flow mapping should be plain scalar content
        let tokens = get_tokens("{ matches\n% : 20 }");
        // Should have: { matches <newline> % : 20 }
        // The % is part of multiline plain scalar, then : 20 is value
        assert!(tokens.contains(&Token::FlowMapStart));
        assert!(tokens.contains(&Token::FlowMapEnd));
        // Should NOT have a reserved directive
        assert!(
            !tokens
                .iter()
                .any(|token| matches!(token, Token::ReservedDirective(_)))
        );
    }

    #[test]
    fn test_colon_value_in_flow() {
        // {x: :x} - value is :x (plain scalar starting with colon)
        let tokens = get_tokens("{x: :x}");
        // Should be: { x : :x }
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Plain("x".into()),
                Token::Colon,
                Token::Plain(":x".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_url_in_flow() {
        // URL in flow context
        let tokens = get_tokens("{url: http://example.org}");
        // Should recognize http://example.org as a plain scalar
        assert!(tokens.contains(&Token::Plain("http://example.org".into())));
    }

    // Tests for tokenize_document
    //
    // Note: Whitespace, WhitespaceWithTabs, and Comment are real tokens in the stream
    // because comments have semantic meaning in YAML (they terminate plain scalars).

    #[test]
    fn test_simple_mapping_tokens() {
        // Test token stream for simple mapping
        let (tokens, errors) = tokenize_document("key: value");
        assert!(errors.is_empty());

        // Should have 5 tokens: LineStart(0), "key", ":", Whitespace, "value"
        assert_eq!(tokens.len(), 5);

        // Check token types
        assert!(matches!(tokens[0].token, Token::LineStart(0)));
        assert_eq!(tokens[1].token, Token::Plain("key".into()));
        assert_eq!(tokens[2].token, Token::Colon);
        assert_eq!(tokens[3].token, Token::Whitespace);
        assert_eq!(tokens[4].token, Token::Plain("value".into()));
    }

    #[test]
    fn test_comment_as_token() {
        // Test that comments are real tokens
        let (tokens, errors) = tokenize_document("key: value # this is a comment");
        assert!(errors.is_empty());

        // Comment should be a real token in the stream
        let comment_token = tokens
            .iter()
            .find(|t| matches!(&t.token, Token::Comment(s) if s.contains("this is a comment")));
        assert!(comment_token.is_some(), "Should find comment token");
    }

    #[test]
    fn test_multiline_with_comments() {
        // Test multiline document with comments
        let input = "# header comment\nkey: value";
        let (tokens, errors) = tokenize_document(input);
        assert!(errors.is_empty());

        // First token is LineStart(0)
        assert!(matches!(tokens[0].token, Token::LineStart(0)));

        // Comment should be a real token
        let comment_token = tokens
            .iter()
            .find(|t| matches!(&t.token, Token::Comment(s) if s.contains("header")));
        assert!(comment_token.is_some(), "Should find header comment token");
    }

    #[test]
    fn test_empty_input() {
        let (tokens, errors) = tokenize_document("");
        assert!(errors.is_empty());
        // Empty input still gets initial LineStart(0) token
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].token, Token::LineStart(0)));
    }

    #[test]
    fn test_comment_only() {
        // Only comments
        let (tokens, errors) = tokenize_document("# just a comment");
        assert!(errors.is_empty());
        // LineStart(0) and Comment are separate tokens
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::LineStart(0)));
        assert!(matches!(&tokens[1].token, Token::Comment(s) if s.contains("just a comment")));
    }

    #[test]
    fn test_tab_as_whitespace_with_tabs_token() {
        // Test that tabs are WhitespaceWithTabs tokens (for error detection)
        let input = "key:\n\tvalue";
        let (tokens, errors) = tokenize_document(input);
        assert!(errors.is_empty());

        // WhitespaceWithTabs should be a real token
        let tab_token = tokens
            .iter()
            .find(|t| matches!(t.token, Token::WhitespaceWithTabs));
        assert!(
            tab_token.is_some(),
            "Should find WhitespaceWithTabs token for tab"
        );
    }
}
