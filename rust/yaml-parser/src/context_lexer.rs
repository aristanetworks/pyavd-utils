// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Context-aware document lexer for YAML.
//!
//! This lexer tracks flow depth to properly tokenize characters that have
//! different meanings in block vs flow context:
//! - In **block context** (flow_depth = 0): `,[]{}` are valid in plain scalars
//! - In **flow context** (flow_depth > 0): `,[]{}` are delimiters
//!
//! This is Layer 2 of the layered parser architecture.

use crate::error::{ErrorKind, ParseError};
use crate::lexer::{BlockScalarHeader, Chomping, Token};
use crate::span::{Span, Spanned};

use chumsky::span::Span as _;

/// Check if a character is valid in an anchor/alias name.
/// Per YAML 1.2 spec, ns-anchor-char is any non-whitespace char
/// except c-flow-indicator: `[`, `]`, `{`, `}`, `,`
fn is_anchor_char(c: char) -> bool {
    !c.is_whitespace() && !matches!(c, '[' | ']' | '{' | '}' | ',')
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
pub struct ContextLexer<'a> {
    input: &'a str,
    chars: Vec<char>,
    pos: usize,
    /// Current flow depth (number of unclosed `{` or `[`)
    flow_depth: usize,
    /// Byte offset of current position
    byte_pos: usize,
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
}

impl<'a> ContextLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().collect(),
            pos: 0,
            flow_depth: 0,
            byte_pos: 0,
            prev_was_json_like: false,
            prev_was_separator: true, // At start, we're at "line start"
            indent_stack: vec![0],    // Base indentation level
            errors: Vec::new(),
        }
    }

    /// Add an error at the given span
    fn add_error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError {
            kind,
            span,
            expected: Vec::new(),
            found: None,
        });
    }

    fn mode(&self) -> LexMode {
        if self.flow_depth > 0 {
            LexMode::Flow
        } else {
            LexMode::Block
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        self.chars.get(self.pos + n).copied()
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.chars.get(self.pos) {
            self.pos += 1;
            self.byte_pos += c.len_utf8();
            Some(*c)
        } else {
            None
        }
    }

    fn current_span(&self, start: usize) -> Span {
        Span::new((), start..self.byte_pos)
    }

    fn skip_inline_whitespace(&mut self) {
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.advance();
        }
    }

    fn is_newline(c: char) -> bool {
        matches!(c, '\n' | '\r' | '\u{0085}' | '\u{2028}' | '\u{2029}')
    }

    fn is_flow_indicator(c: char) -> bool {
        matches!(c, ',' | '[' | ']' | '{' | '}')
    }

    /// Check if current position starts with a string.
    fn starts_with(&self, s: &str) -> bool {
        let remaining: String = self.chars[self.pos..].iter().collect();
        remaining.starts_with(s)
    }

    /// Tokenize the document and return tokens with spans and any errors.
    pub fn tokenize(mut self) -> (Vec<Spanned<Token>>, Vec<ParseError>) {
        let mut tokens = Vec::new();

        // Start with LineStart(0) for initial indentation
        tokens.push((Token::LineStart(0), Span::new((), 0..0)));

        while !self.is_eof() {
            if let Some((token, span)) = self.next_token() {
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

                // Track if this token is "JSON-like" for colon indicator detection.
                // Whitespace, LineStart, and Comment tokens don't reset the flag -
                // they act as separators that preserve the "just saw JSON value" state.
                // This allows for multiline flow mappings like:
                //   { "foo"
                //     :bar }
                match &token {
                    Token::Whitespace | Token::LineStart(_) | Token::Comment(_) => {
                        // Don't change prev_was_json_like
                        // These are separators that allow comments to follow
                        self.prev_was_separator = true;
                    }
                    Token::SingleQuoted(_)
                    | Token::DoubleQuoted(_)
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
                if let Token::LineStart(n) = &token {
                    tokens.push((token.clone(), span));

                    // Only emit INDENT/DEDENT in block context (not inside flow collections)
                    if self.flow_depth == 0 {
                        self.emit_indent_dedent_tokens(*n, span, &mut tokens);
                    }
                } else {
                    tokens.push((token, span));
                }
            }
        }

        // At end of input, emit Dedent for remaining stack levels (except base 0)
        if self.flow_depth == 0 {
            let end_span = Span::new((), self.byte_pos..self.byte_pos);
            while self.indent_stack.len() > 1 {
                self.indent_stack.pop();
                tokens.push((Token::Dedent, end_span));
            }
        }

        (tokens, self.errors)
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
        tokens: &mut Vec<Spanned<Token>>,
    ) {
        let current_indent = *self.indent_stack.last().unwrap_or(&0);

        if new_indent > current_indent {
            // Indent increased - push new level
            self.indent_stack.push(new_indent);
            tokens.push((Token::Indent(new_indent), span));
        } else if new_indent < current_indent {
            // Indent decreased - pop levels and emit Dedent for each
            while let Some(&top) = self.indent_stack.last() {
                if top <= new_indent {
                    break;
                }
                self.indent_stack.pop();
                tokens.push((Token::Dedent, span));
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

    /// Get the next token.
    fn next_token(&mut self) -> Option<Spanned<Token>> {
        let start = self.byte_pos;
        let c = self.peek()?;

        // Handle newlines -> produce LineStart with indentation
        if Self::is_newline(c) {
            self.advance();
            // Handle \r\n
            if c == '\r' && self.peek() == Some('\n') {
                self.advance();
            }

            // Count indentation spaces only (tabs are NOT valid for indentation in YAML)
            // The parser will validate that tabs aren't used as indentation in block context.
            let mut indent = 0;
            while self.peek() == Some(' ') {
                self.advance();
                indent += 1;
            }

            return Some((Token::LineStart(indent), self.current_span(start)));
        }

        // Skip inline whitespace
        if c == ' ' || c == '\t' {
            self.skip_inline_whitespace();
            return Some((Token::Whitespace, self.current_span(start)));
        }

        // Comments - `#` only starts a comment if preceded by whitespace or at line start
        if c == '#' {
            if !self.prev_was_separator {
                // `#` without preceding whitespace is invalid - report error
                self.advance();
                let span = self.current_span(start);
                self.add_error(ErrorKind::UnexpectedToken, span);
                // Try to recover by consuming rest of line as if it were a comment
                while let Some(ch) = self.peek() {
                    if Self::is_newline(ch) {
                        break;
                    }
                    self.advance();
                }
                // Return as invalid content - we'll report the error
                return Some((Token::Comment(String::new()), self.current_span(start)));
            }
            let mut content = String::new();
            self.advance(); // consume #
            while let Some(ch) = self.peek() {
                if Self::is_newline(ch) {
                    break;
                }
                content.push(ch);
                self.advance();
            }
            return Some((Token::Comment(content), self.current_span(start)));
        }

        // Flow indicators - always recognized as flow indicators
        // (but can appear in plain scalars in block mode - handled below)
        match c {
            '{' => {
                self.advance();
                return Some((Token::FlowMapStart, self.current_span(start)));
            }
            '}' => {
                self.advance();
                return Some((Token::FlowMapEnd, self.current_span(start)));
            }
            '[' => {
                self.advance();
                return Some((Token::FlowSeqStart, self.current_span(start)));
            }
            ']' => {
                self.advance();
                return Some((Token::FlowSeqEnd, self.current_span(start)));
            }
            ',' if self.mode() == LexMode::Flow => {
                self.advance();
                return Some((Token::Comma, self.current_span(start)));
            }
            _ => {}
        }

        // Block sequence indicator: - followed by whitespace/newline
        if c == '-' {
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
        if c == '?' {
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

        // Colon: Complex rules based on context
        // - After JSON-like value (quoted string, alias, flow end): always indicator
        // - In flow mode: : is indicator if followed by whitespace, newline, EOF, or flow indicator
        //   Otherwise, it starts a plain scalar (e.g., :x or http://...)
        // - In block mode: : is indicator only if followed by whitespace/newline/EOF
        if c == ':' {
            let next = self.peek_n(1);
            let is_indicator = if self.prev_was_json_like && self.mode() == LexMode::Flow {
                // After a JSON-like value, : is ALWAYS an indicator in flow context
                true
            } else {
                match self.mode() {
                    LexMode::Flow => {
                        // In flow context, : is indicator if followed by:
                        // - whitespace, newline, EOF, or flow indicator
                        // Otherwise, it's the start of a plain scalar like :x or ://
                        next.is_none()
                            || next == Some(' ')
                            || next == Some('\t')
                            || next.map(Self::is_newline).unwrap_or(false)
                            || next.map(Self::is_flow_indicator).unwrap_or(false)
                    }
                    LexMode::Block => {
                        // In block context, : is indicator only if followed by whitespace/newline/EOF
                        next.is_none()
                            || next == Some(' ')
                            || next == Some('\t')
                            || next.map(Self::is_newline).unwrap_or(false)
                    }
                }
            };
            if is_indicator {
                self.advance();
                return Some((Token::Colon, self.current_span(start)));
            }
            // Otherwise, : starts a plain scalar - fall through to plain scalar parsing
        }

        // Anchors: &name
        // Per YAML 1.2, anchor names can contain any non-whitespace char except flow indicators
        if c == '&' {
            if let Some(next) = self.peek_n(1) {
                if is_anchor_char(next) {
                    self.advance(); // consume &
                    let name = self.consume_anchor_name();
                    return Some((Token::Anchor(name), self.current_span(start)));
                }
            }
            // & not followed by valid anchor char - will be part of plain scalar
        }

        // Aliases: *name
        if c == '*' {
            if let Some(next) = self.peek_n(1) {
                if is_anchor_char(next) {
                    self.advance(); // consume *
                    let name = self.consume_anchor_name();
                    return Some((Token::Alias(name), self.current_span(start)));
                }
            }
        }

        // Tags: !, !!type, !<uri>
        if c == '!' {
            return Some(self.consume_tag(start));
        }

        // Block scalar headers: | and >
        if c == '|' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::LiteralBlockHeader(header), self.current_span(start)));
        }
        if c == '>' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::FoldedBlockHeader(header), self.current_span(start)));
        }

        // Quoted scalars
        if c == '\'' {
            return Some(self.consume_single_quoted(start));
        }
        if c == '"' {
            return Some(self.consume_double_quoted(start));
        }

        // Plain scalar
        Some(self.consume_plain_scalar(start))
    }

    fn consume_anchor_name(&mut self) -> String {
        // According to YAML 1.2 spec, anchor names (ns-anchor-char+) can contain:
        // - Any non-whitespace character except c-flow-indicator ([]{},)
        // This includes colons and other special characters!
        let mut name = String::new();
        while let Some(c) = self.peek() {
            if is_anchor_char(c) {
                name.push(c);
                self.advance();
            } else {
                break;
            }
        }
        name
    }

    fn consume_tag(&mut self, start: usize) -> Spanned<Token> {
        let mut tag = String::new();
        self.advance(); // consume !

        // Check for !! or !<
        match self.peek() {
            Some('!') => {
                tag.push('!');
                self.advance();
                // Named tag like !!str
                while let Some(c) = self.peek() {
                    if c.is_whitespace() || Self::is_flow_indicator(c) {
                        break;
                    }
                    tag.push(c);
                    self.advance();
                }
            }
            Some('<') => {
                self.advance();
                // Verbatim tag !<uri>
                while let Some(c) = self.peek() {
                    if c == '>' {
                        self.advance();
                        break;
                    }
                    tag.push(c);
                    self.advance();
                }
            }
            Some(c) if Self::is_valid_tag_start_char(c) => {
                // Regular tag !name
                while let Some(c) = self.peek() {
                    if c.is_whitespace() || Self::is_flow_indicator(c) {
                        break;
                    }
                    tag.push(c);
                    self.advance();
                }
            }
            // ! followed by whitespace, EOF, or flow indicator is the non-specific tag
            Some(c) if c.is_whitespace() || Self::is_flow_indicator(c) => {
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
                while let Some(c) = self.peek() {
                    // Stop at whitespace
                    if c.is_whitespace() {
                        break;
                    }
                    // Flow indicators only terminate in flow mode
                    if in_flow && Self::is_flow_indicator(c) {
                        break;
                    }
                    // Colon ends plain scalar if followed by whitespace (or EOF)
                    if c == ':' {
                        let next = self.peek_n(1);
                        if next.map_or(true, |n| n.is_whitespace()) {
                            break;
                        }
                    }
                    plain.push(c);
                    self.advance();
                }
                return (Token::Plain(plain), self.current_span(start));
            }
        }

        (Token::Tag(tag), self.current_span(start))
    }

    /// Check if a character is valid at the start of a tag name.
    /// Valid tag characters are URI characters: alphanumerics and certain punctuation.
    fn is_valid_tag_start_char(c: char) -> bool {
        c.is_alphanumeric()
            || matches!(
                c,
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
                Some(c) if c.is_ascii_digit() && c != '0' => {
                    indent = Some((c as u8) - b'0');
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
        while let Some(c) = self.peek() {
            if Self::is_newline(c) {
                break;
            }
            if c == ' ' || c == '\t' {
                saw_whitespace = true;
                self.advance();
                continue;
            }
            if c == '#' {
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

    fn consume_single_quoted(&mut self, start: usize) -> Spanned<Token> {
        let mut content = String::new();
        self.advance(); // consume opening '
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
                Some(c) => {
                    content.push(c);
                    self.advance();
                }
            }
        }

        let span = self.current_span(start);
        if !terminated {
            self.add_error(ErrorKind::UnterminatedString, span);
        }
        (Token::SingleQuoted(content), span)
    }

    fn consume_double_quoted(&mut self, start: usize) -> Spanned<Token> {
        let mut content = String::new();
        self.advance(); // consume opening "
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
                Some(c) => {
                    content.push(c);
                    self.advance();
                }
            }
        }

        let span = self.current_span(start);
        if !terminated {
            self.add_error(ErrorKind::UnterminatedString, span);
        }
        (Token::DoubleQuoted(content), span)
    }

    fn consume_escape_sequence(&mut self, start_byte_pos: usize) -> Option<String> {
        let c = self.advance()?;
        let result = match c {
            '0' => "\0".to_string(),
            'a' => "\x07".to_string(),
            'b' => "\x08".to_string(),
            't' | '\t' => "\t".to_string(),
            'n' => "\n".to_string(),
            'v' => "\x0B".to_string(),
            'f' => "\x0C".to_string(),
            'r' => "\r".to_string(),
            'e' => "\x1B".to_string(),
            ' ' => " ".to_string(),
            '"' => "\"".to_string(),
            '/' => "/".to_string(),
            '\\' => "\\".to_string(),
            'N' => "\u{0085}".to_string(),
            '_' => "\u{00A0}".to_string(),
            'L' => "\u{2028}".to_string(),
            'P' => "\u{2029}".to_string(),
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
                self.add_error(ErrorKind::InvalidEscape(c), span);
                // Still return the escaped char for error recovery
                c.to_string()
            }
        };
        Some(result)
    }

    fn consume_hex_escape(&mut self, digits: usize) -> String {
        let mut hex = String::new();
        for _ in 0..digits {
            if let Some(c) = self.peek() {
                if c.is_ascii_hexdigit() {
                    hex.push(c);
                    self.advance();
                } else {
                    break;
                }
            }
        }
        if let Ok(code) = u32::from_str_radix(&hex, 16) {
            if let Some(c) = char::from_u32(code) {
                return c.to_string();
            }
        }
        format!("\\x{hex}") // Invalid escape - keep as-is
    }

    /// Consume a plain scalar, respecting the current mode.
    fn consume_plain_scalar(&mut self, start: usize) -> Spanned<Token> {
        let mut content = String::new();
        let mut at_start = true;

        while let Some(c) = self.peek() {
            // Always stop at newlines
            if Self::is_newline(c) {
                break;
            }

            // Handle special indicators at start: -, ?, :
            // These can only start a plain scalar if followed by a "safe" character
            // In flow context, flow indicators are not safe
            if at_start && (c == '-' || c == '?' || c == ':') {
                let next = self.peek_n(1);
                // Check if next is "safe" for plain scalar start
                let is_safe = match next {
                    None => false, // EOF not safe
                    Some(n) => {
                        !n.is_whitespace()
                            && !Self::is_newline(n)
                            && !(self.mode() == LexMode::Flow && Self::is_flow_indicator(n))
                    }
                };
                if !is_safe {
                    // Cannot start plain scalar with this character - emit error and skip it
                    let span = self.current_span(start);
                    self.add_error(ErrorKind::UnexpectedToken, span);
                    self.advance(); // Consume the invalid character to avoid infinite loop
                    return (Token::Plain(String::new()), self.current_span(start));
                }
            }

            // Handle colon - behavior differs between block and flow mode
            if c == ':' {
                if self.mode() == LexMode::Flow {
                    // In flow mode:
                    // - At the start of plain scalar, : can be included if followed by
                    //   non-whitespace and non-flow-indicator (e.g., :x, ://, etc.)
                    // - In the middle, : terminates if followed by whitespace/flow-indicator/EOF
                    let next = self.peek_n(1);
                    let colon_terminates = next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.map(Self::is_newline).unwrap_or(false)
                        || next.map(Self::is_flow_indicator).unwrap_or(false);

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
            if c == ' ' || c == '\t' {
                if self.peek_n(1) == Some('#') {
                    break;
                }
            }

            // Flow indicators - depends on mode
            if Self::is_flow_indicator(c) {
                if self.mode() == LexMode::Flow {
                    break;
                }
                // In block mode, flow indicators are part of plain scalar
            }

            content.push(c);
            self.advance();
            at_start = false;
        }

        (
            Token::Plain(content.trim_end().to_string()),
            self.current_span(start),
        )
    }
}

/// Tokenize document content with context awareness.
/// Returns both the tokens and any errors encountered during lexing.
pub fn tokenize_document(input: &str) -> (Vec<Spanned<Token>>, Vec<ParseError>) {
    ContextLexer::new(input).tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_tokens(input: &str) -> Vec<Token> {
        let (tokens, _errors) = tokenize_document(input);
        tokens
            .into_iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .map(|(t, _)| t)
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
        println!("Tokens: {:?}", tokens);
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::DoubleQuoted("adjacent".into()),
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
                .any(|t| matches!(t, Token::ReservedDirective(_)))
        );
    }

    #[test]
    fn test_colon_value_in_flow() {
        // {x: :x} - value is :x (plain scalar starting with colon)
        let tokens = get_tokens("{x: :x}");
        println!("Tokens: {:?}", tokens);
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
        println!("Tokens: {:?}", tokens);
        // Should recognize http://example.org as a plain scalar
        assert!(tokens.contains(&Token::Plain("http://example.org".into())));
    }
}
