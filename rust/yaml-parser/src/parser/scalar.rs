// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Scalar parsing (plain, quoted, and block scalars).

use crate::error::ErrorKind;
use crate::span::Span;
use crate::token::{BlockScalarHeader, Chomping, QuoteStyle, Token};
use crate::value::{Node, Value};

use super::{NodeProperties, Parser};

impl<'tokens: 'input, 'input> Parser<'tokens, 'input> {
    /// Parse a simple scalar token.
    /// For mapping keys (typically single-line), uses `min_indent=0`.
    pub fn parse_scalar(&mut self) -> Option<Node<'input>> {
        self.parse_scalar_with_indent(0)
    }

    /// Parse a scalar token with a specified minimum indentation for continuations.
    /// This is used when parsing values where multi-line strings must respect indentation.
    pub fn parse_scalar_with_indent(&mut self, min_indent: usize) -> Option<Node<'input>> {
        let (tok, span) = self.peek()?;

        match tok {
            Token::Plain(string) => {
                let value = Self::scalar_to_value(string.to_string());
                self.advance();
                Some(Node::new(value, span))
            }
            Token::StringStart(_style) => self.parse_quoted_string(min_indent),
            _ => None,
        }
    }

    /// Parse a quoted string from `StringStart`/`StringContent`/`LineStart`/`StringEnd` tokens.
    /// Returns the assembled string as a Node, or None if not at a `StringStart` token.
    /// Validates that continuation lines have proper indentation (>= `min_indent`).
    ///
    /// `min_indent` is the minimum indentation required for continuation lines.
    /// For root-level strings (`min_indent=0`), continuation at column 0 is valid.
    /// For strings that are values in mappings, continuation must be >= `min_indent`.
    pub fn parse_quoted_string(&mut self, min_indent: usize) -> Option<Node<'input>> {
        let (first_token, start_span) = self.peek()?;

        let style = match first_token {
            Token::StringStart(style) => *style,
            _ => return None,
        };
        self.advance(); // consume StringStart

        let mut content = String::new();
        let mut end_span = start_span;
        let mut first_content = true;

        loop {
            let Some((tok, span)) = self.peek() else {
                // Unexpected EOF - unterminated string
                self.errors.push(
                    crate::error::ParseError::new(
                        ErrorKind::UnterminatedString,
                        Span::new(
                            start_span.start
                                ..self.tokens.last().map_or(start_span.end, |rt| rt.span.end),
                        ),
                    )
                    .with_expected(vec!["closing quote".to_owned()])
                    .with_found("end of input".to_owned()),
                );
                break;
            };

            match tok {
                Token::StringContent(string) => {
                    if !first_content && !content.is_empty() {
                        // Multi-line string: previous content ended, new content starts
                        // YAML spec: folded newlines become spaces (for flow scalars)
                        content.push(' ');
                    }
                    content.push_str(string);
                    first_content = false;
                    end_span = span;
                    self.advance();
                }
                Token::LineStart(indent) => {
                    // Validate continuation indentation
                    // In YAML, quoted string continuation lines must be indented
                    // at least at the same level as the parent block structure.
                    // For root-level strings (min_indent=0), continuation at column 0 is valid.
                    // For strings in mappings/sequences, continuation must be >= min_indent.
                    //
                    // IMPORTANT: Empty lines (where the next token is LineStart or StringEnd)
                    // are allowed regardless of indentation. Only lines with actual content
                    // (StringContent) need to respect the indentation requirement.
                    let indent = *indent;
                    self.advance();

                    // Check if this is an empty line by peeking at the next token
                    let is_content_line = matches!(self.peek(), Some((Token::StringContent(_), _)));

                    if is_content_line && indent < min_indent {
                        self.errors.push(
                            crate::error::ParseError::new(ErrorKind::InvalidIndentation, span)
                                .with_expected(vec![format!(
                                    "indentation of at least {min_indent} for continuation",
                                )])
                                .with_found(format!("indentation of {indent}")),
                        );
                    }
                }
                Token::StringEnd(end_style) => {
                    if *end_style != style {
                        // Mismatched quotes (shouldn't happen with proper lexing)
                        self.errors.push(
                            crate::error::ParseError::new(ErrorKind::UnexpectedToken, span)
                                .with_expected(vec![format!("closing {style:?} quote")])
                                .with_found(format!("{end_style:?} quote")),
                        );
                    }
                    end_span = span;
                    self.advance();
                    break;
                }
                _ => {
                    // Unexpected token inside string
                    self.errors.push(
                        crate::error::ParseError::new(ErrorKind::UnexpectedToken, span)
                            .with_expected(vec!["string content or closing quote".to_owned()])
                            .with_found(format!("{tok:?}")),
                    );
                    break;
                }
            }
        }

        let full_span = Span::new(start_span.start..end_span.end);
        Some(Node::new(Value::String(content.into()), full_span))
    }

    /// Convert a plain scalar string to an appropriate Value.
    pub fn scalar_to_value(scalar: String) -> Value<'static> {
        match scalar.as_str() {
            "null" | "Null" | "NULL" | "~" | "" => Value::Null,
            "true" | "True" | "TRUE" => Value::Bool(true),
            "false" | "False" | "FALSE" => Value::Bool(false),
            _ => {
                if let Ok(integer) = scalar.parse::<i64>() {
                    return Value::Int(integer);
                }
                if let Some(hex) = scalar
                    .strip_prefix("0x")
                    .or_else(|| scalar.strip_prefix("0X"))
                    && let Ok(integer) = i64::from_str_radix(hex, 16)
                {
                    return Value::Int(integer);
                }
                if let Some(oct) = scalar
                    .strip_prefix("0o")
                    .or_else(|| scalar.strip_prefix("0O"))
                    && let Ok(integer) = i64::from_str_radix(oct, 8)
                {
                    return Value::Int(integer);
                }
                if let Ok(float) = scalar.parse::<f64>() {
                    return Value::Float(float);
                }
                match scalar.as_str() {
                    ".inf" | ".Inf" | ".INF" => return Value::Float(f64::INFINITY),
                    "-.inf" | "-.Inf" | "-.INF" => return Value::Float(f64::NEG_INFINITY),
                    ".nan" | ".NaN" | ".NAN" => return Value::Float(f64::NAN),
                    _ => {}
                }
                Value::String(scalar.into())
            }
        }
    }

    /// Parse an alias: *name
    pub fn parse_alias(&mut self) -> Option<Node<'input>> {
        let (tok, span) = self.advance()?;

        let name = if let Token::Alias(name) = tok {
            name.clone()
        } else {
            return None;
        };

        if !self.anchors.contains_key(name.as_ref()) {
            self.error(ErrorKind::UndefinedAlias, span);
        }
        Some(Node::new(Value::Alias(name), span))
    }

    /// Parse a literal block scalar: |
    pub fn parse_literal_block_scalar(&mut self, _min_indent: usize) -> Option<Node<'input>> {
        let (tok, span) = self.advance()?;
        let start = span.start;

        let header = if let Token::LiteralBlockHeader(hdr) = tok {
            hdr.clone()
        } else {
            return None;
        };

        let (content, end) = self.collect_block_scalar_content(&header, true);
        Some(Node::new(
            Value::String(content.into()),
            Span::new(start..end),
        ))
    }

    /// Parse a folded block scalar: >
    pub fn parse_folded_block_scalar(&mut self, _min_indent: usize) -> Option<Node<'input>> {
        let (tok, span) = self.advance()?;
        let start = span.start;

        let header = if let Token::FoldedBlockHeader(hdr) = tok {
            hdr.clone()
        } else {
            return None;
        };

        let (content, end) = self.collect_block_scalar_content(&header, false);
        Some(Node::new(
            Value::String(content.into()),
            Span::new(start..end),
        ))
    }

    /// Collect block scalar content, respecting indentation.
    #[allow(
        clippy::too_many_lines,
        reason = "Complex block scalar parsing logic, will be refactored later"
    )]
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by parser logic before access"
    )]
    pub fn collect_block_scalar_content(
        &mut self,
        header: &BlockScalarHeader,
        literal: bool,
    ) -> (String, usize) {
        let mut lines: Vec<String> = Vec::new();
        let mut content_indent: Option<usize> = header.indent.map(usize::from);
        let mut end_pos = self.current_span().end;

        // Track whitespace-only lines before the first content line.
        // These must have at most as many spaces as the first content line's indentation.
        // Store (indentation_level, span) for each empty line before content.
        let mut empty_lines_before_content: Vec<(usize, Span)> = Vec::new();

        loop {
            while let Some((Token::Dedent | Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            let Some((Token::LineStart(n), span)) = self.peek() else {
                break;
            };
            let n = *n;

            let is_empty_line = {
                let mut check_pos = self.pos + 1;
                while check_pos < self.tokens.len() {
                    match &self.tokens[check_pos].token {
                        Token::Dedent | Token::Indent(_) => check_pos += 1,
                        _ => break,
                    }
                }
                if check_pos >= self.tokens.len() {
                    true
                } else {
                    matches!(self.tokens[check_pos].token, Token::LineStart(_))
                }
            };

            if content_indent.is_none() && !is_empty_line {
                content_indent = Some(n);
                // Validate that all preceding empty lines have at most n spaces
                for (empty_indent, empty_span) in &empty_lines_before_content {
                    if *empty_indent > n {
                        self.error(ErrorKind::InvalidIndentation, *empty_span);
                    }
                }
            }

            let min_indent = content_indent.unwrap_or(1);

            if !is_empty_line && n < min_indent {
                break;
            }

            self.advance(); // consume LineStart
            end_pos = span.end;

            if let Some((Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            // Check for tabs used as indentation in block scalar content.
            // Tabs are invalid for indentation at the start of a line (after LineStart).
            // However, tabs are allowed AFTER space-based indentation (as content).
            // Only report error if LineStart showed 0 indentation (no spaces before tab).
            if n == 0
                && let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek()
            {
                self.error(ErrorKind::InvalidIndentation, tab_span);
            }

            if is_empty_line {
                // Track this empty line's indentation if we haven't found content yet
                if content_indent.is_none() {
                    empty_lines_before_content.push((n, span));
                }
                lines.push(String::new());
                continue;
            }

            let mut line_content = String::new();
            let extra_indent = n.saturating_sub(min_indent);

            for _ in 0..extra_indent {
                line_content.push(' ');
            }

            while let Some((tok, tok_span)) = self.peek() {
                match tok {
                    Token::Dedent | Token::Indent(_) => {
                        self.advance();
                    }
                    Token::Plain(string) | Token::StringContent(string) => {
                        line_content.push_str(string);
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::StringStart(style) => {
                        // In block scalar content, quote characters are literal content
                        let quote_char = match style {
                            QuoteStyle::Single => '\'',
                            QuoteStyle::Double => '"',
                        };
                        line_content.push(quote_char);
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::StringEnd(style) => {
                        let quote_char = match style {
                            QuoteStyle::Single => '\'',
                            QuoteStyle::Double => '"',
                        };
                        line_content.push(quote_char);
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::Whitespace | Token::WhitespaceWithTabs => {
                        line_content.push(' ');
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::Comment(comment) => {
                        line_content.push('#');
                        line_content.push_str(comment);
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::LineStart(_) | Token::DocStart | Token::DocEnd => break,
                    _ => {
                        end_pos = tok_span.end;
                        self.advance();
                    }
                }
            }

            lines.push(line_content);
        }

        let mut content = if literal {
            lines.join("\n")
        } else {
            let mut result = String::new();
            let mut prev_empty = false;
            for line in &lines {
                if line.is_empty() {
                    result.push('\n');
                    prev_empty = true;
                } else {
                    if !result.is_empty() && !prev_empty {
                        result.push(' ');
                    }
                    result.push_str(line);
                    prev_empty = false;
                }
            }
            result
        };

        match header.chomping {
            Chomping::Strip => {
                while content.ends_with('\n') {
                    content.pop();
                }
            }
            Chomping::Clip => {
                while content.ends_with("\n\n") {
                    content.pop();
                }
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }
            Chomping::Keep => {
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }
        }

        (content, end_pos)
    }

    /// Convert multiline scalar content to final value.
    fn finalize_multiline_scalar(
        content: String,
        had_continuations: bool,
        start: usize,
        end: usize,
    ) -> Node<'static> {
        // If we had continuations, always treat as string (no type coercion)
        // If no continuations, apply normal scalar type detection
        let value = if had_continuations {
            Value::String(content.into())
        } else {
            Self::scalar_to_value(content)
        };
        Node::new(value, Span::new(start..end))
    }

    /// Parse a scalar and check if it's actually a mapping key.
    pub fn parse_scalar_or_mapping(&mut self, min_indent: usize) -> Option<Node<'input>> {
        // Remember if the scalar we're about to parse is a quoted scalar, and its start position
        let is_quoted_scalar = matches!(self.peek(), Some((Token::StringStart(_), _)));
        let scalar_start_pos = self.pos;

        // Use min_indent for proper continuation validation in quoted strings
        let scalar = self.parse_scalar_with_indent(min_indent)?;

        // Skip whitespace and track if we saw a comment
        // A comment terminates a plain scalar, so we shouldn't look for continuations
        let mut saw_comment = false;
        while let Some((tok, _)) = self.peek() {
            match tok {
                Token::Whitespace | Token::WhitespaceWithTabs | Token::Indent(_) => {
                    self.advance();
                }
                Token::Comment(_) => {
                    saw_comment = true;
                    self.advance();
                }
                _ => break,
            }
        }

        // Check if this is a mapping key (followed by colon)
        if let Some((Token::Colon, colon_span)) = self.peek() {
            // Check for multiline implicit key (invalid in block context)
            self.check_multiline_implicit_key(scalar.span.start, scalar.span.end);

            // Check for invalid nested mapping on same line.
            // Pattern like `a: b: c` or `a: 'b': c` is invalid
            let is_nested_value_position = {
                let mut found_implicit_key_colon = false;
                for i in (0..scalar_start_pos).rev() {
                    #[allow(
                        clippy::indexing_slicing,
                        reason = "i is from range 0..scalar_start_pos"
                    )]
                    match &self.tokens[i].token {
                        Token::Colon => {
                            let mut has_key_before_colon = false;
                            for j in (0..i).rev() {
                                #[allow(clippy::indexing_slicing, reason = "j is from range 0..i")]
                                match &self.tokens[j].token {
                                    Token::LineStart(_) | Token::MappingKey => {
                                        break;
                                    }
                                    Token::Plain(_)
                                    | Token::StringStart(_)
                                    | Token::StringEnd(_)
                                    | Token::FlowSeqEnd
                                    | Token::FlowMapEnd => {
                                        has_key_before_colon = true;
                                        break;
                                    }
                                    _ => {}
                                }
                            }
                            if has_key_before_colon {
                                found_implicit_key_colon = true;
                            }
                        }
                        Token::LineStart(_) => {
                            break;
                        }
                        _ => {}
                    }
                }
                found_implicit_key_colon
            };

            if is_nested_value_position {
                self.error(ErrorKind::UnexpectedToken, colon_span);
                return Some(scalar);
            }

            // This is actually a mapping - reparse
            let start = scalar.span.start;
            let map_indent = self.column_of_position(scalar.span.start);
            let mut pairs: Vec<(Node, Node)> = Vec::new();

            let key = scalar;

            self.advance(); // consume ':'
            self.check_tabs_after_block_indicator();
            self.skip_ws();

            // Check for block sequence indicator on same line as key - invalid in YAML
            if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                self.error(ErrorKind::UnexpectedToken, span);
            }

            // Parse the value
            let value_on_same_line = !matches!(self.peek(), Some((Token::LineStart(_), _)));
            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.check_tabs_as_indentation();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

            // Check for trailing content after quoted scalars in block context
            if value_on_same_line && let Value::String(_) = &value_node.value {
                self.check_trailing_content_after_scalar();
            }

            pairs.push((key, value_node));

            // Continue parsing more key-value pairs at same indentation
            self.parse_remaining_mapping_entries(&mut pairs, map_indent);

            let end = pairs.last().map_or(start, |(_, val)| val.span.end);
            Some(Node::new(Value::Mapping(pairs), Span::new(start..end)))
        } else {
            // Just a scalar - check for multiline continuation (plain scalars only)
            // Note: If we saw a comment after the scalar, it terminates the plain scalar
            // and we should NOT look for continuation lines (YAML spec section 7.3.3)
            if is_quoted_scalar || saw_comment {
                // Comment terminated the scalar - check for trailing content at root
                if min_indent == 0 && saw_comment {
                    self.check_trailing_content_at_root(0);
                }
                Some(scalar)
            } else {
                Some(self.consume_plain_scalar_continuations(scalar, min_indent))
            }
        }
    }

    /// Helper to parse remaining mapping entries at the same indentation level.
    #[allow(
        clippy::too_many_lines,
        reason = "Complex mapping parsing logic, will be refactored later"
    )]
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by parser logic before access"
    )]
    fn parse_remaining_mapping_entries(
        &mut self,
        pairs: &mut Vec<(Node<'input>, Node<'input>)>,
        map_indent: usize,
    ) {
        loop {
            let loop_start_pos = self.pos;

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            // Check for next line at same indent
            let at_same_indent = loop {
                if let Some((Token::LineStart(n), _)) = self.peek() {
                    if *n < map_indent {
                        break false;
                    }
                    if *n == map_indent {
                        self.advance();
                        while let Some((Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }
                        // Check for tabs used as indentation after LineStart
                        self.check_tabs_as_indentation();
                        break true;
                    }
                    self.advance();
                } else if let Some((Token::Dedent, _)) = self.peek() {
                    self.advance();
                } else {
                    break false;
                }
            };

            if !at_same_indent {
                break;
            }

            self.skip_ws();

            // Skip any additional LineStart tokens (e.g., from blank lines)
            while let Some((Token::LineStart(n), _)) = self.peek() {
                if *n != map_indent {
                    break;
                }
                self.advance();
            }

            self.skip_ws();

            // Collect any anchor/tag properties before the key
            let mut key_props = NodeProperties::default();
            loop {
                match self.peek() {
                    Some((Token::Anchor(name), anchor_span)) => {
                        let anchor_name = name.clone();
                        self.advance();
                        self.skip_ws();
                        if key_props.anchor.is_some() {
                            self.error(ErrorKind::DuplicateAnchor, anchor_span);
                        }
                        key_props.anchor = Some((anchor_name, anchor_span));
                    }
                    Some((Token::Tag(name), tag_span)) => {
                        let tag = name.clone();
                        self.advance();
                        self.skip_ws();
                        if key_props.tag.is_some() {
                            self.error(ErrorKind::DuplicateTag, tag_span);
                        }
                        key_props.tag = Some((tag, tag_span));
                    }
                    Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => {
                        self.advance();
                    }
                    _ => break,
                }
            }

            // Try to parse another key (can be scalar or alias)
            let key = if let Some((Token::Alias(name), span)) = self.peek() {
                // Check if the key is an alias with properties (invalid)
                let alias_name = name.clone();
                if !key_props.is_empty() {
                    self.error(ErrorKind::PropertiesOnAlias, span);
                }
                self.advance();
                if !self.anchors.contains_key(alias_name.as_ref()) {
                    self.error(ErrorKind::UndefinedAlias, span);
                }
                Node::new(Value::Alias(alias_name), span)
            } else if let Some(key) = self.parse_scalar() {
                if key_props.is_empty() {
                    key
                } else {
                    self.apply_properties_and_register(key_props, key)
                }
            } else {
                // If we collected properties (anchor/tag) but couldn't parse a scalar key,
                // report an error for the orphaned properties (e.g., &anchor at column 0
                // followed by a block sequence indicator on the next line)
                if let Some((_, anchor_span)) = key_props.anchor {
                    self.error(ErrorKind::UnexpectedToken, anchor_span);
                } else if let Some((_, tag_span)) = key_props.tag {
                    self.error(ErrorKind::UnexpectedToken, tag_span);
                }
                break;
            };

            self.skip_ws();

            if let Some((Token::Colon, _)) = self.peek() {
                self.check_multiline_implicit_key(key.span.start, key.span.end);
                self.advance();
            } else {
                self.error(ErrorKind::UnexpectedToken, key.span);
                break;
            }

            self.skip_ws();

            // Check for block sequence indicator on same line as key - invalid
            if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                self.error(ErrorKind::UnexpectedToken, span);
            }

            let value_on_same_line = !matches!(self.peek(), Some((Token::LineStart(_), _)));
            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

            if value_on_same_line && let Value::String(_) = &value_node.value {
                self.check_trailing_content_after_scalar();
            }

            pairs.push((key, value_node));

            // Ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                break;
            }
        }
    }

    /// Consume multiline continuation lines for a plain scalar.
    ///
    /// Called after the first line of a plain scalar has been parsed.
    /// Continuation lines must be more indented than the scalar's starting column.
    #[allow(
        clippy::too_many_lines,
        reason = "Complex plain scalar continuation logic, will be refactored later"
    )]
    #[allow(
        clippy::string_slice,
        reason = "Positions are from lexer tokens and guaranteed to be on UTF-8 boundaries"
    )]
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by parser logic before access"
    )]
    pub fn consume_plain_scalar_continuations(
        &mut self,
        initial: Node<'input>,
        block_min_indent: usize,
    ) -> Node<'input> {
        let start = initial.span.start;
        let mut end = initial.span.end;

        // For plain scalar continuations, the minimum indent for continuation lines is:
        // - At least 1 (continuation must be indented)
        // - Greater than or equal to block_min_indent (must be in valid block context)
        let min_indent = block_min_indent.max(1);

        // Extract the initial content
        let mut content = match &initial.value {
            Value::String(string) => string.clone().into_owned(),
            Value::Bool(boolean) => boolean.to_string(),
            Value::Int(integer) => integer.to_string(),
            Value::Float(float) => float.to_string(),
            Value::Null => String::new(),
            _ => return initial, // Not a scalar type, return as-is
        };

        // Track consecutive empty lines for folding
        let mut consecutive_newlines = 0;
        let mut had_continuations = false;

        while let Some((Token::LineStart(n), _)) = self.peek() {
            let indent = *n;

            // Check if this might be an empty line (LineStart(0) followed by LineStart)
            if indent < min_indent {
                // Could be an empty line - check if later content continues
                let mut next_pos = self.pos + 1;
                let mut found_continuation = false;
                while next_pos < self.tokens.len() {
                    match &self.tokens[next_pos].token {
                        Token::Dedent | Token::Indent(_) => {
                            next_pos += 1;
                        }
                        Token::LineStart(next_indent) if *next_indent >= min_indent => {
                            consecutive_newlines += 1;
                            had_continuations = true;
                            self.advance();
                            while matches!(self.peek(), Some((Token::Dedent, _))) {
                                self.advance();
                            }
                            found_continuation = true;
                            break;
                        }
                        Token::Whitespace | Token::WhitespaceWithTabs => {
                            // Tab/whitespace at start of line followed by content
                            let after_ws = next_pos + 1;
                            if after_ws < self.tokens.len() {
                                // Check if this is a mapping key (Plain followed by Colon)
                                // If so, it's NOT a scalar continuation - return early
                                if let Token::Plain(_) = &self.tokens[after_ws].token {
                                    let mut lookahead = after_ws + 1;
                                    while lookahead < self.tokens.len() {
                                        match &self.tokens[lookahead].token {
                                            Token::Whitespace | Token::WhitespaceWithTabs => {
                                                lookahead += 1;
                                            }
                                            Token::Colon => {
                                                // This is a mapping key, not a scalar continuation
                                                // Don't emit tab error here - let the mapping
                                                // parser handle it via check_tabs_as_indentation
                                                return Self::finalize_multiline_scalar(
                                                    content,
                                                    had_continuations,
                                                    start,
                                                    end,
                                                );
                                            }
                                            _ => break,
                                        }
                                    }
                                }

                                match &self.tokens[after_ws].token {
                                    Token::Plain(_)
                                    | Token::Anchor(_)
                                    | Token::Tag(_)
                                    | Token::Alias(_)
                                    | Token::BlockSeqIndicator => {
                                        if consecutive_newlines == 0 {
                                            content.push(' ');
                                        } else {
                                            for _ in 0..consecutive_newlines {
                                                content.push('\n');
                                            }
                                        }

                                        self.advance(); // consume LineStart
                                        while matches!(self.peek(), Some((Token::Dedent, _))) {
                                            self.advance();
                                        }
                                        self.advance(); // consume Whitespace (tab)

                                        match self.peek() {
                                            Some((Token::Plain(string), plain_span)) => {
                                                let continuation = string.clone();
                                                end = plain_span.end;
                                                content.push_str(&continuation);
                                                self.advance();
                                            }
                                            Some((
                                                Token::Anchor(_)
                                                | Token::Tag(_)
                                                | Token::Alias(_)
                                                | Token::BlockSeqIndicator,
                                                span,
                                            )) => {
                                                let line_start_pos = span.start;
                                                let mut line_end = span.end;
                                                while let Some((tok, tok_span)) = self.peek() {
                                                    if let Token::LineStart(_) = tok {
                                                        break;
                                                    }
                                                    line_end = tok_span.end;
                                                    self.advance();
                                                }
                                                let line_text =
                                                    &self.input[line_start_pos..line_end];
                                                content.push_str(line_text.trim_end());
                                                end = line_end;
                                            }
                                            _ => {}
                                        }

                                        had_continuations = true;
                                        consecutive_newlines = 0;
                                        found_continuation = true;
                                        break;
                                    }
                                    _ => {}
                                }
                            }
                            return Self::finalize_multiline_scalar(
                                content,
                                had_continuations,
                                start,
                                end,
                            );
                        }
                        _ => {
                            return Self::finalize_multiline_scalar(
                                content,
                                had_continuations,
                                start,
                                end,
                            );
                        }
                    }
                }
                if !found_continuation && next_pos >= self.tokens.len() {
                    break;
                }
            }

            // Check if the following content is a mapping key
            let next_pos = self.pos + 1;
            if next_pos < self.tokens.len() {
                let mut content_pos = next_pos;
                while content_pos < self.tokens.len() {
                    match &self.tokens[content_pos].token {
                        Token::Whitespace | Token::WhitespaceWithTabs | Token::Indent(_) => {
                            content_pos += 1;
                        }
                        _ => break,
                    }
                }

                if content_pos < self.tokens.len()
                    && let Token::Plain(_) = &self.tokens[content_pos].token
                {
                    let mut lookahead = content_pos + 1;
                    while lookahead < self.tokens.len() {
                        match &self.tokens[lookahead].token {
                            Token::Whitespace | Token::WhitespaceWithTabs => lookahead += 1,
                            Token::Colon => {
                                return Self::finalize_multiline_scalar(
                                    content,
                                    had_continuations,
                                    start,
                                    end,
                                );
                            }
                            _ => break,
                        }
                    }
                }
            }

            // Valid continuation, consume the LineStart
            self.advance();

            while let Some((Token::Indent(_) | Token::Whitespace | Token::WhitespaceWithTabs, _)) =
                self.peek()
            {
                self.advance();
            }

            match self.peek() {
                Some((Token::Plain(string), plain_span)) => {
                    let continuation = string.clone();
                    end = plain_span.end;
                    had_continuations = true;

                    if consecutive_newlines == 0 {
                        content.push(' ');
                    } else {
                        for _ in 0..consecutive_newlines {
                            content.push('\n');
                        }
                    }
                    content.push_str(&continuation);
                    consecutive_newlines = 0;

                    self.advance();
                }
                Some((Token::LineStart(_), _)) => {
                    consecutive_newlines += 1;
                    had_continuations = true;
                }
                Some((
                    Token::Anchor(_) | Token::Tag(_) | Token::Alias(_) | Token::BlockSeqIndicator,
                    span,
                )) => {
                    let line_start_pos = span.start;
                    let mut line_end = span.end;

                    while let Some((tok, tok_span)) = self.peek() {
                        if let Token::LineStart(_) = tok {
                            break;
                        }
                        line_end = tok_span.end;
                        self.advance();
                    }

                    let line_text = &self.input[line_start_pos..line_end];
                    let continuation = line_text.trim_end();

                    if !continuation.is_empty() {
                        had_continuations = true;

                        if consecutive_newlines == 0 {
                            content.push(' ');
                        } else {
                            for _ in 0..consecutive_newlines {
                                content.push('\n');
                            }
                        }
                        content.push_str(continuation);
                        consecutive_newlines = 0;
                        end = line_end;
                    }
                }
                _ => {
                    break;
                }
            }
        }
        Self::finalize_multiline_scalar(content, had_continuations, start, end)
    }
}
