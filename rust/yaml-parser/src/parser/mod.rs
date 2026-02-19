// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML parser with error recovery.
//!
//! This module implements the second phase of parsing: converting
//! the token stream into an AST. The parser handles both block and
//! flow styles, tracking indentation for block structures.
//!
//! The parser properly treats anchors and tags as node properties rather
//! than value wrappers. This means `&anchor key: value` correctly parses
//! with the anchor attached to the key scalar, not wrapping the mapping.

mod block;
mod flow;
mod scalar;

use std::collections::HashMap;

use chumsky::span::Span as _;

use crate::error::{ErrorKind, ParseError};
use crate::lexer::Token;
use crate::span::{Span, Spanned};
use crate::value::{Node, Value};

/// A stream of YAML documents.
pub type Stream = Vec<Node>;

/// Pending node properties (anchor, tag) collected before parsing the value.
#[derive(Debug, Default, Clone)]
pub(crate) struct NodeProperties {
    pub anchor: Option<(String, Span)>,
    pub tag: Option<(String, Span)>,
    /// Whether we've crossed a line boundary while accumulating these properties.
    /// This is important for distinguishing between:
    /// - `&a &b value` (invalid: two anchors on same node)
    /// - `&a\n&b value` (valid: &a on containing structure, &b on nested value)
    pub crossed_line_boundary: bool,
}

impl NodeProperties {
    pub fn is_empty(&self) -> bool {
        self.anchor.is_none() && self.tag.is_none()
    }

    /// Apply these properties to a node, updating its span to include properties.
    pub fn apply_to(self, mut node: Node) -> Node {
        if let Some((anchor, anchor_span)) = self.anchor {
            node.anchor = Some(anchor);
            // Extend span to include the anchor
            if anchor_span.start < node.span.start {
                node.span = Span::new((), anchor_span.start..node.span.end);
            }
        }
        if let Some((tag, tag_span)) = self.tag {
            node.tag = Some(tag);
            // Extend span to include the tag
            if tag_span.start < node.span.start {
                node.span = Span::new((), tag_span.start..node.span.end);
            }
        }
        node
    }
}

/// Parser state for tracking position and context.
#[derive(Debug)]
pub(crate) struct Parser<'a> {
    pub tokens: &'a [Spanned<Token>],
    pub input: &'a str,
    pub pos: usize,
    pub errors: Vec<ParseError>,
    /// Map of anchor names to their nodes (for alias resolution)
    pub anchors: HashMap<String, Node>,
    /// Flow depth tracking (0 = block context, > 0 = inside flow collections)
    pub flow_depth: usize,
    /// Indentation stack tracking active block structure levels.
    /// Each entry is the indentation level of an active block structure.
    /// Used to detect orphan indentation (content at levels not in the stack).
    pub indent_stack: Vec<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Spanned<Token>], input: &'a str) -> Self {
        Self {
            tokens,
            input,
            pos: 0,
            errors: Vec::new(),
            anchors: HashMap::new(),
            flow_depth: 0,
            indent_stack: vec![0], // Start with base level 0
        }
    }

    /// Check if we've reached the end of input.
    pub fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Peek at the current token without consuming it.
    pub fn peek(&self) -> Option<&Spanned<Token>> {
        self.tokens.get(self.pos)
    }

    /// Peek at the token N positions ahead.
    #[allow(dead_code)]
    pub fn peek_n(&self, n: usize) -> Option<&Spanned<Token>> {
        self.tokens.get(self.pos + n)
    }

    /// Consume the current token and return it.
    pub fn advance(&mut self) -> Option<&Spanned<Token>> {
        if self.pos < self.tokens.len() {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    /// Skip whitespace and comments, but NOT line starts.
    /// Also skips Indent tokens (they follow LineStart and are informational).
    pub fn skip_ws(&mut self) {
        while let Some((tok, _)) = self.peek() {
            match tok {
                Token::Whitespace | Token::Comment(_) | Token::Indent(_) => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Skip whitespace, comments, AND line starts.
    /// Also checks for tabs used as indentation in block context.
    /// Note: This does NOT skip Dedent tokens - those signal structure boundaries.
    pub fn skip_ws_and_newlines(&mut self) {
        while let Some((tok, _)) = self.peek() {
            match tok {
                Token::LineStart(_) => {
                    self.advance();
                    // After LineStart, check if next token is Whitespace containing tabs
                    // This indicates tabs being used as indentation
                    self.check_tabs_as_indentation();
                }
                Token::Indent(_) => {
                    // Indent tokens are informational after LineStart - skip them
                    self.advance();
                }
                Token::Whitespace | Token::Comment(_) => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Get the column (0-based) of the current token by looking back to the last newline.
    pub fn current_token_column(&self) -> usize {
        if let Some((_, span)) = self.peek() {
            self.column_of_position(span.start)
        } else {
            0
        }
    }

    /// Get the column (0-based) of a byte position by looking back to the last newline.
    pub fn column_of_position(&self, pos: usize) -> usize {
        let before = &self.input[..pos];
        if let Some(newline_pos) = before.rfind('\n') {
            pos - newline_pos - 1
        } else {
            pos // No newline, column is the byte position from start of input
        }
    }

    /// Apply node properties to a node and register the anchor if present.
    pub fn apply_properties_and_register(&mut self, props: NodeProperties, node: Node) -> Node {
        let node = props.apply_to(node);
        // If the node has an anchor, register it in the anchors map
        if let Some(ref anchor_name) = node.anchor {
            self.anchors.insert(anchor_name.clone(), node.clone());
        }
        node
    }

    /// Push an indentation level onto the stack when entering a block structure.
    pub fn push_indent(&mut self, indent: usize) {
        self.indent_stack.push(indent);
    }

    /// Pop an indentation level from the stack when exiting a block structure.
    pub fn pop_indent(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    /// Check if an indentation level is valid (matches some level in the stack).
    /// Returns true if valid, false if orphan.
    #[allow(dead_code)]
    pub fn is_valid_indent(&self, indent: usize) -> bool {
        self.indent_stack.contains(&indent)
    }

    /// Check for orphan block indicator at the current position.
    /// An orphan block indicator is a `-`, `?`, or `:` at an indentation level
    /// that doesn't match any active structure in the indent stack.
    /// Only checks in block context (flow_depth == 0).
    #[allow(dead_code)]
    pub fn check_orphan_block_indicator(&mut self) {
        if self.flow_depth > 0 {
            return; // No orphan detection in flow context
        }

        // Get current indentation from the most recent LineStart
        let indent = self.current_indent();

        // Check if we're at a block indicator
        if let Some((tok, span)) = self.peek() {
            let is_block_indicator = matches!(
                tok,
                Token::BlockSeqIndicator | Token::MappingKey | Token::Colon
            );

            if is_block_indicator && !self.is_valid_indent(indent) {
                // Orphan block indicator - doesn't match any active structure
                self.error(ErrorKind::InvalidIndentation, *span);
            }
        }
    }

    /// Check if there's whitespace (tabs) after LineStart that would indicate
    /// invalid tab indentation in block context.
    pub fn check_tabs_as_indentation(&mut self) {
        // Tabs are only invalid for indentation in BLOCK context
        if self.flow_depth > 0 {
            return;
        }

        if self.pos == 0 {
            return;
        }

        // Check: is the previous token LineStart and current token Whitespace?
        if let Some((Token::LineStart(_), _)) = self.tokens.get(self.pos - 1) {
            if let Some((Token::Whitespace, ws_span)) = self.peek() {
                let ws_span = *ws_span;
                let ws_content = &self.input[ws_span.start..ws_span.end];

                if ws_content.contains('\t') {
                    let next_token_pos = self.pos + 1;
                    let is_flow_content = if let Some((tok, _)) = self.tokens.get(next_token_pos) {
                        matches!(
                            tok,
                            Token::FlowMapStart
                                | Token::FlowMapEnd
                                | Token::FlowSeqStart
                                | Token::FlowSeqEnd
                        )
                    } else {
                        false
                    };

                    if !is_flow_content {
                        self.error(ErrorKind::InvalidIndentation, ws_span);
                    }
                }
            }
        }
    }

    /// Check for invalid content immediately after a flow collection in block context.
    pub fn check_content_after_flow(&mut self, flow_end: usize) {
        if let Some((tok, span)) = self.peek() {
            let is_content = matches!(
                tok,
                Token::Plain(_)
                    | Token::SingleQuoted(_)
                    | Token::DoubleQuoted(_)
                    | Token::Anchor(_)
                    | Token::Alias(_)
                    | Token::Tag(_)
                    | Token::BlockSeqIndicator
            );
            if is_content && span.start == flow_end {
                self.error(ErrorKind::UnexpectedToken, *span);
            }
        }
    }

    /// Check if a value used as an implicit key spans multiple lines.
    pub fn check_multiline_implicit_key(&mut self, key_start: usize, key_end: usize) {
        let mut check_pos = self.pos;
        while check_pos < self.tokens.len() {
            match &self.tokens[check_pos].0 {
                Token::Whitespace => check_pos += 1,
                Token::Colon => break,
                _ => return,
            }
        }
        if check_pos >= self.tokens.len() {
            return;
        }
        let colon_span = self.tokens[check_pos].1;

        let key_text = &self.input[key_start..key_end.min(self.input.len())];
        if key_text.contains('\n') {
            self.error(ErrorKind::UnexpectedToken, colon_span);
            return;
        }

        for (tok, span) in &self.tokens[..self.pos] {
            if span.start > key_start && span.end <= key_end {
                if matches!(tok, Token::LineStart(_)) {
                    self.error(ErrorKind::UnexpectedToken, colon_span);
                    return;
                }
            }
        }
    }

    /// Check for invalid trailing content after a scalar value in block context.
    pub fn check_trailing_content_after_scalar(&mut self) {
        self.skip_ws();

        if let Some((Token::Comment(_), _)) = self.peek() {
            return;
        }

        if let Some((Token::LineStart(_), _)) = self.peek() {
            return;
        }

        if self.is_eof() {
            return;
        }

        if let Some((Token::DocStart | Token::DocEnd, _)) = self.peek() {
            return;
        }

        if let Some((_, span)) = self.peek() {
            let span = *span;
            self.error(ErrorKind::UnexpectedToken, span);
        }
    }

    /// Get the current indentation level from the most recent LineStart.
    pub fn current_indent(&self) -> usize {
        for i in (0..self.pos).rev() {
            if let (Token::LineStart(n), _) = &self.tokens[i] {
                return *n;
            }
        }
        0
    }

    /// Get the span of the current position.
    pub fn current_span(&self) -> Span {
        self.peek()
            .map(|(_, span)| *span)
            .unwrap_or_else(|| Span::new((), 0..0))
    }

    /// Check if current position is a mapping key pattern (scalar followed by colon).
    pub fn is_mapping_key_pattern(&self) -> bool {
        let mut i = self.pos;
        match self.tokens.get(i) {
            Some((Token::Plain(_) | Token::SingleQuoted(_) | Token::DoubleQuoted(_), _)) => {
                i += 1;
            }
            _ => return false,
        }
        while let Some((Token::Whitespace, _)) = self.tokens.get(i) {
            i += 1;
        }
        matches!(self.tokens.get(i), Some((Token::Colon, _)))
    }

    /// Add an error.
    pub fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError {
            kind,
            span,
            expected: Vec::new(),
            found: None,
        });
    }

    /// Parse a complete YAML stream (multiple documents).
    pub fn parse_stream(&mut self) -> Stream {
        let mut documents = Vec::new();

        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let start_pos = self.pos;

            let explicit_doc_start = if let Some((Token::DocStart, span)) = self.peek() {
                let span = *span;
                self.advance();
                self.skip_ws_and_newlines();
                Some(span)
            } else {
                None
            };

            if self.is_eof() || matches!(self.peek(), Some((Token::DocStart | Token::DocEnd, _))) {
                if explicit_doc_start.is_some() || !documents.is_empty() || self.pos > start_pos {
                    let span = explicit_doc_start
                        .map(|s| Span::new((), s.end..s.end))
                        .unwrap_or_else(|| self.current_span());
                    documents.push(Node::null(span));
                }
            } else {
                if let Some(node) = self.parse_value(0) {
                    documents.push(node);
                }
            }

            self.skip_ws_and_newlines();

            if let Some((Token::DocEnd, _)) = self.peek() {
                self.advance();
                self.skip_ws_and_newlines();
            }

            if self.pos == start_pos && !self.is_eof() {
                self.advance();
            }
        }

        documents
    }

    /// Parse a YAML value at the given minimum indentation level.
    pub fn parse_value(&mut self, min_indent: usize) -> Option<Node> {
        self.parse_value_with_properties(min_indent, NodeProperties::default())
    }

    /// Parse a value with already-collected node properties.
    pub fn parse_value_with_properties(
        &mut self,
        min_indent: usize,
        mut props: NodeProperties,
    ) -> Option<Node> {
        self.skip_ws();

        let (tok, span) = self.peek()?;
        let span = *span;

        match tok {
            // Flow mapping
            Token::FlowMapStart => {
                let result = self.parse_flow_mapping();
                if let Some(flow_node) = result {
                    self.check_content_after_flow(flow_node.span.end);
                    self.check_multiline_implicit_key(span.start, flow_node.span.end);

                    self.skip_ws();
                    if let Some((Token::Colon, _)) = self.peek() {
                        let key = self.apply_properties_and_register(props, flow_node);
                        self.parse_block_mapping_starting_with_key(min_indent, key)
                    } else {
                        Some(self.apply_properties_and_register(props, flow_node))
                    }
                } else {
                    None
                }
            }
            // Flow sequence
            Token::FlowSeqStart => {
                let result = self.parse_flow_sequence();
                if let Some(flow_node) = result {
                    self.check_content_after_flow(flow_node.span.end);
                    self.check_multiline_implicit_key(span.start, flow_node.span.end);

                    self.skip_ws();
                    if let Some((Token::Colon, _)) = self.peek() {
                        let key = self.apply_properties_and_register(props, flow_node);
                        self.parse_block_mapping_starting_with_key(min_indent, key)
                    } else {
                        Some(self.apply_properties_and_register(props, flow_node))
                    }
                } else {
                    None
                }
            }
            // Block sequence
            Token::BlockSeqIndicator => {
                if !props.is_empty() && !props.crossed_line_boundary {
                    if let Some((_, anchor_span)) = &props.anchor {
                        self.error(ErrorKind::UnexpectedToken, *anchor_span);
                    }
                    if let Some((_, tag_span)) = &props.tag {
                        self.error(ErrorKind::UnexpectedToken, *tag_span);
                    }
                }
                if let Some(n) = self.parse_block_sequence(min_indent) {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            // Anchor - collect as property and continue parsing
            Token::Anchor(name) => {
                let name = name.clone();
                let anchor_span = span;
                self.advance();
                self.skip_ws();

                if props.anchor.is_some() && !props.crossed_line_boundary {
                    self.error(ErrorKind::DuplicateAnchor, anchor_span);
                }

                if props.crossed_line_boundary && props.anchor.is_some() {
                    let inner_props = NodeProperties {
                        anchor: Some((name.clone(), anchor_span)),
                        tag: None,
                        crossed_line_boundary: false,
                    };
                    if let Some(mapping) =
                        self.parse_block_mapping_with_props(min_indent, inner_props)
                    {
                        Some(self.apply_properties_and_register(props, mapping))
                    } else {
                        self.error(ErrorKind::DuplicateAnchor, anchor_span);
                        props.anchor = Some((name, anchor_span));
                        self.parse_value_with_properties(min_indent, props)
                    }
                } else {
                    props.anchor = Some((name, anchor_span));
                    self.parse_value_with_properties(min_indent, props)
                }
            }
            // Alias
            Token::Alias(alias_name) => {
                if !props.is_empty() && !props.crossed_line_boundary {
                    self.error(ErrorKind::PropertiesOnAlias, span);
                    self.parse_alias()
                } else {
                    let alias_name = alias_name.clone();
                    let alias_span = span;
                    self.advance();
                    self.skip_ws();

                    if matches!(self.peek(), Some((Token::Colon, _))) {
                        self.parse_alias_as_mapping_key(alias_name, alias_span, props)
                    } else {
                        if !self.anchors.contains_key(&alias_name) {
                            self.error(ErrorKind::UndefinedAlias, alias_span);
                        }
                        Some(Node::new(Value::Alias(alias_name), alias_span))
                    }
                }
            }
            // Tag - collect as property and continue parsing
            Token::Tag(tag) => {
                let tag = tag.clone();
                let tag_span = span;
                self.advance();

                let tag_looks_legitimate = !tag.contains('"') && !tag.contains('`');
                let tag_end = tag_span.end;
                if tag_looks_legitimate {
                    if let Some((next_tok, next_span)) = self.peek() {
                        let is_content = matches!(
                            next_tok,
                            Token::Plain(_)
                                | Token::SingleQuoted(_)
                                | Token::DoubleQuoted(_)
                                | Token::FlowSeqStart
                                | Token::FlowMapStart
                                | Token::BlockSeqIndicator
                        );
                        if is_content && next_span.start == tag_end {
                            self.error(ErrorKind::UnexpectedToken, *next_span);
                        }
                    }
                }

                self.skip_ws();

                if props.tag.is_some() && !props.crossed_line_boundary {
                    self.error(ErrorKind::DuplicateTag, tag_span);
                }

                if props.crossed_line_boundary && props.tag.is_some() {
                    let inner_props = NodeProperties {
                        anchor: None,
                        tag: Some((tag.clone(), tag_span)),
                        crossed_line_boundary: false,
                    };
                    if let Some(mapping) =
                        self.parse_block_mapping_with_props(min_indent, inner_props)
                    {
                        Some(self.apply_properties_and_register(props, mapping))
                    } else {
                        self.error(ErrorKind::DuplicateTag, tag_span);
                        props.tag = Some((tag, tag_span));
                        self.parse_value_with_properties(min_indent, props)
                    }
                } else {
                    props.tag = Some((tag, tag_span));
                    self.parse_value_with_properties(min_indent, props)
                }
            }
            // Block scalars
            Token::LiteralBlockHeader(_) => {
                if let Some(n) = self.parse_literal_block_scalar(min_indent) {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            Token::FoldedBlockHeader(_) => {
                if let Some(n) = self.parse_folded_block_scalar(min_indent) {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            // Scalars
            Token::Plain(_) | Token::SingleQuoted(_) | Token::DoubleQuoted(_) => {
                if !props.is_empty() && self.is_mapping_key_pattern() {
                    self.parse_block_mapping_with_props(min_indent, props)
                } else if let Some(n) = self.parse_scalar_or_mapping(min_indent) {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            // Document markers
            Token::DocStart | Token::DocEnd => None,
            // Line start
            Token::LineStart(n) => {
                if *n >= min_indent {
                    self.advance();
                    while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                        self.advance();
                    }
                    self.check_tabs_as_indentation();
                    let mut new_props = props;
                    new_props.crossed_line_boundary = true;
                    self.parse_value_with_properties(min_indent, new_props)
                } else {
                    if !props.is_empty() {
                        Some(self.apply_properties_and_register(props, Node::null(span)))
                    } else {
                        None
                    }
                }
            }
            // Skip comments
            Token::Comment(_) => {
                self.advance();
                self.parse_value_with_properties(min_indent, props)
            }
            // Mapping key indicator
            Token::MappingKey => {
                if let Some(n) = self.parse_block_mapping(min_indent) {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            // Empty key
            Token::Colon => {
                if !props.is_empty() {
                    if let Some(n) =
                        self.parse_block_mapping_with_tagged_null_key(min_indent, props)
                    {
                        Some(n)
                    } else {
                        None
                    }
                } else if let Some(n) = self.parse_block_mapping_with_empty_key(min_indent) {
                    Some(n)
                } else {
                    None
                }
            }
            // Directives
            Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_) => {
                self.advance();
                self.skip_ws_and_newlines();
                self.parse_value_with_properties(min_indent, props)
            }
            // Indent
            Token::Indent(_) => {
                self.advance();
                self.parse_value_with_properties(min_indent, props)
            }
            // Dedent
            Token::Dedent => {
                if !props.is_empty() {
                    Some(self.apply_properties_and_register(props, Node::null(span)))
                } else {
                    None
                }
            }
            // Invalid or unexpected
            _ => {
                self.error(ErrorKind::UnexpectedToken, span);
                self.advance();
                Some(self.apply_properties_and_register(props, Node::invalid(span)))
            }
        }
    }
}

/// Parse a token stream into a YAML stream (multiple documents).
///
/// Returns the parsed documents and any errors encountered.
/// Due to error recovery, partial values may be returned even when
/// errors are present.
pub fn parse_tokens(tokens: &[Spanned<Token>], input: &str) -> (Stream, Vec<ParseError>) {
    let mut parser = Parser::new(tokens, input);
    let stream = parser.parse_stream();
    (stream, parser.errors)
}

/// Parse a token stream as a single document (for use with layered architecture).
///
/// Unlike `parse_tokens` which may produce multiple documents, this function:
/// 1. Parses exactly ONE value at indent 0
/// 2. Reports an error if there's remaining content that doesn't form a valid continuation
///
/// Returns the parsed node (or None if empty) and any errors encountered.
pub fn parse_single_document(
    tokens: &[Spanned<Token>],
    input: &str,
) -> (Option<Node>, Vec<ParseError>) {
    let mut parser = Parser::new(tokens, input);

    parser.skip_ws_and_newlines();

    let doc = if parser.is_eof() {
        None
    } else {
        parser.parse_value(0)
    };

    // After parsing the document, skip remaining whitespace, newlines, and Dedent tokens.
    loop {
        parser.skip_ws_and_newlines();
        if matches!(parser.peek(), Some((Token::Dedent, _))) {
            parser.advance();
        } else {
            break;
        }
    }

    // Check for remaining content that wasn't consumed
    while !parser.is_eof() {
        if matches!(parser.peek(), Some((Token::Dedent, _))) {
            parser.advance();
            continue;
        }

        let col = parser.current_token_column();

        if col > 0 {
            parser.error(ErrorKind::UnexpectedToken, parser.current_span());
            parser.advance();
            parser.skip_ws_and_newlines();
        } else {
            break;
        }
    }

    (doc, parser.errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    fn parse(input: &str) -> (Stream, Vec<ParseError>) {
        let (tokens, _) = tokenize(input);
        parse_tokens(&tokens, input)
    }

    #[test]
    fn test_parse_simple_scalar() {
        let (docs, errors) = parse("hello");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        assert!(matches!(&docs[0].value, Value::String(s) if s == "hello"));
    }

    #[test]
    fn test_parse_simple_mapping() {
        let (docs, errors) = parse("key: value");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        if let Value::Mapping(pairs) = &docs[0].value {
            assert_eq!(pairs.len(), 1);
            assert!(matches!(&pairs[0].0.value, Value::String(s) if s == "key"));
            assert!(matches!(&pairs[0].1.value, Value::String(s) if s == "value"));
        } else {
            panic!("Expected mapping");
        }
    }

    #[test]
    fn test_parse_flow_mapping() {
        let (docs, errors) = parse("{a: 1, b: 2}");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        if let Value::Mapping(pairs) = &docs[0].value {
            assert_eq!(pairs.len(), 2);
        } else {
            panic!("Expected mapping");
        }
    }

    #[test]
    fn test_parse_flow_sequence() {
        let (docs, errors) = parse("[1, 2, 3]");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        if let Value::Sequence(items) = &docs[0].value {
            assert_eq!(items.len(), 3);
        } else {
            panic!("Expected sequence");
        }
    }

    #[test]
    fn test_parse_block_sequence() {
        let (docs, errors) = parse("- a\n- b\n- c");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        if let Value::Sequence(items) = &docs[0].value {
            assert_eq!(items.len(), 3);
        } else {
            panic!("Expected sequence");
        }
    }

    #[test]
    fn test_parse_null_values() {
        let (docs, errors) = parse("~");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        assert!(matches!(&docs[0].value, Value::Null));
    }

    #[test]
    fn test_parse_boolean_values() {
        let (docs, errors) = parse("true");
        assert!(errors.is_empty());
        assert!(matches!(&docs[0].value, Value::Bool(true)));
    }

    #[test]
    fn test_parse_number_values() {
        let (docs, errors) = parse("42");
        assert!(errors.is_empty());
        assert!(matches!(&docs[0].value, Value::Int(42)));

        let (docs, errors) = parse("3.14");
        assert!(errors.is_empty());
        if let Value::Float(f) = &docs[0].value {
            assert!((f - 3.14).abs() < 0.001);
        } else {
            panic!("Expected float");
        }
    }

    #[test]
    fn test_parse_multi_document() {
        let (docs, _) = parse("---\na\n---\nb");
        assert!(docs.len() >= 2);
    }

    #[test]
    fn test_parse_anchor_alias() {
        let (docs, errors) = parse("a: &anchor 1\nb: *anchor");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        if let Value::Mapping(pairs) = &docs[0].value {
            assert_eq!(pairs.len(), 2);
            assert!(matches!(&pairs[1].1.value, Value::Alias(name) if name == "anchor"));
        } else {
            panic!("Expected mapping");
        }
    }

    #[test]
    fn test_multiline_quoted_key_error() {
        let input = "\"c\n d\": 1";
        let (_, errors) = parse(input);
        assert!(!errors.is_empty());
    }
}
