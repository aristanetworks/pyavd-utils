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

use std::collections::HashMap;

use chumsky::span::Span as _;

use crate::error::{ErrorKind, ParseError};
use crate::lexer::{BlockScalarHeader, Token};
use crate::span::{Span, Spanned};
use crate::value::{Node, Value};

/// A stream of YAML documents.
pub type Stream = Vec<Node>;

/// Pending node properties (anchor, tag) collected before parsing the value.
#[derive(Debug, Default, Clone)]
struct NodeProperties {
    anchor: Option<(String, Span)>,
    tag: Option<(String, Span)>,
    /// Whether we've crossed a line boundary while accumulating these properties.
    /// This is important for distinguishing between:
    /// - `&a &b value` (invalid: two anchors on same node)
    /// - `&a\n&b value` (valid: &a on containing structure, &b on nested value)
    crossed_line_boundary: bool,
}

impl NodeProperties {
    fn is_empty(&self) -> bool {
        self.anchor.is_none() && self.tag.is_none()
    }

    /// Apply these properties to a node, updating its span to include properties.
    fn apply_to(self, mut node: Node) -> Node {
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
struct Parser<'a> {
    tokens: &'a [Spanned<Token>],
    input: &'a str,
    pos: usize,
    errors: Vec<ParseError>,
    /// Map of anchor names to their nodes (for alias resolution)
    anchors: HashMap<String, Node>,
    /// Flow depth tracking (0 = block context, > 0 = inside flow collections)
    flow_depth: usize,
    /// Indentation stack tracking active block structure levels.
    /// Each entry is the indentation level of an active block structure.
    /// Used to detect orphan indentation (content at levels not in the stack).
    indent_stack: Vec<usize>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Spanned<Token>], input: &'a str) -> Self {
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
    fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Peek at the current token without consuming it.
    fn peek(&self) -> Option<&Spanned<Token>> {
        self.tokens.get(self.pos)
    }

    /// Peek at the token N positions ahead.
    fn peek_n(&self, n: usize) -> Option<&Spanned<Token>> {
        self.tokens.get(self.pos + n)
    }

    /// Consume the current token and return it.
    fn advance(&mut self) -> Option<&Spanned<Token>> {
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
    fn skip_ws(&mut self) {
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
    fn skip_ws_and_newlines(&mut self) {
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
    fn current_token_column(&self) -> usize {
        if let Some((_, span)) = self.peek() {
            self.column_of_position(span.start)
        } else {
            0
        }
    }

    /// Get the column (0-based) of a byte position by looking back to the last newline.
    fn column_of_position(&self, pos: usize) -> usize {
        let before = &self.input[..pos];
        if let Some(newline_pos) = before.rfind('\n') {
            pos - newline_pos - 1
        } else {
            pos // No newline, column is the byte position from start of input
        }
    }

    /// Apply node properties to a node and register the anchor if present.
    fn apply_properties_and_register(&mut self, props: NodeProperties, node: Node) -> Node {
        let node = props.apply_to(node);
        // If the node has an anchor, register it in the anchors map
        if let Some(ref anchor_name) = node.anchor {
            self.anchors.insert(anchor_name.clone(), node.clone());
        }
        node
    }

    /// Push an indentation level onto the stack when entering a block structure.
    fn push_indent(&mut self, indent: usize) {
        self.indent_stack.push(indent);
    }

    /// Pop an indentation level from the stack when exiting a block structure.
    fn pop_indent(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    /// Check if an indentation level is valid (matches some level in the stack).
    /// Returns true if valid, false if orphan.
    fn is_valid_indent(&self, indent: usize) -> bool {
        self.indent_stack.contains(&indent)
    }

    /// Check for orphan block indicator at the current position.
    /// An orphan block indicator is a `-`, `?`, or `:` at an indentation level
    /// that doesn't match any active structure in the indent stack.
    /// Only checks in block context (flow_depth == 0).
    fn check_orphan_block_indicator(&mut self) {
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
    /// invalid tab indentation in block context. Call this when about to parse
    /// a block structure element (mapping key or sequence indicator).
    ///
    /// In YAML, indentation must be spaces only. If we see:
    ///   LineStart(n) -> Whitespace -> content
    /// That whitespace represents tabs or other characters that shouldn't be there,
    /// because all valid space indentation should be captured in LineStart.
    fn check_tabs_as_indentation(&mut self) {
        // Tabs are only invalid for indentation in BLOCK context
        // In flow context (inside [] or {}), indentation doesn't matter
        if self.flow_depth > 0 {
            return;
        }

        // Look back to see if we just passed LineStart and are now at Whitespace
        if self.pos == 0 {
            return;
        }

        // Check: is the previous token LineStart and current token Whitespace?
        if let Some((Token::LineStart(_), _)) = self.tokens.get(self.pos - 1) {
            if let Some((Token::Whitespace, ws_span)) = self.peek() {
                // We have LineStart followed by Whitespace
                let ws_span = *ws_span;
                // Check if the whitespace contains tabs by looking at the input
                let ws_content = &self.input[ws_span.start..ws_span.end];

                if ws_content.contains('\t') {
                    // Tabs are only invalid when used as indentation for BLOCK content.
                    // Tabs at the start of a line are valid when followed by:
                    // - Flow indicators (flow content doesn't care about indentation)
                    // - End of document
                    //
                    // Look past the whitespace to see what follows
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
    /// After a flow mapping/sequence ends (}] ), if there's content on the same line
    /// without whitespace, that's invalid (e.g., `{y: z}in: valid` or `{y: z}- item`).
    fn check_content_after_flow(&mut self, flow_end: usize) {
        // Check if the next non-whitespace token starts immediately after the flow end
        if let Some((tok, span)) = self.peek() {
            // Only check for tokens that would be invalid right after flow
            let is_content = matches!(
                tok,
                Token::Plain(_)
                    | Token::SingleQuoted(_)
                    | Token::DoubleQuoted(_)
                    | Token::Anchor(_)
                    | Token::Alias(_)
                    | Token::Tag(_)
                    | Token::BlockSeqIndicator // - can't be immediately after flow
            );
            if is_content && span.start == flow_end {
                // No gap between flow end and next content - invalid
                self.error(ErrorKind::UnexpectedToken, *span);
            }
        }
    }

    /// Check if a value used as an implicit key spans multiple lines.
    /// In block context, implicit keys must be on a single line.
    /// If the value spans multiple lines (contains a newline in the input or a LineStart token),
    /// that's invalid.
    fn check_multiline_implicit_key(&mut self, key_start: usize, key_end: usize) {
        // Check if the next non-whitespace token is a colon (meaning this is a key)
        // Don't modify state - just peek ahead
        let mut check_pos = self.pos;
        while check_pos < self.tokens.len() {
            match &self.tokens[check_pos].0 {
                Token::Whitespace => check_pos += 1,
                Token::Colon => break,
                _ => return, // Not followed by colon, not a key
            }
        }
        if check_pos >= self.tokens.len() {
            return;
        }
        let colon_span = self.tokens[check_pos].1;

        // Check if the key span contains newlines in the source
        // This catches multiline quoted strings (which are single tokens)
        let key_text = &self.input[key_start..key_end.min(self.input.len())];
        if key_text.contains('\n') {
            self.error(ErrorKind::UnexpectedToken, colon_span);
            return;
        }

        // Also look through tokens to see if there's a LineStart within the key range
        // This catches flow collections that span multiple lines
        // Use span.start > key_start to exclude LineStart tokens at or before key start
        for (tok, span) in &self.tokens[..self.pos] {
            if span.start > key_start && span.end <= key_end {
                if matches!(tok, Token::LineStart(_)) {
                    // Multiline implicit key is invalid
                    self.error(ErrorKind::UnexpectedToken, colon_span);
                    return;
                }
            }
        }
    }

    /// Check for invalid trailing content after a scalar value in block context.
    /// After a quoted scalar on the same line as a mapping value, only whitespace
    /// and comments should be allowed.
    fn check_trailing_content_after_scalar(&mut self) {
        // Skip whitespace
        self.skip_ws();

        // Comments are allowed
        if let Some((Token::Comment(_), _)) = self.peek() {
            return;
        }

        // LineStart is fine - value ends at newline
        if let Some((Token::LineStart(_), _)) = self.peek() {
            return;
        }

        // EOF is fine
        if self.is_eof() {
            return;
        }

        // Document markers are fine
        if let Some((Token::DocStart | Token::DocEnd, _)) = self.peek() {
            return;
        }

        // Any other content is invalid
        if let Some((_, span)) = self.peek() {
            let span = *span;
            self.error(ErrorKind::UnexpectedToken, span);
        }
    }

    /// Get the current indentation level from the most recent LineStart.
    fn current_indent(&self) -> usize {
        // Look backwards for the most recent LineStart
        for i in (0..self.pos).rev() {
            if let (Token::LineStart(n), _) = &self.tokens[i] {
                return *n;
            }
        }
        0
    }

    /// Get the span of the current position.
    fn current_span(&self) -> Span {
        self.peek()
            .map(|(_, span)| *span)
            .unwrap_or_else(|| Span::new((), 0..0))
    }

    /// Check if current position is a mapping key pattern (scalar followed by colon).
    /// This is a lookahead check that doesn't consume any tokens.
    fn is_mapping_key_pattern(&self) -> bool {
        // Check if current token is a scalar
        let mut i = self.pos;
        match self.tokens.get(i) {
            Some((Token::Plain(_) | Token::SingleQuoted(_) | Token::DoubleQuoted(_), _)) => {
                i += 1;
            }
            _ => return false,
        }
        // Skip whitespace
        while let Some((Token::Whitespace, _)) = self.tokens.get(i) {
            i += 1;
        }
        // Check for colon
        matches!(self.tokens.get(i), Some((Token::Colon, _)))
    }

    /// Add an error.
    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError {
            kind,
            span,
            expected: Vec::new(),
            found: None,
        });
    }

    /// Parse a complete YAML stream (multiple documents).
    fn parse_stream(&mut self) -> Stream {
        let mut documents = Vec::new();

        // Skip initial whitespace/newlines
        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let start_pos = self.pos;

            // Check for document start marker
            let explicit_doc_start = if let Some((Token::DocStart, span)) = self.peek() {
                let span = *span;
                self.advance();
                self.skip_ws_and_newlines();
                Some(span)
            } else {
                None
            };

            // Parse the document value
            // If we're at EOF or at another document marker, the document has an empty value
            if self.is_eof() || matches!(self.peek(), Some((Token::DocStart | Token::DocEnd, _))) {
                // Empty document - produce null value
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

            // Skip to next document or end
            self.skip_ws_and_newlines();

            // Check for document end marker
            if let Some((Token::DocEnd, _)) = self.peek() {
                self.advance();
                self.skip_ws_and_newlines();
            }

            // Safety: if we didn't make progress, skip the current token
            // to avoid infinite loops
            if self.pos == start_pos && !self.is_eof() {
                self.advance();
            }
        }

        documents
    }

    /// Parse a YAML value at the given minimum indentation level.
    ///
    /// This function handles node properties (anchor, tag) by collecting them
    /// first, then parsing the actual value and applying the properties.
    fn parse_value(&mut self, min_indent: usize) -> Option<Node> {
        self.parse_value_with_properties(min_indent, NodeProperties::default())
    }

    /// Parse a value with already-collected node properties.
    fn parse_value_with_properties(
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
                    // Check for invalid content immediately after flow mapping in block context
                    self.check_content_after_flow(flow_node.span.end);
                    // If followed by :, check for multiline implicit key (invalid)
                    self.check_multiline_implicit_key(span.start, flow_node.span.end);

                    // Check if flow mapping is used as a block mapping key
                    self.skip_ws();
                    if let Some((Token::Colon, _)) = self.peek() {
                        // Flow mapping as key of block mapping
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
                    // Check for invalid content immediately after flow sequence in block context
                    self.check_content_after_flow(flow_node.span.end);
                    // If followed by :, check for multiline implicit key (invalid)
                    self.check_multiline_implicit_key(span.start, flow_node.span.end);

                    // Check if flow sequence is used as a block mapping key
                    self.skip_ws();
                    if let Some((Token::Colon, _)) = self.peek() {
                        // Flow sequence as key of block mapping
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
                // Check for invalid anchor/tag before block sequence on same line.
                // Pattern like `&anchor - item` is invalid (anchor can't precede `-` on same line).
                // Valid patterns:
                // - `&anchor\n- item` (anchor on sequence, items below)
                // - `- &anchor item` (anchor on item)
                if !props.is_empty() && !props.crossed_line_boundary {
                    // Props without line boundary + block sequence = error
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

                // Check for duplicate anchor on same node
                // UNLESS we've crossed a line boundary, which means we might have:
                // 1. A nested block structure: &outer\n&inner key: value
                //    - &outer applies to the mapping, &inner to the key
                // 2. Multiple properties on same node: &a1\n!!str\nscalar
                //    - Both &a1 and !!str apply to scalar
                //
                // We distinguish these by checking: does this line form a mapping key?
                // (i.e., is there a `:` after the value on this line?)
                if props.anchor.is_some() && !props.crossed_line_boundary {
                    self.error(ErrorKind::DuplicateAnchor, anchor_span);
                }

                if props.crossed_line_boundary && props.anchor.is_some() {
                    // We have TWO anchors across a line boundary.
                    // This could be:
                    // - &outer\n&inner key: value (nested structure - valid)
                    // - &a1\n&a2\nscalar (two anchors on same scalar - invalid)
                    //
                    // Try to parse as block mapping. If that fails, it's invalid.
                    let inner_props = NodeProperties {
                        anchor: Some((name.clone(), anchor_span)),
                        tag: None,
                        crossed_line_boundary: false,
                    };
                    // Parse as a block mapping with the key having the inner props
                    if let Some(mapping) =
                        self.parse_block_mapping_with_props(min_indent, inner_props)
                    {
                        Some(self.apply_properties_and_register(props, mapping))
                    } else {
                        // Not a block mapping - this is a duplicate anchor error
                        self.error(ErrorKind::DuplicateAnchor, anchor_span);
                        props.anchor = Some((name, anchor_span));
                        self.parse_value_with_properties(min_indent, props)
                    }
                } else {
                    // Either no line boundary crossed, or props only has tag (no anchor).
                    // Just accumulate the anchor and continue.
                    props.anchor = Some((name, anchor_span));
                    // Continue parsing the actual value with accumulated properties
                    self.parse_value_with_properties(min_indent, props)
                }
            }
            // Alias - aliases can't have additional properties ON THE SAME LINE
            // If we've crossed a line boundary with props, the props apply to a
            // containing structure, not to the alias itself.
            Token::Alias(alias_name) => {
                if !props.is_empty() && !props.crossed_line_boundary {
                    self.error(ErrorKind::PropertiesOnAlias, span);
                    self.parse_alias()
                } else {
                    // Check if alias is followed by colon - it may be a mapping key
                    let alias_name = alias_name.clone();
                    let alias_span = span;
                    self.advance();
                    self.skip_ws();

                    // Check for colon (mapping key)
                    if matches!(self.peek(), Some((Token::Colon, _))) {
                        // The alias is used as a mapping key - delegate to alias mapping parser
                        self.parse_alias_as_mapping_key(alias_name, alias_span, props)
                    } else {
                        // Not a mapping key, just return the alias
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

                // Check if there's whitespace or a valid boundary after the tag.
                // Tags must be separated from their content by whitespace.
                // Valid separators: Whitespace, LineStart, end of input
                // Invalid: content immediately following the tag (Plain, Quoted, etc.)
                //
                // IMPORTANT: Only do this check if the tag looks legitimate.
                // Due to lexer limitations, `!"#$%&...` on a continuation line
                // may be incorrectly tokenized as Tag("\"#$%...") + Plain(",...")
                // when it should be part of a multiline plain scalar.
                // Tags should only contain ns-tag-char (URI chars, not quotes).
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
                        // If next token is content and it starts exactly where tag ends,
                        // there's no whitespace separation - that's an error
                        if is_content && next_span.start == tag_end {
                            self.error(ErrorKind::UnexpectedToken, *next_span);
                        }
                    }
                }

                self.skip_ws();

                // Check for duplicate tag on same node
                // UNLESS we've crossed a line boundary, which means we might have:
                // 1. A nested block structure: !!map\n!!str key: value
                //    - !!map applies to the mapping, !!str to the key
                // 2. Multiple properties on same node: !!str\n&a2\nscalar
                //    - Both !!str and &a2 apply to scalar
                //
                // Similar to anchor handling, we distinguish by checking if content forms a mapping.
                if props.tag.is_some() && !props.crossed_line_boundary {
                    self.error(ErrorKind::DuplicateTag, tag_span);
                }

                if props.crossed_line_boundary && props.tag.is_some() {
                    // Two tags across a line boundary.
                    // Try to parse as block mapping. If successful, the outer tag applies to
                    // the mapping and the inner tag to the key.
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
                        // Not a block mapping - this is a duplicate tag error
                        self.error(ErrorKind::DuplicateTag, tag_span);
                        props.tag = Some((tag, tag_span));
                        self.parse_value_with_properties(min_indent, props)
                    }
                } else {
                    // Either no line boundary crossed, or props only has anchor (no tag).
                    // Just accumulate the tag and continue.
                    props.tag = Some((tag, tag_span));
                    // Continue parsing the actual value with accumulated properties
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
                // Check if this is a mapping key pattern (scalar followed by colon).
                // If we have properties (anchor/tag), they should attach to the KEY,
                // not to the mapping structure. Use parse_block_mapping_with_props
                // to properly handle this case.
                if !props.is_empty() && self.is_mapping_key_pattern() {
                    // Properties attach to the first key of the mapping
                    self.parse_block_mapping_with_props(min_indent, props)
                } else if let Some(n) = self.parse_scalar_or_mapping(min_indent) {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            // Document markers and other tokens
            Token::DocStart | Token::DocEnd => None,
            // Line start - check indentation and continue
            Token::LineStart(n) => {
                if *n >= min_indent {
                    self.advance();
                    // Skip any Indent/Dedent tokens that follow LineStart
                    // These are informational and shouldn't affect value parsing
                    while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                        self.advance();
                    }
                    // Check for tabs used as indentation (invalid in YAML)
                    self.check_tabs_as_indentation();
                    // Mark that we've crossed a line boundary - important for distinguishing
                    // `&a &b value` (duplicate anchor) from `&a\n&b value` (nested structure)
                    let mut new_props = props;
                    new_props.crossed_line_boundary = true;
                    self.parse_value_with_properties(min_indent, new_props)
                } else {
                    // Indentation is less than required - this means no value follows.
                    // But if we have collected properties (anchor/tag), we need to create
                    // a null node with those properties attached.
                    // Example: `a: &anchor\nb: *anchor` - the anchor attaches to empty/null value
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
            // Handle mapping key indicator
            Token::MappingKey => {
                if let Some(n) = self.parse_block_mapping(min_indent) {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            // Handle colon at start of line (empty key)
            // If we have props (tag/anchor), they apply to the first key (a null), not the mapping
            Token::Colon => {
                if !props.is_empty() {
                    // Props apply to the first key of the mapping
                    // Example: `!!null : a` - the !!null tag is on the key (null value), not on the mapping
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
            // Directives - skip them
            Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_) => {
                self.advance();
                self.skip_ws_and_newlines();
                self.parse_value_with_properties(min_indent, props)
            }
            // Indent - skip it (informational after LineStart)
            Token::Indent(_) => {
                self.advance();
                self.parse_value_with_properties(min_indent, props)
            }
            // Dedent - signals we've left the current indentation level
            // This means no value at this position
            Token::Dedent => {
                if !props.is_empty() {
                    // Apply properties to a null node
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

    /// Parse a flow mapping: { key: value, ... }
    fn parse_flow_mapping(&mut self) -> Option<Node> {
        let (_, start_span) = self.advance()?; // consume '{'
        let start = start_span.start;
        let mut pairs: Vec<(Node, Node)> = Vec::new();
        let mut just_saw_comma = true; // Start true to catch leading comma

        // Track flow depth - inside flow collections, indentation doesn't matter
        self.flow_depth += 1;

        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            // Check for end of mapping
            if let Some((Token::FlowMapEnd, end_span)) = self.peek() {
                let end = end_span.end;
                self.advance();
                self.flow_depth -= 1;
                return Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)));
            }

            // Check for consecutive commas (e.g., `{ a: 1, , b: 2 }`)
            if let Some((Token::Comma, comma_span)) = self.peek() {
                if just_saw_comma {
                    // Consecutive comma without key in between is invalid
                    self.error(ErrorKind::UnexpectedToken, *comma_span);
                }
                self.advance(); // skip the comma
                self.skip_ws_and_newlines();
                just_saw_comma = true;
                continue;
            }

            // Handle explicit key marker (?)
            let explicit_key = matches!(self.peek(), Some((Token::MappingKey, _)));
            if explicit_key {
                self.advance();
                self.skip_ws_and_newlines();
            }

            // Check for empty key (: at start without key)
            if let Some((Token::Colon, _)) = self.peek() {
                if explicit_key || matches!(self.peek(), Some((Token::Colon, _))) {
                    // Empty key followed by colon - key is null
                    let key_span = self.current_span();
                    let key = Node::null(key_span);
                    just_saw_comma = false;

                    self.advance(); // consume ':'
                    self.skip_ws_and_newlines();

                    // Parse value
                    let value =
                        if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
                            Node::null(self.current_span())
                        } else {
                            self.parse_flow_value()
                                .unwrap_or_else(|| Node::null(self.current_span()))
                        };

                    pairs.push((key, value));
                    self.skip_ws_and_newlines();

                    // Handle trailing comma
                    if let Some((Token::Comma, _)) = self.peek() {
                        self.advance();
                        self.skip_ws_and_newlines();
                        just_saw_comma = true;
                    }
                    continue;
                }
            }

            // Parse key
            let key = match self.parse_flow_value() {
                Some(k) => {
                    just_saw_comma = false;
                    k
                }
                None => {
                    if explicit_key {
                        // Explicit key with no following key value - key is null
                        Node::null(self.current_span())
                    } else {
                        // Recovery: skip to comma or end
                        self.skip_to_flow_delimiter();
                        // Safety: ensure progress
                        if self.pos == loop_start_pos && !self.is_eof() {
                            self.advance();
                        }
                        continue;
                    }
                }
            };

            // Skip whitespace and newlines between key and colon
            // Note: In flow context, newlines between key and colon are VALID
            // What's invalid is if the key VALUE itself spans multiple lines
            // (that's handled by check_multiline_implicit_key for flow collections)
            self.skip_ws_and_newlines();

            // Check for colon (explicit value) or comma/end (implicit null value)
            let value = if let Some((Token::Colon, _colon_span)) = self.peek() {
                self.advance();
                self.skip_ws_and_newlines();

                // Parse value - if comma or end follows, value is null
                if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
                    Node::null(self.current_span())
                } else {
                    self.parse_flow_value()
                        .unwrap_or_else(|| Node::null(self.current_span()))
                }
            } else if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
                // No colon - this is a key with implicit null value
                Node::null(self.current_span())
            } else {
                self.error(ErrorKind::UnexpectedToken, self.current_span());
                Node::null(self.current_span())
            };

            pairs.push((key, value));

            self.skip_ws_and_newlines();

            // Check for comma or end
            if let Some((Token::Comma, _)) = self.peek() {
                self.advance();
                self.skip_ws_and_newlines();
                just_saw_comma = true;
            } else if let Some((Token::FlowMapEnd, _)) = self.peek() {
                // Will be handled at top of loop
            } else if !self.is_eof() {
                self.error(ErrorKind::UnexpectedToken, self.current_span());
                self.skip_to_flow_delimiter();
            }

            // Safety: ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
            }
        }

        // Unterminated mapping
        self.error(ErrorKind::UnexpectedEof, self.current_span());
        self.flow_depth -= 1;
        let end = self.tokens.last().map(|(_, s)| s.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a flow sequence: [ item, ... ]
    /// Also handles implicit flow mappings like [ key: value, ... ]
    fn parse_flow_sequence(&mut self) -> Option<Node> {
        let (_, start_span) = self.advance()?; // consume '['
        let start = start_span.start;
        let mut items: Vec<Node> = Vec::new();
        let mut just_saw_comma = true; // Start true to catch leading comma

        // Track flow depth - inside flow collections, indentation doesn't matter
        self.flow_depth += 1;

        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            // Check for end of sequence
            if let Some((Token::FlowSeqEnd, end_span)) = self.peek() {
                let end = end_span.end;
                self.advance();
                self.flow_depth -= 1;
                return Some(Node::new(Value::Sequence(items), Span::new((), start..end)));
            }

            // Check for consecutive commas (e.g., `[ a, , b ]`)
            if let Some((Token::Comma, comma_span)) = self.peek() {
                if just_saw_comma {
                    // Consecutive comma without value in between is invalid
                    self.error(ErrorKind::UnexpectedToken, *comma_span);
                }
                self.advance(); // skip the comma
                self.skip_ws_and_newlines();
                just_saw_comma = true;
                continue;
            }

            // Parse item - could be a simple value or an implicit mapping entry
            if let Some(item) = self.parse_flow_value() {
                just_saw_comma = false;
                // Skip whitespace only (not newlines) to check for implicit mapping
                // In flow sequences, implicit mapping entries must have key and colon on same line
                self.skip_ws();

                // Check if this is an implicit mapping entry (key: value)
                // Only valid if colon is on the same line as the key
                if let Some((Token::Colon, _)) = self.peek() {
                    self.advance(); // consume ':'
                    self.skip_ws_and_newlines();

                    // Parse the value (may be null if comma/end follows)
                    let value =
                        if matches!(self.peek(), Some((Token::Comma | Token::FlowSeqEnd, _))) {
                            Node::null(self.current_span())
                        } else {
                            self.parse_flow_value()
                                .unwrap_or_else(|| Node::null(self.current_span()))
                        };

                    // Create an implicit mapping with one entry
                    let map_start = item.span.start;
                    let map_end = value.span.end;
                    let mapping_node = Node::new(
                        Value::Mapping(vec![(item, value)]),
                        Span::new((), map_start..map_end),
                    );
                    items.push(mapping_node);
                } else {
                    // Not an implicit mapping entry, just add the item
                    self.skip_ws_and_newlines();
                    items.push(item);
                }
            } else {
                // Recovery: skip to comma or end
                self.skip_to_flow_delimiter();
            }

            self.skip_ws_and_newlines();

            // Check for comma or end
            if let Some((Token::Comma, _)) = self.peek() {
                self.advance();
                self.skip_ws_and_newlines();
                just_saw_comma = true;
            } else if let Some((Token::FlowSeqEnd, _)) = self.peek() {
                // Will be handled at top of loop
            } else if !self.is_eof() {
                self.error(ErrorKind::UnexpectedToken, self.current_span());
                self.skip_to_flow_delimiter();
            }

            // Safety: ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
            }
        }

        // Unterminated sequence
        self.error(ErrorKind::UnexpectedEof, self.current_span());
        self.flow_depth -= 1;
        let end = self.tokens.last().map(|(_, s)| s.end).unwrap_or(start);
        Some(Node::new(Value::Sequence(items), Span::new((), start..end)))
    }

    /// Parse a value in flow context (no block structures).
    /// Handles multiline plain scalars by folding newlines to spaces.
    fn parse_flow_value(&mut self) -> Option<Node> {
        self.parse_flow_value_with_properties(NodeProperties::default())
    }

    /// Parse a flow value with already-collected node properties.
    fn parse_flow_value_with_properties(&mut self, mut props: NodeProperties) -> Option<Node> {
        self.skip_ws_and_newlines();

        let (tok, span) = self.peek()?;
        let start_span = *span;

        match tok {
            Token::FlowMapStart => {
                if let Some(n) = self.parse_flow_mapping() {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            Token::FlowSeqStart => {
                if let Some(n) = self.parse_flow_sequence() {
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            // Anchor - collect as property and continue parsing
            Token::Anchor(name) => {
                let name = name.clone();
                let anchor_span = start_span;
                self.advance();

                if props.anchor.is_some() {
                    self.error(ErrorKind::DuplicateAnchor, anchor_span);
                }
                props.anchor = Some((name, anchor_span));

                self.parse_flow_value_with_properties(props)
            }
            // Alias - aliases can't have additional properties
            Token::Alias(_) => {
                if !props.is_empty() {
                    self.error(ErrorKind::PropertiesOnAlias, start_span);
                }
                self.parse_alias()
            }
            // Tag - collect as property and continue parsing
            Token::Tag(tag) => {
                let tag = tag.clone();
                let tag_span = start_span;
                self.advance();

                if props.tag.is_some() {
                    self.error(ErrorKind::DuplicateTag, tag_span);
                }
                props.tag = Some((tag, tag_span));

                self.parse_flow_value_with_properties(props)
            }
            Token::Plain(s) => {
                let mut combined = s.clone();
                let mut end_span = start_span;
                self.advance();

                // In flow context, plain scalars can span multiple lines.
                // Newlines are folded to spaces.
                loop {
                    // Skip whitespace tokens but not plain scalars
                    while matches!(self.peek(), Some((Token::Whitespace, _))) {
                        self.advance();
                    }

                    // Check for LineStart followed by Plain (continuation)
                    // But stop if we see a flow delimiter or colon
                    let Some((Token::LineStart(_), _)) = self.peek() else {
                        break;
                    };

                    // Peek ahead to see if there's a plain scalar continuation
                    let next_pos = self.pos + 1;
                    if next_pos >= self.tokens.len() {
                        break;
                    }

                    match &self.tokens[next_pos].0 {
                        Token::Plain(continuation) => {
                            // This is a continuation - fold newline to space
                            combined.push(' ');
                            combined.push_str(continuation);
                            end_span = self.tokens[next_pos].1;
                            self.advance(); // consume LineStart
                            self.advance(); // consume Plain
                        }
                        // Stop at flow delimiters, colon, etc.
                        _ => break,
                    }
                }

                let value = self.scalar_to_value(combined);
                let node = Node::new(value, Span::new((), start_span.start..end_span.end));
                Some(self.apply_properties_and_register(props, node))
            }
            Token::SingleQuoted(s) | Token::DoubleQuoted(s) => {
                let value = Value::String(s.clone());
                self.advance();
                let node = Node::new(value, start_span);
                Some(self.apply_properties_and_register(props, node))
            }
            // Flow delimiters with collected properties = empty value with tag/anchor
            // Example: { foo: !!str, } - the tag applies to an empty value
            Token::Comma | Token::FlowSeqEnd | Token::FlowMapEnd | Token::Colon => {
                if !props.is_empty() {
                    // Tag or anchor on empty value - create null node with properties
                    Some(self.apply_properties_and_register(props, Node::null(start_span)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Skip to the next flow delimiter (, ] })
    fn skip_to_flow_delimiter(&mut self) {
        while let Some((tok, _)) = self.peek() {
            match tok {
                Token::Comma | Token::FlowSeqEnd | Token::FlowMapEnd => break,
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Parse a block sequence: - item\n- item
    fn parse_block_sequence(&mut self, _min_indent: usize) -> Option<Node> {
        let (_, start_span) = self.peek()?;
        let start = start_span.start;
        // Use the actual column of the `-` token, not the LineStart indent.
        // This handles cases like " - item" where LineStart(0) but `-` is at column 1.
        let seq_indent = self.current_token_column();
        let mut items: Vec<Node> = Vec::new();

        // Push this sequence's indentation level onto the stack
        self.push_indent(seq_indent);

        while let Some((Token::BlockSeqIndicator, _)) = self.peek() {
            // Check we're at the right indentation (use column of `-` token)
            let item_col = self.current_token_column();
            if item_col < seq_indent {
                // This `-` is at a lower indentation - end of sequence
                break;
            }
            if item_col > seq_indent {
                // This `-` is at a higher indentation - this is a nested sequence
                // within an item, not a sibling item. Stop this sequence.
                break;
            }

            self.advance(); // consume '-'
            self.skip_ws();

            // Parse the item value
            // For plain scalar continuation, we need min_indent = seq_indent + 1 (content must
            // be more indented than the `-`). But for nested mappings, we need content_col
            // so keys align. We use seq_indent + 1 for plain scalars to allow tabs after
            // minimal indentation (e.g., `  \tx` at indent 2 after `- x` at seq_indent 1).
            // This means seq_indent + 1 is the true "block indent" for continuation.
            let item = if let Some((Token::LineStart(_), _)) = self.peek() {
                // Item on next line
                self.advance();
                // Skip any Indent token that follows LineStart
                if let Some((Token::Indent(_), _)) = self.peek() {
                    self.advance();
                }
                // Content starts at the column of the first token on this new line
                let content_col = self.current_token_column();
                self.parse_value(content_col)
            } else {
                // Item on same line
                // Use seq_indent + 1 as min_indent, which allows continuation lines
                // at any indentation > seq_indent (the column of `-`).
                // This handles cases like `- x\n  \tx` where line 2 has indent 2 > 1.
                self.parse_value(seq_indent + 1)
            };

            if let Some(item) = item {
                items.push(item);
            } else {
                // Empty item is null
                items.push(Node::null(self.current_span()));
            }

            // Skip to next line
            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            // Skip to next potential sequence item
            // We need to navigate through LineStart, Indent, and Dedent tokens
            // to find the next BlockSeqIndicator or determine end of sequence.
            loop {
                match self.peek() {
                    Some((Token::Whitespace | Token::Comment(_) | Token::Indent(_), _)) => {
                        self.advance();
                    }
                    Some((Token::LineStart(n), _)) => {
                        let n = *n;
                        if n < seq_indent {
                            // Dedented below sequence level - end of sequence
                            let end = items.last().map(|n| n.span.end).unwrap_or(start);
                            self.pop_indent();
                            return Some(Node::new(
                                Value::Sequence(items),
                                Span::new((), start..end),
                            ));
                        }
                        self.advance();
                    }
                    Some((Token::Dedent, _)) => {
                        // A Dedent means some nested block ended. Skip it and
                        // check if there's a continuation of this sequence.
                        self.advance();
                    }
                    Some((Token::BlockSeqIndicator, _)) => {
                        // Found next sequence item - check if it's at correct column
                        break;
                    }
                    _ => {
                        // Something else - end of sequence items
                        break;
                    }
                }
            }
        }

        let end = items.last().map(|n| n.span.end).unwrap_or(start);
        self.pop_indent();
        Some(Node::new(Value::Sequence(items), Span::new((), start..end)))
    }

    /// Parse a block mapping with explicit key indicator or implicit keys.
    fn parse_block_mapping(&mut self, _min_indent: usize) -> Option<Node> {
        let start = self.current_span().start;
        // Use the actual column of the first key token, not the LineStart indent.
        // This handles cases where there's whitespace between LineStart and the key.
        let map_indent = self.current_token_column();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        // Push this mapping's indentation level onto the stack
        self.push_indent(map_indent);

        // Track if this is the first entry - we don't check indentation for the first entry
        // because the mapping might start inline (e.g., after `- ` in a sequence)
        let mut first_entry = true;

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            // Check indentation - but NOT for the first entry, which might be inline
            // For example: `- ? key` - the `?` is at column 2 but LineStart is at indent 0
            if !first_entry && self.current_indent() < map_indent {
                break;
            }

            // Handle explicit key (?)
            let explicit_key = matches!(self.peek(), Some((Token::MappingKey, _)));
            if explicit_key {
                self.advance();
                self.skip_ws();
            }

            // Check for empty key (: at start without explicit key marker)
            let empty_key = !explicit_key && matches!(self.peek(), Some((Token::Colon, _)));

            // Parse key - could be a scalar, block scalar, or empty (implicit null)
            let key = if explicit_key {
                // For explicit keys, parse a full value (which can have anchors, tags, etc.)
                match self.peek() {
                    Some((Token::MappingKey | Token::Colon, _)) => {
                        // No key provided, implicit null
                        None
                    }
                    Some((Token::LineStart(_), _)) => {
                        // Skip to content
                        self.advance();
                        // Skip any Indent token
                        if let Some((Token::Indent(_), _)) = self.peek() {
                            self.advance();
                        }
                        self.parse_value(map_indent + 1)
                    }
                    Some((Token::Indent(_), _)) => {
                        // Skip Indent token
                        self.advance();
                        self.parse_value(map_indent + 1)
                    }
                    _ => {
                        // Explicit keys can have anchors, tags, flow collections, etc.
                        // Use parse_value to handle all these cases
                        self.parse_value(map_indent + 1)
                    }
                }
            } else if empty_key {
                // Empty key (bare colon) - key is null, don't parse anything
                None
            } else {
                self.parse_scalar()
            };

            let key = match key {
                Some(k) => k,
                None if explicit_key || empty_key => {
                    // Empty explicit key or bare colon - use null
                    Node::null(self.current_span())
                }
                None => break,
            };

            self.skip_ws();

            // Check for value indicator (:) - may be on same line or next line for explicit keys
            // For explicit keys, the colon can be on the next line at same or greater indent
            let mut has_value = matches!(self.peek(), Some((Token::Colon, _)));
            if !has_value && explicit_key {
                // Check if colon is on next line at same indent level
                if let Some((Token::LineStart(n), _)) = self.peek() {
                    if *n >= map_indent {
                        // Save position to potentially backtrack
                        let saved_pos = self.pos;
                        self.advance(); // skip LineStart

                        // Skip any Indent token that follows LineStart
                        while let Some((Token::Indent(_), _)) = self.peek() {
                            self.advance();
                        }

                        // Skip any Dedent token (might be emitted at line boundary)
                        while let Some((Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }

                        if matches!(self.peek(), Some((Token::Colon, _))) {
                            has_value = true;
                        } else {
                            // Not a colon, restore position
                            self.pos = saved_pos;
                        }
                    }
                }
            }

            if has_value && !matches!(self.peek(), Some((Token::Colon, _))) {
                // We already advanced past LineStart above, now at colon
            }
            if has_value && matches!(self.peek(), Some((Token::Colon, _))) {
                self.advance();
                self.skip_ws();
            }

            // Parse value
            let value = if has_value {
                match self.peek() {
                    Some((Token::LineStart(_), _)) => {
                        // Value on next line
                        self.advance();
                        // Skip any Indent/Dedent tokens
                        while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }
                        self.parse_value(map_indent + 1)
                    }
                    Some((Token::Indent(_), _)) => {
                        // Skip Indent token
                        self.advance();
                        self.parse_value(map_indent + 1)
                    }
                    _ => {
                        // Value on same line
                        self.parse_value(map_indent + 1)
                    }
                }
            } else {
                // No colon, so value is null
                None
            };

            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key, value));
            first_entry = false;

            // Skip to next key
            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            // Check for Dedent token - signals end of block
            if let Some((Token::Dedent, _)) = self.peek() {
                self.advance();
                let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
                self.pop_indent();
                return Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)));
            }

            // Check for next line
            while let Some((tok, _)) = self.peek() {
                match tok {
                    Token::LineStart(n) => {
                        let n = *n;
                        if n < map_indent {
                            // Dedented - end of mapping
                            let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
                            self.pop_indent();
                            return Some(Node::new(
                                Value::Mapping(pairs),
                                Span::new((), start..end),
                            ));
                        }
                        if n == map_indent {
                            self.advance();
                            // Skip any Dedent that follows (back to same level)
                            while let Some((Token::Dedent, _)) = self.peek() {
                                self.advance();
                            }
                            break;
                        }
                        self.advance();
                    }
                    Token::Indent(_) => {
                        // Skip indent tokens
                        self.advance();
                    }
                    Token::Dedent => {
                        // End of block
                        self.advance();
                        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
                        self.pop_indent();
                        return Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)));
                    }
                    _ => break,
                }
            }

            // Check if next token can start a key
            if let Some((tok, _)) = self.peek() {
                match tok {
                    Token::Plain(_)
                    | Token::SingleQuoted(_)
                    | Token::DoubleQuoted(_)
                    | Token::MappingKey
                    | Token::Colon // Empty key followed by value
                    | Token::LiteralBlockHeader(_)
                    | Token::FoldedBlockHeader(_) => {}
                    _ => break,
                }
            }

            // Safety: ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
                break;
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        self.pop_indent();
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping where the first key already has properties (anchor/tag).
    /// This is used when we have a pattern like:
    /// ```yaml
    /// &outer
    /// &inner [a]: value
    /// ```
    /// Where &outer applies to the mapping, and &inner applies to the key [a].
    fn parse_block_mapping_with_props(
        &mut self,
        min_indent: usize,
        first_key_props: NodeProperties,
    ) -> Option<Node> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        // Parse the first key with the given properties.
        // IMPORTANT: For scalar keys, we must NOT use parse_scalar_or_mapping
        // because that would consume the `: value` part too. Instead, we need
        // to parse just the key and manually check for `:`.
        //
        // We also need to accumulate any additional tags/anchors before the actual key.
        // For example: &a5 !!str key5: value4
        // Here first_key_props starts with {anchor: a5}, then we see !!str (tag),
        // and finally key5 (the actual key scalar).
        let mut key_props = first_key_props;

        // Accumulate any additional properties (tags/anchors) before the actual key value
        loop {
            match self.peek() {
                Some((Token::Tag(tag), tag_span)) => {
                    let tag = tag.clone();
                    let tag_span = *tag_span;
                    self.advance();
                    self.skip_ws();
                    if key_props.tag.is_some() {
                        self.error(ErrorKind::DuplicateTag, tag_span);
                    }
                    key_props.tag = Some((tag, tag_span));
                }
                Some((Token::Anchor(name), anchor_span)) => {
                    let name = name.clone();
                    let anchor_span = *anchor_span;
                    self.advance();
                    self.skip_ws();
                    if key_props.anchor.is_some() {
                        self.error(ErrorKind::DuplicateAnchor, anchor_span);
                    }
                    key_props.anchor = Some((name, anchor_span));
                }
                Some((Token::Whitespace, _)) => {
                    self.advance();
                }
                _ => break,
            }
        }

        let first_key = match self.peek() {
            Some((Token::Plain(_) | Token::SingleQuoted(_) | Token::DoubleQuoted(_), _)) => {
                // Parse just the scalar, not the full scalar_or_mapping
                let scalar = self.parse_scalar()?;
                self.apply_properties_and_register(key_props, scalar)
            }
            Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                // Flow collection as key - parse it with properties
                if let Some(n) = self.parse_flow_value() {
                    self.apply_properties_and_register(key_props, n)
                } else {
                    return None;
                }
            }
            _ => {
                // Not a valid key token - this isn't a block mapping
                return None;
            }
        };

        self.skip_ws();

        // Check for value indicator (:)
        let has_value = matches!(self.peek(), Some((Token::Colon, _)));
        if !has_value {
            // Not a mapping key, just return None to fall back
            return None;
        }

        self.advance(); // consume ':'
        self.skip_ws();

        // Parse the first value
        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value));

        // Continue parsing remaining entries (same as regular parse_block_mapping)
        self.skip_ws();
        if let Some((Token::Comment(_), _)) = self.peek() {
            self.advance();
        }

        // Check for next line and continue parsing
        while let Some((Token::LineStart(n), _)) = self.peek() {
            if *n < map_indent {
                break;
            }
            if *n == map_indent {
                self.advance();

                // Parse the next key-value pair
                if let Some(key) = self.parse_scalar() {
                    self.skip_ws();
                    if matches!(self.peek(), Some((Token::Colon, _))) {
                        self.advance();
                        self.skip_ws();
                        let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                            self.advance();
                            self.parse_value(map_indent + 1)
                        } else {
                            self.parse_value(map_indent + 1)
                        };
                        let value = value.unwrap_or_else(|| Node::null(self.current_span()));
                        pairs.push((key, value));
                    }
                }
            } else {
                self.advance();
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a scalar and check if it's actually a mapping key.
    fn parse_scalar_or_mapping(&mut self, _min_indent: usize) -> Option<Node> {
        // Remember if the scalar we're about to parse is a quoted scalar, and its start position
        let is_quoted_scalar = matches!(
            self.peek(),
            Some((Token::SingleQuoted(_) | Token::DoubleQuoted(_), _))
        );
        let scalar_start_pos = self.pos;

        let scalar = self.parse_scalar()?;

        self.skip_ws();

        // Check if this is a mapping key (followed by colon)
        if let Some((Token::Colon, colon_span)) = self.peek() {
            // Copy the span before any mutable borrows
            let colon_span = *colon_span;

            // Check for multiline implicit key (invalid in block context)
            // This catches cases like: "c\n d": 1 (quoted string spanning lines)
            self.check_multiline_implicit_key(scalar.span.start, scalar.span.end);

            // Check for invalid nested mapping on same line.
            // Pattern like `a: b: c` or `a: 'b': c` is invalid - a scalar appears
            // in the value position of `a:`, and then `:` tries to make it a key.
            //
            // Valid cases:
            // - `key: value` - at start of line
            // - `a:\n  b: c` - on new line with proper indentation
            // - `: value` (explicit value) `- : moon: white` - implicit key after explicit value colon
            // - Flow context content
            //
            // The rule: if a scalar followed by `:` comes after `key:` pattern on the
            // same line (implicit key value position), it's invalid. But if the scalar
            // comes after just `:` at line start (explicit value), it's valid.
            //
            // Check: look backward for pattern `<scalar> <colon>` on the same line.
            // If we find just `<colon>` at line start (explicit value marker), it's OK.
            let is_nested_value_position = {
                let mut found_implicit_key_colon = false;
                for i in (0..scalar_start_pos).rev() {
                    match &self.tokens[i].0 {
                        Token::Colon => {
                            // Found a colon - check if there's content before it on this line
                            // that would make it an implicit key (vs explicit value marker)
                            // Must stop at LineStart to only check current line
                            let mut has_key_before_colon = false;
                            for j in (0..i).rev() {
                                match &self.tokens[j].0 {
                                    Token::LineStart(_) | Token::MappingKey => {
                                        // Hit line start or explicit key - stop checking
                                        break;
                                    }
                                    Token::Whitespace | Token::Comment(_) => {
                                        // Continue checking further back
                                        continue;
                                    }
                                    Token::Plain(_)
                                    | Token::SingleQuoted(_)
                                    | Token::DoubleQuoted(_)
                                    | Token::FlowSeqEnd
                                    | Token::FlowMapEnd => {
                                        // Found key content before the colon on this line
                                        has_key_before_colon = true;
                                        break;
                                    }
                                    _ => continue,
                                }
                            }
                            if has_key_before_colon {
                                found_implicit_key_colon = true;
                            }
                            // Continue looking back to line start
                        }
                        Token::LineStart(_) => {
                            break;
                        }
                        Token::Whitespace | Token::Comment(_) => continue,
                        _ => continue,
                    }
                }
                found_implicit_key_colon
            };

            if is_nested_value_position {
                self.error(ErrorKind::UnexpectedToken, colon_span);
                // Return just the scalar, don't try to parse as mapping
                return Some(scalar);
            }

            // This is actually a mapping - reparse
            let start = scalar.span.start;
            // map_indent is the column of the first key, used to find sibling keys
            // We use the key's actual column, not the most recent LineStart,
            // because the key may be indented beyond the LineStart (e.g., after a `-`)
            let map_indent = self.column_of_position(scalar.span.start);
            let mut pairs: Vec<(Node, Node)> = Vec::new();

            // Use the scalar we already parsed as the first key
            let key = scalar;

            self.advance(); // consume ':'
            self.skip_ws();

            // Check for block sequence indicator on same line as key - invalid in YAML
            // Block sequences must start on a new line after the key
            if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                let span = *span;
                self.error(ErrorKind::UnexpectedToken, span);
            }

            // Parse the value
            let value_on_same_line = !matches!(self.peek(), Some((Token::LineStart(_), _)));
            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                // Check for tabs used as indentation (invalid in YAML)
                self.check_tabs_as_indentation();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            // Check for trailing content after quoted scalars in block context
            if value_on_same_line {
                if let Value::String(_) = &value.value {
                    self.check_trailing_content_after_scalar();
                }
            }

            pairs.push((key, value));

            // Continue parsing more key-value pairs at same indentation
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
                            // Skip any Dedent tokens that follow (returning to this indent level)
                            while let Some((Token::Dedent, _)) = self.peek() {
                                self.advance();
                            }
                            break true;
                        }
                        self.advance();
                    } else if let Some((Token::Dedent, _)) = self.peek() {
                        // Skip Dedent tokens - they signal end of nested blocks
                        self.advance();
                    } else {
                        break false;
                    }
                };

                if !at_same_indent {
                    break;
                }

                // Skip any Indent tokens that follow LineStart
                self.skip_ws();

                // Try to parse another key
                let key = match self.parse_scalar() {
                    Some(k) => k,
                    None => break,
                };

                self.skip_ws();

                if let Some((Token::Colon, _)) = self.peek() {
                    // Check for multiline implicit key (invalid in block context)
                    self.check_multiline_implicit_key(key.span.start, key.span.end);
                    self.advance();
                } else {
                    // We have a scalar at the same indentation level but it's not followed by a colon.
                    // This is invalid YAML - it looks like a key but isn't properly formed.
                    // Examples:
                    //   top1:
                    //     key1: val1
                    //   top2          <- invalid: at same indent as top1 but no colon
                    self.error(ErrorKind::UnexpectedToken, key.span);
                    break;
                }

                self.skip_ws();

                // Check for block sequence indicator on same line as key - invalid
                if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                    let span = *span;
                    self.error(ErrorKind::UnexpectedToken, span);
                }

                let value_on_same_line = !matches!(self.peek(), Some((Token::LineStart(_), _)));
                let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                    self.advance();
                    self.parse_value(map_indent + 1)
                } else {
                    self.parse_value(map_indent + 1)
                };

                let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                // Check for trailing content after quoted scalars in block context
                if value_on_same_line {
                    if let Value::String(_) = &value.value {
                        self.check_trailing_content_after_scalar();
                    }
                }

                pairs.push((key, value));

                // Safety: ensure progress
                if self.pos == loop_start_pos && !self.is_eof() {
                    break;
                }
            }

            let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
            Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
        } else {
            // Just a scalar - check for multiline continuation (plain scalars only)
            if is_quoted_scalar {
                // Quoted scalars cannot have multiline continuations
                Some(scalar)
            } else {
                // Plain scalar - try to consume multiline continuations
                // min_indent determines which continuation lines are valid
                self.consume_plain_scalar_continuations(scalar, _min_indent)
            }
        }
    }

    /// Consume multiline continuation lines for a plain scalar.
    ///
    /// Called after the first line of a plain scalar has been parsed.
    /// Continuation lines must be more indented than the scalar's starting column.
    fn consume_plain_scalar_continuations(
        &mut self,
        initial: Node,
        block_min_indent: usize,
    ) -> Option<Node> {
        let start = initial.span.start;
        let mut end = initial.span.end;

        // For plain scalar continuations, the minimum indent for continuation lines is:
        // - At least 1 (continuation must be indented)
        // - Greater than or equal to block_min_indent (must be in valid block context)
        //
        // For top-level scalars, block_min_indent = 0, but continuations still need indent >= 1.
        // For nested scalars, block_min_indent already accounts for the parent structure.
        let min_indent = block_min_indent.max(1);

        // Extract the initial content
        let mut content = match &initial.value {
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Null => String::new(),
            _ => return Some(initial), // Not a scalar type, return as-is
        };

        // Track consecutive empty lines for folding
        let mut consecutive_newlines = 0;
        let mut had_continuations = false;

        loop {
            // Look for LineStart tokens for continuation
            match self.peek() {
                Some((Token::LineStart(n), _)) => {
                    let indent = *n;

                    // Check if this might be an empty line (LineStart(0) followed by LineStart)
                    // Empty lines in multiline scalars should be preserved as newlines
                    if indent < min_indent {
                        // Could be an empty line - check if later content continues at valid indent
                        // Skip past any Dedent/Indent tokens to find the next meaningful token
                        let mut next_pos = self.pos + 1;
                        let mut found_continuation = false;
                        while next_pos < self.tokens.len() {
                            match &self.tokens[next_pos].0 {
                                Token::Dedent | Token::Indent(_) => {
                                    next_pos += 1;
                                    continue;
                                }
                                Token::LineStart(next_indent) if *next_indent >= min_indent => {
                                    // This is an empty line followed by a valid continuation
                                    consecutive_newlines += 1;
                                    had_continuations = true;
                                    self.advance(); // consume the empty line's LineStart
                                    // Also skip any Dedent tokens that follow
                                    while matches!(self.peek(), Some((Token::Dedent, _))) {
                                        self.advance();
                                    }
                                    found_continuation = true;
                                    break;
                                }
                                // Special case: Tab at start of line followed by content
                                // This is valid in YAML - the tab acts as separator whitespace
                                // Example: "\n\t3rd line" where \t is not indentation but separator
                                Token::Whitespace => {
                                    // Check if there's content after the whitespace
                                    let after_ws = next_pos + 1;
                                    if after_ws < self.tokens.len() {
                                        match &self.tokens[after_ws].0 {
                                            Token::Plain(_)
                                            | Token::Anchor(_)
                                            | Token::Tag(_)
                                            | Token::Alias(_)
                                            | Token::BlockSeqIndicator => {
                                                // This is a continuation line starting with tab
                                                // Apply folding rules:
                                                // - consecutive_newlines > 0: preserve empty lines as newlines
                                                // - consecutive_newlines == 0: fold to space
                                                if consecutive_newlines == 0 {
                                                    // No empty lines, fold to space
                                                    content.push(' ');
                                                } else {
                                                    // Had empty lines, preserve them
                                                    for _ in 0..consecutive_newlines {
                                                        content.push('\n');
                                                    }
                                                }

                                                // Consume LineStart and Dedent tokens
                                                self.advance(); // consume the LineStart
                                                while matches!(
                                                    self.peek(),
                                                    Some((Token::Dedent, _))
                                                ) {
                                                    self.advance();
                                                }
                                                // Consume the Whitespace (tab)
                                                self.advance();

                                                // Now handle the content - could be Plain or indicators
                                                match self.peek() {
                                                    Some((Token::Plain(s), plain_span)) => {
                                                        let continuation = s.clone();
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
                                                        // Consume all tokens until LineStart as plain text
                                                        let line_start_pos = span.start;
                                                        let mut line_end = span.end;
                                                        while let Some((tok, tok_span)) =
                                                            self.peek()
                                                        {
                                                            match tok {
                                                                Token::LineStart(_) => break,
                                                                _ => {
                                                                    line_end = tok_span.end;
                                                                    self.advance();
                                                                }
                                                            }
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
                                    // Not followed by content, end of scalar
                                    return Some(self.finalize_multiline_scalar(
                                        content,
                                        had_continuations,
                                        start,
                                        end,
                                    ));
                                }
                                _ => {
                                    // Not followed by valid continuation, end of scalar
                                    return Some(self.finalize_multiline_scalar(
                                        content,
                                        had_continuations,
                                        start,
                                        end,
                                    ));
                                }
                            }
                        }
                        // Continue if we found a valid continuation (break from inner loop)
                        // Otherwise end of scalar
                        if !found_continuation {
                            if next_pos >= self.tokens.len() {
                                break;
                            }
                        }
                        continue;
                    }

                    // BEFORE consuming the LineStart, check if the following content
                    // is a mapping key. If so, we should NOT consume the LineStart
                    // because the mapping parser needs it.
                    let next_pos = self.pos + 1;
                    if next_pos < self.tokens.len() {
                        // Skip past LineStart, Indent, and any whitespace to find the content
                        let mut content_pos = next_pos;
                        while content_pos < self.tokens.len() {
                            match &self.tokens[content_pos].0 {
                                Token::Whitespace | Token::Indent(_) => content_pos += 1,
                                _ => break,
                            }
                        }

                        // Check if content is a Plain token followed by Colon (mapping key)
                        if content_pos < self.tokens.len() {
                            if let Token::Plain(_) = &self.tokens[content_pos].0 {
                                // Look ahead past the Plain and any whitespace for ':'
                                let mut lookahead = content_pos + 1;
                                while lookahead < self.tokens.len() {
                                    match &self.tokens[lookahead].0 {
                                        Token::Whitespace => lookahead += 1,
                                        Token::Colon => {
                                            // This is a mapping key, not a continuation
                                            // Do NOT consume the LineStart - leave it for
                                            // the mapping parser to find
                                            return Some(self.finalize_multiline_scalar(
                                                content,
                                                had_continuations,
                                                start,
                                                end,
                                            ));
                                        }
                                        _ => break, // Not followed by colon
                                    }
                                }
                            }
                        }
                    }

                    // Now we know it's a valid continuation, consume the LineStart
                    self.advance(); // consume LineStart

                    // Skip any additional whitespace (but NOT Indent, which we need to skip)
                    // We need to skip Indent tokens manually since skip_ws might not handle them correctly here
                    while let Some((Token::Indent(_) | Token::Whitespace, _)) = self.peek() {
                        self.advance();
                    }

                    // Check what follows
                    match self.peek() {
                        Some((Token::Plain(s), plain_span)) => {
                            let continuation = s.clone();
                            end = plain_span.end;
                            had_continuations = true;

                            // Apply folding rules
                            if consecutive_newlines == 0 {
                                // Single newline → space
                                content.push(' ');
                            } else {
                                // Multiple consecutive newlines → preserve (count) newlines
                                for _ in 0..consecutive_newlines {
                                    content.push('\n');
                                }
                            }
                            content.push_str(&continuation);
                            consecutive_newlines = 0;

                            self.advance(); // consume Plain
                        }
                        Some((Token::LineStart(_), _)) => {
                            // Empty line (another LineStart immediately after)
                            consecutive_newlines += 1;
                            had_continuations = true;
                            continue;
                        }
                        // In plain scalar continuations, indicator characters like &, !, *, -, etc.
                        // are NOT special - they're part of the plain text. The lexer doesn't
                        // know this context, so it tokenized them as Anchor/Tag/BlockSeqIndicator/etc.
                        // We need to consume all tokens on this line as plain text.
                        Some((
                            Token::Anchor(_)
                            | Token::Tag(_)
                            | Token::Alias(_)
                            | Token::BlockSeqIndicator,
                            span,
                        )) => {
                            // This is a continuation line where indicators should be plain text
                            // Consume all tokens until the next LineStart
                            let line_start_pos = span.start;
                            let mut line_end = span.end;

                            // Consume all tokens until LineStart
                            while let Some((tok, tok_span)) = self.peek() {
                                match tok {
                                    Token::LineStart(_) => break,
                                    _ => {
                                        line_end = tok_span.end;
                                        self.advance();
                                    }
                                }
                            }

                            // Extract the raw text from input
                            let line_text = &self.input[line_start_pos..line_end];
                            let continuation = line_text.trim_end();

                            if !continuation.is_empty() {
                                had_continuations = true;

                                // Apply folding rules
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
                            // End of continuations (comment, colon, etc.)
                            break;
                        }
                    }
                }
                _ => {
                    // Not a LineStart - end of multiline scalar
                    break;
                }
            }
        }

        Some(self.finalize_multiline_scalar(content, had_continuations, start, end))
    }

    /// Convert multiline scalar content to final value.
    fn finalize_multiline_scalar(
        &self,
        content: String,
        had_continuations: bool,
        start: usize,
        end: usize,
    ) -> Node {
        // If we had continuations, always treat as string (no type coercion)
        // If no continuations, apply normal scalar type detection
        let value = if had_continuations {
            Value::String(content)
        } else {
            self.scalar_to_value(content)
        };
        Node::new(value, Span::new((), start..end))
    }

    /// Parse a simple scalar token (single-line only, used for keys).
    fn parse_scalar(&mut self) -> Option<Node> {
        let (tok, span) = self.peek()?;
        let span = *span;

        match tok {
            Token::Plain(s) => {
                let value = self.scalar_to_value(s.clone());
                self.advance();
                Some(Node::new(value, span))
            }
            Token::SingleQuoted(s) | Token::DoubleQuoted(s) => {
                let value = Value::String(s.clone());
                self.advance();
                Some(Node::new(value, span))
            }
            _ => None,
        }
    }

    /// Convert a plain scalar string to an appropriate Value.
    fn scalar_to_value(&self, s: String) -> Value {
        // YAML Core Schema type detection
        match s.as_str() {
            // Null
            "null" | "Null" | "NULL" | "~" | "" => Value::Null,
            // Boolean
            "true" | "True" | "TRUE" => Value::Bool(true),
            "false" | "False" | "FALSE" => Value::Bool(false),
            // Try to parse as number
            _ => {
                // Try integer
                if let Ok(i) = s.parse::<i64>() {
                    return Value::Int(i);
                }
                // Try hex integer
                if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
                    if let Ok(i) = i64::from_str_radix(hex, 16) {
                        return Value::Int(i);
                    }
                }
                // Try octal integer
                if let Some(oct) = s.strip_prefix("0o").or_else(|| s.strip_prefix("0O")) {
                    if let Ok(i) = i64::from_str_radix(oct, 8) {
                        return Value::Int(i);
                    }
                }
                // Try float
                if let Ok(f) = s.parse::<f64>() {
                    return Value::Float(f);
                }
                // Special floats
                match s.as_str() {
                    ".inf" | ".Inf" | ".INF" => return Value::Float(f64::INFINITY),
                    "-.inf" | "-.Inf" | "-.INF" => return Value::Float(f64::NEG_INFINITY),
                    ".nan" | ".NaN" | ".NAN" => return Value::Float(f64::NAN),
                    _ => {}
                }
                // Default to string
                Value::String(s)
            }
        }
    }

    /// Parse an alias: *name
    fn parse_alias(&mut self) -> Option<Node> {
        let (tok, span) = self.advance()?;
        let span = *span;

        let name = if let Token::Alias(name) = tok {
            name.clone()
        } else {
            return None;
        };

        // Look up the anchor
        if !self.anchors.contains_key(&name) {
            self.error(ErrorKind::UndefinedAlias, span);
        }
        Some(Node::new(Value::Alias(name), span))
    }

    /// Parse an alias used as a mapping key.
    /// Called when we've already seen `*alias :` pattern.
    /// The alias has been consumed, whitespace skipped, and we're at the colon.
    fn parse_alias_as_mapping_key(
        &mut self,
        alias_name: String,
        alias_span: Span,
        props: NodeProperties,
    ) -> Option<Node> {
        // Validate alias reference
        if !self.anchors.contains_key(&alias_name) {
            self.error(ErrorKind::UndefinedAlias, alias_span);
        }

        // Create the alias key node
        let key = Node::new(Value::Alias(alias_name), alias_span);

        // Consume the colon
        self.advance(); // ':'
        self.skip_ws();

        let map_indent = self.column_of_position(alias_span.start);

        // Parse the value
        let value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let value = value.unwrap_or_else(|| Node::null(self.current_span()));

        let mut pairs = vec![(key, value)];

        // Continue parsing more key-value pairs at the same indent level
        loop {
            // Check for next line at same indent
            let at_same_indent = loop {
                if let Some((Token::LineStart(n), _)) = self.peek() {
                    if *n < map_indent {
                        break false;
                    }
                    if *n == map_indent {
                        self.advance();
                        // Skip any Dedent tokens
                        while let Some((Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }
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

            // Try to parse next key
            let key = match self.peek() {
                Some((Token::Plain(_) | Token::SingleQuoted(_) | Token::DoubleQuoted(_), _)) => {
                    self.parse_scalar()
                }
                Some((Token::Alias(name), span)) => {
                    let name = name.clone();
                    let span = *span;
                    self.advance();
                    if !self.anchors.contains_key(&name) {
                        self.error(ErrorKind::UndefinedAlias, span);
                    }
                    Some(Node::new(Value::Alias(name), span))
                }
                _ => None,
            };

            let Some(key) = key else {
                break;
            };

            self.skip_ws();

            // Expect colon
            if !matches!(self.peek(), Some((Token::Colon, _))) {
                break;
            }
            self.advance(); // ':'
            self.skip_ws();

            // Parse value
            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };
            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key, value));
        }

        let end = pairs
            .last()
            .map(|(_, v)| v.span.end)
            .unwrap_or(alias_span.start);
        let mapping = Node::new(Value::Mapping(pairs), Span::new((), alias_span.start..end));

        Some(self.apply_properties_and_register(props, mapping))
    }

    /// Parse a literal block scalar: |
    fn parse_literal_block_scalar(&mut self, _min_indent: usize) -> Option<Node> {
        let (tok, span) = self.advance()?;
        let start = span.start;

        let header = if let Token::LiteralBlockHeader(h) = tok {
            h.clone()
        } else {
            return None;
        };

        // Collect block scalar content
        let (content, end) = self.collect_block_scalar_content(&header, true);
        Some(Node::new(Value::String(content), Span::new((), start..end)))
    }

    /// Parse a folded block scalar: >
    fn parse_folded_block_scalar(&mut self, _min_indent: usize) -> Option<Node> {
        let (tok, span) = self.advance()?;
        let start = span.start;

        let header = if let Token::FoldedBlockHeader(h) = tok {
            h.clone()
        } else {
            return None;
        };

        // Collect block scalar content
        let (content, end) = self.collect_block_scalar_content(&header, false);
        Some(Node::new(Value::String(content), Span::new((), start..end)))
    }

    /// Collect block scalar content, respecting indentation.
    fn collect_block_scalar_content(
        &mut self,
        header: &BlockScalarHeader,
        literal: bool,
    ) -> (String, usize) {
        use crate::lexer::Chomping;

        let mut lines: Vec<String> = Vec::new();
        let mut content_indent: Option<usize> = header.indent.map(|i| i as usize);
        let mut end_pos = self.current_span().end;

        // Helper to skip Dedent/Indent tokens and find the next meaningful token
        loop {
            // Skip any Dedent/Indent tokens at the start of iteration
            while let Some((Token::Dedent | Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            // Check if we have a LineStart token
            let Some((Token::LineStart(n), span)) = self.peek() else {
                break;
            };
            let n = *n;
            let span = *span;

            // Check if this is an empty line (no content follows)
            // Need to skip past any Dedent/Indent tokens to find if next meaningful token is LineStart
            let is_empty_line = {
                let mut check_pos = self.pos + 1;
                // Skip past Dedent/Indent tokens
                while check_pos < self.tokens.len() {
                    match &self.tokens[check_pos].0 {
                        Token::Dedent | Token::Indent(_) => check_pos += 1,
                        _ => break,
                    }
                }
                if check_pos >= self.tokens.len() {
                    true
                } else {
                    matches!(self.tokens[check_pos].0, Token::LineStart(_))
                }
            };

            // First non-empty line sets the indent if not specified
            if content_indent.is_none() && !is_empty_line {
                content_indent = Some(n);
            }

            let min_indent = content_indent.unwrap_or(1);

            // Check if this line is indented enough (empty lines are always allowed)
            if !is_empty_line && n < min_indent {
                // End of block scalar
                break;
            }

            self.advance(); // consume LineStart
            end_pos = span.end;

            // Skip any Indent token that follows LineStart
            if let Some((Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            // For empty lines, just add an empty string
            if is_empty_line {
                lines.push(String::new());
                continue;
            }

            // Collect content on this line
            let mut line_content = String::new();
            let extra_indent = n.saturating_sub(min_indent);

            // Add extra indentation
            for _ in 0..extra_indent {
                line_content.push(' ');
            }

            // Consume tokens until next line or EOF
            while let Some((tok, tok_span)) = self.peek() {
                match tok {
                    Token::LineStart(_) => break,
                    Token::Dedent | Token::Indent(_) => {
                        // Skip structural Dedent/Indent tokens - they're not content
                        self.advance();
                    }
                    Token::Plain(s) | Token::SingleQuoted(s) | Token::DoubleQuoted(s) => {
                        line_content.push_str(s);
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::Whitespace => {
                        line_content.push(' ');
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::Comment(_) => {
                        // Comments in block scalars are part of content
                        if let Token::Comment(c) = tok {
                            line_content.push('#');
                            line_content.push_str(c);
                        }
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::DocStart | Token::DocEnd => break,
                    _ => {
                        // Include any other token as text
                        end_pos = tok_span.end;
                        self.advance();
                    }
                }
            }

            lines.push(line_content);
        }

        // Join lines based on style
        let mut content = if literal {
            // Literal: preserve line breaks
            lines.join("\n")
        } else {
            // Folded: fold lines (convert single newlines to spaces)
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

        // Apply chomping
        match header.chomping {
            Chomping::Strip => {
                // Remove all trailing newlines
                while content.ends_with('\n') {
                    content.pop();
                }
            }
            Chomping::Clip => {
                // Keep exactly one trailing newline
                while content.ends_with("\n\n") {
                    content.pop();
                }
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }
            Chomping::Keep => {
                // Keep all trailing newlines - add the final one if content exists
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }
        }

        (content, end_pos)
    }

    /// Parse a block mapping starting with an already-parsed key (e.g., flow collection as key).
    /// Example: `[flow]: block` - flow sequence as key of block mapping.
    fn parse_block_mapping_starting_with_key(
        &mut self,
        _min_indent: usize,
        first_key: Node,
    ) -> Option<Node> {
        let start = first_key.span.start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        // Expect colon (already checked by caller, but be safe)
        if !matches!(self.peek(), Some((Token::Colon, _))) {
            return Some(first_key); // Not a mapping, just return the key
        }
        self.advance(); // consume ':'
        self.skip_ws();

        // Parse first value
        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value));

        // Continue parsing mapping entries at the same indent
        loop {
            self.skip_ws();

            // Check for line continuation
            let Some((Token::LineStart(n), _)) = self.peek() else {
                break;
            };
            let n = *n;

            if n < map_indent {
                break;
            }
            if n > map_indent {
                // Skip indented content - it might be part of previous value
                self.advance();
                continue;
            }
            // n == map_indent
            self.advance();

            // Skip Indent/Dedent tokens
            while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                self.advance();
            }

            // Check what kind of key we have next
            match self.peek() {
                Some((Token::Plain(_) | Token::SingleQuoted(_) | Token::DoubleQuoted(_), _)) => {
                    // Regular scalar key
                    let key = self.parse_scalar()?;
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        // Not a mapping key, we're done
                        break;
                    }
                    self.advance(); // consume ':'
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value));
                }
                Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                    // Flow collection as key
                    let key = if matches!(self.peek(), Some((Token::FlowSeqStart, _))) {
                        self.parse_flow_sequence()?
                    } else {
                        self.parse_flow_mapping()?
                    };
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        break;
                    }
                    self.advance(); // consume ':'
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value));
                }
                _ => break,
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping where the first key is a null with properties (tag/anchor).
    /// Example: `!!null : a` - creates a mapping with key being a tagged null.
    fn parse_block_mapping_with_tagged_null_key(
        &mut self,
        _min_indent: usize,
        key_props: NodeProperties,
    ) -> Option<Node> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        // First key is a null with the given properties
        let first_key =
            self.apply_properties_and_register(key_props, Node::null(self.current_span()));

        // Expect colon
        if !matches!(self.peek(), Some((Token::Colon, _))) {
            return None;
        }
        self.advance(); // consume ':'
        self.skip_ws();

        // Parse first value
        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value));

        // Continue parsing mapping entries at the same indent
        loop {
            self.skip_ws();

            // Check for line continuation
            let Some((Token::LineStart(n), _)) = self.peek() else {
                break;
            };
            let n = *n;

            if n < map_indent {
                break;
            }
            if n > map_indent {
                // Skip indented content - it might be part of previous value
                self.advance();
                continue;
            }
            // n == map_indent
            self.advance();

            // Skip Indent/Dedent tokens
            while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                self.advance();
            }

            // Check what kind of key we have next
            match self.peek() {
                Some((Token::Plain(_) | Token::SingleQuoted(_) | Token::DoubleQuoted(_), _)) => {
                    // Regular scalar key
                    let key = self.parse_scalar()?;
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        // Not a mapping key, we're done
                        break;
                    }
                    self.advance(); // consume ':'
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value));
                }
                Some((Token::Tag(_) | Token::Anchor(_), _)) => {
                    // Tagged/anchored key - collect properties first
                    let mut inner_props = NodeProperties::default();
                    loop {
                        match self.peek() {
                            Some((Token::Tag(tag), tag_span)) => {
                                let tag = tag.clone();
                                let tag_span = *tag_span;
                                self.advance();
                                self.skip_ws();
                                if inner_props.tag.is_some() {
                                    self.error(ErrorKind::DuplicateTag, tag_span);
                                }
                                inner_props.tag = Some((tag, tag_span));
                            }
                            Some((Token::Anchor(name), anchor_span)) => {
                                let name = name.clone();
                                let anchor_span = *anchor_span;
                                self.advance();
                                self.skip_ws();
                                if inner_props.anchor.is_some() {
                                    self.error(ErrorKind::DuplicateAnchor, anchor_span);
                                }
                                inner_props.anchor = Some((name, anchor_span));
                            }
                            Some((Token::Whitespace, _)) => {
                                self.advance();
                            }
                            _ => break,
                        }
                    }

                    // Now check what follows - scalar key or colon (tagged null key)
                    match self.peek() {
                        Some((
                            Token::Plain(_) | Token::SingleQuoted(_) | Token::DoubleQuoted(_),
                            _,
                        )) => {
                            let key = self.parse_scalar()?;
                            let key = self.apply_properties_and_register(inner_props, key);
                            self.skip_ws();

                            if !matches!(self.peek(), Some((Token::Colon, _))) {
                                break;
                            }
                            self.advance(); // consume ':'
                            self.skip_ws();

                            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                                self.advance();
                                self.parse_value(map_indent + 1)
                            } else {
                                self.parse_value(map_indent + 1)
                            };
                            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                            pairs.push((key, value));
                        }
                        Some((Token::Colon, _)) => {
                            // Tagged null key
                            let key = self.apply_properties_and_register(
                                inner_props,
                                Node::null(self.current_span()),
                            );
                            self.advance(); // consume ':'
                            self.skip_ws();

                            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                                self.advance();
                                self.parse_value(map_indent + 1)
                            } else {
                                self.parse_value(map_indent + 1)
                            };
                            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                            pairs.push((key, value));
                        }
                        _ => break,
                    }
                }
                Some((Token::Colon, _)) => {
                    // Empty key
                    let key = Node::null(self.current_span());
                    self.advance(); // consume ':'
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value));
                }
                _ => break,
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping starting with an empty key (colon at line start)
    fn parse_block_mapping_with_empty_key(&mut self, _min_indent: usize) -> Option<Node> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        while let Some((Token::Colon, _)) = self.peek() {
            // Empty key
            let key = Node::null(self.current_span());

            self.advance(); // consume ':'
            self.skip_ws();

            // Parse value
            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key, value));

            // Check for next line at same indent
            self.skip_ws();
            while let Some((Token::LineStart(n), _)) = self.peek() {
                if *n < map_indent {
                    let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
                    return Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)));
                }
                if *n == map_indent {
                    self.advance();
                    break;
                }
                self.advance();
            }

            // Check if next token is another empty key
            if !matches!(self.peek(), Some((Token::Colon, _))) {
                break;
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
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

    // Skip initial whitespace/newlines
    parser.skip_ws_and_newlines();

    // Parse the document value
    let doc = if parser.is_eof() {
        None
    } else {
        parser.parse_value(0)
    };

    // After parsing the document, skip remaining whitespace, newlines, and Dedent tokens.
    // Dedent tokens are structural markers that may remain after all block structures are closed.
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
        // Skip any more Dedent tokens
        if matches!(parser.peek(), Some((Token::Dedent, _))) {
            parser.advance();
            continue;
        }

        // There's remaining content after the document
        // Check if it's at column 0 (valid) or column > 0 (invalid)
        let col = parser.current_token_column();

        if col > 0 {
            // Content at column > 0 without being part of the document is invalid
            // This catches cases like:
            //   key:
            //     - ok
            //   - wrong  <-- at column 2, not part of any structure
            parser.error(ErrorKind::UnexpectedToken, parser.current_span());
            parser.advance();
            parser.skip_ws_and_newlines();
        } else {
            // Content at column 0 - this could indicate orphan content
            // For now, break and let it be (future: could be continuation of implicit document)
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
        let (docs, errors) = parse("null");
        assert!(errors.is_empty());
        assert!(matches!(&docs[0].value, Value::Null));

        let (docs, _) = parse("~");
        assert!(matches!(&docs[0].value, Value::Null));
    }

    #[test]
    fn test_parse_boolean_values() {
        let (docs, _) = parse("true");
        assert!(matches!(&docs[0].value, Value::Bool(true)));

        let (docs, _) = parse("false");
        assert!(matches!(&docs[0].value, Value::Bool(false)));
    }

    #[test]
    fn test_parse_number_values() {
        let (docs, _) = parse("42");
        assert!(matches!(&docs[0].value, Value::Int(42)));

        let (docs, _) = parse("3.14");
        if let Value::Float(f) = &docs[0].value {
            assert!((f - 3.14).abs() < 0.001);
        } else {
            panic!("Expected float");
        }
    }

    #[test]
    fn test_parse_multi_document() {
        let (docs, errors) = parse("---\na\n---\nb");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 2);
    }

    #[test]
    fn test_parse_anchor_alias() {
        let (docs, errors) = parse("a: &anchor value\nb: *anchor");
        assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);
        assert_eq!(docs.len(), 1);
        if let Value::Mapping(pairs) = &docs[0].value {
            assert_eq!(pairs.len(), 2);
            // First value should have anchor property set
            assert_eq!(pairs[0].1.anchor.as_deref(), Some("anchor"));
            assert!(matches!(&pairs[0].1.value, Value::String(s) if s == "value"));
            // Second value should be an alias
            assert!(matches!(&pairs[1].1.value, Value::Alias(name) if name == "anchor"));
        } else {
            panic!("Expected mapping");
        }
    }

    #[test]
    fn test_multiline_quoted_key_error() {
        // A double-quoted string that spans multiple lines as an implicit key should be an error
        let input = "\"c\n d\": 1";
        let (docs, errors) = parse(input);
        // Should have an error because the key spans multiple lines
        assert!(
            !errors.is_empty(),
            "Expected error for multiline key, got docs: {:?}",
            docs
        );
    }
}
