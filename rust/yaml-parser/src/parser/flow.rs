// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Flow collection parsing (sequences and mappings in `[]` and `{}`).

use chumsky::span::Span as _;

use crate::error::ErrorKind;
use crate::lexer::Token;
use crate::span::Span;
use crate::value::{Node, Value};

use super::{NodeProperties, Parser};

impl Parser<'_> {
    /// Parse a flow mapping: { key: value, ... }
    #[allow(
        clippy::too_many_lines,
        reason = "Complex flow mapping parsing logic, will be refactored later"
    )]
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by parser logic before access"
    )]
    pub fn parse_flow_mapping(&mut self) -> Option<Node> {
        let (_, start_span) = self.advance()?; // consume '{'
        let start = start_span.start;
        let flow_start_column = self.column_of_position(start);
        let mut pairs: Vec<(Node, Node)> = Vec::new();
        let mut just_saw_comma = true; // Start true to catch leading comma

        // Track flow depth and starting column for indentation validation
        self.flow_depth += 1;
        self.flow_context_columns.push(flow_start_column);

        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            // Check for end of mapping
            if let Some((Token::FlowMapEnd, end_span)) = self.peek() {
                let end = end_span.end;
                self.advance();
                self.flow_depth -= 1;
                self.flow_context_columns.pop();
                return Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)));
            }

            // Check for consecutive commas (e.g., `{ a: 1, , b: 2 }`)
            if let Some((Token::Comma, comma_span)) = self.peek() {
                if just_saw_comma {
                    self.error(ErrorKind::UnexpectedToken, comma_span);
                }
                self.advance();
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
            if let Some((Token::Colon, _)) = self.peek()
                && (explicit_key || matches!(self.peek(), Some((Token::Colon, _))))
            {
                let key_span = self.current_span();
                let key = Node::null(key_span);
                just_saw_comma = false;

                self.advance(); // consume ':'
                self.skip_ws_and_newlines();

                let value = if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
                    Node::null(self.current_span())
                } else {
                    self.parse_flow_value()
                        .unwrap_or_else(|| Node::null(self.current_span()))
                };

                pairs.push((key, value));
                self.skip_ws_and_newlines();

                if let Some((Token::Comma, _)) = self.peek() {
                    self.advance();
                    self.skip_ws_and_newlines();
                    just_saw_comma = true;
                }
                continue;
            }

            // Parse key
            let key = match self.parse_flow_value() {
                Some(node) => {
                    just_saw_comma = false;
                    node
                }
                None => {
                    if explicit_key {
                        Node::null(self.current_span())
                    } else {
                        self.skip_to_flow_delimiter();
                        if self.pos == loop_start_pos && !self.is_eof() {
                            self.advance();
                        }
                        continue;
                    }
                }
            };

            self.skip_ws_and_newlines();

            // Check for colon (explicit value) or comma/end (implicit null value)
            let value = if let Some((Token::Colon, _)) = self.peek() {
                self.advance();
                self.skip_ws_and_newlines();

                if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
                    Node::null(self.current_span())
                } else {
                    self.parse_flow_value()
                        .unwrap_or_else(|| Node::null(self.current_span()))
                }
            } else if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
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

            // Ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
            }
        }

        // Unterminated mapping
        self.error(ErrorKind::UnexpectedEof, self.current_span());
        self.flow_depth -= 1;
        self.flow_context_columns.pop();
        let end = self.tokens.last().map_or(start, |rt| rt.span.end);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a flow sequence: [ item, ... ]
    /// Also handles implicit flow mappings like [ key: value, ... ]
    pub fn parse_flow_sequence(&mut self) -> Option<Node> {
        let (_, start_span) = self.advance()?; // consume '['
        let start = start_span.start;
        let flow_start_column = self.column_of_position(start);
        let mut items: Vec<Node> = Vec::new();
        let mut just_saw_comma = true;

        // Track flow depth and starting column for indentation validation
        self.flow_depth += 1;
        self.flow_context_columns.push(flow_start_column);

        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            if let Some((Token::FlowSeqEnd, end_span)) = self.peek() {
                let end = end_span.end;
                self.advance();
                self.flow_depth -= 1;
                self.flow_context_columns.pop();
                return Some(Node::new(Value::Sequence(items), Span::new((), start..end)));
            }

            if let Some((Token::Comma, comma_span)) = self.peek() {
                if just_saw_comma {
                    self.error(ErrorKind::UnexpectedToken, comma_span);
                }
                self.advance();
                self.skip_ws_and_newlines();
                just_saw_comma = true;
                continue;
            }

            if let Some(item) = self.parse_flow_value() {
                just_saw_comma = false;
                self.skip_ws();

                if let Some((Token::Colon, _)) = self.peek() {
                    self.advance();
                    self.skip_ws_and_newlines();

                    let value =
                        if matches!(self.peek(), Some((Token::Comma | Token::FlowSeqEnd, _))) {
                            Node::null(self.current_span())
                        } else {
                            self.parse_flow_value()
                                .unwrap_or_else(|| Node::null(self.current_span()))
                        };

                    let map_start = item.span.start;
                    let map_end = value.span.end;
                    let mapping_node = Node::new(
                        Value::Mapping(vec![(item, value)]),
                        Span::new((), map_start..map_end),
                    );
                    items.push(mapping_node);
                } else {
                    self.skip_ws_and_newlines();
                    items.push(item);
                }
            } else {
                self.skip_to_flow_delimiter();
            }

            self.skip_ws_and_newlines();

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

            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
            }
        }

        self.error(ErrorKind::UnexpectedEof, self.current_span());
        self.flow_depth -= 1;
        self.flow_context_columns.pop();
        let end = self.tokens.last().map_or(start, |rt| rt.span.end);
        Some(Node::new(Value::Sequence(items), Span::new((), start..end)))
    }

    /// Parse a value in flow context (no block structures).
    pub fn parse_flow_value(&mut self) -> Option<Node> {
        self.parse_flow_value_with_properties(NodeProperties::default())
    }

    /// Parse a flow value with already-collected node properties.
    pub fn parse_flow_value_with_properties(&mut self, mut props: NodeProperties) -> Option<Node> {
        self.skip_ws_and_newlines();

        let (tok, span) = self.peek()?;
        let start_span = span;

        match tok {
            Token::FlowMapStart => self
                .parse_flow_mapping()
                .map(|node| self.apply_properties_and_register(props, node)),
            Token::FlowSeqStart => self
                .parse_flow_sequence()
                .map(|node| self.apply_properties_and_register(props, node)),
            Token::Anchor(name) => {
                let anchor_name = name.clone();
                let anchor_span = start_span;
                self.advance();

                if props.anchor.is_some() {
                    self.error(ErrorKind::DuplicateAnchor, anchor_span);
                }
                props.anchor = Some((anchor_name, anchor_span));

                self.parse_flow_value_with_properties(props)
            }
            Token::Alias(_) => {
                if !props.is_empty() {
                    self.error(ErrorKind::PropertiesOnAlias, start_span);
                }
                self.parse_alias()
            }
            Token::Tag(tag) => {
                let tag_name = tag.clone();
                let tag_span = start_span;
                self.advance();

                if props.tag.is_some() {
                    self.error(ErrorKind::DuplicateTag, tag_span);
                }
                props.tag = Some((tag_name, tag_span));

                self.parse_flow_value_with_properties(props)
            }
            Token::Plain(string) => {
                let mut combined = string.clone();
                let mut end_span = start_span;
                self.advance();

                // In flow context, plain scalars can span multiple lines.
                // Newlines are folded to spaces.
                loop {
                    while matches!(
                        self.peek(),
                        Some((Token::Whitespace | Token::WhitespaceWithTabs, _))
                    ) {
                        self.advance();
                    }

                    let Some((Token::LineStart(_), _)) = self.peek() else {
                        break;
                    };

                    let next_pos = self.pos + 1;
                    if next_pos >= self.tokens.len() {
                        break;
                    }

                    #[allow(clippy::indexing_slicing, reason = "next_pos bounds checked above")]
                    match &self.tokens[next_pos].token {
                        Token::Plain(continuation) => {
                            combined.push(' ');
                            combined.push_str(continuation);
                            #[allow(
                                clippy::indexing_slicing,
                                reason = "next_pos bounds checked above"
                            )]
                            let continuation_span = self.tokens[next_pos].span;
                            end_span = continuation_span;
                            self.advance(); // consume LineStart
                            self.advance(); // consume Plain
                        }
                        _ => break,
                    }
                }

                let value = Self::scalar_to_value(combined);
                let node = Node::new(value, Span::new((), start_span.start..end_span.end));
                Some(self.apply_properties_and_register(props, node))
            }
            Token::StringStart(_) => {
                // Parse the quoted string using the new token sequence
                // In flow context, indentation rules are relaxed, so use min_indent=0
                self.parse_quoted_string(0)
                    .map(|node| self.apply_properties_and_register(props, node))
            }
            Token::Comma | Token::FlowSeqEnd | Token::FlowMapEnd | Token::Colon => {
                if props.is_empty() {
                    None
                } else {
                    Some(self.apply_properties_and_register(props, Node::null(start_span)))
                }
            }
            _ => None,
        }
    }

    /// Skip to the next flow delimiter (, ] })
    pub fn skip_to_flow_delimiter(&mut self) {
        while let Some((tok, _)) = self.peek() {
            match tok {
                Token::Comma | Token::FlowSeqEnd | Token::FlowMapEnd => break,
                _ => {
                    self.advance();
                }
            }
        }
    }
}
