// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Flow collection parsing (sequences and mappings in `[]` and `{}`).

use std::borrow::Cow;

use crate::error::ErrorKind;
use crate::event::{CollectionStyle, Event, ScalarStyle};
use crate::lexer::Token;
use crate::span::Span;
use crate::value::{Node, Value};

use super::{NodeProperties, Parser};

impl<'tokens: 'input, 'input> Parser<'tokens, 'input> {
    /// Enter a flow collection context.
    ///
    /// Tracks flow depth and starting column for proper indentation validation.
    /// Returns the start position for span tracking.
    fn enter_flow_collection(&mut self) -> Option<usize> {
        let (_, start_span) = self.advance()?;
        let start = start_span.start_usize();
        let flow_start_column = self.column_of_position(start);

        self.flow_depth += 1;
        self.flow_context_columns.push(flow_start_column);

        Some(start)
    }

    /// Exit a flow collection context.
    ///
    /// Decrements flow depth and removes the column tracking.
    fn exit_flow_collection(&mut self) {
        self.flow_depth -= 1;
        self.flow_context_columns.pop();
    }

    /// Handle a comma in flow context.
    ///
    /// Returns true if a comma was consumed, false otherwise.
    /// Reports an error if consecutive commas are detected.
    fn handle_flow_comma(&mut self, just_saw_comma: &mut bool) -> bool {
        if let Some((Token::Comma, comma_span)) = self.peek() {
            if *just_saw_comma {
                self.error(ErrorKind::MissingSeparator, comma_span);
            }
            self.advance();
            self.skip_ws_and_newlines();
            *just_saw_comma = true;
            true
        } else {
            false
        }
    }

    /// Handle end of flow entry (comma or closing delimiter).
    ///
    /// Consumes a comma if present, otherwise checks for expected end token.
    /// Reports error and skips to delimiter if unexpected token found.
    fn handle_flow_entry_end(
        &mut self,
        just_saw_comma: &mut bool,
        is_end_token: impl Fn(&Token) -> bool,
    ) {
        if let Some((Token::Comma, _)) = self.peek() {
            self.advance();
            self.skip_ws_and_newlines();
            *just_saw_comma = true;
        } else if let Some((tok, _)) = self.peek() {
            if is_end_token(tok) {
                // Will be handled at top of loop
            } else if !self.is_eof() {
                self.error(ErrorKind::MissingSeparator, self.current_span());
                self.skip_to_flow_delimiter();
            }
        }
    }

    /// Parse a flow mapping: { key: value, ... }
    ///
    /// Emits: `MappingStart` at entry, key/value pairs recursively, `MappingEnd` at exit.
    #[allow(clippy::too_many_lines, reason = "Complex flow mapping logic")]
    pub fn parse_flow_mapping(&mut self) -> Option<Node<'input>> {
        let start = self.enter_flow_collection()?;
        let start_span = Span::from_usize_range(start..start + 1);
        let mut pairs: Vec<(Node<'input>, Node<'input>)> = Vec::new();
        let mut just_saw_comma = true; // Start true to catch leading comma

        // Emit MappingStart event
        self.emit(Event::MappingStart {
            style: CollectionStyle::Flow,
            anchor: None, // Properties are handled by caller
            tag: None,
            span: start_span,
        });

        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            // Check for end of mapping
            if let Some((Token::FlowMapEnd, end_span)) = self.peek() {
                let end = end_span.end_usize();
                self.advance();
                self.exit_flow_collection();
                self.emit(Event::MappingEnd { span: end_span });
                return Some(Node::new(
                    Value::Mapping(pairs),
                    Span::from_usize_range(start..end),
                ));
            }

            // Check for consecutive commas (e.g., `{ a: 1, , b: 2 }`)
            if self.handle_flow_comma(&mut just_saw_comma) {
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
                let key = self.emit_null_scalar();
                just_saw_comma = false;

                self.advance(); // consume ':'
                self.skip_ws_and_newlines();

                let value = if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
                    self.emit_null_scalar()
                } else {
                    self.parse_flow_value()
                        .unwrap_or_else(|| self.emit_null_scalar())
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
                        self.emit_null_scalar()
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
                    self.emit_null_scalar()
                } else {
                    self.parse_flow_value()
                        .unwrap_or_else(|| self.emit_null_scalar())
                }
            } else if matches!(self.peek(), Some((Token::Comma | Token::FlowMapEnd, _))) {
                self.emit_null_scalar()
            } else {
                self.error(ErrorKind::MissingSeparator, self.current_span());
                self.emit_null_scalar()
            };

            pairs.push((key, value));

            self.skip_ws_and_newlines();

            // Check for comma or end
            self.handle_flow_entry_end(&mut just_saw_comma, |tok| matches!(tok, Token::FlowMapEnd));

            // Ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
            }
        }

        // Unterminated mapping
        self.error(ErrorKind::UnexpectedEof, self.current_span());
        self.exit_flow_collection();
        let end = self.tokens.last().map_or(start, |rt| rt.span.end_usize());
        self.emit(Event::MappingEnd {
            span: Span::from_usize_range(end..end),
        });
        Some(Node::new(
            Value::Mapping(pairs),
            Span::from_usize_range(start..end),
        ))
    }

    /// Parse a flow sequence: [ item, ... ]
    /// Also handles implicit flow mappings like [ key: value, ... ]
    ///
    /// Emits: `SequenceStart` at entry, items recursively, `SequenceEnd` at exit.
    #[allow(clippy::too_many_lines, reason = "Complex flow sequence logic")]
    pub fn parse_flow_sequence(&mut self) -> Option<Node<'input>> {
        let start = self.enter_flow_collection()?;
        let start_span = Span::from_usize_range(start..start + 1);
        let mut items: Vec<Node<'input>> = Vec::new();
        let mut just_saw_comma = true;

        // Emit SequenceStart event
        self.emit(Event::SequenceStart {
            style: CollectionStyle::Flow,
            anchor: None, // Properties are handled by caller
            tag: None,
            span: start_span,
        });

        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            if let Some((Token::FlowSeqEnd, end_span)) = self.peek() {
                let end = end_span.end_usize();
                self.advance();
                self.exit_flow_collection();
                self.emit(Event::SequenceEnd { span: end_span });
                return Some(Node::new(
                    Value::Sequence(items),
                    Span::from_usize_range(start..end),
                ));
            }

            // Check for consecutive commas
            if self.handle_flow_comma(&mut just_saw_comma) {
                continue;
            }

            // Handle explicit key marker (?) - creates a single-pair mapping entry
            let explicit_key = matches!(self.peek(), Some((Token::MappingKey, _)));
            if explicit_key {
                self.advance();
                self.skip_ws_and_newlines();
            }

            // Check for empty key (: at start without key) - creates single-pair mapping
            if let Some((Token::Colon, colon_span)) = self.peek() {
                let key_span = colon_span;
                just_saw_comma = false;

                // Emit MappingStart for implicit single-pair mapping
                self.emit(Event::MappingStart {
                    style: CollectionStyle::Flow,
                    anchor: None,
                    tag: None,
                    span: key_span,
                });

                // Emit empty scalar for null key
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: None,
                    tag: None,
                    span: key_span,
                });
                let key = Node::null(key_span);

                self.advance(); // consume ':'
                self.skip_ws_and_newlines();

                let value = if matches!(self.peek(), Some((Token::Comma | Token::FlowSeqEnd, _))) {
                    self.emit_null_scalar()
                } else {
                    self.parse_flow_value()
                        .unwrap_or_else(|| self.emit_null_scalar())
                };

                let map_start = key_span.start_usize();
                let map_end = value.span.end_usize();

                // Emit MappingEnd
                self.emit(Event::MappingEnd {
                    span: Span::from_usize_range(map_end..map_end),
                });

                let mapping_node = Node::new(
                    Value::Mapping(vec![(key, value)]),
                    Span::from_usize_range(map_start..map_end),
                );
                items.push(mapping_node);
                self.skip_ws_and_newlines();
                self.handle_flow_entry_end(&mut just_saw_comma, |tok| {
                    matches!(tok, Token::FlowSeqEnd)
                });
                continue;
            }

            // Check if this could be an implicit single-pair mapping by looking ahead
            // We need to parse the potential key, then check for colon
            let item_start_event_index = self.events.len();
            if let Some(item) = self.parse_flow_value() {
                just_saw_comma = false;
                self.skip_ws();

                if let Some((Token::Colon, _)) = self.peek() {
                    // This is an implicit single-pair mapping!
                    // Insert MappingStart before the key event(s)
                    let map_start_span = item.span;
                    let mapping_start_event = Event::MappingStart {
                        style: CollectionStyle::Flow,
                        anchor: None,
                        tag: None,
                        span: map_start_span,
                    };
                    self.events
                        .insert(item_start_event_index, mapping_start_event);

                    self.advance();
                    self.skip_ws_and_newlines();

                    let value =
                        if matches!(self.peek(), Some((Token::Comma | Token::FlowSeqEnd, _))) {
                            self.emit_null_scalar()
                        } else {
                            self.parse_flow_value()
                                .unwrap_or_else(|| self.emit_null_scalar())
                        };

                    let map_start = item.span.start_usize();
                    let map_end = value.span.end_usize();

                    // Emit MappingEnd
                    self.emit(Event::MappingEnd {
                        span: Span::from_usize_range(map_end..map_end),
                    });

                    let mapping_node = Node::new(
                        Value::Mapping(vec![(item, value)]),
                        Span::from_usize_range(map_start..map_end),
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

            // Check for comma or end
            self.handle_flow_entry_end(&mut just_saw_comma, |tok| matches!(tok, Token::FlowSeqEnd));

            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
            }
        }

        self.error(ErrorKind::UnexpectedEof, self.current_span());
        self.exit_flow_collection();
        let end = self.tokens.last().map_or(start, |rt| rt.span.end_usize());
        self.emit(Event::SequenceEnd {
            span: Span::from_usize_range(end..end),
        });
        Some(Node::new(
            Value::Sequence(items),
            Span::from_usize_range(start..end),
        ))
    }

    /// Parse a value in flow context (no block structures).
    pub fn parse_flow_value(&mut self) -> Option<Node<'input>> {
        self.parse_flow_value_with_properties(NodeProperties::default())
    }

    /// Parse a flow value with already-collected node properties.
    pub fn parse_flow_value_with_properties(
        &mut self,
        initial_props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        self.skip_ws_and_newlines();

        // Collect any properties (anchors/tags) before the value
        let props = self.collect_node_properties(initial_props);
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
            Token::Alias(_) => {
                if !props.is_empty() {
                    self.error(ErrorKind::PropertiesOnAlias, start_span);
                }
                self.parse_alias()
            }
            Token::Plain(string) => {
                let mut combined = string.to_string();
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

                    let Some(next_rt) = self.tokens.get(self.pos + 1) else {
                        break;
                    };

                    match &next_rt.token {
                        Token::Plain(continuation) => {
                            combined.push(' ');
                            combined.push_str(continuation);
                            end_span = next_rt.span;
                            self.advance(); // consume LineStart
                            self.advance(); // consume Plain
                        }
                        _ => break,
                    }
                }

                let value = Self::scalar_to_value(combined.clone());
                let full_span =
                    Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

                // Emit Scalar event for flow plain scalar
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Owned(combined),
                    anchor: None, // Properties applied by caller
                    tag: None,
                    span: full_span,
                });

                let node = Node::new(value, full_span);
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
                    // Emit scalar event for the null with properties
                    self.emit(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        anchor: props.anchor.as_ref().map(|(name, _)| Cow::Borrowed(*name)),
                        tag: props.tag.as_ref().map(|(tag, _)| tag.clone()),
                        span: start_span,
                    });
                    let null_node = Node::null(start_span);
                    let node_with_props = props.clone().apply_to(null_node);
                    if let Some((name, _)) = &props.anchor {
                        self.anchors.insert(name, node_with_props.clone());
                    }
                    Some(node_with_props)
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
