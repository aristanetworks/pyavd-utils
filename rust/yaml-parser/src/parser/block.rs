// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Block structure parsing (block sequences and mappings).

use std::borrow::Cow;

use chumsky::span::Span as _;

use crate::error::ErrorKind;
use crate::span::Span;
use crate::token::Token;
use crate::value::{Node, Value};

use super::{NodeProperties, Parser};

impl<'tokens: 'input, 'input> Parser<'tokens, 'input> {
    /// Parse a block sequence: - item\n- item
    pub fn parse_block_sequence(&mut self, _min_indent: usize) -> Option<Node<'input>> {
        let (_, start_span) = self.peek()?;
        let start = start_span.start;
        let seq_indent = self.current_token_column();
        let mut items: Vec<Node<'input>> = Vec::new();

        self.push_indent(seq_indent);

        while let Some((Token::BlockSeqIndicator, _)) = self.peek() {
            let item_col = self.current_token_column();
            if item_col < seq_indent {
                break;
            }
            if item_col > seq_indent {
                break;
            }

            self.advance(); // consume '-'
            self.check_tabs_after_seq_indicator();
            self.skip_ws();

            let item = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                if let Some((Token::Indent(_), _)) = self.peek() {
                    self.advance();
                }
                let content_col = self.current_token_column();
                self.parse_value(content_col)
            } else {
                self.parse_value(seq_indent + 1)
            };

            if let Some(node) = item {
                items.push(node);
            } else {
                items.push(Node::null(self.current_span()));
            }

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            loop {
                match self.peek() {
                    Some((Token::LineStart(n), _)) => {
                        let n = *n;
                        if n < seq_indent {
                            let end = items.last().map_or(start, |node| node.span.end);
                            self.pop_indent();
                            return Some(Node::new(
                                Value::Sequence(items),
                                Span::new((), start..end),
                            ));
                        }
                        self.advance();
                    }
                    Some((
                        Token::Whitespace
                        | Token::WhitespaceWithTabs
                        | Token::Comment(_)
                        | Token::Indent(_)
                        | Token::Dedent,
                        _,
                    )) => {
                        self.advance();
                    }
                    _ => {
                        break;
                    }
                }
            }
        }

        let end = items.last().map_or(start, |node| node.span.end);
        self.pop_indent();
        Some(Node::new(Value::Sequence(items), Span::new((), start..end)))
    }

    /// Parse a block mapping with explicit key indicator or implicit keys.
    #[allow(
        clippy::too_many_lines,
        reason = "Complex block mapping parsing logic, will be refactored later"
    )]
    pub fn parse_block_mapping(&mut self, _min_indent: usize) -> Node<'input> {
        let start = self.current_span().start;
        let map_indent = self.current_token_column();
        let mut pairs: Vec<(Node<'input>, Node<'input>)> = Vec::new();

        self.push_indent(map_indent);

        let mut first_entry = true;

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            if !first_entry && self.current_indent() < map_indent {
                break;
            }

            let explicit_key = matches!(self.peek(), Some((Token::MappingKey, _)));
            if explicit_key {
                self.advance();
                self.check_tabs_after_block_indicator();
                self.skip_ws();
            }

            let empty_key = !explicit_key && matches!(self.peek(), Some((Token::Colon, _)));

            // Collect any anchor/tag properties before the key
            let key_props = self.collect_node_properties(NodeProperties::default());

            let key = if explicit_key {
                match self.peek() {
                    Some((Token::MappingKey | Token::Colon, _)) => None,
                    Some((Token::LineStart(_), _)) => {
                        self.advance();
                        if let Some((Token::Indent(_), _)) = self.peek() {
                            self.advance();
                        }
                        self.parse_value(map_indent + 1)
                    }
                    Some((Token::Indent(_), _)) => {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    }
                    _ => self.parse_value(map_indent + 1),
                }
            } else if empty_key {
                None
            } else {
                // Check if the key is an alias with properties (invalid)
                if let Some((Token::Alias(name), span)) = self.peek() {
                    let alias_name = name.clone();
                    if !key_props.is_empty() {
                        self.error(ErrorKind::PropertiesOnAlias, span);
                    }
                    self.advance();
                    if !self.anchors.contains_key(alias_name.as_ref()) {
                        self.error(ErrorKind::UndefinedAlias, span);
                    }
                    Some(Node::new(Value::Alias(alias_name), span))
                } else {
                    self.parse_scalar()
                }
            };

            let key_node = match key {
                Some(node) => {
                    // Apply properties to the key (unless it's an alias, which already errored)
                    if !key_props.is_empty() && !matches!(node.value, Value::Alias(_)) {
                        self.apply_properties_and_register(key_props, node)
                    } else {
                        node
                    }
                }
                None if explicit_key || empty_key => Node::null(self.current_span()),
                None => break,
            };

            self.skip_ws();

            let mut has_value = matches!(self.peek(), Some((Token::Colon, _)));
            if !has_value
                && explicit_key
                && let Some((Token::LineStart(n), _)) = self.peek()
                && *n >= map_indent
            {
                let saved_pos = self.pos;
                self.advance();

                while let Some((Token::Indent(_), _)) = self.peek() {
                    self.advance();
                }

                while let Some((Token::Dedent, _)) = self.peek() {
                    self.advance();
                }

                if matches!(self.peek(), Some((Token::Colon, _))) {
                    has_value = true;
                } else {
                    self.pos = saved_pos;
                }
            }

            if has_value && !matches!(self.peek(), Some((Token::Colon, _))) {
                // We already advanced past LineStart above, now at colon
            }
            if has_value && matches!(self.peek(), Some((Token::Colon, _))) {
                self.advance();
                self.check_tabs_after_block_indicator();
                self.skip_ws();
            }

            let value = if has_value {
                match self.peek() {
                    Some((Token::LineStart(_), _)) => {
                        self.advance();
                        while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }
                        self.parse_value(map_indent + 1)
                    }
                    Some((Token::Indent(_), _)) => {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    }
                    _ => self.parse_value(map_indent + 1),
                }
            } else {
                None
            };

            let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key_node, value_node));
            first_entry = false;

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            if let Some((Token::Dedent, _)) = self.peek() {
                self.advance();
                let end = pairs.last().map_or(start, |(_, node)| node.span.end);
                self.pop_indent();
                return Node::new(Value::Mapping(pairs), Span::new((), start..end));
            }

            while let Some((tok, _)) = self.peek() {
                match tok {
                    Token::LineStart(n) => {
                        let n = *n;
                        if n < map_indent {
                            let end = pairs.last().map_or(start, |(_, node)| node.span.end);
                            self.pop_indent();
                            return Node::new(Value::Mapping(pairs), Span::new((), start..end));
                        }
                        if n == map_indent {
                            self.advance();
                            while let Some((Token::Dedent, _)) = self.peek() {
                                self.advance();
                            }
                            break;
                        }
                        self.advance();
                    }
                    Token::Indent(_) => {
                        self.advance();
                    }
                    Token::Dedent => {
                        self.advance();
                        let end = pairs.last().map_or(start, |(_, node)| node.span.end);
                        self.pop_indent();
                        return Node::new(Value::Mapping(pairs), Span::new((), start..end));
                    }
                    _ => break,
                }
            }

            if let Some((tok, _)) = self.peek() {
                match tok {
                    Token::Plain(_)
                    | Token::StringStart(_)
                    | Token::MappingKey
                    | Token::Colon
                    | Token::LiteralBlockHeader(_)
                    | Token::FoldedBlockHeader(_)
                    | Token::Anchor(_)
                    | Token::Alias(_)
                    | Token::Tag(_) => {}
                    _ => break,
                }
            }

            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
                break;
            }
        }

        let end = pairs.last().map_or(start, |(_, node)| node.span.end);
        self.pop_indent();
        Node::new(Value::Mapping(pairs), Span::new((), start..end))
    }

    /// Parse a block mapping where the first key already has properties (anchor/tag).
    pub fn parse_block_mapping_with_props(
        &mut self,
        _min_indent: usize,
        first_key_props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node<'input>, Node<'input>)> = Vec::new();

        // Collect any additional properties after the initial ones
        let key_props = self.collect_node_properties(first_key_props);

        let first_key = match self.peek() {
            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                let scalar = self.parse_scalar()?;
                self.apply_properties_and_register(key_props, scalar)
            }
            Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                if let Some(n) = self.parse_flow_value() {
                    self.apply_properties_and_register(key_props, n)
                } else {
                    return None;
                }
            }
            _ => {
                return None;
            }
        };

        self.skip_ws();

        let has_value = matches!(self.peek(), Some((Token::Colon, _)));
        if !has_value {
            return None;
        }

        self.advance(); // consume ':'
        self.skip_ws();

        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value_node = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value_node));

        self.skip_ws();
        if let Some((Token::Comment(_), _)) = self.peek() {
            self.advance();
        }

        while let Some((Token::LineStart(n), _)) = self.peek() {
            if *n < map_indent {
                break;
            }
            if *n == map_indent {
                self.advance();

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
                        let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));
                        pairs.push((key, value_node));
                    }
                }
            } else {
                self.advance();
            }
        }

        let end = pairs.last().map_or(start, |(_, node)| node.span.end);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping when we already have the first key.
    pub fn parse_block_mapping_starting_with_key(
        &mut self,
        _min_indent: usize,
        first_key: Node<'input>,
    ) -> Option<Node<'input>> {
        let start = first_key.span.start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node<'input>, Node<'input>)> = Vec::new();

        if !matches!(self.peek(), Some((Token::Colon, _))) {
            return Some(first_key);
        }
        self.advance(); // consume ':'
        self.skip_ws();

        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value_node = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value_node));

        loop {
            self.skip_ws();

            let Some((Token::LineStart(n), _)) = self.peek() else {
                break;
            };
            let n = *n;

            if n < map_indent {
                break;
            }
            if n > map_indent {
                self.advance();
                continue;
            }
            self.advance();

            while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                self.advance();
            }

            match self.peek() {
                Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                    let key = self.parse_scalar()?;
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        break;
                    }
                    self.advance();
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value_node));
                }
                Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                    let key = if matches!(self.peek(), Some((Token::FlowSeqStart, _))) {
                        self.parse_flow_sequence()?
                    } else {
                        self.parse_flow_mapping()?
                    };
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        break;
                    }
                    self.advance();
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value_node));
                }
                _ => break,
            }
        }

        let end = pairs.last().map_or(start, |(_, node)| node.span.end);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping where the first key is a null with properties.
    #[allow(
        clippy::too_many_lines,
        reason = "Complex block mapping parsing logic, will be refactored later"
    )]
    pub fn parse_block_mapping_with_tagged_null_key(
        &mut self,
        _min_indent: usize,
        key_props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node<'input>, Node<'input>)> = Vec::new();

        let first_key =
            self.apply_properties_and_register(key_props, Node::null(self.current_span()));

        if !matches!(self.peek(), Some((Token::Colon, _))) {
            return None;
        }
        self.advance();
        self.skip_ws();

        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value_node = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value_node));

        loop {
            self.skip_ws();

            let Some((Token::LineStart(n), _)) = self.peek() else {
                break;
            };
            let n = *n;

            if n < map_indent {
                break;
            }
            if n > map_indent {
                self.advance();
                continue;
            }
            self.advance();

            while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                self.advance();
            }

            match self.peek() {
                Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                    let key = self.parse_scalar()?;
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        break;
                    }
                    self.advance();
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value_node));
                }
                Some((Token::Tag(_) | Token::Anchor(_), _)) => {
                    let inner_props = self.collect_node_properties(NodeProperties::default());

                    match self.peek() {
                        Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                            let key = self.parse_scalar()?;
                            let key_node = self.apply_properties_and_register(inner_props, key);
                            self.skip_ws();

                            if !matches!(self.peek(), Some((Token::Colon, _))) {
                                break;
                            }
                            self.advance();
                            self.skip_ws();

                            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                                self.advance();
                                self.parse_value(map_indent + 1)
                            } else {
                                self.parse_value(map_indent + 1)
                            };
                            let value_node =
                                value.unwrap_or_else(|| Node::null(self.current_span()));

                            pairs.push((key_node, value_node));
                        }
                        Some((Token::Colon, _)) => {
                            let key = self.apply_properties_and_register(
                                inner_props,
                                Node::null(self.current_span()),
                            );
                            self.advance();
                            self.skip_ws();

                            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                                self.advance();
                                self.parse_value(map_indent + 1)
                            } else {
                                self.parse_value(map_indent + 1)
                            };
                            let value_node =
                                value.unwrap_or_else(|| Node::null(self.current_span()));

                            pairs.push((key, value_node));
                        }
                        _ => break,
                    }
                }
                Some((Token::Colon, _)) => {
                    let key = Node::null(self.current_span());
                    self.advance();
                    self.check_tabs_after_block_indicator();
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value_node));
                }
                _ => break,
            }
        }

        let end = pairs.last().map_or(start, |(_, node)| node.span.end);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping starting with an empty key (colon at line start).
    pub fn parse_block_mapping_with_empty_key(&mut self, _min_indent: usize) -> Node<'input> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node<'input>, Node<'input>)> = Vec::new();

        while let Some((Token::Colon, _)) = self.peek() {
            let key = Node::null(self.current_span());

            self.advance();
            self.check_tabs_after_block_indicator();
            self.skip_ws();

            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key, value_node));

            self.skip_ws();
            while let Some((Token::LineStart(n), _)) = self.peek() {
                if *n < map_indent {
                    let end = pairs.last().map_or(start, |(_, node)| node.span.end);
                    return Node::new(Value::Mapping(pairs), Span::new((), start..end));
                }
                if *n == map_indent {
                    self.advance();
                    break;
                }
                self.advance();
            }

            if !matches!(self.peek(), Some((Token::Colon, _))) {
                break;
            }
        }

        let end = pairs.last().map_or(start, |(_, node)| node.span.end);
        Node::new(Value::Mapping(pairs), Span::new((), start..end))
    }

    /// Parse a block mapping where the key is an alias.
    pub fn parse_alias_as_mapping_key(
        &mut self,
        alias_name: Cow<'input, str>,
        alias_span: Span,
        props: NodeProperties<'input>,
    ) -> Node<'input> {
        if !self.anchors.contains_key(alias_name.as_ref()) {
            self.error(ErrorKind::UndefinedAlias, alias_span);
        }

        let alias_node = Node::new(Value::Alias(alias_name), alias_span);

        self.advance(); // ':'
        self.skip_ws();

        let map_indent = self.column_of_position(alias_span.start);

        let value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

        let mut pairs = vec![(alias_node, value_node)];

        loop {
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

            let key = match self.peek() {
                Some((Token::Plain(_) | Token::StringStart(_), _)) => self.parse_scalar(),
                Some((Token::Alias(name), span)) => {
                    let new_alias_name = name.clone();
                    self.advance();
                    if !self.anchors.contains_key(new_alias_name.as_ref()) {
                        self.error(ErrorKind::UndefinedAlias, span);
                    }
                    Some(Node::new(Value::Alias(new_alias_name), span))
                }
                _ => None,
            };

            let Some(key_node) = key else {
                break;
            };

            self.skip_ws();

            if !matches!(self.peek(), Some((Token::Colon, _))) {
                break;
            }
            self.advance();
            self.skip_ws();

            let linestart = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };
            let linestart_node = linestart.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key_node, linestart_node));
        }

        let end = pairs
            .last()
            .map_or(alias_span.start, |(_, node)| node.span.end);
        let mapping = Node::new(Value::Mapping(pairs), Span::new((), alias_span.start..end));

        self.apply_properties_and_register(props, mapping)
    }
}
