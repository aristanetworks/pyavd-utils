// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::ast_event::AstEvent;
use crate::event::Event;
use crate::value::{Comment, Node};

use super::{ParsedNode, Parser};

impl<'input, I> Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Parse a single node from events.
    pub(super) fn parse_node(&mut self) -> Option<Node<'input>> {
        self.parse_node_with_metadata().map(|parsed| parsed.node)
    }

    pub(super) fn parse_node_with_metadata(&mut self) -> Option<ParsedNode<'input>> {
        self.next_event()
            .and_then(|event| self.parse_node_from_ast_event(event))
    }

    /// Parse a mapping value unless the next event is a collection end marker.
    ///
    /// Recovery paths may queue a `MappingEnd` or `SequenceEnd` immediately
    /// after emitting an orphan key. In that case the end marker belongs to
    /// the surrounding collection and must remain buffered for the outer loop.
    pub(super) fn parse_mapping_value_with_metadata(&mut self) -> Option<ParsedNode<'input>> {
        match self.peek() {
            Some(AstEvent::Event(Event::InvalidatePair { .. })) => {
                self.advance();
                None
            }
            Some(
                AstEvent::Event(Event::MappingEnd { .. } | Event::SequenceEnd { .. })
                | AstEvent::RichEvent {
                    event: Event::MappingEnd { .. } | Event::SequenceEnd { .. },
                    ..
                },
            ) => None,
            _ => self.parse_node_with_metadata(),
        }
    }

    pub(super) fn parse_node_from_ast_event(
        &mut self,
        event: AstEvent<'input>,
    ) -> Option<ParsedNode<'input>> {
        match event {
            AstEvent::Event(inner_event) => self.parse_node_from_event(inner_event, None, None),
            AstEvent::SequenceItem {
                event: inner_event,
                leading_comment,
                trailing_comment,
                ..
            }
            | AstEvent::RichEvent {
                event: inner_event,
                leading_comment,
                trailing_comment,
            }
            | AstEvent::MappingKey {
                key_event: inner_event,
                leading_comment,
                trailing_comment,
                ..
            } => self.parse_node_from_event(inner_event, leading_comment, trailing_comment),
        }
    }

    pub(super) fn parse_node_from_event(
        &mut self,
        event: Event<'input>,
        leading_comment: Option<Comment<'input>>,
        trailing_comment: Option<Comment<'input>>,
    ) -> Option<ParsedNode<'input>> {
        match event {
            Event::MappingStart {
                properties, span, ..
            } => Some(ParsedNode {
                node: self.parse_mapping(properties, span),
                leading_comment,
            }),
            Event::SequenceStart {
                properties, span, ..
            } => Some(ParsedNode {
                node: self.parse_sequence(properties, span),
                leading_comment,
            }),
            Event::Scalar {
                style,
                value,
                properties,
                span,
            } => {
                let mut node = self.build_scalar(style, value, properties, span);
                if let Some(comment) = trailing_comment {
                    node = node.with_trailing_comment(comment);
                }
                Some(ParsedNode {
                    node,
                    leading_comment,
                })
            }
            Event::Alias { name, span } => self.build_alias(&name, span).map(|mut node| {
                if let Some(comment) = trailing_comment {
                    node = node.with_trailing_comment(comment);
                }
                ParsedNode {
                    node,
                    leading_comment,
                }
            }),
            // Skip document markers, stream markers
            Event::StreamStart
            | Event::StreamEnd
            | Event::DocumentStart { .. }
            | Event::DocumentEnd { .. }
            | Event::MappingEnd { .. }
            | Event::SequenceEnd { .. }
            | Event::InvalidatePair { .. } => None,
        }
    }
}
