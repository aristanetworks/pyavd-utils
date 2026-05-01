// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::ast_event::AstEvent;
use crate::error::ErrorKind;
use crate::event::Property as EventProperty;
use crate::span::Span;
use crate::value::{Node, Properties as NodeProperties};

use super::Parser;

impl<'input, I> Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Build a resolved alias node.
    /// TODO: Reconsider storage model of the anchors to avoid the clone on consumption.
    ///       But consider that the cost is fairly low because of Cow.
    pub(super) fn build_alias(
        &mut self,
        name: &Cow<'input, str>,
        span: Span,
    ) -> Option<Node<'input>> {
        if let Some(node) = self.anchor_nodes.get(name.as_ref()) {
            return Some(node.clone().into_resolved_alias_root(span));
        }

        if self.anchors.contains(name.as_ref()) {
            self.error(ErrorKind::UndefinedAlias, span);
        }

        None
    }

    /// Register an anchor in the anchor tracking set.
    pub(super) fn register_anchor(&mut self, anchor: Option<&EventProperty<'input>>) {
        if let Some(prop) = anchor {
            // Use as_ref() to avoid cloning if already owned, or convert borrowed to owned
            self.anchors.insert(prop.value.as_ref().to_owned());
        }
    }

    /// Store a completed anchored node for later alias resolution.
    pub(super) fn store_anchor_node(&mut self, node: &Node<'input>) {
        if let Some(anchor) = node.anchor() {
            self.anchor_nodes.insert(anchor.to_owned(), node.clone());
        }
    }

    /// Apply anchor and tag properties to a node.
    ///
    /// Note: We intentionally do NOT extend the node span to include property spans.
    /// The node span should cover only the value itself, so that span-based extraction
    /// (used by tests) returns just the value text without anchor/tag syntax.
    pub(super) fn apply_properties(
        mut node: Node<'input>,
        props: Option<Box<NodeProperties<'input>>>,
    ) -> Node<'input> {
        node.properties = props;
        node
    }
}
