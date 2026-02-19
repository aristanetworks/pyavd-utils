// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML value types with span information.
//!
//! This module implements a proper YAML AST where node properties (anchor, tag)
//! are separate from node content. In YAML, anchors and tags are properties that
//! can be attached to any node, not separate node types.

use crate::span::Span;

/// A YAML node with optional properties (anchor, tag) and a value.
///
/// This properly represents YAML's structure where anchors and tags are
/// node properties, not value wrappers. For example, in `&anchor key: value`,
/// the anchor attaches to the scalar `key`, which is then used as a mapping key.
#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    /// Optional anchor name (from `&name`)
    pub anchor: Option<String>,
    /// Optional tag (from `!tag`)
    pub tag: Option<String>,
    /// The node's value
    pub value: Value,
    /// Source span covering the entire node (including properties)
    pub span: Span,
}

impl Node {
    /// Create a new node with just a value and span (no properties).
    #[must_use]
    pub fn new(value: Value, span: Span) -> Self {
        Self {
            anchor: None,
            tag: None,
            value,
            span,
        }
    }

    /// Create a new node with an anchor.
    #[must_use]
    pub fn with_anchor(mut self, anchor: String) -> Self {
        self.anchor = Some(anchor);
        self
    }

    /// Create a new node with a tag.
    #[must_use]
    pub fn with_tag(mut self, tag: String) -> Self {
        self.tag = Some(tag);
        self
    }

    /// Create a null node.
    #[must_use]
    pub fn null(span: Span) -> Self {
        Self::new(Value::Null, span)
    }

    /// Create an invalid node (for error recovery).
    #[must_use]
    pub fn invalid(span: Span) -> Self {
        Self::new(Value::Invalid, span)
    }

    /// Returns `true` if this node has an anchor.
    #[must_use]
    pub fn has_anchor(&self) -> bool {
        self.anchor.is_some()
    }

    /// Returns `true` if this node has a tag.
    #[must_use]
    pub fn has_tag(&self) -> bool {
        self.tag.is_some()
    }
}

/// The core YAML value types.
///
/// This represents the actual content of a YAML node, separate from
/// node properties like anchors and tags.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A null value (`null`, `~`, or empty)
    Null,

    /// A boolean value (`true` or `false`)
    Bool(bool),

    /// An integer value
    Int(i64),

    /// A floating-point value
    Float(f64),

    /// A string value (quoted or unquoted)
    String(String),

    /// A sequence (array/list)
    Sequence(Vec<Node>),

    /// A mapping (object/dictionary)
    Mapping(Vec<(Node, Node)>),

    /// An alias reference (`*name`)
    ///
    /// Note: Aliases don't have their own anchor/tag properties in YAML.
    Alias(String),

    /// An invalid node (placeholder for error recovery)
    ///
    /// When the parser encounters an error and recovers, it may insert
    /// this node to represent the invalid portion of the input.
    Invalid,
}

impl Value {
    /// Returns `true` if this is a null value.
    #[must_use]
    pub const fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    /// Returns `true` if this is an invalid/error node.
    #[must_use]
    pub const fn is_invalid(&self) -> bool {
        matches!(self, Self::Invalid)
    }

    /// Returns `true` if this is a scalar value (null, bool, int, float, string).
    #[must_use]
    pub const fn is_scalar(&self) -> bool {
        matches!(
            self,
            Self::Null | Self::Bool(_) | Self::Int(_) | Self::Float(_) | Self::String(_)
        )
    }

    /// Returns `true` if this is a collection (sequence or mapping).
    #[must_use]
    pub const fn is_collection(&self) -> bool {
        matches!(self, Self::Sequence(_) | Self::Mapping(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;
    use chumsky::span::Span as _;

    #[test]
    fn test_value_types() {
        assert!(Value::Null.is_null());
        assert!(Value::Null.is_scalar());
        assert!(!Value::Null.is_collection());

        assert!(Value::Bool(true).is_scalar());
        assert!(Value::Int(42).is_scalar());
        assert!(Value::Float(1.5).is_scalar());
        assert!(Value::String("hello".into()).is_scalar());

        assert!(Value::Sequence(vec![]).is_collection());
        assert!(Value::Mapping(vec![]).is_collection());

        assert!(Value::Invalid.is_invalid());
    }

    #[test]
    fn test_node_construction() {
        let span = Span::new((), 0..4);

        // Basic node
        let node = Node::new(Value::String("test".into()), span);
        assert!(!node.has_anchor());
        assert!(!node.has_tag());

        // Node with anchor
        let node = Node::new(Value::String("test".into()), span).with_anchor("myanchor".into());
        assert!(node.has_anchor());
        assert_eq!(node.anchor, Some("myanchor".into()));

        // Node with tag
        let node = Node::new(Value::String("test".into()), span).with_tag("str".into());
        assert!(node.has_tag());
        assert_eq!(node.tag, Some("str".into()));

        // Null node
        let node = Node::null(span);
        assert!(node.value.is_null());

        // Invalid node
        let node = Node::invalid(span);
        assert!(node.value.is_invalid());
    }
}
