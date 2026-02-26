// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML value types with span information.
//!
//! This module implements a proper YAML AST where node properties (anchor, tag)
//! are separate from node content. In YAML, anchors and tags are properties that
//! can be attached to any node, not separate node types.
//!
//! # Zero-Copy Design
//!
//! The `Node` and `Value` types use `Cow<'input, str>` for string content,
//! allowing zero-copy parsing when possible. String content that can be
//! borrowed directly from the input (plain scalars, simple quoted strings)
//! avoids allocation. Content that requires transformation (escape sequences,
//! multiline folding) uses owned strings.
//!
//! Use [`Node::into_owned()`] or [`Value::into_owned()`] to convert to
//! `'static` lifetime when you need to store values beyond the input's lifetime.

use std::borrow::Cow;

use crate::span::Span;

/// A YAML node with optional properties (anchor, tag) and a value.
///
/// This properly represents YAML's structure where anchors and tags are
/// node properties, not value wrappers. For example, in `&anchor key: value`,
/// the anchor attaches to the scalar `key`, which is then used as a mapping key.
///
/// The lifetime `'input` refers to the input string being parsed. String content
/// uses `Cow<'input, str>` for zero-copy when possible.
#[derive(Debug, Clone, PartialEq)]
pub struct Node<'input> {
    /// Optional anchor name (from `&name`)
    pub anchor: Option<Cow<'input, str>>,
    /// Optional tag (from `!tag`)
    pub tag: Option<Cow<'input, str>>,
    /// The node's value
    pub value: Value<'input>,
    /// Source span covering the entire node (including properties)
    pub span: Span,
}

impl<'input> Node<'input> {
    /// Create a new node with just a value and span (no properties).
    #[must_use]
    pub fn new(value: Value<'input>, span: Span) -> Self {
        Self {
            anchor: None,
            tag: None,
            value,
            span,
        }
    }

    /// Create a new node with an anchor.
    #[must_use]
    pub fn with_anchor(mut self, anchor: Cow<'input, str>) -> Self {
        self.anchor = Some(anchor);
        self
    }

    /// Create a new node with a tag.
    #[must_use]
    pub fn with_tag(mut self, tag: Cow<'input, str>) -> Self {
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

    /// Convert this node to an owned version with `'static` lifetime.
    ///
    /// This is useful when you need to store the node beyond the input's lifetime.
    #[must_use]
    pub fn into_owned(self) -> Node<'static> {
        Node {
            anchor: self.anchor.map(|cow| Cow::Owned(cow.into_owned())),
            tag: self.tag.map(|cow| Cow::Owned(cow.into_owned())),
            value: self.value.into_owned(),
            span: self.span,
        }
    }
}

/// The core YAML value types.
///
/// This represents the actual content of a YAML node, separate from
/// node properties like anchors and tags.
///
/// The lifetime `'input` refers to the input string being parsed. String content
/// uses `Cow<'input, str>` for zero-copy when possible.
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'input> {
    /// A null value (`null`, `~`, or empty)
    Null,

    /// A boolean value (`true` or `false`)
    Bool(bool),

    /// An integer value
    Int(i64),

    /// A floating-point value
    Float(f64),

    /// A string value (quoted or unquoted)
    ///
    /// Uses `Cow` for zero-copy: plain scalars and simple quoted strings
    /// borrow from input, while escaped/multiline content is owned.
    String(Cow<'input, str>),

    /// A sequence (array/list)
    Sequence(Vec<Node<'input>>),

    /// A mapping (object/dictionary)
    Mapping(Vec<(Node<'input>, Node<'input>)>),

    /// An alias reference (`*name`)
    ///
    /// Note: Aliases don't have their own anchor/tag properties in YAML.
    Alias(Cow<'input, str>),

    /// An invalid node (placeholder for error recovery)
    ///
    /// When the parser encounters an error and recovers, it may insert
    /// this node to represent the invalid portion of the input.
    Invalid,
}

impl Value<'_> {
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

    /// Convert this value to an owned version with `'static` lifetime.
    ///
    /// This is useful when you need to store the value beyond the input's lifetime.
    #[must_use]
    pub fn into_owned(self) -> Value<'static> {
        match self {
            Self::Null => Value::Null,
            Self::Bool(val) => Value::Bool(val),
            Self::Int(val) => Value::Int(val),
            Self::Float(val) => Value::Float(val),
            Self::String(cow) => Value::String(Cow::Owned(cow.into_owned())),
            Self::Sequence(seq) => Value::Sequence(seq.into_iter().map(Node::into_owned).collect()),
            Self::Mapping(map) => Value::Mapping(
                map.into_iter()
                    .map(|(key, val)| (key.into_owned(), val.into_owned()))
                    .collect(),
            ),
            Self::Alias(cow) => Value::Alias(Cow::Owned(cow.into_owned())),
            Self::Invalid => Value::Invalid,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    #[test]
    fn test_value_types() {
        assert!(Value::Null.is_null());
        assert!(Value::Null.is_scalar());
        assert!(!Value::<'_>::Null.is_collection());

        assert!(Value::Bool(true).is_scalar());
        assert!(Value::Int(42).is_scalar());
        assert!(Value::Float(1.5).is_scalar());
        assert!(Value::String(Cow::Borrowed("hello")).is_scalar());

        assert!(Value::<'_>::Sequence(vec![]).is_collection());
        assert!(Value::<'_>::Mapping(vec![]).is_collection());

        assert!(Value::<'_>::Invalid.is_invalid());
    }

    #[test]
    fn test_node_construction() {
        let span = Span::new(0..4);

        // Basic node
        let node1 = Node::new(Value::String(Cow::Borrowed("test")), span);
        assert!(!node1.has_anchor());
        assert!(!node1.has_tag());

        // Node with anchor
        let node2 = Node::new(Value::String(Cow::Borrowed("test")), span)
            .with_anchor(Cow::Borrowed("myanchor"));
        assert!(node2.has_anchor());
        assert_eq!(node2.anchor, Some(Cow::Borrowed("myanchor")));

        // Node with tag
        let node3 =
            Node::new(Value::String(Cow::Borrowed("test")), span).with_tag(Cow::Borrowed("str"));
        assert!(node3.has_tag());
        assert_eq!(node3.tag, Some(Cow::Borrowed("str")));

        // Null node
        let node4 = Node::null(span);
        assert!(node4.value.is_null());

        // Invalid node
        let node5 = Node::invalid(span);
        assert!(node5.value.is_invalid());
    }

    #[test]
    fn test_into_owned() {
        let span = Span::new(0..4);

        // Test Value::into_owned
        let borrowed: Value<'_> = Value::String(Cow::Borrowed("test"));
        let owned: Value<'static> = borrowed.into_owned();
        assert!(matches!(owned, Value::String(Cow::Owned(str)) if str == "test"));

        // Test Node::into_owned
        let node = Node::new(Value::String(Cow::Borrowed("test")), span)
            .with_anchor(Cow::Borrowed("anchor"))
            .with_tag(Cow::Borrowed("tag"));
        let owned_node: Node<'static> = node.into_owned();
        assert!(matches!(owned_node.anchor, Some(Cow::Owned(str)) if str == "anchor"));
        assert!(matches!(owned_node.tag, Some(Cow::Owned(str)) if str == "tag"));
        assert!(matches!(owned_node.value, Value::String(Cow::Owned(str)) if str == "test"));
    }
}
