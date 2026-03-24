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

/// Integer value representation used by `Value::Int`.
///
/// This allows representing a wide range of integer values while still
/// preserving the original textual representation for very large numbers.
#[derive(Debug, Clone, PartialEq)]
pub enum Integer<'input> {
    /// Negative / positive integers that fit in `i64`.
    I64(i64),
    /// Non-negative integers that fit in `u64`.
    U64(u64),
    /// Larger negative integers.
    I128(i128),
    /// Larger non-negative integers.
    U128(u128),
    /// Decimal integer that did not fit in the above, stored as text.
    BigIntStr(Cow<'input, str>),
}

impl Integer<'_> {
    /// Convert this number to an owned `'static` variant.
    #[must_use]
    pub fn into_owned(self) -> Integer<'static> {
        match self {
            Integer::I64(value) => Integer::I64(value),
            Integer::U64(value) => Integer::U64(value),
            Integer::I128(value) => Integer::I128(value),
            Integer::U128(value) => Integer::U128(value),
            Integer::BigIntStr(text) => Integer::BigIntStr(Cow::Owned(text.into_owned())),
        }
    }

    /// Return the decimal string representation of this number.
    #[must_use]
    pub fn to_decimal_string(&self) -> Cow<'_, str> {
        match self {
            Integer::I64(value) => Cow::Owned(value.to_string()),
            Integer::U64(value) => Cow::Owned(value.to_string()),
            Integer::I128(value) => Cow::Owned(value.to_string()),
            Integer::U128(value) => Cow::Owned(value.to_string()),
            Integer::BigIntStr(text) => Cow::Borrowed(text.as_ref()),
        }
    }
}

#[cfg(feature = "serde")]
mod serde_impls {
    use super::{Integer, Node, Value};
    use crate::span::Span;
    use serde::de::{self, Deserialize, Deserializer, MapAccess, SeqAccess, Visitor};
    use serde::ser::{Serialize, SerializeMap as _, SerializeSeq as _, Serializer};
    use std::fmt;

    impl Serialize for Value<'_> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self {
                Value::Null => serializer.serialize_unit(),
                Value::Bool(bool_value) => serializer.serialize_bool(*bool_value),
                Value::Int(number) => match number {
                    Integer::I64(i64_value) => serializer.serialize_i64(*i64_value),
                    Integer::U64(u64_value) => serializer.serialize_u64(*u64_value),
                    Integer::I128(i128_value) => serializer.serialize_i128(*i128_value),
                    Integer::U128(u128_value) => serializer.serialize_u128(*u128_value),
                    // Fall back to string for very large integers.
                    Integer::BigIntStr(text) => serializer.serialize_str(text.as_ref()),
                },
                Value::Float(float_value) => serializer.serialize_f64(*float_value),
                Value::String(string_value) => serializer.serialize_str(string_value.as_ref()),
                Value::Sequence(items) => {
                    let mut seq = serializer.serialize_seq(Some(items.len()))?;
                    for node in items {
                        seq.serialize_element(&node.value)?;
                    }
                    seq.end()
                }
                Value::Mapping(pairs) => {
                    let mut map = serializer.serialize_map(Some(pairs.len()))?;
                    for (key_node, value_node) in pairs {
                        map.serialize_entry(&key_node.value, &value_node.value)?;
                    }
                    map.end()
                }
                // For generic serde, aliases are represented as plain strings.
                Value::Alias(name) => serializer.serialize_str(name.as_ref()),
            }
        }
    }

    struct ValueVisitor;

    impl<'de> Visitor<'de> for ValueVisitor {
        type Value = Value<'de>;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("any valid serde value")
        }

        fn visit_unit<E>(self) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::Null)
        }

        fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::Null)
        }

        fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
        where
            D: Deserializer<'de>,
        {
            Value::deserialize(deserializer)
        }

        fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::Bool(v))
        }

        fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::Int(Integer::I64(v)))
        }

        fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::Int(Integer::I128(v)))
        }

        fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::Int(Integer::U64(v)))
        }

        fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::Int(Integer::U128(v)))
        }

        fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::Float(v))
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::String(std::borrow::Cow::Owned(v.to_owned())))
        }

        fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            // Zero-copy: borrow from input instead of allocating
            Ok(Value::String(std::borrow::Cow::Borrowed(v)))
        }

        fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Value::String(std::borrow::Cow::Owned(v)))
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut items = Vec::new();
            while let Some(elem) = seq.next_element::<Value<'de>>()? {
                let node = Node {
                    properties: None,
                    value: elem,
                    span: Span::default(),
                };
                items.push(node);
            }
            Ok(Value::Sequence(items))
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let mut pairs = Vec::new();
            while let Some((key, value)) = map.next_entry::<Value<'de>, Value<'de>>()? {
                let key_node = Node {
                    properties: None,
                    value: key,
                    span: Span::default(),
                };
                let value_node = Node {
                    properties: None,
                    value,
                    span: Span::default(),
                };
                pairs.push((key_node, value_node));
            }
            Ok(Value::Mapping(pairs))
        }

        fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
        where
            A: de::EnumAccess<'de>,
        {
            use de::VariantAccess as _;

            // Represent enums as a single-entry mapping from variant name to
            // the associated value (or null for unit variants). This matches
            // the general "YAML-ish" data model while keeping the
            // implementation simple and generic over the underlying
            // `Deserializer`.
            let (variant, access) = data.variant::<String>()?;

            // Try to deserialize a newtype variant into `Value<'de>`; this
            // covers the common case for tagged scalars like timestamps.
            let value: Value<'de> = access.newtype_variant::<Value<'de>>()?;

            let key_node = Node {
                properties: None,
                value: Value::String(std::borrow::Cow::Owned(variant)),
                span: Span::default(),
            };
            let value_node = Node {
                properties: None,
                value,
                span: Span::default(),
            };
            Ok(Value::Mapping(vec![(key_node, value_node)]))
        }
    }

    impl<'de> Deserialize<'de> for Value<'de> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_any(ValueVisitor)
        }
    }
}

/// Optional properties (anchor, tag) for a YAML node.
///
/// This is boxed within `Node` to keep the common no-properties case compact,
/// since most nodes do not carry anchors or tags.
///
/// Anchors use `Cow<'input, str>` and are usually borrowed from the input.
/// Tags also use `Cow` because they may be expanded or normalized.
#[derive(Debug, Clone, PartialEq)]
pub struct Properties<'input> {
    /// Optional anchor name (from `&name`) - usually borrowed from input
    pub anchor: Option<Cow<'input, str>>,
    /// Optional tag (from `!tag`) - may need transformation
    pub tag: Option<Cow<'input, str>>,
}

impl<'input> Properties<'input> {
    /// Create properties with just an anchor.
    #[must_use]
    pub fn with_anchor(anchor: Cow<'input, str>) -> Self {
        Self {
            anchor: Some(anchor),
            tag: None,
        }
    }

    /// Create properties with just a tag.
    #[must_use]
    pub fn with_tag(tag: Cow<'input, str>) -> Self {
        Self {
            anchor: None,
            tag: Some(tag),
        }
    }

    /// Convert to owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> Properties<'static> {
        Properties {
            anchor: self.anchor.map(|cow| Cow::Owned(cow.into_owned())),
            tag: self.tag.map(|cow| Cow::Owned(cow.into_owned())),
        }
    }
}

/// A YAML node with optional properties (anchor, tag) and a value.
///
/// This properly represents YAML's structure where anchors and tags are
/// node properties, not value wrappers. For example, in `&anchor key: value`,
/// the anchor attaches to the scalar `key`, which is then used as a mapping key.
///
/// The lifetime `'input` refers to the input string being parsed. String content
/// uses `Cow<'input, str>` for zero-copy when possible.
///
/// Properties (anchor, tag) are boxed so nodes without properties stay small,
/// trading an occasional heap allocation for a more compact base node layout.
#[derive(Debug, Clone, PartialEq)]
pub struct Node<'input> {
    /// Optional properties (anchor, tag) - boxed to reduce node size
    pub properties: Option<Box<Properties<'input>>>,
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
            properties: None,
            value,
            span,
        }
    }

    /// Create a new node with an anchor.
    #[must_use]
    pub fn with_anchor(mut self, anchor: &'input str) -> Self {
        match &mut self.properties {
            Some(props) => props.anchor = Some(Cow::Borrowed(anchor)),
            None => {
                self.properties = Some(Box::new(Properties::with_anchor(Cow::Borrowed(anchor))));
            }
        }
        self
    }

    /// Create a new node with a tag.
    #[must_use]
    pub fn with_tag(mut self, tag: Cow<'input, str>) -> Self {
        match &mut self.properties {
            Some(props) => props.tag = Some(tag),
            None => self.properties = Some(Box::new(Properties::with_tag(tag))),
        }
        self
    }

    /// Create a null node.
    #[must_use]
    pub fn null(span: Span) -> Self {
        Self::new(Value::Null, span)
    }

    /// Returns the anchor if present.
    #[must_use]
    pub fn anchor(&self) -> Option<&str> {
        self.properties
            .as_ref()
            .and_then(|props| props.anchor.as_deref())
    }

    /// Returns the tag if present.
    #[must_use]
    pub fn tag(&self) -> Option<&Cow<'input, str>> {
        self.properties
            .as_ref()
            .and_then(|props| props.tag.as_ref())
    }

    /// Returns `true` if this node has an anchor.
    #[must_use]
    pub fn has_anchor(&self) -> bool {
        self.properties
            .as_ref()
            .is_some_and(|props| props.anchor.is_some())
    }

    /// Returns `true` if this node has a tag.
    #[must_use]
    pub fn has_tag(&self) -> bool {
        self.properties
            .as_ref()
            .is_some_and(|props| props.tag.is_some())
    }

    /// Convert this node to an owned version with `'static` lifetime.
    ///
    /// This is useful when you need to store the node beyond the input's lifetime.
    #[must_use]
    pub fn into_owned(self) -> Node<'static> {
        Node {
            properties: self.properties.map(|props| Box::new(props.into_owned())),
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

    /// An integer value represented using the flexible `Integer` type.
    Int(Integer<'input>),

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
    /// Uses `Cow` (usually borrowed) for zero-copy while supporting `into_owned()`.
    Alias(Cow<'input, str>),
}

impl Value<'_> {
    /// Returns `true` if this is a null value.
    #[must_use]
    pub const fn is_null(&self) -> bool {
        matches!(self, Self::Null)
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
            Self::Int(number) => Value::Int(number.into_owned()),
            Self::Float(val) => Value::Float(val),
            Self::String(cow) => Value::String(Cow::Owned(cow.into_owned())),
            Self::Sequence(seq) => Value::Sequence(seq.into_iter().map(Node::into_owned).collect()),
            Self::Mapping(map) => Value::Mapping(
                map.into_iter()
                    .map(|(key, val)| (key.into_owned(), val.into_owned()))
                    .collect(),
            ),
            Self::Alias(cow) => Value::Alias(Cow::Owned(cow.into_owned())),
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
        assert!(Value::Int(Integer::I64(42)).is_scalar());
        assert!(Value::Float(1.5).is_scalar());
        assert!(Value::String(Cow::Borrowed("hello")).is_scalar());

        assert!(Value::<'_>::Sequence(vec![]).is_collection());
        assert!(Value::<'_>::Mapping(vec![]).is_collection());
    }

    #[test]
    fn test_node_construction() {
        let span = Span::from_usize_range(0..4);

        // Basic node
        let node1 = Node::new(Value::String(Cow::Borrowed("test")), span);
        assert!(!node1.has_anchor());
        assert!(!node1.has_tag());

        // Node with anchor (now takes &str directly)
        let node2 = Node::new(Value::String(Cow::Borrowed("test")), span).with_anchor("myanchor");
        assert!(node2.has_anchor());
        assert_eq!(node2.anchor(), Some("myanchor"));

        // Node with tag
        let node3 =
            Node::new(Value::String(Cow::Borrowed("test")), span).with_tag(Cow::Borrowed("str"));
        assert!(node3.has_tag());
        assert_eq!(node3.tag(), Some(&Cow::Borrowed("str")));

        // Null node
        let node4 = Node::null(span);
        assert!(node4.value.is_null());
    }

    #[test]
    fn test_into_owned() {
        let span = Span::from_usize_range(0..4);

        // Test Value::into_owned
        let borrowed: Value<'_> = Value::String(Cow::Borrowed("test"));
        let owned: Value<'static> = borrowed.into_owned();
        assert!(matches!(owned, Value::String(Cow::Owned(str)) if str == "test"));

        // Test Value::Alias into_owned
        let alias: Value<'_> = Value::Alias(Cow::Borrowed("myalias"));
        let owned_alias: Value<'static> = alias.into_owned();
        assert!(matches!(owned_alias, Value::Alias(Cow::Owned(str)) if str == "myalias"));

        // Test Node::into_owned - anchors and tags are converted to owned
        let node = Node::new(Value::String(Cow::Borrowed("test")), span)
            .with_anchor("anchor")
            .with_tag(Cow::Borrowed("tag"));
        let owned_node: Node<'static> = node.into_owned();
        // Anchor is preserved after into_owned
        assert_eq!(owned_node.anchor(), Some("anchor"));
        assert!(matches!(owned_node.tag(), Some(Cow::Owned(str)) if str == "tag"));
        assert!(matches!(owned_node.value, Value::String(Cow::Owned(str)) if str == "test"));
    }
}
