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
use std::ops::{Deref, DerefMut};

pub use crate::event::{Comment, Properties, Property};
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
    use super::{Integer, MappingPair, Node, SequenceItem, Value};
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
                    for item in items {
                        seq.serialize_element(&item.node.value)?;
                    }
                    seq.end()
                }
                Value::Mapping(pairs) => {
                    let mut map = serializer.serialize_map(Some(pairs.len()))?;
                    for pair in pairs {
                        map.serialize_entry(&pair.key.value, &pair.value.value)?;
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
                    trailing_comment: None,
                };
                items.push(SequenceItem::new(Span::default(), node));
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
                    trailing_comment: None,
                };
                let value_node = Node {
                    properties: None,
                    value,
                    span: Span::default(),
                    trailing_comment: None,
                };
                pairs.push(MappingPair::new(Span::default(), key_node, value_node));
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
                trailing_comment: None,
            };
            let value_node = Node {
                properties: None,
                value,
                span: Span::default(),
                trailing_comment: None,
            };
            Ok(Value::Mapping(vec![MappingPair::new(
                Span::default(),
                key_node,
                value_node,
            )]))
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
    /// Source span covering the semantic value of this node.
    pub span: Span,
    /// Same-line comment that trails this node's source representation.
    pub trailing_comment: Option<Box<Comment<'input>>>,
}

impl<'input> Node<'input> {
    /// Create a new node with just a value and span (no properties).
    #[must_use]
    pub fn new(value: Value<'input>, span: Span) -> Self {
        Self {
            properties: None,
            value,
            span,
            trailing_comment: None,
        }
    }

    /// Create a new node with an anchor.
    #[must_use]
    pub fn with_anchor(mut self, anchor: &'input str) -> Self {
        match &mut self.properties {
            Some(props) => props.anchor = Some(Property::new(Cow::Borrowed(anchor), self.span)),
            None => {
                self.properties = Some(Box::new(Properties::with_anchor(Property::new(
                    Cow::Borrowed(anchor),
                    self.span,
                ))));
            }
        }
        self
    }

    /// Create a new node with a tag.
    #[must_use]
    pub fn with_tag(mut self, tag: Cow<'input, str>) -> Self {
        match &mut self.properties {
            Some(props) => props.tag = Some(Property::new(tag, self.span)),
            None => {
                self.properties = Some(Box::new(Properties::with_tag(Property::new(
                    tag, self.span,
                ))));
            }
        }
        self
    }

    /// Returns all properties, if present.
    #[must_use]
    pub fn properties(&self) -> Option<&Properties<'input>> {
        self.properties.as_deref()
    }

    /// Returns the anchor property if present.
    #[must_use]
    pub fn anchor_property(&self) -> Option<&Property<'input>> {
        self.properties()
            .and_then(|properties| properties.anchor.as_ref())
    }

    /// Returns the tag property if present.
    #[must_use]
    pub fn tag_property(&self) -> Option<&Property<'input>> {
        self.properties()
            .and_then(|properties| properties.tag.as_ref())
    }

    /// Create a new node with a trailing same-line comment.
    #[must_use]
    pub fn with_trailing_comment(mut self, comment: Comment<'input>) -> Self {
        self.trailing_comment = Some(Box::new(comment));
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
        self.anchor_property()
            .map(|property| property.value.as_ref())
    }

    /// Returns the tag if present.
    #[must_use]
    pub fn tag(&self) -> Option<&Property<'input>> {
        self.tag_property()
    }

    /// Returns `true` if this node has an anchor.
    #[must_use]
    pub fn has_anchor(&self) -> bool {
        self.anchor_property().is_some()
    }

    /// Returns `true` if this node has a tag.
    #[must_use]
    pub fn has_tag(&self) -> bool {
        self.tag_property().is_some()
    }

    /// Returns the trailing same-line comment if present.
    #[must_use]
    pub fn trailing_comment(&self) -> Option<&Comment<'input>> {
        self.trailing_comment.as_deref()
    }

    /// Convert this node to an owned version with `'static` lifetime.
    ///
    /// This is useful when you need to store the node beyond the input's lifetime.
    #[must_use]
    pub fn into_owned(self) -> Node<'static> {
        Node {
            properties: self.properties.map(|props| Box::new((*props).into_owned())),
            value: self.value.into_owned(),
            span: self.span,
            trailing_comment: self
                .trailing_comment
                .map(|comment| Box::new((*comment).into_owned())),
        }
    }
}

/// A sequence item with both structural and semantic spans.
#[derive(Debug, Clone, PartialEq)]
pub struct SequenceItem<'input> {
    /// Source span covering the full sequence entry, including `-` in block style.
    pub item_span: Span,
    /// The semantic node value for this entry.
    pub node: Node<'input>,
    /// Same-line comment after `-` before a nested block value starts.
    pub header_comment: Option<Box<Comment<'input>>>,
}

impl<'input> SequenceItem<'input> {
    /// Create a new sequence item.
    #[must_use]
    pub fn new(item_span: Span, node: Node<'input>) -> Self {
        Self {
            item_span,
            node,
            header_comment: None,
        }
    }

    /// Borrow the semantic node for this item.
    #[must_use]
    pub const fn as_node(&self) -> &Node<'input> {
        &self.node
    }

    /// Create a new sequence item with a header-line comment.
    #[must_use]
    pub fn with_header_comment(mut self, comment: Comment<'input>) -> Self {
        self.header_comment = Some(Box::new(comment));
        self
    }

    /// Returns the header-line comment if present.
    #[must_use]
    pub fn header_comment(&self) -> Option<&Comment<'input>> {
        self.header_comment.as_deref()
    }

    /// Convert this item to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> SequenceItem<'static> {
        SequenceItem {
            item_span: self.item_span,
            node: self.node.into_owned(),
            header_comment: self
                .header_comment
                .map(|comment| Box::new((*comment).into_owned())),
        }
    }
}

impl<'input> Deref for SequenceItem<'input> {
    type Target = Node<'input>;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl DerefMut for SequenceItem<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

/// A mapping pair with both structural and semantic spans.
#[derive(Debug, Clone, PartialEq)]
pub struct MappingPair<'input> {
    /// Source span covering the full mapping entry.
    pub pair_span: Span,
    /// The semantic key node.
    pub key: Node<'input>,
    /// The semantic value node.
    pub value: Node<'input>,
    /// Same-line comment after `key:` before a nested block value starts.
    pub header_comment: Option<Box<Comment<'input>>>,
}

impl<'input> MappingPair<'input> {
    /// Create a new mapping pair.
    #[must_use]
    pub fn new(pair_span: Span, key: Node<'input>, value: Node<'input>) -> Self {
        Self {
            pair_span,
            key,
            value,
            header_comment: None,
        }
    }

    /// Borrow the semantic key node.
    #[must_use]
    pub const fn key(&self) -> &Node<'input> {
        &self.key
    }

    /// Borrow the semantic value node.
    #[must_use]
    pub const fn value(&self) -> &Node<'input> {
        &self.value
    }

    /// Create a new mapping pair with a header-line comment.
    #[must_use]
    pub fn with_header_comment(mut self, comment: Comment<'input>) -> Self {
        self.header_comment = Some(Box::new(comment));
        self
    }

    /// Returns the header-line comment if present.
    #[must_use]
    pub fn header_comment(&self) -> Option<&Comment<'input>> {
        self.header_comment.as_deref()
    }

    /// Convert this pair to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> MappingPair<'static> {
        MappingPair {
            pair_span: self.pair_span,
            key: self.key.into_owned(),
            value: self.value.into_owned(),
            header_comment: self
                .header_comment
                .map(|comment| Box::new((*comment).into_owned())),
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
    Sequence(Vec<SequenceItem<'input>>),

    /// A mapping (object/dictionary)
    Mapping(Vec<MappingPair<'input>>),

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
            Self::Sequence(seq) => {
                Value::Sequence(seq.into_iter().map(SequenceItem::into_owned).collect())
            }
            Self::Mapping(map) => {
                Value::Mapping(map.into_iter().map(MappingPair::into_owned).collect())
            }
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
        assert_eq!(
            node2.anchor_property().map(|property| property.span),
            Some(span)
        );

        // Node with tag
        let node3 =
            Node::new(Value::String(Cow::Borrowed("test")), span).with_tag(Cow::Borrowed("str"));
        assert!(node3.has_tag());
        assert_eq!(
            node3.tag().map(|property| property.value.as_ref()),
            Some("str")
        );
        assert_eq!(
            node3.tag_property().map(|property| property.span),
            Some(span)
        );

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
        assert!(matches!(
            owned_node.tag(),
            Some(property) if matches!(&property.value, Cow::Owned(str) if str == "tag")
        ));
        assert!(matches!(owned_node.value, Value::String(Cow::Owned(str)) if str == "test"));
    }
}
