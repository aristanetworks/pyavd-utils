// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Post-validation consolidation of owned, coerced data.

use std::borrow::Cow;

use serde_json::Map;
use serde_json::Value as JsonValue;
use yaml_parser::MappingPair;
use yaml_parser::Node;
use yaml_parser::SequenceItem;
use yaml_parser::Span;
use yaml_parser::Value as YamlValue;

use crate::context::Context;

/// An owned value that can be transformed after validation and coercion.
pub trait ConsolidatableValue: Sized {
    /// Mutable mapping representation used by this value type.
    type Mapping: ConsolidatableMapping<Value = Self>;

    /// Mutable sequence representation used by this value type.
    type Sequence: ConsolidatableSequence<Value = Self>;

    /// Return whether this value is null.
    fn is_null(&self) -> bool;

    /// Return the mutable mapping when this value is a mapping.
    fn as_mapping_mut(&mut self) -> Option<&mut Self::Mapping>;

    /// Return the mutable sequence when this value is a sequence.
    fn as_sequence_mut(&mut self) -> Option<&mut Self::Sequence>;

    /// Construct a mapping from string keys and owned values.
    fn mapping(items: Vec<(String, Self)>) -> Self;

    /// Construct a sequence from owned values.
    fn sequence(items: Vec<Self>) -> Self;

    /// Construct a string value.
    fn string(value: String) -> Self;
}

/// Mutable mapping operations required by data consolidation.
pub trait ConsolidatableMapping {
    /// Value stored in the mapping.
    type Value;

    /// Borrow a value mutably by key.
    fn get_mut(&mut self, key: &str) -> Option<&mut Self::Value>;

    /// Remove a key and return its owned value.
    fn take(&mut self, key: &str) -> Option<Self::Value>;

    /// Insert a value, returning the previous value when the key existed.
    fn insert(&mut self, key: String, value: Self::Value) -> Option<Self::Value>;
}

/// Mutable sequence operations required by data consolidation.
pub trait ConsolidatableSequence {
    /// Value stored in the sequence.
    type Value;

    /// Append an owned value.
    fn push(&mut self, value: Self::Value);
}

impl ConsolidatableValue for JsonValue {
    type Mapping = Map<String, JsonValue>;
    type Sequence = Vec<JsonValue>;

    fn is_null(&self) -> bool {
        self.is_null()
    }

    fn as_mapping_mut(&mut self) -> Option<&mut Self::Mapping> {
        self.as_object_mut()
    }

    fn as_sequence_mut(&mut self) -> Option<&mut Self::Sequence> {
        self.as_array_mut()
    }

    fn mapping(items: Vec<(String, Self)>) -> Self {
        Self::Object(items.into_iter().collect())
    }

    fn sequence(items: Vec<Self>) -> Self {
        Self::Array(items)
    }

    fn string(value: String) -> Self {
        Self::String(value)
    }
}

impl ConsolidatableMapping for Map<String, JsonValue> {
    type Value = JsonValue;

    fn get_mut(&mut self, key: &str) -> Option<&mut Self::Value> {
        Map::get_mut(self, key)
    }

    fn take(&mut self, key: &str) -> Option<Self::Value> {
        self.remove(key)
    }

    fn insert(&mut self, key: String, value: Self::Value) -> Option<Self::Value> {
        Map::insert(self, key, value)
    }
}

impl ConsolidatableSequence for Vec<JsonValue> {
    type Value = JsonValue;

    fn push(&mut self, value: Self::Value) {
        Vec::push(self, value);
    }
}

impl ConsolidatableValue for Node<'static> {
    type Mapping = Vec<MappingPair<'static>>;
    type Sequence = Vec<SequenceItem<'static>>;

    fn is_null(&self) -> bool {
        matches!(self.value, YamlValue::Null)
    }

    fn as_mapping_mut(&mut self) -> Option<&mut Self::Mapping> {
        match &mut self.value {
            YamlValue::Mapping(pairs) => Some(pairs),
            _ => None,
        }
    }

    fn as_sequence_mut(&mut self) -> Option<&mut Self::Sequence> {
        match &mut self.value {
            YamlValue::Sequence(items) => Some(items),
            _ => None,
        }
    }

    fn mapping(items: Vec<(String, Self)>) -> Self {
        let pairs = items
            .into_iter()
            .map(|(key, value)| {
                MappingPair::new(
                    Span::default(),
                    Node::new(YamlValue::String(Cow::Owned(key)), Span::default()),
                    value,
                )
            })
            .collect();
        Self::new(YamlValue::Mapping(pairs), Span::default())
    }

    fn sequence(items: Vec<Self>) -> Self {
        let items = items
            .into_iter()
            .map(|node| SequenceItem::new(node.span, node))
            .collect();
        Self::new(YamlValue::Sequence(items), Span::default())
    }

    fn string(value: String) -> Self {
        Self::new(YamlValue::String(Cow::Owned(value)), Span::default())
    }
}

impl ConsolidatableMapping for Vec<MappingPair<'static>> {
    type Value = Node<'static>;

    fn get_mut(&mut self, key: &str) -> Option<&mut Self::Value> {
        self.iter_mut()
            .find(|pair| yaml_key_matches(&pair.key, key))
            .map(|pair| &mut pair.value)
    }

    fn take(&mut self, key: &str) -> Option<Self::Value> {
        let index = self
            .iter()
            .position(|pair| yaml_key_matches(&pair.key, key))?;
        Some(self.remove(index).value)
    }

    fn insert(&mut self, key: String, value: Self::Value) -> Option<Self::Value> {
        if let Some(pair) = self
            .iter_mut()
            .find(|pair| yaml_key_matches(&pair.key, &key))
        {
            return Some(std::mem::replace(&mut pair.value, value));
        }

        self.push(MappingPair::new(
            Span::default(),
            Node::new(YamlValue::String(Cow::Owned(key)), Span::default()),
            value,
        ));
        None
    }
}

impl ConsolidatableSequence for Vec<SequenceItem<'static>> {
    type Value = Node<'static>;

    fn push(&mut self, value: Self::Value) {
        Vec::push(self, SequenceItem::new(value.span, value));
    }
}

fn yaml_key_matches(node: &Node<'_>, key: &str) -> bool {
    matches!(&node.value, YamlValue::String(value) if value == key)
}

/// An invariant required by consolidation was not satisfied after successful validation.
#[derive(Debug, derive_more::Display)]
pub enum DataConsolidationError {
    #[display("Data consolidation requires coerced data to be returned")]
    CoercedDataNotRequested,
    #[display("Unable to consolidate AVD Design data: validated data is not a mapping")]
    InvalidRoot,
    #[display(
        "Unable to consolidate AVD Design dynamic key '{key}': target collection '{collection}' is invalid"
    )]
    InvalidDynamicKeyCollection { key: String, collection: String },
}

/// Apply the consolidation operations selected for a schema while validation metadata is alive.
pub(crate) fn consolidate_data<C: ConsolidatableValue>(
    schema_name: &str,
    data: &mut C,
    ctx: &Context<'_>,
) -> Result<(), DataConsolidationError> {
    if schema_name == "avd_design" {
        consolidate_avd_design_dynamic_keys(data, ctx)?;
    }
    Ok(())
}

fn consolidate_avd_design_dynamic_keys<C: ConsolidatableValue>(
    data: &mut C,
    ctx: &Context<'_>,
) -> Result<(), DataConsolidationError> {
    let root = data
        .as_mapping_mut()
        .ok_or(DataConsolidationError::InvalidRoot)?;

    // These collections are the stable shape consumed by the generated AVD models. The selector
    // paths remain validation metadata and are represented on each item by `source`.
    let mut dynamic_keys = C::mapping(vec![
        ("connected_endpoints".to_owned(), C::sequence(Vec::new())),
        ("network_services".to_owned(), C::sequence(Vec::new())),
        ("node_types".to_owned(), C::sequence(Vec::new())),
    ]);

    for (key, dynamic_key_match) in ctx.dynamic_key_matches.iter().flatten() {
        let Some(source) = dynamic_key_match.source else {
            continue;
        };
        if root.get_mut(key).is_none_or(|value| value.is_null()) {
            continue;
        }
        let Some(value) = root.take(key) else {
            continue;
        };
        let collection_key = source.collection_key();
        let sequence = dynamic_keys
            .as_mapping_mut()
            .and_then(|mapping| mapping.get_mut(collection_key))
            .and_then(ConsolidatableValue::as_sequence_mut)
            .ok_or_else(|| DataConsolidationError::InvalidDynamicKeyCollection {
                key: key.clone(),
                collection: collection_key.to_owned(),
            })?;
        sequence.push(C::mapping(vec![
            ("key".to_owned(), C::string(key.clone())),
            ("source".to_owned(), C::string(source.as_str().to_owned())),
            ("value".to_owned(), value),
        ]));
    }

    root.insert("_dynamic_keys".to_owned(), dynamic_keys);
    Ok(())
}

#[cfg(test)]
mod tests {
    use avdschema::any::AnySchema;
    use avdschema::dict::Dict;
    use avdschema::dict::DynamicKeyInfo;
    use avdschema::dict::DynamicKeySource;
    use ordermap::OrderMap;
    use serde_json::json;
    use yaml_parser::Value;
    use yaml_parser::parse;

    use super::*;
    use crate::validatable::ValidatableValue;
    use crate::validation::test_utils::get_test_store;

    #[test]
    fn dynamic_key_consolidation_supports_all_coerced_value_types() {
        let store = get_test_store();
        let dynamic_schema: AnySchema = Dict::default().into();
        let mut json_context = Context::new(&store, None);
        json_context.dynamic_key_matches = Some(OrderMap::from_iter([
            (
                "leaf".to_owned(),
                DynamicKeyInfo {
                    dynamic_key_path: "node_type_keys.key",
                    schema: &dynamic_schema,
                    source: Some(DynamicKeySource::NodeTypes),
                },
            ),
            (
                "null_leaf".to_owned(),
                DynamicKeyInfo {
                    dynamic_key_path: "node_type_keys.key",
                    schema: &dynamic_schema,
                    source: Some(DynamicKeySource::NodeTypes),
                },
            ),
            (
                "default_only_leaf".to_owned(),
                DynamicKeyInfo {
                    dynamic_key_path: "node_type_keys.key",
                    schema: &dynamic_schema,
                    source: Some(DynamicKeySource::NodeTypes),
                },
            ),
        ]));
        let mut json_data =
            json!({"leaf": {"defaults": {}}, "null_leaf": null, "fabric_name": "FABRIC"});
        let json_leaf = json_data.get("leaf").cloned().unwrap();

        consolidate_data("avd_design", &mut json_data, &json_context).unwrap();

        assert_eq!(
            json_data,
            json!({
                "fabric_name": "FABRIC",
                "null_leaf": null,
                "_dynamic_keys": {
                    "connected_endpoints": [],
                    "network_services": [],
                    "node_types": [{"key": "leaf", "source": "node_types", "value": json_leaf}],
                },
            })
        );

        let (mut documents, errors) = parse("leaf:\n  defaults: {}\nfabric_name: FABRIC\n");
        assert!(errors.is_empty());
        let mut yaml_data = documents.remove(0).into_owned();
        let yaml_leaf = yaml_data.get("leaf").cloned().unwrap();
        let mut yaml_context = Context::new(&store, None);
        yaml_context.dynamic_key_matches = Some(OrderMap::from_iter([(
            "leaf".to_owned(),
            DynamicKeyInfo {
                dynamic_key_path: "node_type_keys.key",
                schema: &dynamic_schema,
                source: Some(DynamicKeySource::NodeTypes),
            },
        )]));

        consolidate_data("avd_design", &mut yaml_data, &yaml_context).unwrap();

        assert!(yaml_data.get("leaf").is_none());
        assert_eq!(
            yaml_data
                .get("fabric_name")
                .and_then(|value| match &value.value {
                    Value::String(value) => Some(value.as_ref()),
                    _ => None,
                }),
            Some("FABRIC")
        );
        let node_types = yaml_data
            .get("_dynamic_keys")
            .and_then(|value| value.get("node_types"))
            .and_then(ValidatableValue::as_sequence)
            .unwrap();
        assert_eq!(node_types.len(), 1);
        assert_eq!(
            node_types[0]
                .get("key")
                .and_then(ValidatableValue::as_str)
                .as_deref(),
            Some("leaf")
        );
        assert_eq!(
            node_types[0]
                .get("source")
                .and_then(ValidatableValue::as_str)
                .as_deref(),
            Some("node_types")
        );
        assert_eq!(node_types[0].get("value"), Some(&yaml_leaf));
    }
}
