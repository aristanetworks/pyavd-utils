// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{SchemaDataMapping, SchemaDataSequence, SchemaDataValue};

use crate::{MappingPair, SequenceItem, Value};

#[derive(Debug, Clone, Copy)]
pub struct YamlMapping<'a, 'input>(&'a [MappingPair<'input>]);

#[derive(Debug, Clone, Copy)]
pub struct YamlSequence<'a, 'input>(&'a [SequenceItem<'input>]);

impl<'a, 'input: 'a> SchemaDataValue<'a> for &'a Value<'input> {
    type Mapping = YamlMapping<'a, 'input>;
    type Sequence = YamlSequence<'a, 'input>;

    fn as_mapping(self) -> Option<Self::Mapping> {
        match self {
            Value::Mapping(pairs) => Some(YamlMapping(pairs.as_slice())),
            _ => None,
        }
    }

    fn as_sequence(self) -> Option<Self::Sequence> {
        match self {
            Value::Sequence(items) => Some(YamlSequence(items.as_slice())),
            _ => None,
        }
    }

    fn as_str(self) -> Option<&'a str> {
        match self {
            Value::String(text) => Some(text.as_ref()),
            _ => None,
        }
    }
}

impl<'a, 'input: 'a> SchemaDataMapping<'a> for YamlMapping<'a, 'input> {
    type Value = &'a Value<'input>;

    fn get(&self, key: &str) -> Option<Self::Value> {
        self.0.iter().find_map(|pair| match &pair.key.value {
            Value::String(text) if text.as_ref() == key => Some(&pair.value.value),
            _ => None,
        })
    }
}

#[derive(Debug)]
pub struct SequenceValueIter<'a, 'input> {
    inner: std::slice::Iter<'a, SequenceItem<'input>>,
}

impl<'a, 'input> Iterator for SequenceValueIter<'a, 'input> {
    type Item = &'a Value<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|item| &item.node.value)
    }
}

impl<'a, 'input: 'a> SchemaDataSequence<'a> for YamlSequence<'a, 'input> {
    type Value = &'a Value<'input>;
    type Iter = SequenceValueIter<'a, 'input>;

    fn iter(&self) -> Self::Iter {
        SequenceValueIter {
            inner: self.0.iter(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use avdschema::{
        SchemaDataMapping as _, SchemaDataSequence as _, SchemaDataValue as _, SchemaKeys, Store,
        any::AnySchema, dict::Dict, get_schema_from_path,
    };
    use serde_yaml::from_str;

    use super::{YamlMapping, YamlSequence};
    use crate::{Integer, MappingPair, Node, SequenceItem, Span, Value, parse};

    fn string_value(value: &'static str) -> Value<'static> {
        Value::String(Cow::Borrowed(value))
    }

    fn string_node(value: &'static str) -> Node<'static> {
        Node::new(string_value(value), Span::default())
    }

    fn int_node(value: i64) -> Node<'static> {
        Node::new(Value::Int(Integer::I64(value)), Span::default())
    }

    fn mapping_pair(key: Node<'static>, value: Node<'static>) -> MappingPair<'static> {
        MappingPair::new(Span::default(), key, value)
    }

    fn sequence_item(value: Value<'static>) -> SequenceItem<'static> {
        SequenceItem::new(Span::default(), Node::new(value, Span::default()))
    }

    fn parse_single_document(input: &str) -> Node<'static> {
        let (docs, errors) = parse(input);
        assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");
        assert_eq!(docs.len(), 1);
        docs.into_iter().next().unwrap()
    }

    fn test_store() -> Store {
        from_str(
            "
eos_config:
  type: dict
  keys:
    key2:
      type: str
      description: this is from key2
    dynamic:
      type: list
      items:
        type: dict
        keys:
          key:
            type: str
  dynamic_keys:
    dynamic.key:
      type: int
      max: 10
",
        )
        .unwrap()
    }

    #[test]
    fn schema_data_value_views_match_value_variants() {
        let mapping = Value::Mapping(vec![mapping_pair(
            string_node("key"),
            Node::new(Value::Bool(true), Span::default()),
        )]);
        let sequence = Value::Sequence(vec![sequence_item(Value::Bool(true))]);
        let string = string_value("value");
        let int = Value::Int(Integer::I64(7));

        assert!((&mapping).as_mapping().is_some());
        assert!((&mapping).as_sequence().is_none());
        assert!((&mapping).as_str().is_none());

        assert!((&sequence).as_mapping().is_none());
        assert!((&sequence).as_sequence().is_some());
        assert!((&sequence).as_str().is_none());

        assert_eq!((&string).as_str(), Some("value"));
        assert!((&string).as_mapping().is_none());
        assert!((&string).as_sequence().is_none());

        assert!((&int).as_mapping().is_none());
        assert!((&int).as_sequence().is_none());
        assert!((&int).as_str().is_none());
    }

    #[test]
    fn mapping_lookup_returns_first_matching_string_key() {
        let mapping = vec![
            mapping_pair(
                string_node("dup"),
                Node::new(Value::Int(Integer::I64(1)), Span::default()),
            ),
            mapping_pair(
                string_node("dup"),
                Node::new(Value::Int(Integer::I64(2)), Span::default()),
            ),
        ];
        let slice = YamlMapping(mapping.as_slice());

        let found = slice.get("dup");

        assert!(matches!(found, Some(Value::Int(Integer::I64(1)))));
        assert!(std::ptr::eq(
            std::ptr::from_ref(found.unwrap()),
            std::ptr::from_ref(&mapping.first().unwrap().value.value),
        ));
    }

    #[test]
    fn mapping_lookup_returns_none_for_missing_key() {
        let mapping = vec![mapping_pair(
            string_node("present"),
            Node::new(Value::Bool(true), Span::default()),
        )];

        assert_eq!(YamlMapping(mapping.as_slice()).get("missing"), None);
    }

    #[test]
    fn mapping_lookup_ignores_non_string_keys() {
        let mapping = vec![mapping_pair(
            int_node(1),
            Node::new(string_value("value"), Span::default()),
        )];

        assert_eq!(YamlMapping(mapping.as_slice()).get("1"), None);
    }

    #[test]
    fn sequence_iteration_yields_values_in_order() {
        let sequence = vec![
            sequence_item(Value::Bool(true)),
            sequence_item(string_value("two")),
            sequence_item(Value::Int(Integer::I64(3))),
        ];
        let sequence_view = YamlSequence(sequence.as_slice());

        let values = sequence_view.iter().collect::<Vec<_>>();

        assert!(matches!(values.first(), Some(Value::Bool(true))));
        assert!(matches!(
            values.get(1),
            Some(Value::String(text)) if text.as_ref() == "two"
        ));
        assert!(matches!(values.get(2), Some(Value::Int(Integer::I64(3)))));
    }

    #[test]
    fn walk_follows_nested_dict_path_on_parsed_yaml() {
        let doc = parse_single_document(
            "
outer:
  inner:
    leaf: value
",
        );
        let mut trail = Vec::new();

        let walked = (&doc.value).walk(["outer", "inner", "leaf"].into_iter(), Some(&mut trail));

        let key = vec!["outer".into(), "inner".into(), "leaf".into()];
        assert_eq!(walked.len(), 1);
        assert!(matches!(
            walked.get(&key).copied(),
            Some(Value::String(text)) if text.as_ref() == "value"
        ));
    }

    #[test]
    fn walk_collects_multiple_trails_across_list_of_dicts() {
        let doc = parse_single_document(
            "
items:
  - inner: one
  - inner: two
  - other: skip
",
        );
        let mut trail = Vec::new();

        let walked = (&doc.value).walk(["items", "inner"].into_iter(), Some(&mut trail));

        assert_eq!(walked.len(), 2);
        assert!(matches!(
            walked
                .get(&vec!["items".into(), "inner".into(), "0".into()])
                .copied(),
            Some(Value::String(text)) if text.as_ref() == "one"
        ));
        assert!(matches!(
            walked
                .get(&vec!["items".into(), "inner".into(), "1".into()])
                .copied(),
            Some(Value::String(text)) if text.as_ref() == "two"
        ));
    }

    #[test]
    fn get_dynamic_keys_handles_string_sequence_wrong_type_and_missing_root() {
        let schema: Dict = from_str(
            "
dynamic_keys:
  name:
    type: int
  names:
    type: bool
  wrong:
    type: str
",
        )
        .unwrap();

        let string_doc = parse_single_document("name: single\n");
        let string_dynamic_keys =
            schema.get_dynamic_keys((&string_doc.value).as_mapping().unwrap());
        assert_eq!(
            string_dynamic_keys
                .unwrap()
                .keys()
                .cloned()
                .collect::<Vec<_>>(),
            vec!["single".to_owned()]
        );

        let sequence_doc = parse_single_document("names: [one, two]\n");
        let sequence_dynamic_keys =
            schema.get_dynamic_keys((&sequence_doc.value).as_mapping().unwrap());
        assert_eq!(
            sequence_dynamic_keys
                .unwrap()
                .keys()
                .cloned()
                .collect::<Vec<_>>(),
            vec!["one".to_owned(), "two".to_owned()]
        );

        let wrong_type_doc = parse_single_document("wrong: 7\n");
        let wrong_type_dynamic_keys =
            schema.get_dynamic_keys((&wrong_type_doc.value).as_mapping().unwrap());
        assert!(wrong_type_dynamic_keys.unwrap().is_empty());

        let missing_root_doc = parse_single_document("other: value\n");
        let missing_root_dynamic_keys =
            schema.get_dynamic_keys((&missing_root_doc.value).as_mapping().unwrap());
        assert!(missing_root_dynamic_keys.unwrap().is_empty());
    }

    #[test]
    fn schema_keys_include_dynamic_keys_for_parsed_yaml() {
        let schema: AnySchema = from_str(
            "
type: dict
keys:
  key2:
    type: str
  dynamic:
    type: list
    items:
      type: dict
      keys:
        key:
          type: str
dynamic_keys:
  dynamic.key:
    type: int
    max: 10
allow_other_keys: true
",
        )
        .unwrap();
        let doc = parse_single_document(
            "
dynamic:
  - key: one
  - key: two
key2: value
",
        );

        let schema_keys = SchemaKeys::try_from_schema_with_value(&schema, &doc.value).unwrap();

        assert_eq!(schema_keys.keys.len(), 4);
        assert!(schema_keys.keys.contains_key("key2"));
        assert!(schema_keys.keys.contains_key("dynamic"));
        assert!(schema_keys.keys.contains_key("one"));
        assert!(schema_keys.keys.contains_key("two"));
    }

    #[test]
    fn get_schema_from_path_works_with_parsed_yaml_values() {
        let store = test_store();
        let doc = parse_single_document(
            "
dynamic:
  - key: one
  - key: two
key2: value
",
        );

        let root = get_schema_from_path("eos_config", &store, &[], &doc.value).unwrap();
        assert_eq!(root, Some(store.get("eos_config").unwrap()));

        let static_key =
            get_schema_from_path("eos_config", &store, &["key2".to_owned()], &doc.value).unwrap();
        let expected_static: AnySchema =
            from_str("type: str\ndescription: this is from key2\n").unwrap();
        assert_eq!(static_key, Some(&expected_static),);

        let dynamic_key =
            get_schema_from_path("eos_config", &store, &["two".to_owned()], &doc.value).unwrap();
        let expected_dynamic: AnySchema = from_str("type: int\nmax: 10\n").unwrap();
        assert_eq!(dynamic_key, Some(&expected_dynamic),);
    }
}
