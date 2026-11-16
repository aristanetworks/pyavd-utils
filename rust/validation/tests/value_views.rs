// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Exercise validation with child wrappers returned by value, rather than by reference.

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;
    use std::borrow::Cow;

    use avdschema::Store;
    use serde_json::Map;
    use serde_json::Value;
    use serde_json::json;
    use validation::Configuration;
    use validation::StoreValidate as _;
    use validation::ValidatableMapping;
    use validation::ValidatableMappingPair;
    use validation::ValidatableSequence;
    use validation::ValidatableValue;
    use validation::feedback;

    /// A lightweight wrapper constructed on demand for each child.
    ///
    /// No child `WrappedValue` is stored anywhere. All structural access must
    /// return wrappers by value without cloning the underlying JSON tree.
    #[derive(Debug)]
    struct WrappedValue<'data>(&'data Value);

    /// An owned view which deliberately does not implement `ValidatableValue`.
    #[derive(Debug)]
    struct OwnedView<'data>(WrappedValue<'data>);

    impl<'data> Borrow<WrappedValue<'data>> for OwnedView<'data> {
        fn borrow(&self) -> &WrappedValue<'data> {
            &self.0
        }
    }

    fn owned_view(value: &Value) -> OwnedView<'_> {
        OwnedView(WrappedValue(value))
    }

    impl<'data> ValidatableValue for WrappedValue<'data> {
        type View<'a>
            = OwnedView<'data>
        where
            Self: 'a;
        type Mapping<'a>
            = WrappedMapping<'data>
        where
            Self: 'a;
        type Sequence<'a>
            = WrappedSequence<'data>
        where
            Self: 'a;
        type Coerced = Value;
        type CoercedMappingItem = (String, Value);

        fn is_null(&self) -> bool {
            self.0.is_null()
        }
        fn is_str(&self) -> bool {
            self.0.is_str()
        }
        fn is_int(&self) -> bool {
            self.0.is_int()
        }
        fn is_bool(&self) -> bool {
            self.0.is_bool()
        }
        fn is_float(&self) -> bool {
            self.0.is_float()
        }
        fn as_str(&self) -> Option<Cow<'_, str>> {
            ValidatableValue::as_str(self.0)
        }
        fn as_i64(&self) -> Option<i64> {
            ValidatableValue::as_i64(self.0)
        }
        fn as_bool(&self) -> Option<bool> {
            self.0.as_bool()
        }
        fn as_mapping(&self) -> Option<Self::Mapping<'_>> {
            self.0.as_object().map(WrappedMapping)
        }
        fn as_sequence(&self) -> Option<Self::Sequence<'_>> {
            self.0.as_array().map(|items| WrappedSequence(items))
        }
        fn get(&self, key: &str) -> Option<Self::View<'_>> {
            self.0.get(key).map(owned_view)
        }
        fn value_type(&self) -> feedback::Type {
            self.0.value_type()
        }
        fn to_feedback_value(&self) -> feedback::Value {
            self.0.to_feedback_value()
        }
        fn coerce_null(&self) -> Self::Coerced {
            self.0.coerce_null()
        }
        fn coerce_bool(&self, value: bool) -> Self::Coerced {
            self.0.coerce_bool(value)
        }
        fn coerce_int(&self, value: i64) -> Self::Coerced {
            self.0.coerce_int(value)
        }
        fn coerce_str(&self, value: String) -> Self::Coerced {
            self.0.coerce_str(value)
        }
        fn coerce_sequence(&self, items: Vec<Self::Coerced>) -> Self::Coerced {
            self.0.coerce_sequence(items)
        }
        fn coerce_mapping(&self, items: Vec<Self::CoercedMappingItem>) -> Self::Coerced {
            self.0.coerce_mapping(items)
        }
        fn clone_to_coerced(&self) -> Self::Coerced {
            self.0.clone_to_coerced()
        }
    }

    #[derive(Debug)]
    struct WrappedMapping<'data>(&'data Map<String, Value>);

    impl<'a, 'data: 'a> ValidatableMapping<'a> for WrappedMapping<'data> {
        type Value = WrappedValue<'data>;
        type SchemaDataMapping<'s>
            = &'s Map<String, Value>
        where
            Self: 's;
        type Pair = WrappedPair<'data>;
        type Iter = std::iter::Map<
            serde_json::map::Iter<'data>,
            fn((&'data String, &'data Value)) -> WrappedPair<'data>,
        >;

        fn get(&self, key: &str) -> Option<<Self::Value as ValidatableValue>::View<'a>> {
            self.0.get(key).map(owned_view)
        }
        fn as_schema_data_mapping(&self) -> Self::SchemaDataMapping<'_> {
            self.0
        }
        fn contains_key(&self, key: &str) -> bool {
            self.0.contains_key(key)
        }
        fn iter(&self) -> Self::Iter {
            self.0.iter().map(|(key, value)| WrappedPair(key, value))
        }
        fn len(&self) -> usize {
            self.0.len()
        }
    }

    #[derive(Debug)]
    struct WrappedPair<'data>(&'data String, &'data Value);

    impl<'a, 'data: 'a> ValidatableMappingPair<'a> for WrappedPair<'data> {
        type Value = WrappedValue<'data>;

        fn schema_key(&self) -> Option<Cow<'a, str>> {
            Some(Cow::Borrowed(self.0))
        }
        fn display_key(&self) -> Cow<'a, str> {
            Cow::Borrowed(self.0)
        }
        fn value(&self) -> <Self::Value as ValidatableValue>::View<'a> {
            owned_view(self.1)
        }
        fn coerced_item(&self, value: Value) -> (String, Value) {
            (self.0.clone(), value)
        }
    }

    #[derive(Debug)]
    struct WrappedSequence<'data>(&'data [Value]);

    impl<'a, 'data: 'a> ValidatableSequence<'a> for WrappedSequence<'data> {
        type Value = WrappedValue<'data>;
        type Iter =
            std::iter::Map<std::slice::Iter<'data, Value>, fn(&'data Value) -> OwnedView<'data>>;

        fn iter(&self) -> Self::Iter {
            self.0.iter().map(owned_view)
        }
        fn len(&self) -> usize {
            self.0.len()
        }
    }

    fn store() -> Store {
        Store::from_json(
            &json!({
                "test": {
                    "type": "dict",
                    "keys": {
                        "old": {
                            "type": "str",
                            "deprecation": {
                                "warning": true,
                                "new_key": "items.name",
                                "allow_with_new_key": false
                            }
                        },
                        "items": {
                            "type": "list",
                            "primary_key": "name",
                            "unique_keys": ["aliases.name"],
                            "items": {
                                "type": "dict",
                                "keys": {
                                    "name": {"type": "str"},
                                    "count": {"type": "int"},
                                    "aliases": {
                                        "type": "list",
                                        "items": {
                                            "type": "dict",
                                            "keys": {"name": {"type": "str"}}
                                        }
                                    }
                                }
                            }
                        },
                        "untyped": {"type": "dict"}
                    }
                }
            })
            .to_string(),
        )
        .expect("test schema should compile")
    }

    #[test]
    fn owned_child_views_preserve_validation_and_coercion() {
        let input = json!({
            "old": "deprecated",
            "items": [
                {"name": "a", "count": "1", "aliases": [{"name": "same"}]},
                {"name": "a", "count": "bad", "aliases": [{"name": "same"}]},
                {"count": 3},
                {"name": null},
                {"name": "b"}
            ],
            "untyped": {"nested": [1, {"key": "value"}]}
        });
        let store = store();
        let configuration = Configuration {
            return_coerced_data: true,
            return_coercion_infos: true,
            ..Default::default()
        };
        let expected = store
            .validate_value(&input, "test", Some(&configuration))
            .unwrap();
        let actual = store
            .validate_value(&WrappedValue(&input), "test", Some(&configuration))
            .unwrap();

        assert_eq!(actual.result.errors, expected.result.errors);
        assert_eq!(actual.result.warnings, expected.result.warnings);
        assert_eq!(actual.result.infos, expected.result.infos);
        assert_eq!(actual.coerced, expected.coerced);
        assert!(!actual.result.errors.is_empty());
        assert_eq!(actual.result.warnings.len(), 1);
        assert_eq!(actual.coerced.unwrap()["items"][0]["count"], 1);
    }

    #[test]
    fn owned_child_views_support_nested_path_traversal() {
        let input = json!({"items": [{"name": "a"}, {"name": "b"}, {}]});
        let wrapped = WrappedValue(&input);
        let mut matches = Vec::new();
        wrapped.walk_path("items.name", &mut |trail, value| {
            matches.push((trail.to_vec(), value.as_str().unwrap().into_owned()));
        });
        assert_eq!(
            matches,
            vec![
                (
                    vec!["items".to_owned(), "0".to_owned(), "name".to_owned()],
                    "a".to_owned()
                ),
                (
                    vec!["items".to_owned(), "1".to_owned(), "name".to_owned()],
                    "b".to_owned()
                ),
            ]
        );
        assert!(wrapped.path_exists("items.name"));
        assert!(!wrapped.path_exists("items.missing"));
        assert!(wrapped.path_exists(""));
    }

    #[test]
    fn owned_child_views_expand_sequences_on_empty_paths() {
        let input = json!([["a"], "b"]);
        let mut matches = Vec::new();
        WrappedValue(&input).walk_path("", &mut |trail, value| {
            matches.push((trail.to_vec(), value.as_str().unwrap().into_owned()));
        });
        assert_eq!(
            matches,
            vec![
                (vec!["0".to_owned(), "0".to_owned()], "a".to_owned()),
                (vec!["1".to_owned()], "b".to_owned()),
            ]
        );
        assert!(!WrappedValue(&json!([])).path_exists(""));
    }
}
