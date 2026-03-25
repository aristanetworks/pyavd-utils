// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::HashMap;

use avdschema::{any::AnySchema, list::List, resolve_ref};

use crate::feedback::{Type, Violation};
use crate::validatable::{ValidatableSequence, ValidatableValue};
use crate::{context::Context, validation::Validation};

impl Validation for List {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        if let Some(seq) = value.as_sequence() {
            validate_ref(self, value, ctx);
            validate_min_length(self, value, &seq, ctx);
            validate_max_length(self, value, &seq, ctx);
            validate_unique_keys(self, &seq, ctx);
            // Validate items and optionally collect coerced results
            let coerced_items = validate_items(self, &seq, ctx);
            coerced_items.map(|items| value.coerce_sequence(items))
        } else if value.is_null() && !ctx.configuration.restrict_null_values {
            ctx.configuration
                .return_coerced_data
                .then(|| value.coerce_null())
        } else {
            ctx.add_error_for(
                value,
                Violation::InvalidType {
                    expected: Type::List,
                    found: value.value_type(),
                },
            );
            None
        }
    }
}

/// Validate against a referenced schema (for unresolved $ref ending with #).
fn validate_ref<V: ValidatableValue>(schema: &List, value: &V, ctx: &mut Context) {
    if let Some(ref_) = schema.base.schema_ref.as_ref()
        && let Ok(AnySchema::List(ref_schema)) = resolve_ref(ref_, ctx.store)
    {
        let _ = ref_schema.validate(value, ctx);
    }
}

/// Validate and optionally coerce sequence items.
/// Returns Some(coerced_items) when coercion is enabled, None otherwise.
fn validate_items<'a, S: ValidatableSequence<'a>>(
    schema: &List,
    input: &S,
    ctx: &mut Context,
) -> Option<Vec<<S::Value as ValidatableValue>::Coerced>> {
    let return_coerced = ctx.configuration.return_coerced_data;
    let mut coerced = if return_coerced {
        Some(Vec::with_capacity(input.len()))
    } else {
        None
    };

    for (i, item) in input.iter().enumerate() {
        ctx.state.path.push(i.to_string());
        validate_item_primary_key(schema, item, ctx);
        if let Some(ref mut items) = coerced {
            let coerced_item = validate_item_schema(schema, item, ctx);
            items.push(coerced_item);
        } else {
            validate_item_schema_only(schema, item, ctx);
        }
        ctx.state.path.pop();
    }
    coerced
}

fn validate_item_schema<V: ValidatableValue>(
    schema: &List,
    item: &V,
    ctx: &mut Context,
) -> V::Coerced {
    if let Some(item_schema) = &schema.items {
        // validate() returns Option, but we know return_coerced_data is true here
        item_schema
            .validate(item, ctx)
            .unwrap_or_else(|| item.clone_to_coerced())
    } else {
        // No item schema - preserve the value as-is
        item.clone_to_coerced()
    }
}

fn validate_item_schema_only<V: ValidatableValue>(schema: &List, item: &V, ctx: &mut Context) {
    if let Some(item_schema) = &schema.items {
        let _ = item_schema.validate(item, ctx);
    }
}

fn validate_min_length<'a, V: ValidatableValue, S: ValidatableSequence<'a>>(
    schema: &List,
    value: &V,
    input: &S,
    ctx: &mut Context,
) {
    if let Some(min_length) = schema.min_length {
        let length = input.len() as u64;
        if min_length > length {
            ctx.add_error_for(
                value,
                Violation::LengthBelowMinimum {
                    minimum: min_length,
                    found: length,
                },
            );
        }
    }
}

fn validate_max_length<'a, V: ValidatableValue, S: ValidatableSequence<'a>>(
    schema: &List,
    value: &V,
    input: &S,
    ctx: &mut Context,
) {
    if let Some(max_length) = schema.max_length {
        let length = input.len() as u64;
        if max_length < length {
            ctx.add_error_for(
                value,
                Violation::LengthAboveMaximum {
                    maximum: max_length,
                    found: length,
                },
            );
        }
    }
}

fn validate_item_primary_key<V: ValidatableValue>(schema: &List, item: &V, ctx: &mut Context) {
    if let Some(primary_key) = &schema.primary_key
        && item.get(primary_key).is_none_or(|value| value.is_null())
    {
        ctx.add_error_for(
            item,
            Violation::MissingRequiredKey {
                key: primary_key.to_owned(),
            },
        );
    }
}

fn validate_unique_keys<'a, S: ValidatableSequence<'a>>(
    schema: &List,
    items: &S,
    ctx: &mut Context,
) {
    type SeenItem<'a, T> = (Vec<String>, &'a T);

    let unique_keys = schema.unique_keys.iter().flatten().chain(
        // the primary key is considered unique unless told otherwise
        schema
            .primary_key
            .as_ref()
            .filter(|_| !schema.allow_duplicate_primary_key.unwrap_or_default()),
    );

    for unique_key in unique_keys {
        // Map from stringified value to list of (trail, value) pairs.
        let mut seen_items: HashMap<String, Vec<SeenItem<'a, S::Value>>> = HashMap::new();

        for (i, item) in items.iter().enumerate() {
            // Get the value at the unique_key path
            let values = get_values_at_path(item, unique_key);

            for (trail_suffix, value) in values {
                // Build the full trail
                let mut trail = vec![i.to_string()];
                trail.extend(trail_suffix);

                // Convert value to string for comparison
                let value_str = value_to_string(value);

                seen_items
                    .entry(value_str)
                    .and_modify(|seen_item_trails| {
                        // We found at least one other item, so we know we have a duplicate
                        // Add violations for all duplicates in both directions.
                        for (seen_item_trail, seen_value) in seen_item_trails.iter() {
                            ctx.add_duplicate_violation_pair_for(
                                *seen_value,
                                seen_item_trail,
                                value,
                                &trail,
                            );
                        }
                    })
                    .or_insert_with(|| vec![(trail, value)]);
            }
        }
    }
}

/// Get all values at a dot-separated path, returning (trail_suffix, value) pairs.
fn get_values_at_path<'a, V: ValidatableValue>(
    value: &'a V,
    path: &str,
) -> Vec<(Vec<String>, &'a V)> {
    let mut path_parts = path.split('.');
    let Some(first_key) = path_parts.next() else {
        return vec![(vec![], value)];
    };

    let rest: Vec<_> = path_parts.collect();

    let Some(child) = value.get(first_key) else {
        return vec![];
    };

    if rest.is_empty() {
        return vec![(vec![first_key.to_string()], child)];
    }

    // Navigate further
    let rest_path = rest.join(".");
    get_values_at_path(child, &rest_path)
        .into_iter()
        .map(|(mut trail, v)| {
            trail.insert(0, first_key.to_string());
            (trail, v)
        })
        .collect()
}

/// Convert a ValidatableValue to a string for comparison purposes.
fn value_to_string<V: ValidatableValue>(value: &V) -> String {
    if let Some(s) = value.as_str() {
        return s.into_owned();
    }
    if let Some(i) = value.as_i64() {
        return i.to_string();
    }
    if let Some(b) = value.as_bool() {
        return b.to_string();
    }
    if value.is_null() {
        return "null".to_string();
    }
    // For complex types, we can't easily compare
    "<complex>".to_string()
}

#[cfg(test)]
mod tests {
    use avdschema::{any::AnySchema, dict::Dict, str::Str};
    use ordermap::OrderMap;
    use serde_json::Value;

    use super::*;
    use crate::{
        Configuration,
        feedback::{CoercionNote, Feedback},
        validation::test_utils::get_test_store,
    };

    #[test]
    fn validate_type_ok() {
        let schema = List::default();
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = List::default();
        let input: Value = true.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidType {
                    expected: Type::List,
                    found: Type::Bool
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_item_type_ok() {
        let schema = List {
            items: Some(AnySchema::Str(Str::default()).into()),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_item_type_err() {
        let schema = List {
            items: Some(Box::new(Str::default().into())),
            ..Default::default()
        };
        let input = serde_json::json!([{}, {}]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into()].into(),
                    span: None,
                    issue: Violation::InvalidType {
                        expected: Type::Str,
                        found: Type::Dict
                    }
                    .into()
                },
                Feedback {
                    path: vec!["1".into()].into(),
                    span: None,
                    issue: Violation::InvalidType {
                        expected: Type::Str,
                        found: Type::Dict
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_item_type_coercion_ok_err() {
        let schema = List {
            items: Some(Box::new(Str::default().into())),
            ..Default::default()
        };
        let input = serde_json::json!([1, []]);
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration));
        let coerced = schema.validate(&input, &mut ctx);
        // Int 1 is coerced to String "1"
        assert_eq!(
            ctx.result.infos,
            vec![Feedback {
                path: vec!["0".into()].into(),
                span: None,
                issue: CoercionNote {
                    found: 1.into(),
                    made: "1".into()
                }
                .into()
            }]
        );
        // Second item [] is invalid (List, not Str)
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["1".into()].into(),
                span: None,
                issue: Violation::InvalidType {
                    expected: Type::Str,
                    found: Type::List
                }
                .into()
            },]
        );
        // Coerced output should have "1" for first item, original [] preserved for invalid second item
        assert_eq!(coerced, Some(serde_json::json!(["1", []])));
    }

    #[test]
    fn validate_min_length_ok() {
        let schema = List {
            min_length: Some(1),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_min_length_err() {
        let schema = List {
            min_length: Some(3),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::LengthBelowMinimum {
                    minimum: 3,
                    found: 2
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_max_length_ok() {
        let schema = List {
            max_length: Some(2),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_max_length_err() {
        let schema = List {
            max_length: Some(2),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar", "baz"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::LengthAboveMaximum {
                    maximum: 2,
                    found: 3
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_primary_key_ok() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "v1" }, { "foo": "v2" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_primary_key_required_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": null }, { "foo": "v1" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["0".into()].into(),
                span: None,
                issue: Violation::MissingRequiredKey { key: "foo".into() }.into()
            }]
        );
    }

    #[test]
    fn validate_primary_key_not_unique_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "111" }, { "foo": "222" }, { "foo": "111" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into(), "foo".into()].into(),
                    span: None,
                    issue: Violation::ValueNotUnique {
                        other_path: vec!["2".into(), "foo".into()].into(),
                        other_span: None,
                    }
                    .into()
                },
                Feedback {
                    path: vec!["2".into(), "foo".into()].into(),
                    span: None,
                    issue: Violation::ValueNotUnique {
                        other_path: vec!["0".into(), "foo".into()].into(),
                        other_span: None,
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_allow_duplicate_primary_key_ok() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            allow_duplicate_primary_key: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "111" }, { "foo": "222" }, { "foo": "111" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert!(ctx.result.errors.is_empty());
    }
}
