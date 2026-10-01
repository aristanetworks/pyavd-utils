// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::HashMap;

use avdschema::ListView;

use crate::context::Context;
use crate::context::ValidationState;
use crate::feedback::Type;
use crate::feedback::Violation;
use crate::validatable::ValidatableSequence;
use crate::validatable::ValidatableValue;
use crate::validation::NodeValidation;

/// Validate list-wide constraints without recursively validating item bodies.
///
/// Returns [`NodeValidation::Valid`] with a sequence view when traversal may
/// continue, [`NodeValidation::Null`] for an accepted null, or
/// [`NodeValidation::Invalid`] after recording an invalid-type diagnostic.
pub(crate) fn validate_node<'a, V: ValidatableValue>(
    schema: ListView<'_>,
    value: &'a V,
    ctx: &mut Context,
    state: &mut ValidationState,
) -> NodeValidation<V::Sequence<'a>> {
    if let Some(sequence) = value.as_sequence() {
        validate_min_length(schema, value, &sequence, ctx, state);
        validate_max_length(schema, value, &sequence, ctx, state);
        validate_unique_keys(schema, &sequence, ctx, state);
        NodeValidation::Valid(sequence)
    } else if value.is_null() && !ctx.configuration.restrict_null_values {
        NodeValidation::Null
    } else {
        ctx.add_error_for(
            state,
            value,
            Violation::InvalidType {
                expected: Type::List,
                found: value.value_type(),
            },
        );
        NodeValidation::Invalid
    }
}

/// Validate list-item structure before recursively validating the item body.
pub(crate) fn validate_item_node<V: ValidatableValue>(
    schema: ListView<'_>,
    item: &V,
    ctx: &mut Context,
    state: &ValidationState,
) {
    validate_item_primary_key(schema, item, ctx, state);
}

fn validate_min_length<'a, V: ValidatableValue, S: ValidatableSequence<'a>>(
    schema: ListView<'_>,
    value: &V,
    input: &S,
    ctx: &mut Context,
    state: &ValidationState,
) {
    if let Some(min_length) = schema.min_length() {
        let length = input.len() as u64;
        if min_length > length {
            ctx.add_error_for(
                state,
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
    schema: ListView<'_>,
    value: &V,
    input: &S,
    ctx: &mut Context,
    state: &ValidationState,
) {
    if let Some(max_length) = schema.max_length() {
        let length = input.len() as u64;
        if max_length < length {
            ctx.add_error_for(
                state,
                value,
                Violation::LengthAboveMaximum {
                    maximum: max_length,
                    found: length,
                },
            );
        }
    }
}

fn validate_item_primary_key<V: ValidatableValue>(
    schema: ListView<'_>,
    item: &V,
    ctx: &mut Context,
    state: &ValidationState,
) {
    if let Some(primary_key) = schema.primary_key()
        && item.get(primary_key).is_none_or(ValidatableValue::is_null)
    {
        ctx.add_error_for(
            state,
            item,
            Violation::MissingRequiredKey {
                key: primary_key.to_owned(),
            },
        );
    }
}

fn validate_unique_keys<'a, S: ValidatableSequence<'a>>(
    schema: ListView<'_>,
    items: &S,
    ctx: &mut Context,
    state: &ValidationState,
) {
    type SeenItem<'a, T> = (Vec<String>, &'a T);

    let unique_keys = schema.unique_keys().into_iter().flatten().chain(
        // the primary key is considered unique unless told otherwise
        schema
            .primary_key()
            .filter(|_| !schema.allow_duplicate_primary_key()),
    );

    for unique_key in unique_keys {
        // Map from stringified value to list of (trail, value) pairs.
        let mut seen_items: HashMap<String, Vec<SeenItem<'a, S::Value>>> = HashMap::new();

        for (i, item) in items.iter().enumerate() {
            for (trail_suffix, value) in item.walk_path(unique_key) {
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
                            ctx.add_duplicate_value_violation_pair_for(
                                state,
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

/// Convert a `ValidatableValue` to a string for comparison purposes.
fn value_to_string<V: ValidatableValue>(value: &V) -> String {
    if let Some(string) = value.as_str() {
        return string.into_owned();
    }
    if value.is_null() {
        return "__NULL__".to_owned();
    }
    // For complex types, we can't easily compare
    "__COMPLEX__".to_owned()
}

#[cfg(test)]
mod tests {
    use avdschema::any::SourceSchema;
    use avdschema::dict::SourceDict;
    use avdschema::list::SourceList;
    use avdschema::str::SourceStr;
    use ordermap::OrderMap;
    use serde_json::Value;

    use super::*;
    use crate::Configuration;
    use crate::feedback::CoercionNote;
    use crate::feedback::Feedback;
    use crate::validation::test_utils::TestValidate as _;

    #[test]
    fn validate_type_ok() {
        let schema = SourceList::default();
        let input = serde_json::json!(["foo", "bar"]);
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = SourceList::default();
        let input: Value = true.into();
        let mut ctx = Context::new(None);
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
        let schema = SourceList {
            items: Some(SourceSchema::Str(SourceStr::default()).into()),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_item_type_err() {
        let schema = SourceList {
            items: Some(Box::new(SourceStr::default().into())),
            ..Default::default()
        };
        let input = serde_json::json!([{}, {}]);
        let mut ctx = Context::new(None);
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
        let schema = SourceList {
            items: Some(Box::new(SourceStr::default().into())),
            ..Default::default()
        };
        let input = serde_json::json!([1, []]);
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
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
        // Second item [] is invalid (SourceList, not SourceStr)
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
        let schema = SourceList {
            min_length: Some(1),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_min_length_err() {
        let schema = SourceList {
            min_length: Some(3),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let mut ctx = Context::new(None);
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
        let schema = SourceList {
            max_length: Some(2),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_max_length_err() {
        let schema = SourceList {
            max_length: Some(2),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar", "baz"]);
        let mut ctx = Context::new(None);
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
        let schema = SourceList {
            items: Some(Box::new(
                SourceDict {
                    keys: Some(OrderMap::from_iter([(
                        "foo".into(),
                        SourceStr::default().into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "v1" }, { "foo": "v2" }]);
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_primary_key_required_err() {
        let schema = SourceList {
            items: Some(Box::new(
                SourceDict {
                    keys: Some(OrderMap::from_iter([(
                        "foo".into(),
                        SourceStr::default().into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": null }, { "foo": "v1" }]);
        let mut ctx = Context::new(None);
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
        let schema = SourceList {
            items: Some(Box::new(
                SourceDict {
                    keys: Some(OrderMap::from_iter([(
                        "foo".into(),
                        SourceStr::default().into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "111" }, { "foo": "222" }, { "foo": "111" }]);
        let mut ctx = Context::new(None);
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
        let schema = SourceList {
            items: Some(Box::new(
                SourceDict {
                    keys: Some(OrderMap::from_iter([(
                        "foo".into(),
                        SourceStr::default().into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            allow_duplicate_primary_key: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "111" }, { "foo": "222" }, { "foo": "111" }]);
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert!(ctx.result.errors.is_empty());
    }

    #[test]
    fn validate_unique_keys_through_nested_list_err() {
        let schema = SourceList {
            items: Some(Box::new(
                SourceDict {
                    keys: Some(OrderMap::from_iter([(
                        "aliases".into(),
                        SourceList {
                            items: Some(Box::new(
                                SourceDict {
                                    keys: Some(OrderMap::from_iter([(
                                        "name".into(),
                                        SourceStr::default().into(),
                                    )])),
                                    ..Default::default()
                                }
                                .into(),
                            )),
                            ..Default::default()
                        }
                        .into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )),
            unique_keys: Some(vec!["aliases.name".into()]),
            ..Default::default()
        };
        let input = serde_json::json!([
            {"aliases": [{"name": "dup"}]},
            {"aliases": [{"name": "dup"}]}
        ]);
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into(), "aliases".into(), "0".into(), "name".into()].into(),
                    span: None,
                    issue: Violation::ValueNotUnique {
                        other_path: vec!["1".into(), "aliases".into(), "0".into(), "name".into()]
                            .into(),
                        other_span: None,
                    }
                    .into()
                },
                Feedback {
                    path: vec!["1".into(), "aliases".into(), "0".into(), "name".into()].into(),
                    span: None,
                    issue: Violation::ValueNotUnique {
                        other_path: vec!["0".into(), "aliases".into(), "0".into(), "name".into()]
                            .into(),
                        other_span: None,
                    }
                    .into()
                }
            ]
        );
    }
}
