// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::DeprecationView;
use avdschema::DictView;
use avdschema::SchemaView;

use super::NodeValidation;
use crate::context::Context;
use crate::context::ValidationState;
use crate::feedback::Deprecated;
use crate::feedback::Removed;
use crate::feedback::Type;
use crate::feedback::Violation;
use crate::validatable::ValidatableMapping;
use crate::validatable::ValidatableValue;

/// Validate the mapping node before recursively validating its values.
///
/// Required keys are checked by [`finish_node_validation`] to preserve the
/// existing diagnostic order around eager child traversal.
///
/// Returns [`NodeValidation::Valid`] with a mapping view when traversal may
/// continue, [`NodeValidation::Null`] for an accepted null, or
/// [`NodeValidation::Invalid`] after recording an invalid-type diagnostic.
pub(crate) fn validate_node<'a, V: ValidatableValue>(
    value: &'a V,
    ctx: &mut Context,
    state: &mut ValidationState,
) -> NodeValidation<V::Mapping<'a>> {
    if let Some(mapping) = value.as_mapping() {
        validate_duplicate_keys(&mapping, ctx, state);
        NodeValidation::Valid(mapping)
    } else if value.is_null() && !ctx.configuration.restrict_null_values {
        NodeValidation::Null
    } else {
        ctx.add_error_for(
            state,
            value,
            Violation::InvalidType {
                expected: Type::Dict,
                found: value.value_type(),
            },
        );
        NodeValidation::Invalid
    }
}

/// Complete mapping-node validation after recursive child validation.
pub(crate) fn finish_node_validation<'a, M: ValidatableMapping<'a>>(
    schema: DictView<'_>,
    value: &M::Value,
    input: &M,
    ctx: &mut Context,
    state: &ValidationState,
) {
    validate_required_keys(schema, value, input, ctx, state);
}

fn validate_duplicate_keys<'a, M: ValidatableMapping<'a>>(
    input: &M,
    ctx: &mut Context,
    state: &mut ValidationState,
) {
    for duplicate_key in input.duplicate_keys() {
        state.path.push(duplicate_key.key.to_owned());
        for span in duplicate_key.spans {
            ctx.add_error_with_span(state, span, Violation::DuplicateKey());
        }
        state.path.pop();
    }
}

fn validate_required_keys<'a, M: ValidatableMapping<'a>>(
    schema: DictView<'_>,
    value: &M::Value,
    input: &M,
    ctx: &mut Context,
    state: &ValidationState,
) {
    // Don't validate required keys if we are below a dict with relaxed validation or if we are at the root level.
    if state.relaxed_validation
        || (ctx.configuration.ignore_required_keys_on_root_dict && state.path.is_empty())
    {
        return;
    }
    for (key, key_schema) in schema.keys() {
        if key_schema.required() && !input.contains_key(key) {
            ctx.add_error_for(
                state,
                value,
                Violation::MissingRequiredKey {
                    key: key.to_owned(),
                },
            );
        }
    }
}

/// Check for deprecation settings in the given schema and return a bool if there was an error that should stop further validation.
pub(crate) fn check_deprecation<'a, M: ValidatableMapping<'a>>(
    key_schema: SchemaView<'_>,
    key_span: Option<crate::feedback::SourceSpan>,
    parent_dict_input: &M,
    ctx: &mut Context,
    state: &ValidationState,
) -> bool {
    if let Some(deprecation) = key_schema.deprecation()
        && deprecation.warning()
    {
        if deprecation.removed() {
            ctx.add_error_with_span(
                state,
                key_span,
                Violation::Removed(removed_from_view(&state.path, deprecation)),
            );
            true
        } else {
            ctx.add_warning_with_span(
                state,
                key_span.clone(),
                deprecated_from_view(&state.path, deprecation),
            );
            if !deprecation.allow_with_new_key()
                && let Some(schema_new_key) = deprecation.new_key()
            {
                // Split the new_key on ' or ' in case of multiple new keys.
                // Then check if any of the new keys are set in the inputs at the same time as the deprecated key,
                // adding conflict errors if found
                schema_new_key.split(" or ").for_each(|new_key| {
                    let mut path_parts = new_key.split('.');
                    if let Some(root_key) = path_parts.next()
                        && let Some(root_value) = parent_dict_input.get(root_key)
                    {
                        // Check if the rest of the path exists
                        let rest_of_path: Vec<_> = path_parts.collect();
                        let exists = if rest_of_path.is_empty() {
                            true
                        } else {
                            !root_value.walk_path(&rest_of_path.join(".")).is_empty()
                        };
                        if exists {
                            ctx.add_error_with_span(
                                state,
                                key_span.clone(),
                                Violation::DeprecatedConflict {
                                    other_path: new_key.into(),
                                    url: deprecation.url().map(ToOwned::to_owned).into(),
                                },
                            );
                        }
                    }
                });
            }
            // Even with a conflict error we still want to validate everything else.
            false
        }
    } else {
        false
    }
}

fn deprecated_from_view(
    path: &crate::feedback::Path,
    deprecation: DeprecationView<'_>,
) -> Deprecated {
    Deprecated::from_parts(
        path,
        deprecation.new_key(),
        deprecation.remove_in_version(),
        deprecation.url(),
    )
}

fn removed_from_view(path: &crate::feedback::Path, deprecation: DeprecationView<'_>) -> Removed {
    Removed::from_parts(
        path,
        deprecation.new_key(),
        deprecation.remove_in_version(),
        deprecation.url(),
        deprecation.upgrade_handler(),
    )
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use avdschema::any::SourceSchema;
    use avdschema::base::Base;
    use avdschema::dict::DynamicKeyOverrides;
    use avdschema::dict::SourceDict;
    use avdschema::int::SourceInt;
    use avdschema::list::SourceList;
    use avdschema::str::SourceStr;
    use ordermap::OrderMap;
    use serde::Deserialize as _;
    use yaml_parser::parse;

    use super::*;
    use crate::context::Configuration;
    use crate::context::Context;
    use crate::feedback::CoercionNote;
    use crate::feedback::Feedback;
    use crate::feedback::SourceSpan;
    use crate::feedback::WarningIssue;
    use crate::validation::test_utils::TestValidate as _;
    use crate::validation::test_utils::get_test_store;

    #[test]
    fn validate_type_ok() {
        let schema = SourceDict::default();
        let input = serde_json::json!({ "foo": true });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = SourceDict::default();
        let input = serde_json::json!(true);
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidType {
                    expected: Type::Dict,
                    found: Type::Bool
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_key_type_ok() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([
                ("foo".into(), SourceStr::default().into()),
                ("bar".into(), SourceInt::default().into()),
            ])),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": "bar", "bar": 123 });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_key_type_err() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([
                ("foo".into(), SourceStr::default().into()),
                ("bar".into(), SourceInt::default().into()),
            ])),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": [], "bar": "boo" });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["foo".into()].into(),
                    span: None,
                    issue: Violation::InvalidType {
                        expected: Type::Str,
                        found: Type::List
                    }
                    .into()
                },
                Feedback {
                    path: vec!["bar".into()].into(),
                    span: None,
                    issue: Violation::InvalidType {
                        expected: Type::Int,
                        found: Type::Str
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_key_type_coerced_ok() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([
                ("foo".into(), SourceStr::default().into()),
                ("bar".into(), SourceInt::default().into()),
            ])),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": 321, "bar": "123" });
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let coerced = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.infos,
            vec![
                Feedback {
                    path: vec!["foo".into()].into(),
                    span: None,
                    issue: CoercionNote {
                        found: 321.into(),
                        made: "321".into()
                    }
                    .into()
                },
                Feedback {
                    path: vec!["bar".into()].into(),
                    span: None,
                    issue: CoercionNote {
                        found: "123".into(),
                        made: 123.into()
                    }
                    .into()
                }
            ]
        );
        assert_eq!(
            coerced,
            Some(serde_json::json!({ "foo": "321", "bar": 123 }))
        );
    }

    #[test]
    fn any_schema_validation_returns_referenced_coercion_result() {
        let schema = SourceSchema::Dict(SourceDict {
            base: Base {
                schema_ref: Some("eos_config#".into()),
                ..Default::default()
            },
            ..Default::default()
        });
        let input = serde_json::json!({ "key1": 123 });
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let coerced = schema.validate(&input, &mut ctx);

        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.infos,
            vec![Feedback {
                path: vec!["key1".into()].into(),
                span: None,
                issue: CoercionNote {
                    found: 123.into(),
                    made: "123".into(),
                }
                .into(),
            }]
        );
        assert_eq!(coerced, Some(serde_json::json!({ "key1": "123" })));
    }

    #[test]
    fn validate_dynamic_keys_ok() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                SourceList {
                    items: Some(Box::new(
                        SourceDict {
                            keys: Some(OrderMap::from_iter([(
                                "key".into(),
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
            dynamic_keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys.key".into(),
                SourceInt {
                    max: Some(10),
                    ..Default::default()
                }
                .into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!(
            { "my_dynamic_keys": [{"key": "dynkey1"}, {"key": "dynkey2"}], "dynkey1": 5, "dynkey2": 9 });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert_eq!(ctx.result.errors, vec![]);
        assert_eq!(ctx.result.infos, vec![]);
    }

    #[test]
    fn validate_dynamic_keys_err() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                SourceList {
                    items: Some(Box::new(
                        SourceDict {
                            keys: Some(OrderMap::from_iter([(
                                "key".into(),
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
            dynamic_keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys.key".into(),
                SourceInt {
                    max: Some(10),
                    ..Default::default()
                }
                .into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!(
            { "my_dynamic_keys": [{"key": "dynkey1"}, {"key": "dynkey2"}], "dynkey1": 11, "dynkey2": "wrong" });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert_eq!(ctx.result.infos, vec![]);
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["dynkey1".into()].into(),
                    span: None,
                    issue: Violation::ValueAboveMaximum {
                        maximum: 10,
                        found: 11
                    }
                    .into()
                },
                Feedback {
                    path: vec!["dynkey2".into()].into(),
                    span: None,
                    issue: Violation::InvalidType {
                        expected: Type::Int,
                        found: Type::Str
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_dynamic_keys_from_overrides_ok() {
        let schema = SourceDict {
            dynamic_keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys.key".into(),
                SourceInt {
                    max: Some(10),
                    ..Default::default()
                }
                .into(),
            )])),
            allow_other_keys: Some(true),
            keys: Some(Default::default()),
            ..Default::default()
        };
        let input = serde_json::json!({ "dynkey1": 5 });
        let configuration = Configuration {
            dynamic_key_overrides: Some(Arc::new(DynamicKeyOverrides::from_iter([(
                "dynkey1".into(),
                "my_dynamic_keys.key".into(),
            )]))),
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_static_key_beats_dynamic_key_override_collision() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "dynkey1".into(),
                SourceStr::default().into(),
            )])),
            dynamic_keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys.key".into(),
                SourceInt {
                    max: Some(10),
                    ..Default::default()
                }
                .into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!({ "dynkey1": "static schema value" });
        let configuration = Configuration {
            dynamic_key_overrides: Some(Arc::new(DynamicKeyOverrides::from_iter([(
                "dynkey1".into(),
                "my_dynamic_keys.key".into(),
            )]))),
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_dynamic_keys_from_defaults_ok() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                SourceList {
                    items: Some(Box::new(SourceStr::default().into())),
                    base: Base {
                        default: Some(vec!["dynkey1".into(), "dynkey2".into()]),
                        ..Default::default()
                    },
                    ..Default::default()
                }
                .into(),
            )])),
            dynamic_keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                SourceInt {
                    max: Some(10),
                    ..Default::default()
                }
                .into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!({ "dynkey1": 5, "dynkey2": 9 });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_dynamic_keys_from_defaults_err() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                SourceList {
                    items: Some(Box::new(SourceStr::default().into())),
                    base: Base {
                        default: Some(vec!["dynkey1".into(), "dynkey2".into()]),
                        ..Default::default()
                    },
                    ..Default::default()
                }
                .into(),
            )])),
            dynamic_keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                SourceDict {
                    keys: Some(OrderMap::from_iter([(
                        "sub_key".into(),
                        SourceInt {
                            max: Some(10),
                            ..Default::default()
                        }
                        .into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let input =
            serde_json::json!({ "dynkey1": {"sub_key": 11, "bad_key": true}, "dynkey2": "wrong" });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["dynkey1".into(), "sub_key".into()].into(),
                    span: None,
                    issue: Violation::ValueAboveMaximum {
                        maximum: 10,
                        found: 11
                    }
                    .into()
                },
                Feedback {
                    path: vec!["dynkey1".into(), "bad_key".into()].into(),
                    span: None,
                    issue: Violation::UnexpectedKey {}.into()
                },
                Feedback {
                    path: vec!["dynkey2".into()].into(),
                    span: None,
                    issue: Violation::InvalidType {
                        expected: Type::Dict,
                        found: Type::Str
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_key_allowed_ok() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr::default().into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": "ok", "foo1": "wrong" });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_key_allowed_err() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr::default().into(),
            )])),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": "ok", "foo1": "wrong" });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["foo1".into()].into(),
                span: None,
                issue: Violation::UnexpectedKey().into()
            }]
        );
    }

    #[test]
    fn validate_yaml_unexpected_key_uses_key_span() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr::default().into(),
            )])),
            ..Default::default()
        };
        let (docs, errors) = parse("bar: 1\n");
        assert!(errors.is_empty());
        let input = docs.first().expect("expected a parsed document");

        let mut ctx = Context::new(None);
        let _ = schema.validate(input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["bar".into()].into(),
                span: Some(SourceSpan { start: 0, end: 3 }),
                issue: Violation::UnexpectedKey().into()
            }]
        );
    }

    #[test]
    fn validate_yaml_duplicate_key_err() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr::default().into(),
            )])),
            ..Default::default()
        };
        let (docs, errors) = parse("foo: one\nfoo: two\n");
        assert!(errors.is_empty());
        let input = docs.first().expect("expected a parsed document");

        let mut ctx = Context::new(None);
        let _ = schema.validate(input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["foo".into()].into(),
                    span: Some(SourceSpan { start: 0, end: 3 }),
                    issue: Violation::DuplicateKey().into()
                },
                Feedback {
                    path: vec!["foo".into()].into(),
                    span: Some(SourceSpan { start: 9, end: 12 }),
                    issue: Violation::DuplicateKey().into()
                }
            ]
        );
    }

    #[test]
    fn validate_yaml_duplicate_key_err_on_schema_validated_nested_dict() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "outer".into(),
                SourceDict {
                    keys: Some(OrderMap::from_iter([(
                        "inner".into(),
                        SourceStr::default().into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )])),
            ..Default::default()
        };
        let (docs, errors) = parse("outer:\n  inner: one\n  inner: two\n");
        assert!(errors.is_empty());
        let input = docs.first().expect("expected a parsed document");

        let mut ctx = Context::new(None);
        let _ = schema.validate(input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["outer".into(), "inner".into()].into(),
                    span: Some(SourceSpan { start: 9, end: 14 }),
                    issue: Violation::DuplicateKey().into()
                },
                Feedback {
                    path: vec!["outer".into(), "inner".into()].into(),
                    span: Some(SourceSpan { start: 22, end: 27 }),
                    issue: Violation::DuplicateKey().into()
                }
            ]
        );
    }

    #[test]
    fn validate_yaml_duplicate_key_restores_path_before_later_errors() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([
                ("foo".into(), SourceStr::default().into()),
                ("baz".into(), SourceStr::default().into()),
            ])),
            ..Default::default()
        };
        let (docs, errors) = parse("foo: one\nfoo: two\nbaz: one\nbaz: two\nbar: one\n");
        assert!(errors.is_empty());
        let input = docs.first().expect("expected a parsed document");

        let mut ctx = Context::new(None);
        let _ = schema.validate(input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["foo".into()].into(),
                    span: Some(SourceSpan { start: 0, end: 3 }),
                    issue: Violation::DuplicateKey().into()
                },
                Feedback {
                    path: vec!["foo".into()].into(),
                    span: Some(SourceSpan { start: 9, end: 12 }),
                    issue: Violation::DuplicateKey().into()
                },
                Feedback {
                    path: vec!["baz".into()].into(),
                    span: Some(SourceSpan { start: 18, end: 21 }),
                    issue: Violation::DuplicateKey().into()
                },
                Feedback {
                    path: vec!["baz".into()].into(),
                    span: Some(SourceSpan { start: 27, end: 30 }),
                    issue: Violation::DuplicateKey().into()
                },
                Feedback {
                    path: vec!["bar".into()].into(),
                    span: Some(SourceSpan { start: 36, end: 39 }),
                    issue: Violation::UnexpectedKey().into()
                },
            ]
        );
    }

    #[test]
    fn validate_yaml_non_string_key_does_not_match_schema_key() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "123".into(),
                SourceStr::default().into(),
            )])),
            ..Default::default()
        };
        let (docs, errors) = parse("123: value\n");
        assert!(errors.is_empty());
        let input = docs.first().expect("expected a parsed document");

        let mut ctx = Context::new(None);
        let _ = schema.validate(input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["123".into()].into(),
                span: Some(SourceSpan { start: 0, end: 3 }),
                issue: Violation::UnexpectedKey().into()
            }]
        );
    }

    #[test]
    fn validate_yaml_non_string_key_allowed_and_preserved_in_coerced_output() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr::default().into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let (docs, errors) = parse("123: value\n");
        assert!(errors.is_empty());
        let input = docs.first().expect("expected a parsed document");

        let configuration = Configuration {
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let coerced = schema
            .validate(input, &mut ctx)
            .expect("expected coerced data");

        assert!(ctx.result.errors.is_empty());
        let yaml_parser::Value::Mapping(pairs) = coerced.value else {
            panic!("expected coerced mapping");
        };
        let pair = pairs.first().expect("expected one mapping pair");
        assert!(matches!(
            pair.key.value,
            yaml_parser::Value::Int(yaml_parser::Integer::I64(123))
        ));
        assert_eq!(pair.key.span, yaml_parser::Span::new(0..3));
        assert_eq!(pair.pair_span, yaml_parser::Span::new(0..10));
    }

    /// Test that keys starting with underscore are preserved in output but not validated
    #[test]
    fn validate_underscore_key_preserved() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr::default().into(),
            )])),
            ..Default::default()
        };
        // _internal key should be preserved but not validated
        let input = serde_json::json!({ "foo": "ok", "_internal": {"nested": "data"} });
        let configuration = Configuration {
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let coerced = schema.validate(&input, &mut ctx);
        // No errors - _internal is ignored, foo is valid
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.infos.is_empty());
        // Coerced output should include _internal key unchanged
        assert_eq!(
            coerced,
            Some(serde_json::json!({ "foo": "ok", "_internal": {"nested": "data"} }))
        );
    }

    // Tests a key that is marked as deprecated returns the proper warning.
    // Also verifies that regular validation is still done on the field even if it is deprecated.
    // Uses min_length to verify validation continues (lenient validation coerces int 123 to "123")
    #[test]
    fn validate_key_deprecated_ok() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "foo": {
                    "type": "str",
                    "min_length": 5,
                    "deprecation": {
                        "warning": true,
                        "remove_in_version": "1.2.3",
                    }
                }
            }
        }))
        .unwrap();
        // Input is int 123, which coerces to "123" (3 chars) - violates min_length: 5
        let input = serde_json::json!({"foo": 123});
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.warnings,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                span: None,
                issue: WarningIssue::Deprecated(Deprecated {
                    path: vec!["foo".into()].into(),
                    replacement: None.into(),
                    version: Some("1.2.3".into()).into(),
                    url: None.into()
                })
            }]
        );
        // SourceInt 123 coerces to string "123"
        // The min_length: 5 constraint is violated (3 < 5)
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                span: None,
                issue: Violation::LengthBelowMinimum {
                    minimum: 5,
                    found: 3
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_yaml_deprecated_key_uses_key_span() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "foo": {
                    "type": "str",
                    "deprecation": {
                        "warning": true,
                        "remove_in_version": "1.2.3",
                    }
                }
            }
        }))
        .unwrap();
        let (docs, errors) = parse("foo: bar\n");
        assert!(errors.is_empty());
        let input = docs.first().expect("expected a parsed document");

        let mut ctx = Context::new(None);
        let _ = schema.validate(input, &mut ctx);

        assert_eq!(
            ctx.result.warnings,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                span: Some(SourceSpan { start: 0, end: 3 }),
                issue: WarningIssue::Deprecated(Deprecated {
                    path: vec!["foo".into()].into(),
                    replacement: None.into(),
                    version: Some("1.2.3".into()).into(),
                    url: None.into()
                })
            }]
        );
    }

    // Tests a key that is marked as removed returns the proper error.
    // Also verifies that no other validation is done on the field,
    // notice the type is wrong in our input but no type error is returned.
    #[test]
    fn validate_key_removed_err() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "foo": {
                    "type": "str",
                    "deprecation": {
                        "warning": true,
                        "removed": true
                    }
                }
            }
        }))
        .unwrap();
        let input = serde_json::json!({"foo": 123});
        let configuration = Configuration {
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let coerced = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(coerced, Some(input));
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                span: None,
                issue: Violation::Removed(Removed {
                    path: vec!["foo".into()].into(),
                    replacement: None.into(),
                    version: None.into(),
                    url: None.into(),
                    upgrade_handler: None,
                })
                .into()
            }]
        );
    }

    // Tests a key that is marked as deprecated but where warning is disabled
    // does not return any warning.
    #[test]
    fn validate_key_deprecated_no_warning_ok() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "foo": {
                    "type": "str",
                    "deprecation": {
                        "warning": false,
                        "remove_in_version": "1.2.3",
                    }
                }
            }
        }))
        .unwrap();
        let input = serde_json::json!({"foo": "blah"});
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert!(ctx.result.warnings.is_empty());
        assert!(ctx.result.errors.is_empty());
    }

    // Tests that when allow_with_new_key is true, using both the deprecated key
    // and the new key simultaneously does NOT produce a DeprecatedConflict error.
    #[test]
    fn validate_key_deprecated_with_allow_with_new_key_ok() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "old_key": {
                    "type": "str",
                    "deprecation": {
                        "warning": true,
                        "new_key": "new_key",
                        "allow_with_new_key": true,
                        "remove_in_version": "2.0.0",
                    }
                },
                "new_key": {
                    "type": "str"
                }
            }
        }))
        .unwrap();
        let input = serde_json::json!({"old_key": "old_value", "new_key": "new_value"});
        let mut ctx = Context::new(None);
        schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        // Should have a deprecation warning
        assert_eq!(
            ctx.result.warnings,
            vec![Feedback {
                path: vec!["old_key".into()].into(),
                span: None,
                issue: WarningIssue::Deprecated(Deprecated {
                    path: vec!["old_key".into()].into(),
                    replacement: Some("new_key".into()).into(),
                    version: Some("2.0.0".into()).into(),
                    url: None.into()
                })
            }]
        );
        // Should NOT have a DeprecatedConflict error
        assert!(ctx.result.errors.is_empty());
    }

    // Tests that when allow_with_new_key is not set (None/undefined), using both the
    // deprecated key and the new key simultaneously DOES produce a DeprecatedConflict error.
    // This tests the default behavior when the field is omitted.
    #[test]
    fn validate_key_deprecated_without_allow_with_new_key_err() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "old_key": {
                    "type": "str",
                    "deprecation": {
                        "warning": true,
                        "new_key": "new_key",
                        "remove_in_version": "2.0.0",
                    }
                },
                "new_key": {
                    "type": "str"
                }
            }
        }))
        .unwrap();
        let input = serde_json::json!({"old_key": "old_value", "new_key": "new_value"});
        let mut ctx = Context::new(None);
        schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        // Should have a deprecation warning
        assert_eq!(
            ctx.result.warnings,
            vec![Feedback {
                path: vec!["old_key".into()].into(),
                span: None,
                issue: WarningIssue::Deprecated(Deprecated {
                    path: vec!["old_key".into()].into(),
                    replacement: Some("new_key".into()).into(),
                    version: Some("2.0.0".into()).into(),
                    url: None.into()
                })
            }]
        );
        // Should have a DeprecatedConflict error
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["old_key".into()].into(),
                span: None,
                issue: Violation::DeprecatedConflict {
                    other_path: "new_key".into(),
                    url: None.into()
                }
                .into()
            }]
        );
    }

    // Tests that when allow_with_new_key is explicitly set to false, using both the
    // deprecated key and the new key simultaneously DOES produce a DeprecatedConflict error.
    #[test]
    fn validate_key_deprecated_with_allow_with_new_key_false_err() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "old_key": {
                    "type": "str",
                    "deprecation": {
                        "warning": true,
                        "new_key": "new_key",
                        "allow_with_new_key": false,
                        "remove_in_version": "2.0.0",
                    }
                },
                "new_key": {
                    "type": "str"
                }
            }
        }))
        .unwrap();
        let input = serde_json::json!({"old_key": "old_value", "new_key": "new_value"});
        let mut ctx = Context::new(None);
        schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        // Should have a deprecation warning
        assert_eq!(
            ctx.result.warnings,
            vec![Feedback {
                path: vec!["old_key".into()].into(),
                span: None,
                issue: WarningIssue::Deprecated(Deprecated {
                    path: vec!["old_key".into()].into(),
                    replacement: Some("new_key".into()).into(),
                    version: Some("2.0.0".into()).into(),
                    url: None.into()
                })
            }]
        );
        // Should have a DeprecatedConflict error
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["old_key".into()].into(),
                span: None,
                issue: Violation::DeprecatedConflict {
                    other_path: "new_key".into(),
                    url: None.into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_key_deprecated_with_new_key_under_list_err() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "old_key": {
                    "type": "str",
                    "deprecation": {
                        "warning": true,
                        "new_key": "methods.group",
                        "remove_in_version": "2.0.0"
                    }
                },
                "methods": {
                    "type": "list",
                    "items": {
                        "type": "dict",
                        "keys": {
                            "group": {
                                "type": "str"
                            }
                        }
                    }
                }
            }
        }))
        .unwrap();
        let input = serde_json::json!({
            "old_key": "old_value",
            "methods": [{"group": "new_value"}]
        });
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.warnings,
            vec![Feedback {
                path: vec!["old_key".into()].into(),
                span: None,
                issue: WarningIssue::Deprecated(Deprecated {
                    path: vec!["old_key".into()].into(),
                    replacement: Some("methods.group".into()).into(),
                    version: Some("2.0.0".into()).into(),
                    url: None.into()
                })
            }]
        );
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["old_key".into()].into(),
                span: None,
                issue: Violation::DeprecatedConflict {
                    other_path: "methods.group".into(),
                    url: None.into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_key_required_ok() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr {
                    base: Base {
                        required: Some(true),
                        ..Default::default()
                    },
                    ..Default::default()
                }
                .into(),
            )])),
            ..Default::default()
        };
        // Bool input for a SourceStr field - coerced to "True"
        let input = serde_json::json!({ "foo": true });
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let coerced = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.infos,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                span: None,
                issue: CoercionNote {
                    found: true.into(),
                    made: "True".into()
                }
                .into()
            }]
        );
        assert_eq!(coerced, Some(serde_json::json!({ "foo": "True" })));
    }

    #[test]
    fn validate_key_required_err() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr {
                    base: Base {
                        required: Some(true),
                        ..Default::default()
                    },
                    ..Default::default()
                }
                .into(),
            )])),
            ..Default::default()
        };
        let input = serde_json::json!({});
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::MissingRequiredKey { key: "foo".into() }.into()
            }]
        );
    }

    #[test]
    fn validate_key_required_relaxed_root_dict_ok() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr {
                    base: Base {
                        required: Some(true),
                        ..Default::default()
                    },
                    ..Default::default()
                }
                .into(),
            )])),
            ..Default::default()
        };
        let input = serde_json::json!({});
        let configuration = Configuration {
            ignore_required_keys_on_root_dict: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_key_required_relaxed_root_dict_err() {
        let schema = SourceDict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                SourceStr {
                    base: Base {
                        required: Some(true),
                        ..Default::default()
                    },
                    ..Default::default()
                }
                .into(),
            )])),
            ..Default::default()
        };
        let input = serde_json::json!({});
        let configuration = Configuration {
            ignore_required_keys_on_root_dict: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        // Using a deeper path and see that we still get the error even though we relax for the root dict.
        let mut state = ValidationState::with_path("deeper".into());
        let _ = crate::validation::test_utils::validate_test_schema_with_state(
            SourceSchema::Dict(schema),
            &input,
            &mut ctx,
            &mut state,
        );
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["deeper".into()].into(),
                span: None,
                issue: Violation::MissingRequiredKey { key: "foo".into() }.into()
            }]
        );
    }

    #[test]
    fn validate_avd_design_with_eos_config_keys_warning() {
        // Test that when validating AVD Design schema with warn_eos_config_keys enabled,
        // if a top-level key from EOS Config is present in the input, a warning is emitted.
        let store = get_test_store();
        let input = serde_json::json!({
            "key3": "valid_avd_design_key",
            "key1": "this_is_an_eos_config_key",
            "key2": "another_eos_config_key"
        });

        let configuration = Configuration {
            warn_eos_config_keys: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let schema = store.get("avd_design").unwrap();
        let _ = schema.validate(&input, &mut ctx);

        // Should have warnings for key1 and key2
        assert_eq!(ctx.result.warnings.len(), 2);
        assert!(ctx.result.warnings.iter().any(|w| {
            matches!(&w.issue, WarningIssue::IgnoredEosConfigKey(_)) && w.path.to_string() == "key1"
        }));
        assert!(ctx.result.warnings.iter().any(|w| {
            matches!(&w.issue, WarningIssue::IgnoredEosConfigKey(_)) && w.path.to_string() == "key2"
        }));
    }

    #[test]
    fn validate_avd_design_without_eos_config_keys_no_warning() {
        // Test that when validating AVD Design with only valid AVD Design keys,
        // no warning is emitted even with warn_eos_config_keys enabled.
        let store = get_test_store();
        let input = serde_json::json!({
            "key3": "valid_avd_design_key"
        });

        let configuration = Configuration {
            warn_eos_config_keys: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let schema = store.get("avd_design").unwrap();
        let _ = schema.validate(&input, &mut ctx);

        // Should have no warnings
        assert!(ctx.result.warnings.is_empty());
    }

    #[test]
    fn validate_eos_config_no_warning() {
        // Test that when validating EOS Config, no warning is emitted
        // (the warn_eos_config_keys flag is only used when validating AVD Design).
        // AVD Design keys are ignored.
        let store = get_test_store();
        let input = serde_json::json!({
            "key1": "valid_key",
            "key2": "another_valid_key",
            "key3": "valid_avd_design_key",
        });

        // Don't set warn_eos_config_keys since we're validating eos_config
        let mut ctx = Context::new(None);
        let schema = store.get("eos_config").unwrap();
        let _ = schema.validate(&input, &mut ctx);

        // Should have no warnings
        assert!(ctx.result.warnings.is_empty());
    }

    #[test]
    fn validate_avd_design_with_shared_key_no_warning() {
        // Test that when a key exists in BOTH AVD Design and EOS Config,
        // no warning is emitted - the key should be validated normally.
        let store = get_test_store();
        let input = serde_json::json!({
            "key3": "shared_key_value"  // key3 exists in both schemas
        });

        let configuration = Configuration {
            warn_eos_config_keys: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let schema = store.get("avd_design").unwrap();
        let _ = schema.validate(&input, &mut ctx);

        // Should have no warnings since key3 exists in both schemas
        assert!(ctx.result.warnings.is_empty());
    }

    #[test]
    fn validate_avd_design_with_eos_cli_config_gen_role_keys_no_warning() {
        // Test that the special eos_cli_config_gen role keys are ignored without warnings.
        let store = get_test_store();
        let input = serde_json::json!({
            "key3": "valid_avd_design_key",
            "avd_structured_config_file_format": "should be ignored",
            "custom_templates": "should be ignored",
            "eos_cli_config_gen_configuration": "should be ignored",
            "eos_cli_config_gen_documentation": "should be ignored",
            "eos_cli_config_gen_keep_tmp_files": "should be ignored",
            "eos_cli_config_gen_tmp_dir": "should be ignored",
            "eos_cli_config_gen_validate_inputs_batch_size": "should be ignored",
            "read_structured_config_from_file": "should be ignored",
        });

        let configuration = Configuration {
            warn_eos_config_keys: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
        let schema = store.get("avd_design").unwrap();
        let _ = schema.validate(&input, &mut ctx);

        // Should have no warnings - these special keys are silently ignored
        assert!(ctx.result.warnings.is_empty());
        // Should have no errors either
        assert!(ctx.result.errors.is_empty());
    }

    #[test]
    fn validate_no_schema_keys_preserves_input_when_coercing() {
        let schema = SourceDict::default();
        let input = serde_json::json!({
            "foo": 123,
            "nested": {
                "bar": true
            }
        });

        let configuration = Configuration {
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));

        let coerced = schema.validate(&input, &mut ctx);

        assert!(ctx.result.errors.is_empty());
        assert_eq!(coerced, Some(input));
    }

    #[test]
    fn validate_dynamic_key_deprecated_ok() {
        let schema: SourceDict = SourceDict::deserialize(serde_json::json!({
            "keys": {
                "my_dynamic_keys": {
                    "type": "list",
                    "items": {
                        "type": "dict",
                        "keys": {
                            "key": {
                                "type": "str"
                            }
                        }
                    }
                }
            },
            "dynamic_keys": {
                "my_dynamic_keys.key": {
                    "type": "int",
                    "deprecation": {
                        "warning": true,
                        "remove_in_version": "1.2.3"
                    }
                }
            },
            "allow_other_keys": true
        }))
        .unwrap();

        let input = serde_json::json!({
            "my_dynamic_keys": [{"key": "dynkey1"}],
            "dynkey1": 5
        });

        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);

        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.warnings,
            vec![Feedback {
                path: vec!["dynkey1".into()].into(),
                span: None,
                issue: WarningIssue::Deprecated(Deprecated {
                    path: vec!["dynkey1".into()].into(),
                    replacement: None.into(),
                    version: Some("1.2.3".into()).into(),
                    url: None.into(),
                })
            }]
        );
    }
}
