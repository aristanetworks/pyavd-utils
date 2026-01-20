// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{
    Schema, Walker as _,
    any::{AnySchema, Shortcuts as _},
    dict::Dict,
    resolve_ref,
};
use ordermap::OrderMap;
use serde_json::{Map, Value};

use crate::{
    context::Context,
    feedback::{Deprecated, IgnoredEosConfigKey, Removed, Type, Violation},
};

use super::Validation;

impl Validation<Map<String, Value>> for Dict {
    fn validate(&self, value: &Map<String, Value>, ctx: &mut Context) {
        validate_keys(self, value, ctx);
        validate_required_keys(self, value, ctx);
        self.validate_ref(value, ctx);
    }

    fn validate_value(&self, value: &Value, ctx: &mut Context) {
        if let Some(v) = value.as_object() {
            self.validate(v, ctx)
        } else if value.is_null() && !ctx.configuration.restrict_null_values {
        } else {
            ctx.add_error(Violation::InvalidType {
                expected: Type::Dict,
                found: value.into(),
            })
        }
    }

    fn validate_ref(&self, value: &Map<String, Value>, ctx: &mut Context) {
        if let Some(ref_) = self.base.schema_ref.as_ref() {
            // Ignoring not being able to resolve the schema.
            // Ignoring a wrong schema type at the ref. Since Validation is infallible.
            // TODO: What to do?
            if let Ok(AnySchema::Dict(ref_schema)) = resolve_ref(ref_, ctx.store) {
                // Handle relaxed validation here, since the places we use it is also where we skip resolving the $ref before validation.
                let previous_relaxed_validation = ctx.state.relaxed_validation;
                if self.relaxed_validation.unwrap_or_default() {
                    ctx.state.relaxed_validation = true
                }
                ref_schema.validate(value, ctx);
                ctx.state.relaxed_validation = previous_relaxed_validation;
            }
        }
    }
}

fn get_dynamic_keys_schemas<'a>(
    schema: &'a Dict,
    input: &'a Map<String, Value>,
) -> OrderMap<String, &'a AnySchema> {
    schema
        .get_dynamic_keys(input)
        .map(|dynamic_keys| {
            dynamic_keys
                .into_iter()
                .map(|(key, dynamic_key_info)| (key, dynamic_key_info.schema))
                .collect()
        })
        .unwrap_or_default()
}

/// Check if we're validating eos_designs at the root level and if any input keys
/// are top-level keys from eos_cli_config_gen schema but NOT in eos_designs schema.
/// If so, emit a warning that the key will be ignored.
fn check_for_eos_cli_config_gen_keys(input: &Map<String, Value>, ctx: &mut Context) {
    // Get the top-level keys from both schemas using the store helper
    let eos_cli_config_gen_keys = ctx.store.get_keys(Schema::EosCliConfigGen);
    let eos_designs_keys = ctx.store.get_keys(Schema::EosDesigns);

    // If we can't get the keys from eos_cli_config_gen, nothing to check
    let Some(eos_cli_config_gen_keys) = eos_cli_config_gen_keys else {
        return;
    };

    // Check each input key
    for input_key in input.keys() {
        // Only warn if the key exists in eos_cli_config_gen but NOT in eos_designs
        if eos_cli_config_gen_keys.contains_key(input_key) {
            let exists_in_eos_designs = eos_designs_keys
                .map(|keys| keys.contains_key(input_key))
                .unwrap_or(false);

            if !exists_in_eos_designs {
                // Found an eos_cli_config_gen-only key in eos_designs input
                ctx.add_warning(IgnoredEosConfigKey {
                    key: input_key.to_owned(),
                });
            }
        }
    }
}

fn validate_keys(schema: &Dict, input: &Map<String, Value>, ctx: &mut Context) {
    let Some(keys) = &schema.keys else { return };

    // Check for eos_cli_config_gen keys in eos_designs input at root level
    if ctx.state.path.is_empty() && ctx.state.schema_name == Some(Schema::EosDesigns) {
        check_for_eos_cli_config_gen_keys(input, ctx);
    }

    let dynamic_keys_schemas = get_dynamic_keys_schemas(schema, input);

    input.iter().for_each(|(input_key, input_value)| {
        ctx.state.path.push(input_key.to_owned());
        if let Some(key_schema) = keys.get(input_key) {
            if !check_deprecation(input_key, key_schema, input, ctx) {
                key_schema.validate(input_value, ctx);
            }
        } else if let Some(key_schema) = dynamic_keys_schemas.get(input_key) {
            if !check_deprecation(input_key, key_schema, input, ctx) {
                key_schema.validate(input_value, ctx);
            }
        } else if !schema.allow_other_keys.unwrap_or_default() && !input_key.starts_with("_") {
            // Key is not part of the schema and does not start with underscore
            ctx.add_error(Violation::UnexpectedKey());
        }
        ctx.state.path.pop();
    });
}

fn validate_required_keys(schema: &Dict, input: &Map<String, Value>, ctx: &mut Context) {
    // Don't validate required keys if we are below a dict with relaxed validation or if we are at the root level.
    if ctx.state.relaxed_validation
        || (ctx.configuration.ignore_required_keys_on_root_dict && ctx.state.path.is_empty())
    {
        return;
    }
    if let Some(keys) = &schema.keys {
        for (key, key_schema) in keys {
            if key_schema.is_required() && !input.contains_key(key) {
                ctx.add_error(Violation::MissingRequiredKey {
                    key: key.to_string(),
                });
            }
        }
    }
}

/// Check for deprecation settings in the given schema and return a bool if there was an error that should stop further validation.
fn check_deprecation(
    _key: &str,
    key_schema: &AnySchema,
    parent_dict_input: &Map<String, Value>,
    ctx: &mut Context,
) -> bool {
    if let Some(deprecation) = key_schema.deprecation()
        && deprecation.warning
    {
        if deprecation.removed.unwrap_or_default() {
            ctx.add_error(Violation::Removed(Removed::from_schema(
                &ctx.state.path,
                deprecation,
            )));
            true
        } else {
            ctx.add_warning(Deprecated::from_schema(&ctx.state.path, deprecation));
            if let Some(schema_new_key) = deprecation.new_key.as_ref() {
                // Split the new_key on ' or ' in case of multiple new keys.
                // Then check if any of the new keys are set in the inputs at the same time as the deprecated key,
                // adding conflict errors if found
                schema_new_key.split(" or ").for_each(|new_key| {
                    let mut path = new_key.split('.');
                    if let Some(root_key) = path.next()
                        && let Some((key, value)) = parent_dict_input.get_key_value(root_key)
                        && value
                            .walk(path, Some(&mut vec![key.to_string()]))
                            .into_iter()
                            .next()
                            .is_some()
                    {
                        ctx.add_error(Violation::DeprecatedConflict {
                            other_path: new_key.into(),
                            url: deprecation.url.to_owned().into(),
                        });
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

#[cfg(test)]
mod tests {
    use avdschema::base::Base;
    use avdschema::int::Int;
    use avdschema::list::List;
    use avdschema::str::Str;
    use ordermap::OrderMap;
    use serde::Deserialize as _;

    use super::*;
    use crate::coercion::Coercion as _;
    use crate::context::{Configuration, Context};
    use crate::feedback::{CoercionNote, Feedback, WarningIssue};
    use crate::validation::test_utils::get_test_store;

    #[test]
    fn validate_type_ok() {
        let schema = Dict::default();
        let input = serde_json::json!({ "foo": true });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = Dict::default();
        let input = serde_json::json!(true);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
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
        let schema = Dict {
            keys: Some(OrderMap::from_iter([
                ("foo".into(), Str::default().into()),
                ("bar".into(), Int::default().into()),
            ])),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": "bar", "bar": 123 });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_key_type_err() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([
                ("foo".into(), Str::default().into()),
                ("bar".into(), Int::default().into()),
            ])),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": [], "bar": "boo" });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["foo".into()].into(),
                    issue: Violation::InvalidType {
                        expected: Type::Str,
                        found: Type::List
                    }
                    .into()
                },
                Feedback {
                    path: vec!["bar".into()].into(),
                    issue: Violation::InvalidType {
                        expected: Type::Int,
                        found: Type::Str
                    }
                    .into()
                }
            ]
        )
    }

    #[test]
    fn validate_key_type_coerced_ok() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([
                ("foo".into(), Str::default().into()),
                ("bar".into(), Int::default().into()),
            ])),
            ..Default::default()
        };
        let mut input = serde_json::json!({ "foo": 321, "bar": "123" });
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration), Schema::EosDesigns);
        schema.coerce(&mut input, &mut ctx);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.infos,
            vec![
                Feedback {
                    path: vec!["foo".into()].into(),
                    issue: CoercionNote {
                        found: 321.into(),
                        made: "321".into()
                    }
                    .into()
                },
                Feedback {
                    path: vec!["bar".into()].into(),
                    issue: CoercionNote {
                        found: "123".into(),
                        made: 123.into()
                    }
                    .into()
                }
            ]
        )
    }

    #[test]
    fn validate_dynamic_keys_ok() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                List {
                    items: Some(Box::new(
                        Dict {
                            keys: Some(OrderMap::from_iter([(
                                "key".into(),
                                Str::default().into(),
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
                Int {
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert_eq!(ctx.result.errors, vec![]);
        assert_eq!(ctx.result.infos, vec![]);
    }

    #[test]
    fn validate_dynamic_keys_err() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                List {
                    items: Some(Box::new(
                        Dict {
                            keys: Some(OrderMap::from_iter([(
                                "key".into(),
                                Str::default().into(),
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
                Int {
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert_eq!(ctx.result.infos, vec![]);
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["dynkey1".into()].into(),
                    issue: Violation::ValueAboveMaximum {
                        maximum: 10,
                        found: 11
                    }
                    .into()
                },
                Feedback {
                    path: vec!["dynkey2".into()].into(),
                    issue: Violation::InvalidType {
                        expected: Type::Int,
                        found: Type::Str
                    }
                    .into()
                }
            ]
        )
    }

    #[test]
    fn validate_dynamic_keys_from_defaults_ok() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                List {
                    items: Some(Box::new(Str::default().into())),
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
                Int {
                    max: Some(10),
                    ..Default::default()
                }
                .into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let mut input = serde_json::json!({ "dynkey1": 5, "dynkey2": 9 });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.coerce(&mut input, &mut ctx);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_dynamic_keys_from_defaults_err() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "my_dynamic_keys".into(),
                List {
                    items: Some(Box::new(Str::default().into())),
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
                Dict {
                    keys: Some(OrderMap::from_iter([(
                        "sub_key".into(),
                        Int {
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
        let mut input =
            serde_json::json!({ "dynkey1": {"sub_key": 11, "bad_key": true}, "dynkey2": "wrong" });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.coerce(&mut input, &mut ctx);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["dynkey1".into(), "sub_key".into()].into(),
                    issue: Violation::ValueAboveMaximum {
                        maximum: 10,
                        found: 11
                    }
                    .into()
                },
                Feedback {
                    path: vec!["dynkey1".into(), "bad_key".into()].into(),
                    issue: Violation::UnexpectedKey {}.into()
                },
                Feedback {
                    path: vec!["dynkey2".into()].into(),
                    issue: Violation::InvalidType {
                        expected: Type::Dict,
                        found: Type::Str
                    }
                    .into()
                }
            ]
        )
    }

    #[test]
    fn validate_key_allowed_ok() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
            allow_other_keys: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": "ok", "foo1": "wrong" });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_key_allowed_err() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": "ok", "foo1": "wrong", "_internal": "ignored" });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["foo1".into()].into(),
                issue: Violation::UnexpectedKey().into()
            }]
        )
    }

    // Tests a key that is marked as deprecated returns the proper warning.
    // Also verifies that regular validation is still done on the field even if it is deprecated.
    #[test]
    fn validate_key_deprecated_ok() {
        let schema: Dict = Dict::deserialize(serde_json::json!({
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
        let input = serde_json::json!({"foo": 123});
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.warnings,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                issue: WarningIssue::Deprecated(Deprecated {
                    path: vec!["foo".into()].into(),
                    replacement: None.into(),
                    version: Some("1.2.3".into()).into(),
                    url: None.into()
                })
            }]
        );
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                issue: Violation::InvalidType {
                    expected: Type::Str,
                    found: Type::Int
                }
                .into()
            }]
        )
    }

    // Tests a key that is marked as removed returns the proper error.
    // Also verifies that no other validation is done on the field,
    // notice the type is wrong in our input but no type error is returned.
    #[test]
    fn validate_key_removed_err() {
        let schema: Dict = Dict::deserialize(serde_json::json!({
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                issue: Violation::Removed(Removed {
                    path: vec!["foo".into()].into(),
                    replacement: None.into(),
                    version: None.into(),
                    url: None.into()
                })
                .into()
            }]
        )
    }

    // Tests a key that is marked as deprecated but where warning is disabled
    // does not return any warning.
    #[test]
    fn validate_key_deprecated_no_warning_ok() {
        let schema: Dict = Dict::deserialize(serde_json::json!({
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert!(ctx.result.warnings.is_empty());
        assert!(ctx.result.errors.is_empty());
    }

    #[test]
    fn validate_key_required_ok() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                Str {
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
        let mut input = serde_json::json!({ "foo": true });
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration), Schema::EosDesigns);
        schema.coerce(&mut input, &mut ctx);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.infos,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                issue: CoercionNote {
                    found: true.into(),
                    made: "True".into()
                }
                .into()
            }]
        )
    }

    #[test]
    fn validate_key_required_err() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                Str {
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                issue: Violation::MissingRequiredKey { key: "foo".into() }.into()
            }]
        )
    }

    #[test]
    fn validate_key_required_relaxed_root_dict_ok() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                Str {
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
        let mut input = serde_json::json!({});
        let store = get_test_store();
        let configuration = Configuration {
            ignore_required_keys_on_root_dict: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration), Schema::EosDesigns);
        schema.coerce(&mut input, &mut ctx);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_key_required_relaxed_root_dict_err() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                Str {
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
        let store = get_test_store();
        let configuration = Configuration {
            ignore_required_keys_on_root_dict: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration), Schema::EosDesigns);
        // Using a deeper path and see that we still get the error even though we relax for the root dict.
        ctx.state.path.push("deeper".into());
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["deeper".into()].into(),
                issue: Violation::MissingRequiredKey { key: "foo".into() }.into()
            }]
        )
    }

    #[test]
    fn validate_eos_designs_with_eos_cli_config_gen_keys_warning() {
        // Test that when validating eos_designs, if a top-level key from eos_cli_config_gen
        // is present in the input, a warning is emitted.
        let store = get_test_store();
        let input = serde_json::json!({
            "key3": "valid_eos_designs_key",
            "key1": "this_is_an_eos_cli_config_gen_key",
            "key2": "another_eos_cli_config_gen_key"
        });

        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        let schema = store.get(Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);

        // Should have warnings for key1 and key2
        assert_eq!(ctx.result.warnings.len(), 2);
        assert!(ctx.result.warnings.iter().any(|w| {
            matches!(&w.issue, WarningIssue::IgnoredEosConfigKey(ignored) if ignored.key == "key1")
        }));
        assert!(ctx.result.warnings.iter().any(|w| {
            matches!(&w.issue, WarningIssue::IgnoredEosConfigKey(ignored) if ignored.key == "key2")
        }));
    }

    #[test]
    fn validate_eos_designs_without_eos_cli_config_gen_keys_no_warning() {
        // Test that when validating eos_designs with only valid eos_designs keys,
        // no warning is emitted.
        let store = get_test_store();
        let input = serde_json::json!({
            "key3": "valid_eos_designs_key"
        });

        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        let schema = store.get(Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);

        // Should have no warnings
        assert!(ctx.result.warnings.is_empty());
    }

    #[test]
    fn validate_eos_cli_config_gen_no_warning() {
        // Test that when validating eos_cli_config_gen, no warning is emitted
        // even if keys are present (since we only check when validating eos_designs).
        // 'eos_designs' key are ignored.
        let store = get_test_store();
        let input = serde_json::json!({
            "key1": "valid_key",
            "key2": "another_valid_key",
            "key3": "valid_eos_designs_key",
        });

        let mut ctx = Context::new(&store, None, Schema::EosCliConfigGen);
        let schema = store.get(Schema::EosCliConfigGen);
        schema.validate_value(&input, &mut ctx);

        // Should have no warnings
        assert!(ctx.result.warnings.is_empty());
    }

    #[test]
    fn validate_eos_designs_with_shared_key_no_warning() {
        // Test that when a key exists in BOTH eos_designs and eos_cli_config_gen,
        // no warning is emitted - the key should be validated normally.
        let store = get_test_store();
        let input = serde_json::json!({
            "key3": "shared_key_value"  // key3 exists in both schemas
        });

        let mut ctx = Context::new(&store, None, Schema::EosDesigns);
        let schema = store.get(Schema::EosDesigns);
        schema.validate_value(&input, &mut ctx);

        // Should have no warnings since key3 exists in both schemas
        assert!(ctx.result.warnings.is_empty());
    }
}
