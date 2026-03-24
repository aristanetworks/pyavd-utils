// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{
    any::{AnySchema, Shortcuts as _},
    dict::Dict,
    resolve_ref,
};
use ordermap::OrderMap;

use crate::{
    context::Context,
    feedback::{Deprecated, IgnoredEosConfigKey, Removed, Type, Violation},
    validatable::{ValidatableMapping, ValidatableSequence, ValidatableValue},
};

use super::Validation;

// This must be kept up to date when adding role keys in eos_config schema.
// TODO: Eventually this will go away as we stop warning.
const EOS_CLI_CONFIG_GEN_ROLE_KEYS: [&str; 8] = [
    "avd_structured_config_file_format",
    "custom_templates",
    "eos_cli_config_gen_configuration",
    "eos_cli_config_gen_documentation",
    "eos_cli_config_gen_keep_tmp_files",
    "eos_cli_config_gen_tmp_dir",
    "eos_cli_config_gen_validate_inputs_batch_size",
    "read_structured_config_from_file",
];

impl Validation for Dict {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        if let Some(mapping) = value.as_mapping() {
            validate_ref(self, value, ctx);
            let coerced_items = validate_keys(self, &mapping, ctx);
            validate_required_keys(self, value, &mapping, ctx);
            coerced_items.map(|items| value.coerce_mapping(items))
        } else if value.is_null() && !ctx.configuration.restrict_null_values {
            ctx.configuration
                .return_coerced_data
                .then(|| value.coerce_null())
        } else {
            ctx.add_error_for(
                value,
                Violation::InvalidType {
                    expected: Type::Dict,
                    found: value.value_type(),
                },
            );
            None
        }
    }
}

/// Validation of ref which will not merge in the schema, so it only works as expected
/// when there are no local variables set. In practice this is only used for
/// structured_config, where we $ref in the full eos_config schema.
fn validate_ref<V: ValidatableValue>(schema: &Dict, value: &V, ctx: &mut Context) {
    if let Some(ref_) = schema.base.schema_ref.as_ref() {
        // Ignoring not being able to resolve the schema.
        // Ignoring a wrong schema type at the ref. Since Validation is infallible.
        // TODO: What to do?
        if let Ok(AnySchema::Dict(ref_schema)) = resolve_ref(ref_, ctx.store) {
            // Handle relaxed validation here, since the places we use it is also where we skip resolving the $ref before validation.
            let previous_relaxed_validation = ctx.state.relaxed_validation;
            if schema.relaxed_validation.unwrap_or_default() {
                ctx.state.relaxed_validation = true
            }
            let _ = ref_schema.validate(value, ctx);
            ctx.state.relaxed_validation = previous_relaxed_validation;
        }
    }
}

// === Generic versions for ValidatableValue ===

/// Check for deprecation settings in the given schema and return a bool if there was an error that should stop further validation.
fn check_deprecation<'a, M: ValidatableMapping<'a>>(
    _key: &str,
    key_schema: &AnySchema,
    input_value: &M::Value,
    parent_dict_input: &M,
    ctx: &mut Context,
) -> bool {
    if let Some(deprecation) = key_schema.deprecation()
        && deprecation.warning
    {
        if deprecation.removed.unwrap_or_default() {
            ctx.add_error_for(
                input_value,
                Violation::Removed(Removed::from_schema(&ctx.state.path, deprecation)),
            );
            true
        } else {
            ctx.add_warning_for(
                input_value,
                Deprecated::from_schema(&ctx.state.path, deprecation),
            );
            if !deprecation.allow_with_new_key.unwrap_or_default()
                && let Some(schema_new_key) = deprecation.new_key.as_ref()
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
                            root_value.path_exists(&rest_of_path.join("."))
                        };
                        if exists {
                            ctx.add_error_for(
                                input_value,
                                Violation::DeprecatedConflict {
                                    other_path: new_key.into(),
                                    url: deprecation.url.to_owned().into(),
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

/// Check if an input key matches a dynamic key from the schema.
/// Returns the schema for the dynamic key if found.
fn find_dynamic_key_schema<'a, 'b, M: ValidatableMapping<'b>>(
    schema: &'a Dict,
    input: &M,
    input_key: &str,
) -> Option<&'a AnySchema> {
    let dynamic_keys = schema.dynamic_keys.as_ref()?;

    // Get defaults for dynamic keys (lazily initialized)
    let default_dynamic_keys = schema
        .default_dynamic_keys
        .get_or_init(|| schema.init_default_dynamic_keys());

    for (dynamic_key_path, dynamic_key_schema) in dynamic_keys {
        if dynamic_key_schema.is_removed() {
            continue;
        }

        // First check if the input key is in the input at this path
        if has_dynamic_key_value(input, dynamic_key_path, input_key) {
            return Some(dynamic_key_schema);
        }

        // If not found in input, check the defaults
        if let Some(defaults) = default_dynamic_keys.as_ref()
            && let Some(default_keys) = defaults.get(dynamic_key_path)
            && default_keys.contains(&input_key.to_string())
        {
            return Some(dynamic_key_schema);
        }
    }
    None
}

/// Check if the input has a value at the given path that matches the target key.
fn has_dynamic_key_value<'a, M: ValidatableMapping<'a>>(
    input: &M,
    path: &str,
    target_key: &str,
) -> bool {
    let mut path_parts = path.split('.');
    let Some(first_key) = path_parts.next() else {
        return false;
    };

    let Some(value) = input.get(first_key) else {
        return false;
    };

    // Collect remaining path
    let rest: Vec<_> = path_parts.collect();

    if rest.is_empty() {
        // If it's a sequence, check all items
        if let Some(seq) = value.as_sequence() {
            for item in ValidatableSequence::iter(&seq) {
                if let Some(s) = item.as_str()
                    && s == target_key
                {
                    return true;
                }
            }
            return false;
        }
        // Single value
        if let Some(s) = value.as_str() {
            return s == target_key;
        }
        return false;
    }

    // Navigate further
    let rest_path = rest.join(".");
    has_nested_dynamic_key_value(value, &rest_path, target_key)
}

/// Recursively check for a dynamic key value at a nested path.
fn has_nested_dynamic_key_value<V: ValidatableValue>(
    value: &V,
    path: &str,
    target_key: &str,
) -> bool {
    let mut path_parts = path.split('.');
    let Some(first_key) = path_parts.next() else {
        // At the end of the path, check if value matches
        if let Some(s) = value.as_str() {
            return s == target_key;
        }
        return false;
    };

    let rest: Vec<_> = path_parts.collect();
    let rest_path = rest.join(".");

    // If it's a sequence, check each item
    if let Some(seq) = value.as_sequence() {
        for item in ValidatableSequence::iter(&seq) {
            if let Some(child) = item.get(first_key) {
                if rest.is_empty() {
                    if let Some(s) = child.as_str()
                        && s == target_key
                    {
                        return true;
                    }
                } else if has_nested_dynamic_key_value(child, &rest_path, target_key) {
                    return true;
                }
            }
        }
        return false;
    }

    // If it's a mapping, look up the key
    if let Some(child) = value.get(first_key) {
        if rest.is_empty() {
            if let Some(s) = child.as_str() {
                return s == target_key;
            }
            return false;
        }
        return has_nested_dynamic_key_value(child, &rest_path, target_key);
    }

    false
}

/// Validate and optionally coerce mapping keys.
/// Returns Some(coerced_items) when coercion is enabled, None otherwise.
fn validate_keys<'a, M: ValidatableMapping<'a>>(
    schema: &Dict,
    input: &M,
    ctx: &mut Context,
) -> Option<Vec<(String, <M::Value as ValidatableValue>::Coerced)>> {
    let return_coerced = ctx.configuration.return_coerced_data;
    let mut coerced_items = if return_coerced {
        Some(Vec::new())
    } else {
        None
    };

    let Some(keys) = &schema.keys else {
        // No schema keys - preserve all input as-is when coercing
        if let Some(ref mut items) = coerced_items {
            for (input_key, input_value) in input.iter() {
                items.push((input_key.into_owned(), input_value.clone_to_coerced()));
            }
        }
        return coerced_items;
    };

    // When at the root level, if warn_eos_config_keys is enabled, get the keys from the eos_config schema.
    let eos_config_keys: Option<&OrderMap<String, AnySchema>> = {
        if ctx.state.path.is_empty()
            && ctx.configuration.warn_eos_config_keys
            && let Ok(AnySchema::Dict(eos_config_schema)) = ctx.store.get("eos_config")
        {
            eos_config_schema.keys.as_ref()
        } else {
            None
        }
    };

    for (input_key, input_value) in input.iter() {
        let input_key_str: &str = &input_key;
        ctx.state.path.push(input_key_str.to_owned());

        // Determine what to do with this key
        let include_in_output = if let Some(key_schema) = keys.get(input_key_str) {
            if !check_deprecation(input_key_str, key_schema, input_value, input, ctx) {
                if let Some(ref mut items) = coerced_items {
                    let coerced = key_schema
                        .validate(input_value, ctx)
                        .unwrap_or_else(|| input_value.clone_to_coerced());
                    items.push((input_key_str.to_owned(), coerced));
                } else {
                    let _ = key_schema.validate(input_value, ctx);
                }
            } else if let Some(ref mut items) = coerced_items {
                // Deprecated key with error - still include with original value
                items.push((input_key_str.to_owned(), input_value.clone_to_coerced()));
            }
            false // Already handled
        } else if let Some(key_schema) = find_dynamic_key_schema(schema, input, input_key_str) {
            if !check_deprecation(input_key_str, key_schema, input_value, input, ctx) {
                if let Some(ref mut items) = coerced_items {
                    let coerced = key_schema
                        .validate(input_value, ctx)
                        .unwrap_or_else(|| input_value.clone_to_coerced());
                    items.push((input_key_str.to_owned(), coerced));
                } else {
                    let _ = key_schema.validate(input_value, ctx);
                }
            } else if let Some(ref mut items) = coerced_items {
                items.push((input_key_str.to_owned(), input_value.clone_to_coerced()));
            }
            false // Already handled
        } else if input_key_str.starts_with("_") {
            // Key starts with underscore - skip validation but include in output
            true
        } else if !schema.allow_other_keys.unwrap_or_default() {
            // Key is not part of the schema and does not start with underscore
            ctx.add_error_for(input_value, Violation::UnexpectedKey());
            true // Include the value in output (error is recorded)
        } else {
            if let Some(eos_config_keys) = &eos_config_keys
                && eos_config_keys.contains_key(input_key_str)
                && !EOS_CLI_CONFIG_GEN_ROLE_KEYS.contains(&input_key_str)
            {
                // Key is not in avd_design schema but is in eos_config_keys
                // and allow_other_keys is true - emit a warning that it will be ignored
                ctx.add_warning_for(input_value, IgnoredEosConfigKey {});
            }
            true // allow_other_keys is true - include as-is
        };

        if include_in_output && let Some(ref mut items) = coerced_items {
            items.push((input_key_str.to_owned(), input_value.clone_to_coerced()));
        }

        ctx.state.path.pop();
    }

    coerced_items
}

fn validate_required_keys<'a, M: ValidatableMapping<'a>>(
    schema: &Dict,
    value: &M::Value,
    input: &M,
    ctx: &mut Context,
) {
    // Don't validate required keys if we are below a dict with relaxed validation or if we are at the root level.
    if ctx.state.relaxed_validation
        || (ctx.configuration.ignore_required_keys_on_root_dict && ctx.state.path.is_empty())
    {
        return;
    }
    if let Some(keys) = &schema.keys {
        for (key, key_schema) in keys {
            if key_schema.is_required() && !input.contains_key(key) {
                ctx.add_error_for(
                    value,
                    Violation::MissingRequiredKey {
                        key: key.to_string(),
                    },
                );
            }
        }
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
    use crate::context::{Configuration, Context};
    use crate::feedback::{CoercionNote, Feedback, WarningIssue};
    use crate::validation::test_utils::get_test_store;

    #[test]
    fn validate_type_ok() {
        let schema = Dict::default();
        let input = serde_json::json!({ "foo": true });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = Dict::default();
        let input = serde_json::json!(true);
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
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
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
        let mut ctx = Context::new(&store, None);
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
        let input = serde_json::json!({ "foo": 321, "bar": "123" });
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration));
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
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
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
        let mut ctx = Context::new(&store, None);
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
        let input = serde_json::json!({ "dynkey1": 5, "dynkey2": 9 });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
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
        let input =
            serde_json::json!({ "dynkey1": {"sub_key": 11, "bad_key": true}, "dynkey2": "wrong" });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
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
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_key_allowed_err() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
            ..Default::default()
        };
        let input = serde_json::json!({ "foo": "ok", "foo1": "wrong" });
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["foo1".into()].into(),
                span: None,
                issue: Violation::UnexpectedKey().into()
            }]
        )
    }

    /// Test that keys starting with underscore are preserved in output but not validated
    #[test]
    fn validate_underscore_key_preserved() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
            ..Default::default()
        };
        // _internal key should be preserved but not validated, foo1 should error
        let input = serde_json::json!({ "foo": "ok", "_internal": {"nested": "data"} });
        let store = get_test_store();
        let configuration = Configuration {
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration));
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
        let schema: Dict = Dict::deserialize(serde_json::json!({
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
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
        // With lenient validation, int 123 coerces to string "123"
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
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["foo".into()].into(),
                span: None,
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
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert!(ctx.result.warnings.is_empty());
        assert!(ctx.result.errors.is_empty());
    }

    // Tests that when allow_with_new_key is true, using both the deprecated key
    // and the new key simultaneously does NOT produce a DeprecatedConflict error.
    #[test]
    fn validate_key_deprecated_with_allow_with_new_key_ok() {
        let schema: Dict = Dict::deserialize(serde_json::json!({
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
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
        let schema: Dict = Dict::deserialize(serde_json::json!({
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
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
        let schema: Dict = Dict::deserialize(serde_json::json!({
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
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
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
        // Bool input for a Str field - coerced to "True"
        let input = serde_json::json!({ "foo": true });
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration));
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
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
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
        let input = serde_json::json!({});
        let store = get_test_store();
        let configuration = Configuration {
            ignore_required_keys_on_root_dict: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration));
        let _ = schema.validate(&input, &mut ctx);
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
        let mut ctx = Context::new(&store, Some(&configuration));
        // Using a deeper path and see that we still get the error even though we relax for the root dict.
        ctx.state.path.push("deeper".into());
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["deeper".into()].into(),
                span: None,
                issue: Violation::MissingRequiredKey { key: "foo".into() }.into()
            }]
        )
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
        let mut ctx = Context::new(&store, Some(&configuration));
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
        let mut ctx = Context::new(&store, Some(&configuration));
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
        let mut ctx = Context::new(&store, None);
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
        let mut ctx = Context::new(&store, Some(&configuration));
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
        let mut ctx = Context::new(&store, Some(&configuration));
        let schema = store.get("avd_design").unwrap();
        let _ = schema.validate(&input, &mut ctx);

        // Should have no warnings - these special keys are silently ignored
        assert!(ctx.result.warnings.is_empty());
        // Should have no errors either
        assert!(ctx.result.errors.is_empty());
    }
}
