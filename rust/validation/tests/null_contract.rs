// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Null contracts tested through compiled schemas and the public JSON/YAML APIs.
//!
//! Requiredness depends on parent traversal state. These tests deliberately
//! exercise real root fields and reference boundaries, not isolated helpers.

#[cfg(test)]
mod tests {
    use avdschema::Store;
    use serde_json::Value;
    use serde_json::json;
    use validation::Configuration;
    use validation::StoreValidate as _;
    use validation::StoreValidateInput as _;
    use validation::feedback::Type;
    use validation::feedback::Violation;

    fn types() -> [(Value, Type); 5] {
        [
            (json!({"type": "bool"}), Type::Bool),
            (json!({"type": "int"}), Type::Int),
            (json!({"type": "str"}), Type::Str),
            (
                json!({"type": "list", "items": {"type": "str"}}),
                Type::List,
            ),
            (json!({"type": "dict", "keys": {}}), Type::Dict),
        ]
    }

    // Own inline JSON fixtures to keep schema definitions concise at call sites.
    #[allow(
        clippy::needless_pass_by_value,
        reason = "Test helpers accept inline owned fixtures for concise call sites."
    )]
    fn compile(schema: Value) -> Store {
        Store::from_json(&schema.to_string()).expect("contract schema should compile")
    }

    /// Compare exact diagnostics across both adapters and both coercion modes.
    // Own inline JSON and configuration fixtures to keep cases concise.
    #[allow(
        clippy::needless_pass_by_value,
        reason = "Test helpers accept inline owned fixtures for concise call sites."
    )]
    fn check(store: &Store, input: Value, configuration: Configuration, expected: &[(&str, Type)]) {
        for return_coerced_data in [false, true] {
            let configuration = Configuration {
                return_coerced_data,
                ..configuration.clone()
            };
            let serialized = input.to_string();
            // JSON is also YAML: the same document exercises both value adapters.
            let json = store
                .validate_json(&serialized, "test", Some(&configuration))
                .expect("valid JSON input should be accepted by the adapter");
            let yaml = store
                .validate_yaml(&serialized, "test", Some(&configuration))
                .expect("JSON-compatible YAML input should be accepted by the adapter");
            assert!(json.input_diagnostics.is_empty());
            assert!(yaml.input_diagnostics.is_empty());
            assert_eq!(yaml.documents.len(), 1);
            for result in [&json.document.result, &yaml.documents[0].result] {
                assert!(result.warnings.is_empty());
                assert!(result.infos.is_empty());
                assert_eq!(
                    result.errors.len(),
                    expected.len(),
                    "input: {input}; errors: {:?}",
                    result.errors
                );
                for (error, (path, expected_type)) in result.errors.iter().zip(expected) {
                    assert_eq!(error.path.to_string(), *path);
                    assert_eq!(
                        error.issue,
                        Violation::InvalidType {
                            expected: expected_type.clone(),
                            found: Type::Null
                        }
                        .into()
                    );
                }
            }
            // Validation diagnostics do not discard the low-level coerced mapping.
            assert_eq!(
                json.document.coerced,
                return_coerced_data.then(|| input.clone())
            );
            assert_eq!(yaml.documents[0].coerced.is_some(), return_coerced_data);
        }
    }

    /// A schema default does not repair an explicitly supplied null.
    #[test]
    fn defaults_do_not_override_explicit_null_requiredness() {
        for ((mut field, expected), default) in types().into_iter().zip([
            json!(true),
            json!(1),
            json!("default"),
            json!([]),
            json!({}),
        ]) {
            field["default"] = default;
            for required in [false, true] {
                field["required"] = json!(required);
                let store = compile(json!({"test": {"type": "dict", "keys": {"value": field}}}));
                let errors = if required {
                    vec![("value", expected.clone())]
                } else {
                    vec![]
                };
                check(
                    &store,
                    json!({"value": null}),
                    Configuration::default(),
                    &errors,
                );
            }
        }
    }

    #[test]
    fn root_exemption_does_not_exempt_required_fields_in_list_items() {
        for (mut field, expected) in types() {
            field["required"] = json!(true);
            let store = compile(json!({"test": {"type": "dict", "keys": {"items": {
                "type": "list", "items": {"type": "dict", "keys": {"value": field}}
            }}}}));
            check(
                &store,
                json!({"items": [{"value": null}]}),
                Configuration {
                    ignore_required_keys_on_root_dict: true,
                    ..Default::default()
                },
                &[("items[0].value", expected)],
            );
        }
    }

    /// The parent enforces the boundary field; relaxation only exempts descendants.
    #[test]
    fn required_relaxed_reference_cannot_null_out_its_strict_parent_field() {
        let store = compile(json!({
            "base": {"type": "dict", "keys": {"child": {"type": "str", "required": true}}},
            "test": {"type": "dict", "keys": {
                "parent": {"type": "dict", "$ref": "base#", "required": true, "relaxed_validation": true}
            }}
        }));
        check(
            &store,
            json!({"parent": null}),
            Configuration::default(),
            &[("parent", Type::Dict)],
        );
        // The same non-null dictionary is allowed to omit or null its children.
        check(&store, json!({"parent": {}}), Configuration::default(), &[]);
        check(
            &store,
            json!({"parent": {"child": null}}),
            Configuration::default(),
            &[],
        );
    }

    // Own the inline configuration fixture shared across generated cases.
    #[allow(
        clippy::needless_pass_by_value,
        reason = "Test helpers accept inline owned fixtures for concise call sites."
    )]
    fn check_root(required: Option<bool>, configuration: Configuration, reject: bool) {
        for (mut field, expected) in types() {
            if let Some(required) = required {
                field["required"] = json!(required);
            }
            let store = compile(json!({"test": {"type": "dict", "keys": {"value": field}}}));
            let errors = if reject {
                vec![("value", expected)]
            } else {
                vec![]
            };
            check(
                &store,
                json!({"value": null}),
                configuration.clone(),
                &errors,
            );
        }
    }

    #[test]
    fn required_null_is_rejected_for_every_type() {
        check_root(Some(true), Configuration::default(), true);
    }

    #[test]
    fn optional_null_is_accepted_for_every_type() {
        check_root(None, Configuration::default(), false);
        check_root(Some(false), Configuration::default(), false);
    }

    #[test]
    fn restrict_null_values_rejects_optional_and_required_nulls() {
        for required in [None, Some(false), Some(true)] {
            check_root(
                required,
                Configuration {
                    restrict_null_values: true,
                    ..Default::default()
                },
                true,
            );
        }
    }

    #[test]
    fn root_exemption_accepts_required_root_nulls() {
        check_root(
            Some(true),
            Configuration {
                ignore_required_keys_on_root_dict: true,
                ..Default::default()
            },
            false,
        );
    }

    /// The root-key exemption does not exempt the root dictionary itself.
    #[test]
    fn root_exemption_does_not_exempt_required_null_root_dict() {
        let store = compile(json!({"test": {"type": "dict", "required": true, "keys": {}}}));
        let configuration = Configuration {
            ignore_required_keys_on_root_dict: true,
            ..Default::default()
        };
        let json = store
            .validate_json("null", "test", Some(&configuration))
            .expect("valid JSON input should be accepted by the adapter");
        let yaml = store
            .validate_yaml("null", "test", Some(&configuration))
            .expect("valid YAML input should be accepted by the adapter");
        assert!(json.input_diagnostics.is_empty());
        assert!(yaml.input_diagnostics.is_empty());
        assert_eq!(yaml.documents.len(), 1);
        for result in [&json.document.result, &yaml.documents[0].result] {
            assert_eq!(result.errors.len(), 1);
            assert_eq!(
                Vec::<String>::from(result.errors[0].path.clone()),
                Vec::<String>::new()
            );
            assert_eq!(
                result.errors[0].issue,
                Violation::InvalidType {
                    expected: Type::Dict,
                    found: Type::Null,
                }
                .into()
            );
        }
    }

    #[test]
    fn restrict_null_values_takes_precedence_over_root_exemption() {
        check_root(
            Some(true),
            Configuration {
                ignore_required_keys_on_root_dict: true,
                restrict_null_values: true,
                ..Default::default()
            },
            true,
        );
    }

    #[test]
    fn root_exemption_does_not_exempt_nested_required_nulls() {
        for (mut field, expected) in types() {
            field["required"] = json!(true);
            let store = compile(json!({"test": {"type": "dict", "keys": {
                "parent": {"type": "dict", "keys": {"value": field}}
            }}}));
            check(
                &store,
                json!({"parent": {"value": null}}),
                Configuration {
                    ignore_required_keys_on_root_dict: true,
                    ..Default::default()
                },
                &[("parent.value", expected)],
            );
        }
    }

    /// Leaving the relaxed reference must restore strict validation for its sibling.
    #[test]
    fn relaxed_reference_accepts_required_nulls_without_leaking_to_siblings() {
        for (mut field, expected) in types() {
            field["required"] = json!(true);
            let store = compile(json!({
                "base": {"type": "dict", "keys": {"value": field}},
                "test": {"type": "dict", "keys": {
                    "relaxed": {"type": "dict", "$ref": "base#", "relaxed_validation": true},
                    "strict": {"type": "dict", "$ref": "base#"}
                }}
            }));
            check(
                &store,
                json!({"relaxed": {"value": null}, "strict": {"value": null}}),
                Configuration::default(),
                &[("strict.value", expected)],
            );
        }
    }

    #[test]
    fn restrict_null_values_takes_precedence_over_relaxed_validation() {
        for (mut field, expected) in types() {
            for required in [false, true] {
                field["required"] = json!(required);
                let store = compile(json!({
                    "base": {"type": "dict", "keys": {"value": field}},
                    "test": {"type": "dict", "keys": {
                        "relaxed": {"type": "dict", "$ref": "base#", "relaxed_validation": true}
                    }}
                }));
                check(
                    &store,
                    json!({"relaxed": {"value": null}}),
                    Configuration {
                        restrict_null_values: true,
                        ..Default::default()
                    },
                    &[("relaxed.value", expected.clone())],
                );
            }
        }
    }

    #[test]
    fn missing_keys_follow_parent_exemptions_without_changing_error_kind() {
        for (mut field, _) in types() {
            field["required"] = json!(true);
            let store = compile(json!({
                "base": {"type": "dict", "keys": {"value": field}},
                "test": {"type": "dict", "keys": {
                    "value": field,
                    "relaxed": {"type": "dict", "$ref": "base#", "relaxed_validation": true},
                    "nested": {"type": "dict", "$ref": "base#"}
                }}
            }));
            for ignore_required_keys_on_root_dict in [false, true] {
                let output = store
                    .validate_value(
                        &json!({"relaxed": {}, "nested": {}}),
                        "test",
                        Some(&Configuration {
                            ignore_required_keys_on_root_dict,
                            ..Default::default()
                        }),
                    )
                    .expect("the test schema and value should be valid inputs");
                let paths = if ignore_required_keys_on_root_dict {
                    vec!["nested"]
                } else {
                    vec!["nested", ""]
                };
                assert_eq!(output.result.errors.len(), paths.len());
                for (error, path) in output.result.errors.iter().zip(paths) {
                    assert_eq!(error.path.to_string(), path);
                    assert_eq!(
                        error.issue,
                        Violation::MissingRequiredKey {
                            key: "value".to_owned()
                        }
                        .into()
                    );
                }
            }
        }
    }

    #[test]
    fn optional_null_parent_skips_required_descendants() {
        let store = compile(json!({"test": {"type": "dict", "keys": {"parent": {
            "type": "dict", "keys": {"child": {"type": "str", "required": true}}
        }}}}));
        check(
            &store,
            json!({"parent": null}),
            Configuration::default(),
            &[],
        );
        check(
            &store,
            json!({"parent": null}),
            Configuration {
                restrict_null_values: true,
                ..Default::default()
            },
            &[("parent", Type::Dict)],
        );
    }

    #[test]
    fn required_null_parent_reports_only_its_own_type_error() {
        let store = compile(json!({"test": {"type": "dict", "keys": {"parent": {
            "type": "dict", "required": true, "keys": {"child": {"type": "str", "required": true}}
        }}}}));
        check(
            &store,
            json!({"parent": null}),
            Configuration::default(),
            &[("parent", Type::Dict)],
        );
    }

    #[test]
    fn required_list_does_not_make_its_items_required() {
        for (field, expected) in types() {
            let store = compile(json!({"test": {"type": "dict", "keys": {
                "items": {"type": "list", "required": true, "items": field}
            }}}));
            check(
                &store,
                json!({"items": [null]}),
                Configuration::default(),
                &[],
            );
            check(
                &store,
                json!({"items": [null]}),
                Configuration {
                    restrict_null_values: true,
                    ..Default::default()
                },
                &[("items[0]", expected)],
            );
        }
    }

    #[test]
    fn primary_keys_reject_null_despite_relaxation_and_root_exemption() {
        let store = compile(json!({
            "base": {"type": "dict", "keys": {"items": {
                "type": "list", "primary_key": "id", "items": {
                    "type": "dict", "keys": {"id": {"type": "str"}}
                }
            }}},
            "test": {"type": "dict", "keys": {
                "relaxed": {"type": "dict", "$ref": "base#", "relaxed_validation": true}
            }}
        }));
        let output = store
            .validate_value(
                &json!({"relaxed": {"items": [{"id": null}]}}),
                "test",
                Some(&Configuration {
                    ignore_required_keys_on_root_dict: true,
                    ..Default::default()
                }),
            )
            .expect("the test schema and value should be valid inputs");
        assert_eq!(output.result.errors.len(), 1);
        assert_eq!(output.result.errors[0].path.to_string(), "relaxed.items[0]");
        assert_eq!(
            output.result.errors[0].issue,
            Violation::MissingRequiredKey {
                key: "id".to_owned()
            }
            .into()
        );
    }
}
