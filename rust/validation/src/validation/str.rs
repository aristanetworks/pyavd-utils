// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{any::AnySchema, resolve_ref, str::Str};

use crate::{
    context::Context,
    feedback::{CoercionNote, Type, Violation},
    validatable::ValidatableValue,
};

use super::{Validation, valid_values::ValidateValidValues as _};

impl Validation for Str {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        // Lenient type check - accept anything coercible to string
        if let Some(v) = value.as_str() {
            let s = v.into_owned();
            // Emit coercion info if original was not a string
            emit_coercion_info(value, &s, ctx);
            // Apply convert_to_lower_case if specified
            let s = convert_to_lower_case(self, value, s, ctx);
            self.valid_values.validate(value, &s, ctx);
            validate_min_length(self, value, &s, ctx);
            validate_max_length(self, value, &s, ctx);
            validate_pattern(self, value, &s, ctx);
            validate_ref(self, value, ctx);
            if ctx.configuration.return_coerced_data {
                Some(value.coerce_str(s))
            } else {
                None
            }
        } else if value.is_null() && !ctx.configuration.restrict_null_values {
            // Null is allowed when not restricted
            ctx.configuration
                .return_coerced_data
                .then(|| value.coerce_null())
        } else {
            ctx.add_error_for(
                value,
                Violation::InvalidType {
                    expected: Type::Str,
                    found: value.value_type(),
                },
            );
            None
        }
    }
}

/// Emit coercion info if the original value was coerced to string.
fn emit_coercion_info<V: ValidatableValue>(value: &V, coerced_str: &str, ctx: &mut Context) {
    if !ctx.configuration.return_coercion_infos || value.is_str() {
        return;
    }
    ctx.add_info_for(
        value,
        CoercionNote {
            found: value.to_feedback_value(),
            made: coerced_str.to_owned().into(),
        },
    );
}

fn convert_to_lower_case<V: ValidatableValue>(
    schema: &Str,
    value: &V,
    s: String,
    ctx: &mut Context,
) -> String {
    if schema.convert_to_lower_case.unwrap_or_default() {
        let lower = s.to_lowercase();
        if lower != s {
            if ctx.configuration.return_coercion_infos {
                ctx.add_info_for(
                    value,
                    CoercionNote {
                        found: s.into(),
                        made: lower.clone().into(),
                    },
                );
            }
            lower
        } else {
            s
        }
    } else {
        s
    }
}

/// Validate against a referenced schema (for unresolved $ref ending with #).
fn validate_ref<V: ValidatableValue>(schema: &Str, value: &V, ctx: &mut Context) {
    if let Some(ref_) = schema.base.schema_ref.as_ref()
        && let Ok(AnySchema::Str(ref_schema)) = resolve_ref(ref_, ctx.store)
    {
        // Note: We ignore the coerced result from ref validation since we already have our coerced value
        let _ = ref_schema.validate(value, ctx);
    }
}

fn validate_min_length<V: ValidatableValue>(
    schema: &Str,
    value: &V,
    input: &str,
    ctx: &mut Context,
) {
    if let Some(min_length) = schema.min_length {
        let length = input.chars().count() as u64;
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

fn validate_max_length<V: ValidatableValue>(
    schema: &Str,
    value: &V,
    input: &str,
    ctx: &mut Context,
) {
    if let Some(max_length) = schema.max_length {
        let length = input.chars().count() as u64;
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

fn validate_pattern<V: ValidatableValue>(schema: &Str, value: &V, input: &str, ctx: &mut Context) {
    if let Some(pattern) = &schema.pattern {
        let regex_pattern = pattern.get_compiled_pattern();
        if !regex_pattern.is_match(input) {
            ctx.add_error_for(
                value,
                Violation::NotMatchingPattern {
                    pattern: pattern.to_string(),
                    found: input.into(),
                },
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use avdschema::base::valid_values::ValidValues;
    use serde_json::Value;

    use super::*;
    use crate::{
        Configuration,
        feedback::{CoercionNote, Feedback},
        validation::test_utils::get_test_store,
    };

    #[test]
    fn validate_type_ok() {
        let schema = Str::default();
        let input: Value = "foo".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = Str::default();
        let input = serde_json::json!([]);
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
                    expected: Type::Str,
                    found: Type::List
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_valid_values_ok() {
        let schema = Str {
            valid_values: ValidValues {
                valid_values: Some(vec!["foo".into()]),
                ..Default::default()
            },
            ..Default::default()
        };
        let input: Value = "foo".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_valid_values_err() {
        let schema = Str {
            valid_values: ValidValues {
                valid_values: Some(vec!["foo".into()]),
                ..Default::default()
            },
            ..Default::default()
        };
        let input: Value = "FOO".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidValue {
                    expected: vec!["foo".to_string()].into(),
                    found: "FOO".into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_valid_values_to_lower_case_ok() {
        let schema = Str {
            valid_values: ValidValues {
                valid_values: Some(vec!["foo".into()]),
                ..Default::default()
            },
            convert_to_lower_case: Some(true),
            ..Default::default()
        };
        let input: Value = "FOO".into();
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
                path: vec![].into(),
                span: None,
                issue: CoercionNote {
                    found: "FOO".into(),
                    made: "foo".into()
                }
                .into()
            }]
        );
        assert_eq!(coerced, Some(Value::String("foo".into())));
    }

    #[test]
    fn validate_valid_values_from_bool_to_lower_case_ok() {
        let schema = Str {
            valid_values: ValidValues {
                valid_values: Some(vec!["true".into()]),
                ..Default::default()
            },
            convert_to_lower_case: Some(true),
            ..Default::default()
        };
        // Bool input - as_str() returns "True" (Title case), then convert_to_lower_case makes it "true"
        let input: Value = true.into();
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration));
        let coerced = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        // Two coercion notes: bool -> "True", then "True" -> "true"
        assert_eq!(
            ctx.result.infos,
            vec![
                Feedback {
                    path: vec![].into(),
                    span: None,
                    issue: CoercionNote {
                        found: true.into(),
                        made: "True".into()
                    }
                    .into()
                },
                Feedback {
                    path: vec![].into(),
                    span: None,
                    issue: CoercionNote {
                        found: "True".into(),
                        made: "true".into()
                    }
                    .into()
                }
            ]
        );
        assert_eq!(coerced, Some(Value::String("true".into())));
    }

    #[test]
    fn validate_type_coerced_from_float_ok() {
        let schema = Str::default();
        // Float 1.5 can be coerced to string "1.5"
        let input: Value = serde_json::json!(1.5);
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
                path: vec![].into(),
                span: None,
                issue: CoercionNote {
                    found: 1.5.into(),
                    made: "1.5".into()
                }
                .into()
            }]
        );
        assert_eq!(coerced, Some(Value::String("1.5".into())));
    }

    #[test]
    fn validate_min_length_ok() {
        let schema = Str {
            min_length: Some(3),
            ..Default::default()
        };
        let input: Value = "foo".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.warnings.is_empty());
        assert!(ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_min_length_err() {
        let schema = Str {
            min_length: Some(3),
            ..Default::default()
        };
        let input: Value = "go".into();
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
        let schema = Str {
            max_length: Some(3),
            ..Default::default()
        };
        let input: Value = "foo".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_max_length_err() {
        let schema = Str {
            max_length: Some(3),
            ..Default::default()
        };
        let input: Value = "fooo".into();
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
                    maximum: 3,
                    found: 4
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_pattern_ok() {
        let schema = Str {
            pattern: Some("[a-z][A-Z][a-z]".into()),
            ..Default::default()
        };
        let input: Value = "fOo".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_pattern_err() {
        let schema = Str {
            pattern: Some("[a-z][A-Z][a-z]".into()),
            ..Default::default()
        };
        let input: Value = "foo".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::NotMatchingPattern {
                    pattern: "[a-z][A-Z][a-z]".into(),
                    found: "foo".into(),
                }
                .into()
            }]
        );
    }
}
