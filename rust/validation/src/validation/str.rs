// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{any::AnySchema, resolve_ref, str::Str};

use crate::{
    context::Context,
    feedback::{CoercionNote, ErrorIssue, Type, Violation},
    validatable::ValidatableValue,
};

use super::{Validation, valid_values::ValidateValidValues as _};

impl Validation for Str {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        if let Some(ref_result) = validate_ref(self, value, ctx) {
            return ref_result;
        }

        // Lenient type check - accept anything coercible to string
        if let Some(v) = value.as_str() {
            let s = v.into_owned();
            // Emit coercion info if original was not a string
            if !value.is_str() {
                ctx.add_coercion_for(value, s.as_str());
            }
            // Apply convert_to_lower_case if specified
            let s = convert_to_lower_case(self, value, s, ctx);
            self.valid_values.validate(value, &s, ctx);
            validate_min_length(self, value, &s, ctx);
            validate_max_length(self, value, &s, ctx);
            validate_pattern(self, value, &s, ctx);
            if ctx.configuration.return_coerced_data {
                Some(value.coerce_str(s))
            } else {
                None
            }
        } else {
            Self::handle_invalid_type(value, ctx, Type::Str)
        }
    }
}

fn convert_to_lower_case<V: ValidatableValue>(
    schema: &Str,
    value: &V,
    s: String,
    ctx: &mut Context,
) -> String {
    if !schema.convert_to_lower_case.unwrap_or_default() {
        return s;
    }
    let lower = s.to_lowercase();
    if lower != s {
        ctx.add_coercion_for(value, lower.as_str());
        lower
    } else {
        s
    }
}

/// Validate against a referenced schema (for unresolved $ref ending with #).
fn validate_ref<V: ValidatableValue>(
    schema: &Str,
    value: &V,
    ctx: &mut Context,
) -> Option<Option<V::Coerced>> {
    if let Some(ref_) = schema.base.schema_ref.as_ref()
        && let Ok(AnySchema::Str(ref_schema)) = resolve_ref(ref_, ctx.store)
    {
        return Some(ref_schema.validate(value, ctx));
    }
    None
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
        match regex_pattern.is_match(input) {
            Ok(true) => {}
            Ok(false) => ctx.add_error_for(
                value,
                Violation::NotMatchingPattern {
                    pattern: pattern.to_string(),
                    found: input.into(),
                },
            ),
            Err(e) => ctx.add_error_for(value, ErrorIssue::InternalError { message: e.to_string() }),
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

    // --- lookaround tests (require fancy-regex) ---

    #[test]
    fn validate_pattern_lookahead_ok() {
        // Proves fancy-regex syntax is accepted: starts with lowercase AND contains a digit.
        let schema = Str {
            pattern: Some("(?=[a-z])(?=.*[0-9])[a-z0-9]+".into()),
            ..Default::default()
        };
        let input: Value = "abc123".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_pattern_lookahead_err() {
        // Same pattern — "abcdef" has no digit so the lookahead fails → NotMatchingPattern.
        let schema = Str {
            pattern: Some("(?=[a-z])(?=.*[0-9])[a-z0-9]+".into()),
            ..Default::default()
        };
        let input: Value = "abcdef".into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::NotMatchingPattern {
                    pattern: "(?=[a-z])(?=.*[0-9])[a-z0-9]+".into(),
                    found: "abcdef".into(),
                }
                .into()
            }]
        );
    }

}
