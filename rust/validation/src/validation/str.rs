// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use avdschema::StrView;

use super::NodeValidation;
use super::invalid_type;
use crate::context::Context;
use crate::context::ValidationState;
use crate::feedback::ErrorIssue;
use crate::feedback::Type;
use crate::feedback::Violation;
use crate::validatable::ValidatableValue;

pub(crate) fn validate_node<'a, V: ValidatableValue>(
    schema: StrView<'_>,
    value: &'a V,
    ctx: &mut Context,
    state: &mut ValidationState,
) -> NodeValidation<Cow<'a, str>> {
    // Lenient type check - accept anything coercible to string
    if let Some(string) = value.as_str() {
        // Emit coercion info if original was not a string
        if !value.is_str() {
            ctx.add_coercion_for(state, value, string.as_ref());
        }
        // Apply convert_to_lower_case if specified
        let input = convert_to_lower_case(schema, value, string, ctx, state);
        if schema
            .valid_values()
            .is_some_and(|mut valid_values| !valid_values.any(|valid_value| valid_value == input))
        {
            ctx.add_error_for(
                state,
                value,
                Violation::InvalidValue {
                    expected: schema
                        .valid_values()
                        .into_iter()
                        .flatten()
                        .map(ToOwned::to_owned)
                        .collect::<Vec<_>>()
                        .into(),
                    found: input.as_ref().into(),
                },
            );
        }
        validate_min_length(schema, value, input.as_ref(), ctx, state);
        validate_max_length(schema, value, input.as_ref(), ctx, state);
        validate_pattern(schema, value, input.as_ref(), ctx, state);
        NodeValidation::Valid(input)
    } else {
        invalid_type(value, ctx, state, Type::Str)
    }
}

fn convert_to_lower_case<'a, V: ValidatableValue>(
    schema: StrView<'_>,
    value: &V,
    input: Cow<'a, str>,
    ctx: &mut Context,
    state: &ValidationState,
) -> Cow<'a, str> {
    if !schema.convert_to_lower_case() {
        return input;
    }
    if input.chars().flat_map(char::to_lowercase).eq(input.chars()) {
        return input;
    }
    let lower = input.to_lowercase();
    ctx.add_string_lowered_for(state, value, &input, &lower);
    Cow::Owned(lower)
}

fn validate_min_length<V: ValidatableValue>(
    schema: StrView<'_>,
    value: &V,
    input: &str,
    ctx: &mut Context,
    state: &ValidationState,
) {
    if let Some(min_length) = schema.min_length() {
        let length = input.chars().count() as u64;
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

fn validate_max_length<V: ValidatableValue>(
    schema: StrView<'_>,
    value: &V,
    input: &str,
    ctx: &mut Context,
    state: &ValidationState,
) {
    if let Some(max_length) = schema.max_length() {
        let length = input.chars().count() as u64;
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

fn validate_pattern<V: ValidatableValue>(
    schema: StrView<'_>,
    value: &V,
    input: &str,
    ctx: &mut Context,
    state: &ValidationState,
) {
    if let Some(pattern) = schema.pattern() {
        let Some(compiled_pattern) = schema.compiled_pattern() else {
            return;
        };
        match compiled_pattern {
            Err(err) => ctx.add_error_for(
                state,
                value,
                ErrorIssue::InternalError {
                    message: format!("Schema contains an invalid regex pattern '{pattern}': {err}"),
                },
            ),
            Ok(regex_pattern) => match regex_pattern.is_match(input) {
                Ok(true) => {}
                Ok(false) => ctx.add_error_for(
                    state,
                    value,
                    Violation::NotMatchingPattern {
                        pattern: pattern.to_owned(),
                        found: input.into(),
                    },
                ),
                Err(err) => ctx.add_error_for(
                    state,
                    value,
                    ErrorIssue::InternalError {
                        message: err.to_string(),
                    },
                ),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use avdschema::base::valid_values::ValidValues;
    use avdschema::str::SourceStr;
    use serde_json::Value;

    use super::*;
    use crate::Configuration;
    use crate::feedback::CoercionNote;
    use crate::feedback::Feedback;
    use crate::feedback::StringLoweredNote;
    use crate::validation::test_utils::TestValidate as _;

    #[test]
    fn validate_type_ok() {
        let schema = SourceStr::default();
        let input: Value = "foo".into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = SourceStr::default();
        let input = serde_json::json!([]);
        let mut ctx = Context::new(None);
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
        let schema = SourceStr {
            valid_values: ValidValues {
                valid_values: Some(vec!["foo".into()]),
                ..Default::default()
            },
            ..Default::default()
        };
        let input: Value = "foo".into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_valid_values_err() {
        let schema = SourceStr {
            valid_values: ValidValues {
                valid_values: Some(vec!["foo".into()]),
                ..Default::default()
            },
            ..Default::default()
        };
        let input: Value = "FOO".into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidValue {
                    expected: vec!["foo".to_owned()].into(),
                    found: "FOO".into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_valid_values_to_lower_case_ok() {
        let schema = SourceStr {
            valid_values: ValidValues {
                valid_values: Some(vec!["foo".into()]),
                ..Default::default()
            },
            convert_to_lower_case: Some(true),
            ..Default::default()
        };
        let input: Value = "FOO".into();
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
                path: vec![].into(),
                span: None,
                issue: StringLoweredNote {
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
        let schema = SourceStr {
            valid_values: ValidValues {
                valid_values: Some(vec!["true".into()]),
                ..Default::default()
            },
            convert_to_lower_case: Some(true),
            ..Default::default()
        };
        // Bool input - as_str() returns "True" (Title case), then convert_to_lower_case makes it "true"
        let input: Value = true.into();
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(Some(&configuration));
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
                    issue: StringLoweredNote {
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
        let schema = SourceStr::default();
        // Float 1.5 can be coerced to string "1.5"
        let input: Value = serde_json::json!(1.5);
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
        let schema = SourceStr {
            min_length: Some(3),
            ..Default::default()
        };
        let input: Value = "foo".into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert!(ctx.result.warnings.is_empty());
        assert!(ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_min_length_err() {
        let schema = SourceStr {
            min_length: Some(3),
            ..Default::default()
        };
        let input: Value = "go".into();
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
        let schema = SourceStr {
            max_length: Some(3),
            ..Default::default()
        };
        let input: Value = "foo".into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_max_length_err() {
        let schema = SourceStr {
            max_length: Some(3),
            ..Default::default()
        };
        let input: Value = "fooo".into();
        let mut ctx = Context::new(None);
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
        let schema = SourceStr {
            pattern: Some("[a-z][A-Z][a-z]".into()),
            ..Default::default()
        };
        let input: Value = "fOo".into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_pattern_err() {
        let schema = SourceStr {
            pattern: Some("[a-z][A-Z][a-z]".into()),
            ..Default::default()
        };
        let input: Value = "foo".into();
        let mut ctx = Context::new(None);
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
        let schema = SourceStr {
            pattern: Some("(?=[a-z])(?=.*[0-9])[a-z0-9]+".into()),
            ..Default::default()
        };
        let input: Value = "abc123".into();
        let mut ctx = Context::new(None);
        schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_pattern_lookahead_err() {
        // Same pattern — "abcdef" has no digit so the lookahead fails → NotMatchingPattern.
        let schema = SourceStr {
            pattern: Some("(?=[a-z])(?=.*[0-9])[a-z0-9]+".into()),
            ..Default::default()
        };
        let input: Value = "abcdef".into();
        let mut ctx = Context::new(None);
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

    #[test]
    fn validate_pattern_invalid_regex_internal_error() {
        // An unterminated character class is rejected by fancy-regex at compile time.
        let pattern_str = "[invalid";
        let schema = SourceStr {
            pattern: Some(pattern_str.into()),
            ..Default::default()
        };
        let input: Value = "foo".into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(ctx.result.errors.len(), 1);
        match &ctx.result.errors[0].issue {
            ErrorIssue::InternalError { message } => {
                assert!(
                    message.starts_with("Schema contains an invalid regex pattern '"),
                    "unexpected message prefix: {message}"
                );
                assert!(
                    message.contains(pattern_str),
                    "message should include the offending pattern: {message}"
                );
            }
            other => panic!("expected InternalError, got {other:?}"),
        }
    }
}
