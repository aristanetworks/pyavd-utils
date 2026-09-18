// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::IntView;

use super::NodeValidation;
use super::invalid_type;
use crate::context::Context;
use crate::context::ValidationState;
use crate::feedback::Type;
use crate::feedback::Violation;
use crate::validatable::ValidatableValue;

pub(crate) fn validate_node<V: ValidatableValue>(
    schema: IntView<'_>,
    value: &V,
    ctx: &mut Context,
    state: &mut ValidationState,
) -> NodeValidation<i64> {
    // Lenient type check - accept anything coercible to int (e.g., "123" -> 123)
    if let Some(integer) = value.as_i64() {
        // Emit coercion info if the original value was not an int
        if !value.is_int() {
            ctx.add_coercion_for(state, value, integer);
        }
        if schema
            .valid_values()
            .is_some_and(|mut valid_values| !valid_values.any(|valid_value| valid_value == integer))
        {
            ctx.add_error_for(
                state,
                value,
                Violation::InvalidValue {
                    expected: schema
                        .valid_values()
                        .into_iter()
                        .flatten()
                        .collect::<Vec<_>>()
                        .into(),
                    found: integer.into(),
                },
            );
        }
        validate_min(schema, value, &integer, ctx, state);
        validate_max(schema, value, &integer, ctx, state);
        NodeValidation::Valid(integer)
    } else if value.is_int() {
        ctx.add_error_for(
            state,
            value,
            Violation::IntegerOutOfRange {
                found: value.as_str().map_or_else(
                    || value.to_feedback_value().to_string(),
                    std::borrow::Cow::into_owned,
                ),
            },
        );
        NodeValidation::Invalid
    } else {
        invalid_type(value, ctx, state, Type::Int)
    }
}

fn validate_min<V: ValidatableValue>(
    schema: IntView<'_>,
    value: &V,
    input: &i64,
    ctx: &mut Context,
    state: &ValidationState,
) {
    if let Some(min) = schema.min()
        && min > *input
    {
        ctx.add_error_for(
            state,
            value,
            Violation::ValueBelowMinimum {
                minimum: min,
                found: *input,
            },
        );
    }
}

fn validate_max<V: ValidatableValue>(
    schema: IntView<'_>,
    value: &V,
    input: &i64,
    ctx: &mut Context,
    state: &ValidationState,
) {
    if let Some(max) = schema.max()
        && max < *input
    {
        ctx.add_error_for(
            state,
            value,
            Violation::ValueAboveMaximum {
                maximum: max,
                found: *input,
            },
        );
    }
}

#[cfg(test)]
mod tests {
    use avdschema::int::SourceInt;
    use serde_json::Value;

    use super::*;
    use crate::Configuration;
    use crate::context::Context;
    use crate::feedback::CoercionNote;
    use crate::feedback::Feedback;
    use crate::feedback::Violation;
    use crate::validation::test_utils::TestValidate as _;

    #[test]
    fn validate_type_ok() {
        let schema = SourceInt::default();
        let input: Value = 123.into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = SourceInt::default();
        let input = serde_json::json!({});
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidType {
                    expected: Type::Int,
                    found: Type::Dict,
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_json_integer_out_of_range_err() {
        let schema = SourceInt::default();
        let input = Value::Number(serde_json::Number::from(i64::MAX as u64 + 1));
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::IntegerOutOfRange {
                    found: "9223372036854775808".into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_yaml_integer_variant_within_i64_ok() {
        let schema = SourceInt::default();
        let input = yaml_parser::Node::new(
            yaml_parser::Value::Int(yaml_parser::Integer::U64(123)),
            yaml_parser::Span::default(),
        );
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_yaml_integer_out_of_range_err() {
        let schema = SourceInt::default();
        let input = yaml_parser::Node::new(
            yaml_parser::Value::Int(yaml_parser::Integer::U64(i64::MAX as u64 + 1)),
            yaml_parser::Span::default(),
        );
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: Some(crate::feedback::SourceSpan { start: 0, end: 0 }),
                issue: Violation::IntegerOutOfRange {
                    found: "9223372036854775808".into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_type_coerced_from_str_ok() {
        let schema = SourceInt::default();
        let input: Value = "123".into();
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
                    found: "123".into(),
                    made: 123.into()
                }
                .into()
            }]
        );
        assert_eq!(coerced, Some(Value::Number(123.into())));
    }

    #[test]
    fn validate_type_coerced_from_str_err() {
        let schema = SourceInt::default();
        let input: Value = "one23".into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidType {
                    expected: Type::Int,
                    found: Type::Str
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_type_coerced_from_bool_ok() {
        let schema = SourceInt::default();
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };

        // Test true -> 1
        let input_true: Value = true.into();
        let mut true_ctx = Context::new(Some(&configuration));
        let true_coerced = schema.validate(&input_true, &mut true_ctx);
        assert!(true_ctx.result.errors.is_empty());
        assert_eq!(
            true_ctx.result.infos,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: CoercionNote {
                    found: true.into(),
                    made: 1.into()
                }
                .into()
            }]
        );
        assert_eq!(true_coerced, Some(Value::Number(1.into())));

        // Test false -> 0
        let input_false: Value = false.into();
        let mut false_ctx = Context::new(Some(&configuration));
        let false_coerced = schema.validate(&input_false, &mut false_ctx);
        assert!(false_ctx.result.errors.is_empty());
        assert_eq!(
            false_ctx.result.infos,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: CoercionNote {
                    found: false.into(),
                    made: 0.into()
                }
                .into()
            }]
        );
        assert_eq!(false_coerced, Some(Value::Number(0.into())));
    }

    #[test]
    fn validate_valid_values_ok() {
        let schema = {
            let mut int = SourceInt::default();
            int.valid_values.valid_values = Some(vec![123]);
            int
        };
        let input: Value = 123.into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_valid_values_err() {
        let schema = {
            let mut int = SourceInt::default();
            int.valid_values.valid_values = Some(vec![123]);
            int
        };
        let input: Value = 321.into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidValue {
                    expected: vec![123].into(),
                    found: input.into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_min_ok() {
        let schema = SourceInt {
            min: Some(122),
            ..Default::default()
        };
        let input: Value = 123.into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_min_err() {
        let schema = SourceInt {
            min: Some(122),
            ..Default::default()
        };
        let input: Value = 121.into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::ValueBelowMinimum {
                    minimum: 122,
                    found: 121
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_max_ok() {
        let schema = SourceInt {
            max: Some(124),
            ..Default::default()
        };
        let input: Value = 123.into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_max_err() {
        let schema = SourceInt {
            max: Some(124),
            ..Default::default()
        };
        let input: Value = 125.into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::ValueAboveMaximum {
                    maximum: 124,
                    found: 125
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_type_coerced_from_integral_float_ok() {
        let schema = SourceInt::default();
        let input = serde_json::json!(1.0);
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
                    found: 1.0.into(),
                    made: 1.into()
                }
                .into()
            }]
        );
        assert_eq!(coerced, Some(Value::Number(1.into())));
    }
}
