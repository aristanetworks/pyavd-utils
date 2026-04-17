// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::{
    context::Context,
    feedback::{CoercionNote, Type, Violation},
    validatable::ValidatableValue,
};

use avdschema::{any::AnySchema, int::Int, resolve_ref};

use super::{Validation, valid_values::ValidateValidValues};

impl Validation for Int {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        if let Some(ref_result) = validate_ref(self, value, ctx) {
            return ref_result;
        }

        // Lenient type check - accept anything coercible to int (e.g., "123" -> 123)
        if let Some(v) = value.as_i64() {
            // Emit coercion info if the original value was not an int
            emit_coercion_info(value, v, ctx);
            self.valid_values.validate(value, &v, ctx);
            validate_min(self, value, &v, ctx);
            validate_max(self, value, &v, ctx);
            if ctx.configuration.return_coerced_data {
                Some(value.coerce_int(v))
            } else {
                None
            }
        } else {
            Self::handle_invalid_type(value, ctx, Type::Int)
        }
    }
}

/// Validate against a referenced schema (for unresolved $ref ending with #).
fn validate_ref<V: ValidatableValue>(
    schema: &Int,
    value: &V,
    ctx: &mut Context,
) -> Option<Option<V::Coerced>> {
    if let Some(ref_) = schema.base.schema_ref.as_ref()
        && let Ok(AnySchema::Int(ref_schema)) = resolve_ref(ref_, ctx.store)
    {
        return Some(ref_schema.validate(value, ctx));
    }
    None
}

/// Emit coercion info if the original value was coerced to int.
fn emit_coercion_info<V: ValidatableValue>(value: &V, coerced_value: i64, ctx: &mut Context) {
    if !ctx.configuration.return_coercion_infos || value.is_int() {
        return;
    }
    ctx.add_info_for(
        value,
        CoercionNote {
            found: value.to_feedback_value(),
            made: coerced_value.into(),
        },
    );
}

fn validate_min<V: ValidatableValue>(schema: &Int, value: &V, input: &i64, ctx: &mut Context) {
    if let Some(min) = schema.min
        && min > *input
    {
        ctx.add_error_for(
            value,
            Violation::ValueBelowMinimum {
                minimum: min,
                found: *input,
            },
        );
    }
}

fn validate_max<V: ValidatableValue>(schema: &Int, value: &V, input: &i64, ctx: &mut Context) {
    if let Some(max) = schema.max
        && max < *input
    {
        ctx.add_error_for(
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
    use serde_json::Value;

    use super::*;

    use crate::Configuration;
    use crate::context::Context;
    use crate::feedback::{CoercionNote, Feedback, Violation};
    use crate::validation::test_utils::get_test_store;

    #[test]
    fn validate_type_ok() {
        let schema = Int::default();
        let input: Value = 123.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = Int::default();
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
                issue: Violation::InvalidType {
                    expected: Type::Int,
                    found: Type::Dict,
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_type_coerced_from_str_ok() {
        let schema = Int::default();
        let input: Value = "123".into();
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
        let schema = Int::default();
        let input: Value = "one23".into();
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
                    expected: Type::Int,
                    found: Type::Str
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_type_coerced_from_bool_ok() {
        let schema = Int::default();
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };

        // Test true -> 1
        let input_true: Value = true.into();
        let mut ctx = Context::new(&store, Some(&configuration));
        let coerced = schema.validate(&input_true, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.infos,
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
        assert_eq!(coerced, Some(Value::Number(1.into())));

        // Test false -> 0
        let input_false: Value = false.into();
        let mut ctx = Context::new(&store, Some(&configuration));
        let coerced = schema.validate(&input_false, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.infos,
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
        assert_eq!(coerced, Some(Value::Number(0.into())));
    }

    #[test]
    fn validate_valid_values_ok() {
        let schema = {
            let mut int = Int::default();
            int.valid_values.valid_values = Some(vec![123]);
            int
        };
        let input: Value = 123.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_valid_values_err() {
        let schema = {
            let mut int = Int::default();
            int.valid_values.valid_values = Some(vec![123]);
            int
        };
        let input: Value = 321.into();
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
                    expected: vec![123].into(),
                    found: input.into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_min_ok() {
        let schema = Int {
            min: Some(122),
            ..Default::default()
        };
        let input: Value = 123.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_min_err() {
        let schema = Int {
            min: Some(122),
            ..Default::default()
        };
        let input: Value = 121.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
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
        let schema = Int {
            max: Some(124),
            ..Default::default()
        };
        let input: Value = 123.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_max_err() {
        let schema = Int {
            max: Some(124),
            ..Default::default()
        };
        let input: Value = 125.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
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
        let schema = Int::default();
        let input = serde_json::json!(1.0);
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
                    found: 1.0.into(),
                    made: 1.into()
                }
                .into()
            }]
        );
        assert_eq!(coerced, Some(Value::Number(1.into())));
    }
}
