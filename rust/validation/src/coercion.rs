// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::delegate_anyschema_method;
use avdschema::{
    any::AnySchema, boolean::Bool, dict::Dict, int::Int, list::List, resolve_ref, str::Str,
};
use serde_json::Value;

use crate::{context::Context, feedback::CoercionNote};

pub trait Coercion
where
    for<'x> &'x Self: TryFrom<&'x AnySchema>,
{
    /// Recursively coerce the given value into the type specified by the schema.
    /// Also insert default values since dynamic keys and dynamic values may rely on these.
    ///
    /// Coercion is called before validation, to allow a more "loose" validation of types.
    /// This is especially useful when the input is coming from YAML where all types are inferred from strings.
    ///
    ///  TODO: Decide whether we should limit this to only coerce according to `convert_types`.
    fn coerce(&self, input: &mut Value, ctx: &mut Context);
    fn coerce_ref(&self, input: &mut Value, ctx: &mut Context);
}
impl Coercion for Bool {
    fn coerce(&self, _input: &mut Value, _ctx: &mut Context) {}
    fn coerce_ref(&self, _input: &mut Value, _ctx: &mut Context) {}
}
impl Coercion for Dict {
    fn coerce(&self, input: &mut Value, ctx: &mut Context) {
        if let Value::Object(dict) = input {
            if let Some(keys) = &self.keys {
                for (key, key_schema) in keys {
                    // Skip removed keys
                    if key_schema.is_removed() {
                        continue;
                    }
                    if let Some(value) = dict.get_mut(key) {
                        ctx.state.path.push(key.to_owned());
                        key_schema.coerce(value, ctx);
                        ctx.state.path.pop();
                    }
                }
            }
            if let Some(dynamic_keys) = self.get_dynamic_keys(dict) {
                for (key, dynamic_key_info) in dynamic_keys.iter() {
                    if let Some(value) = dict.get_mut(key) {
                        ctx.state.path.push(key.to_owned());
                        dynamic_key_info.schema.coerce(value, ctx);
                        ctx.state.path.pop();
                    }
                }
            }
        }
        self.coerce_ref(input, ctx);
    }

    fn coerce_ref(&self, input: &mut Value, ctx: &mut Context) {
        if let Some(ref_) = self.base.schema_ref.as_ref() {
            // Ignoring not being able to resolve the schema.
            // Ignoring a wrong schema type at the ref. Since coercion is infallible.
            // TODO: What to do?
            if let Ok(AnySchema::Dict(ref_schema)) = resolve_ref(ref_, ctx.store) {
                ref_schema.coerce(input, ctx);
            }
        }
    }
}

impl Coercion for Int {
    fn coerce(&self, input: &mut Value, ctx: &mut Context) {
        let value = match input {
            Value::Number(number) => match number.as_i64() {
                Some(integer) => Some(integer),
                None => match number.as_f64() {
                    Some(float) if float.fract() == 0.0 => {
                        if ctx.configuration.return_coercion_infos {
                            ctx.add_info(CoercionNote {
                                found: float.into(),
                                made: (float as i64).into(),
                            });
                        }
                        Some(float as i64)
                    }
                    _ => None,
                },
            },
            Value::Bool(boolean) => {
                let value: i64 = (*boolean).into();
                if ctx.configuration.return_coercion_infos {
                    ctx.add_info(CoercionNote {
                        found: (*boolean).into(),
                        made: value.into(),
                    });
                }
                Some(value)
            }
            Value::String(string) => string
                .parse()
                .inspect(|value: &i64| {
                    if ctx.configuration.return_coercion_infos {
                        ctx.add_info(CoercionNote {
                            found: string.clone().into(),
                            made: (*value).into(),
                        });
                    }
                })
                .ok(),
            _ => None,
        };

        if let Some(value) = value {
            _ = core::mem::replace(input, value.into());
        }
        self.coerce_ref(input, ctx);
    }

    fn coerce_ref(&self, input: &mut Value, ctx: &mut Context) {
        if let Some(ref_) = self.base.schema_ref.as_ref() {
            // Ignoring not being able to resolve the schema.
            // Ignoring a wrong schema type at the ref. Since coercion is infallible.
            // TODO: What to do?
            if let Ok(AnySchema::Int(ref_schema)) = resolve_ref(ref_, ctx.store) {
                ref_schema.coerce(input, ctx);
            }
        }
    }
}
impl Coercion for List {
    fn coerce(&self, input: &mut Value, ctx: &mut Context) {
        if let Some(item_schema) = &self.items
            && let Value::Array(list) = input
        {
            for (i, item) in list.iter_mut().enumerate() {
                ctx.state.path.push(i.to_string());
                item_schema.coerce(item, ctx);
                ctx.state.path.pop();
            }
        }
        self.coerce_ref(input, ctx);
    }

    fn coerce_ref(&self, input: &mut Value, ctx: &mut Context) {
        if let Some(ref_) = self.base.schema_ref.as_ref() {
            // Ignoring not being able to resolve the schema.
            // Ignoring a wrong schema type at the ref. Since coercion is infallible.
            // TODO: What to do?
            if let Ok(AnySchema::List(ref_schema)) = resolve_ref(ref_, ctx.store) {
                ref_schema.coerce(input, ctx);
            }
        }
    }
}
impl Coercion for Str {
    fn coerce(&self, input: &mut Value, ctx: &mut Context) {
        let value = match input {
            Value::String(string) => Some(string.to_string()),
            Value::Number(number) => {
                if ctx.configuration.return_coercion_infos {
                    ctx.add_info(CoercionNote {
                        found: Value::Number(number.clone()).into(),
                        made: number.to_string().into(),
                    });
                }
                Some(number.to_string())
            }
            Value::Bool(boolean) => {
                // Using Title case to match Python behavior.
                let string: String = match boolean {
                    true => "True".into(),
                    false => "False".into(),
                };
                if ctx.configuration.return_coercion_infos {
                    ctx.add_info(CoercionNote {
                        found: boolean.to_owned().into(),
                        made: string.to_owned().into(),
                    });
                }
                Some(string)
            }
            _ => None,
        }
        .map(|string| {
            if self.convert_to_lower_case.unwrap_or_default() {
                let lower_case_string = string.to_lowercase();
                if lower_case_string != string {
                    if ctx.configuration.return_coercion_infos {
                        ctx.add_info(CoercionNote {
                            found: string.into(),
                            made: lower_case_string.to_owned().into(),
                        });
                    }
                    lower_case_string
                } else {
                    string
                }
            } else {
                string
            }
        });

        if let Some(value) = value.as_deref() {
            _ = core::mem::replace(input, value.into());
        }
        self.coerce_ref(input, ctx);
    }

    fn coerce_ref(&self, input: &mut Value, ctx: &mut Context) {
        if let Some(ref_) = self.base.schema_ref.as_ref() {
            // Ignoring not being able to resolve the schema.
            // Ignoring a wrong schema type at the ref. Since coercion is infallible.
            // TODO: What to do?
            if let Ok(AnySchema::Str(ref_schema)) = resolve_ref(ref_, ctx.store) {
                ref_schema.coerce(input, ctx);
            }
        }
    }
}
impl Coercion for AnySchema {
    fn coerce(&self, input: &mut Value, ctx: &mut Context) {
        delegate_anyschema_method!(self, coerce, input, ctx)
    }

    fn coerce_ref(&self, input: &mut Value, ctx: &mut Context) {
        delegate_anyschema_method!(self, coerce_ref, input, ctx)
    }
}

#[cfg(test)]
mod tests {
    use avdschema::base::Base;
    use ordermap::OrderMap;

    use super::*;
    use crate::Validation as _;
    use crate::context::{Configuration, Context};
    use crate::feedback::{CoercionNote, Feedback};
    use crate::validation::test_utils::get_test_store;

    #[test]
    fn validate_coecion_with_ref_ok() {
        let schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "foo".into(),
                Dict {
                    base: Base {
                        // Using ref to the root of the schema since such refs will not get resolved.
                        schema_ref: Some("eos_config#".into()),
                        ..Default::default()
                    },
                    ..Default::default()
                }
                .into(),
            )])),
            ..Default::default()
        };
        let mut input = serde_json::json!({ "foo": {"key1": 123} });
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration));
        schema.coerce(&mut input, &mut ctx);
        schema.validate_value(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty());
        assert_eq!(
            ctx.result.infos,
            vec![Feedback {
                path: vec!["foo".into(), "key1".into()].into(),
                issue: CoercionNote {
                    found: 123.into(),
                    made: "123".into()
                }
                .into()
            },]
        )
    }
}
