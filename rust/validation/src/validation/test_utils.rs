// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::SchemaView;
use avdschema::Store;
use avdschema::StoreSource;
use avdschema::any::SourceSchema;
use avdschema::boolean::SourceBool;
use avdschema::dict::SourceDict;
use avdschema::int::SourceInt;
use avdschema::list::SourceList;
use avdschema::str::SourceStr;
use serde::Deserialize as _;
use serde_json::json;

use crate::context::Context;
use crate::context::ValidationState;
use crate::validatable::ValidatableValue;
use crate::walker::Validator;

const TEST_SCHEMA: &str = "__validation_test__";

pub(crate) trait TestValidate {
    fn validate<V: ValidatableValue>(&self, value: &V, context: &mut Context)
    -> Option<V::Coerced>;
}

impl TestValidate for SourceSchema {
    fn validate<V: ValidatableValue>(
        &self,
        value: &V,
        context: &mut Context,
    ) -> Option<V::Coerced> {
        validate_test_schema(self.clone(), value, context)
    }
}

macro_rules! impl_test_validate {
    ($schema:ty, $variant:ident) => {
        impl TestValidate for $schema {
            fn validate<V: ValidatableValue>(
                &self,
                value: &V,
                context: &mut Context,
            ) -> Option<V::Coerced> {
                validate_test_schema(SourceSchema::$variant(self.clone()), value, context)
            }
        }
    };
}

impl_test_validate!(SourceBool, Bool);
impl_test_validate!(SourceInt, Int);
impl_test_validate!(SourceStr, Str);
impl_test_validate!(SourceList, List);
impl_test_validate!(SourceDict, Dict);

fn validate_test_schema<V: ValidatableValue>(
    schema: SourceSchema,
    value: &V,
    context: &mut Context,
) -> Option<V::Coerced> {
    validate_test_schema_with_state(schema, value, context, &mut ValidationState::default())
}

pub(crate) fn validate_test_schema_with_state<V: ValidatableValue>(
    schema: SourceSchema,
    value: &V,
    context: &mut Context,
    state: &mut ValidationState,
) -> Option<V::Coerced> {
    let archive = compile_test_schema(schema);
    let compiled_schema = get_compiled_test_schema(&archive);
    let mut archived_context = Context::new(Some(&context.configuration));
    let coerced = Validator::new(&archive, &mut archived_context).validate_with_state(
        compiled_schema,
        value,
        state,
    );
    context.result.errors.extend(archived_context.result.errors);
    context
        .result
        .warnings
        .extend(archived_context.result.warnings);
    context.result.infos.extend(archived_context.result.infos);
    coerced
}

fn compile_test_schema(schema: SourceSchema) -> Store {
    let mut raw_store_json = serde_json::to_value(get_test_store())
        .expect("test schema store should serialize")
        .as_object()
        .expect("test schema store should serialize as an object")
        .clone();
    raw_store_json.insert(
        TEST_SCHEMA.to_owned(),
        serde_json::to_value(schema).expect("test schema should serialize"),
    );
    let raw_store = serde_json::from_value(serde_json::Value::Object(raw_store_json))
        .expect("test schema store should deserialize");
    Store::compile(&raw_store).expect("test schema should compile")
}

fn get_compiled_test_schema(archive: &Store) -> SchemaView<'_> {
    archive
        .get(TEST_SCHEMA)
        .expect("compiled test schema should be present")
}

pub(crate) fn get_test_store() -> StoreSource {
    StoreSource::deserialize(json!({
        "eos_config": {
            "type": "dict",
            "keys": {
                "key1": {
                    "type": "str",
                    "$ref": "eos_config#/keys/key2",
                },
                "key2": {
                    "type": "str",
                    "description": "this is from key2",
                }
            }
        },
        "avd_design": {
            "type": "dict",
            "allow_other_keys": true,
            "keys": {
                "key3": {
                    "type": "str",
                    "$ref": "eos_config#/keys/key2",
                }
            }
        },
        "cv_deploy": {
            "type": "dict",
            "keys": {
                "key4": {
                    "type": "str",
                    "description": "this is from key4",
                },
                "key5": {
                    "type": "str",
                    "$ref": "cv_deploy#/keys/key4",
                }
            }
        }
    }))
    .unwrap()
}
