// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use ordermap::OrderMap;

use crate::resolve::{errors::SchemaResolverError, resolve_ref::resolve_ref};
use crate::store::SchemaStoreError;
use crate::{
    SchemaDataValue, Store,
    any::AnySchema,
    dict::{Dict, DictKeyMatch},
};

// Keys that are accepted by the schema from either keys or dynamic keys.
#[derive(Debug, PartialEq)]
pub enum SchemaKey {
    StaticKey,
    DynamicKey { dynamic_key_path: String },
}
impl SchemaKey {
    /// Return a schema $ref like
    /// "eos_config#/keys/somekey/items/" or
    /// "avd_design#/dynamic_keys/connected_endpoint_keys.key"
    /// For dynamic keys the first item of the path is replaced with with dynamic key path.
    pub fn get_schema_ref_from_path(&self, schema_name: &str, data_path: &[String]) -> String {
        let mut path = data_path.iter();
        let mut schema_ref = format!("{schema_name}#");
        match path.next() {
            Some(root_key) => match self {
                SchemaKey::DynamicKey { dynamic_key_path } => {
                    schema_ref.push_str(format!("/dynamic_keys/{dynamic_key_path}").as_str())
                }
                SchemaKey::StaticKey => schema_ref.push_str(format!("/keys/{root_key}").as_str()),
            },
            None => return schema_ref,
        }
        for step in path {
            if step.parse::<usize>().is_ok() {
                schema_ref.push_str("/items");
            } else {
                schema_ref.push_str("/keys/");
                schema_ref.push_str(step);
            }
        }
        schema_ref
    }
}

// TODO: Consider removing SchemaKeys since it is no longer used internally.
#[derive(Debug, PartialEq)]
pub struct SchemaKeys {
    pub keys: OrderMap<String, SchemaKey>,
}
impl SchemaKeys {
    pub fn try_from_schema_with_value<'a, V>(
        schema: &AnySchema,
        value: V,
        store: &Store,
    ) -> Result<Self, SchemaKeysError>
    where
        V: SchemaDataValue<'a>,
    {
        let dict_schema: &Dict = schema
            .try_into()
            .map_err(|_err| SchemaKeysError::SchemaNotDict)?;
        let dict = value.as_mapping().ok_or(SchemaKeysError::ValueNotADict)?;
        let mut schema_keys = SchemaKeys {
            keys: dict_schema
                .keys
                .as_ref()
                .map(|keys| {
                    keys.keys()
                        .map(|key| (key.to_owned(), SchemaKey::StaticKey))
                        .collect()
                })
                .unwrap_or_default(),
        };

        schema_keys.keys.extend(
            dict_schema
                .get_dynamic_keys(dict)
                .unwrap_or_default()
                .into_iter()
                .map(|(key, dynamic_key_info)| {
                    (key.to_owned(), SchemaKey::from(&dynamic_key_info))
                }),
        );

        schema_keys.keys.extend(
            dict_schema
                .get_prefix_keys(dict, store)
                .unwrap_or_default()
                .into_iter()
                .map(|(key, dynamic_key_info)| {
                    (key.to_owned(), SchemaKey::from(&dynamic_key_info))
                }),
        );
        Ok(schema_keys)
    }
}
impl From<&crate::dict::DynamicKeyInfo<'_>> for SchemaKey {
    fn from(value: &crate::dict::DynamicKeyInfo) -> Self {
        Self::DynamicKey {
            dynamic_key_path: value.dynamic_key_path.to_owned(),
        }
    }
}

#[derive(Debug)]
pub enum SchemaKeysError {
    ValueNotADict,
    SchemaNotDict,
}

#[derive(Debug, derive_more::From)]
pub enum GetSchemaFromPathError {
    StoreError(SchemaStoreError),
    Keys(SchemaKeysError),
    Resolve(SchemaResolverError),
}
/// Given a data path return the schema covering this.
/// Assumes that dynamic keys can only exist at the root level.
/// Assumes that the root level is a dict.
pub fn get_schema_from_path<'store, 'value>(
    schema_name: &str,
    store: &'store Store,
    data_path: &'_ [String],
    data_value: impl SchemaDataValue<'value>,
) -> Result<Option<&'store AnySchema>, GetSchemaFromPathError> {
    let mut path = data_path.iter();
    let schema = store.get(schema_name)?;
    match path.next() {
        None => Ok(Some(schema)),
        Some(root_key) => {
            let dict_schema: &Dict = schema
                .try_into()
                .map_err(|_err| SchemaKeysError::SchemaNotDict)?;
            let dict = data_value
                .as_mapping()
                .ok_or(GetSchemaFromPathError::Keys(SchemaKeysError::ValueNotADict))?;
            let resolved_dict_keys = dict_schema.resolve_dict_keys(dict, store);
            match resolved_dict_keys.resolve(root_key) {
                None => Ok(None),
                Some(DictKeyMatch::Static(_)) => {
                    let schema_ref =
                        SchemaKey::StaticKey.get_schema_ref_from_path(schema_name, data_path);
                    Ok(Some(resolve_ref(&schema_ref, store)?))
                }
                Some(DictKeyMatch::Dynamic(dynamic_key_info)) => {
                    let schema_ref = SchemaKey::DynamicKey {
                        dynamic_key_path: dynamic_key_info.dynamic_key_path,
                    }
                    .get_schema_ref_from_path(schema_name, data_path);
                    Ok(Some(resolve_ref(&schema_ref, store)?))
                }
                Some(DictKeyMatch::Prefix(prefix_key_match)) => {
                    let mut schema_ref = prefix_key_match.schema_ref;
                    for step in path {
                        if step.parse::<usize>().is_ok() {
                            schema_ref.push_str("/items");
                        } else {
                            schema_ref.push_str("/keys/");
                            schema_ref.push_str(step);
                        }
                    }
                    Ok(Some(resolve_ref(&schema_ref, store)?))
                }
                Some(DictKeyMatch::PrefixInvalidSuffix) => Ok(None),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use serde::Deserialize as _;

    use crate::{int::Int, list::List, str::Str, utils::test_utils::get_test_store};

    use super::*;
    use ordermap::OrderMap;
    use serde_json::json;

    #[test]
    fn schema_keys_try_from_schema_with_value_ok() {
        let store = get_test_store();
        let schema = AnySchema::Dict(Dict {
            keys: Some(OrderMap::from_iter([
                (
                    "outer".into(),
                    List {
                        items: Some(Box::new(
                            Dict {
                                keys: Some(OrderMap::from_iter([(
                                    "inner".into(),
                                    Str::default().into(),
                                )])),
                                ..Default::default()
                            }
                            .into(),
                        )),
                        ..Default::default()
                    }
                    .into(),
                ),
                (
                    "another_key".into(),
                    Str {
                        ..Default::default()
                    }
                    .into(),
                ),
            ])),
            dynamic_keys: Some(OrderMap::from_iter([(
                "outer.inner".into(),
                Int {
                    max: Some(10),
                    ..Default::default()
                }
                .into(),
            )])),
            allow_other_keys: Some(true),
            ..Default::default()
        });
        let value = json!({"outer": [ {"inner": "one"}, {"inner": "two"}, {"inner": "three"}]});
        let result = SchemaKeys::try_from_schema_with_value(&schema, &value, &store);
        assert!(result.is_ok());
        let schema_keys = result.unwrap();
        assert_eq!(
            schema_keys.keys,
            OrderMap::from([
                ("outer".into(), SchemaKey::StaticKey),
                ("another_key".into(), SchemaKey::StaticKey),
                (
                    "one".into(),
                    SchemaKey::DynamicKey {
                        dynamic_key_path: "outer.inner".into()
                    }
                ),
                (
                    "two".into(),
                    SchemaKey::DynamicKey {
                        dynamic_key_path: "outer.inner".into()
                    }
                ),
                (
                    "three".into(),
                    SchemaKey::DynamicKey {
                        dynamic_key_path: "outer.inner".into()
                    }
                ),
            ])
        );
    }

    #[test]
    fn schema_keys_try_from_schema_with_value_wrong_schema_err() {
        let store = get_test_store();
        let schema = AnySchema::Str(Str {
            ..Default::default()
        });
        let value = json!({});
        let result = SchemaKeys::try_from_schema_with_value(&schema, &value, &store);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, SchemaKeysError::SchemaNotDict));
    }
    #[test]
    fn schema_keys_try_from_schema_with_value_wrong_value_err() {
        let store = get_test_store();
        let schema = AnySchema::Dict(Dict {
            ..Default::default()
        });
        let value = json!([]);
        let result = SchemaKeys::try_from_schema_with_value(&schema, &value, &store);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, SchemaKeysError::ValueNotADict));
    }

    #[test]
    fn get_schema_from_path_empty_path_some_ok() {
        let value = json!(
            {"dynamic": [ {"key": "one"}, {"key": "two"}, {"key": "three"}]});
        let store = get_test_store();
        let result = get_schema_from_path("eos_config", &store, &[], &value);
        assert!(result.is_ok());
        let opt = result.unwrap();
        assert!(opt.is_some());
        let schema = opt.unwrap();
        assert_eq!(schema, store.get("eos_config").unwrap());
    }

    #[test]
    fn get_schema_from_path_regular_key_some_ok() {
        let value = json!(
            {"dynamic": [ {"key": "one"}, {"key": "two"}, {"key": "three"}]});
        let store = get_test_store();
        let result = get_schema_from_path("eos_config", &store, &["key2".into()], &value);
        assert!(result.is_ok());
        let opt = result.unwrap();
        assert!(opt.is_some());
        let schema = opt.unwrap();
        let expected_schema: AnySchema = serde_json::from_value(json!({
            "type": "str",
            "description": "this is from key2",
        }))
        .unwrap();
        assert_eq!(schema, &expected_schema);
    }

    #[test]
    fn get_schema_from_path_dynamic_key_some_ok() {
        let value = json!(
            {"dynamic": [ {"key": "one"}, {"key": "two"}, {"key": "three"}]});
        let store = get_test_store();
        let result = get_schema_from_path("eos_config", &store, &["two".into()], &value);
        assert!(result.is_ok());
        let opt = result.unwrap();
        assert!(opt.is_some());
        let schema = opt.unwrap();
        let expected_schema: AnySchema = serde_json::from_value(json!({
            "type": "int",
            "max": 10,
        }))
        .unwrap();
        assert_eq!(schema, &expected_schema);
    }

    #[test]
    fn get_schema_from_path_prefix_key_some_ok() {
        let store = Store::deserialize(json!({
            "myschema": {
                "type": "dict",
                "keys": {
                    "custom_prefixes": {
                        "type": "list",
                        "items": {
                            "type": "str"
                        }
                    },
                    "prefix_schema": {
                        "type": "int",
                        "max": 100
                    }
                },
                "prefix_keys": [
                    {
                        "prefixes_key": "custom_prefixes",
                        "include_suffix_in_data": false,
                        "schema_ref": "myschema#/keys/prefix_schema"
                    }
                ],
                "allow_other_keys": true
            }
        }))
        .unwrap();
        let value = json!({
            "custom_prefixes": ["custom_"],
            "custom_foo": 42
        });

        let result = get_schema_from_path("myschema", &store, &["custom_foo".into()], &value);

        assert!(result.is_ok());
        let schema = result.unwrap().unwrap();
        let expected_schema: AnySchema = serde_json::from_value(json!({
            "type": "int",
            "max": 100,
        }))
        .unwrap();
        assert_eq!(schema, &expected_schema);
    }
}
