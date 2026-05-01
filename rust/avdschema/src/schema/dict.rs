// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
pub mod dynamic_keys;
pub mod prefix_keys;

use std::sync::OnceLock;

use ordermap::OrderMap;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use serde_with::skip_serializing_none;

use crate::{
    SchemaDataValue,
    any::Shortcuts,
    base::Deprecation,
    utils::schema_data::{SchemaDataMapping, SchemaDataSequence},
};

use super::{
    any::AnySchema,
    base::{Base, documentation_options::DocumentationOptionsDict},
};
use dynamic_keys::CachedDefaultDynamicKeys;
pub use dynamic_keys::{DefaultDynamicKeys, DictKeyMatch, DynamicKeyInfo, ResolvedDictKeys};
pub use prefix_keys::PrefixKeys;

/// AVD Schema for dictionary data.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Dict {
    /// Dictionary of dictionary-keys in the format `{<keyname>: {<schema>}}`.
    /// `keyname` must use snake_case.
    /// `schema` is the schema for each key. This is a recursive schema, so the value must conform to AVD Schema
    pub keys: Option<OrderMap<String, AnySchema>>,
    /// Dictionary of dynamic dictionary-keys in the format `{<variable.path>: {<schema>}}`.
    /// `variable.path` is a variable path using dot-notation and pointing to a variable under the parent dictionary containing dictionary-keys.
    /// If an element of the variable path is a list, every list item will unpacked.
    /// `schema` is the schema for each key. This is a recursive schema, so the value must conform to AVD Schema
    /// Note that this is building the schema from values in the _data_ being validated!
    pub dynamic_keys: Option<OrderMap<String, AnySchema>>,
    pub allow_other_keys: Option<bool>,
    pub relaxed_validation: Option<bool>,
    #[serde(rename = "$id")]
    pub schema_id: Option<String>,
    #[serde(rename = "$schema")]
    pub schema_schema: Option<String>,
    #[serde(rename = "$defs")]
    pub schema_defs: Option<OrderMap<String, AnySchema>>,
    pub prefix_keys: Option<Vec<PrefixKeys>>,
    #[serde(flatten)]
    pub base: Base<OrderMap<String, Value>>,
    pub documentation_options: Option<DocumentationOptionsDict>,
    #[serde(skip)]
    pub _cached_default_dynamic_keys: OnceLock<CachedDefaultDynamicKeys>,
}
impl<'a> Dict {
    /// Return the cached default dynamic keys, initializing them on first access.
    pub fn default_dynamic_keys(&self) -> Option<&DefaultDynamicKeys> {
        self._cached_default_dynamic_keys
            .get_or_init(|| self.init_default_dynamic_keys())
            .as_deref()
    }

    /// Get map of dynamic keys and their corresponding schema.
    /// Reads the dynamic_keys definition in the schema, resolves the pointers in the given inputs (or use the default values for the schema).
    pub fn get_dynamic_keys<'input, M>(
        &'a self,
        dict: M,
    ) -> Option<OrderMap<String, DynamicKeyInfo<'a>>>
    where
        M: SchemaDataMapping<'input>,
    {
        self.dynamic_keys.as_ref().map(|dynamic_keys| {
            let default_dynamic_keys = self.default_dynamic_keys();
            dynamic_keys
                .iter()
                .skip_while(|(_, dynamic_key_schema)| dynamic_key_schema.is_removed())
                .flat_map(|(dynamic_key_path, dynamic_key_schema)| {
                    Dict::get_all(dynamic_key_path, dict)
                        .or_else(|| {
                            default_dynamic_keys.and_then(|default_dynamic_keys| {
                                default_dynamic_keys.get(dynamic_key_path).cloned()
                            })
                        })
                        .map(|keys| {
                            keys.into_iter().map(|key| {
                                (
                                    key.to_owned(),
                                    DynamicKeyInfo {
                                        dynamic_key_path: dynamic_key_path.clone(),
                                        schema: dynamic_key_schema,
                                    },
                                )
                            })
                        })
                        .into_iter()
                        .flatten()
                })
                .collect()
        })
    }

    fn init_default_dynamic_keys(&self) -> CachedDefaultDynamicKeys {
        self.dynamic_keys.as_ref().map(|dynamic_keys| {
            dynamic_keys
                .iter()
                .skip_while(|(_, dynamic_key_schema)| dynamic_key_schema.is_removed())
                .flat_map(|(dynamic_key_path, _)| {
                    dynamic_key_path
                        .split('.')
                        .next()
                        .and_then(|root_key| {
                            Some(self.get_default_for_key(root_key).map(|default_value| {
                                let default_as_input_map =
                                    Map::from_iter([(root_key.to_string(), default_value)]);
                                Dict::get_all(dynamic_key_path, &default_as_input_map).map(
                                    |default_dynamic_keys| {
                                        (dynamic_key_path.clone(), default_dynamic_keys)
                                    },
                                )
                            }))
                            .flatten()
                        })
                        .flatten()
                        .into_iter()
                })
                .collect::<DefaultDynamicKeys>()
                .into()
        })
    }

    pub fn get_default_for_key(&self, key: &str) -> Option<Value> {
        self.keys
            .as_ref()
            .and_then(|keys| keys.get(key).and_then(|key_schema| key_schema.default_()))
    }

    // Get all string values from the given key_path. Non-string values are ignored.
    // Returns None if the first key was missing. That will tell us if we need to look at the default value instead.
    pub(self) fn get_all<'input, M>(key_path: &str, dict: M) -> Option<Vec<String>>
    where
        M: SchemaDataMapping<'input>,
    {
        let mut path = key_path.split('.');
        path.next()
            .and_then(|root_key| dict.get(root_key).map(|value| (root_key, value)))
            .map(|(key, value)| {
                value
                    .walk(path, Some(&mut vec![key.to_string()]))
                    .into_iter()
                    .flat_map(|(_, value)| {
                        if let Some(string) = value.as_str() {
                            return vec![string.to_owned()];
                        }
                        let Some(array) = value.as_sequence() else {
                            // Ignore an incorrect type targeted by the key_path.
                            // The validation will report this during validation of that model.
                            return Vec::new();
                        };
                        let mut strings = Vec::new();
                        for item in array.iter() {
                            if let Some(string) = item.as_str() {
                                strings.push(string.to_string());
                            }
                        }
                        strings
                    })
                    .collect::<Vec<String>>()
            })
    }

    pub fn resolve_dict_keys<'input, M>(
        &'a self,
        dict: M,
        store: &'a crate::Store,
    ) -> ResolvedDictKeys<'a>
    where
        M: SchemaDataMapping<'input>,
    {
        ResolvedDictKeys {
            static_keys: self.keys.as_ref(),
            dynamic_keys: self.get_dynamic_keys(dict),
            prefix_configs: self.get_resolved_prefix_configs(dict, store),
        }
    }
}

impl Shortcuts for Dict {
    fn is_required(&self) -> bool {
        self.base.required.unwrap_or_default()
    }

    fn deprecation(&self) -> &Option<Deprecation> {
        &self.base.deprecation
    }
    fn default_(&self) -> Option<Value> {
        self.base
            .default
            .as_ref()
            .map(|value| Value::Object(Map::from_iter(value.to_owned())))
    }
}

impl<'x> TryFrom<&'x AnySchema> for &'x Dict {
    type Error = &'static str;

    fn try_from(value: &'x AnySchema) -> Result<Self, Self::Error> {
        match value {
            AnySchema::Dict(dict) => Ok(dict),
            _ => Err("Unable to convert from AnySchema to Dict. Invalid Schema type."),
        }
    }
}

#[cfg(test)]
mod tests {
    use ordermap::OrderMap;
    use serde::Deserialize as _;
    use serde_json::{Value, json};

    use crate::{
        Store,
        any::AnySchema,
        boolean::Bool,
        int::Int,
        str::Str,
        utils::test_utils::{get_test_dict_schema, get_test_store},
    };

    use super::{DefaultDynamicKeys, Dict, DictKeyMatch, DynamicKeyInfo};

    #[test]
    fn try_from_anyschema_ok() {
        let anyschema = &AnySchema::Dict(Dict::default());
        let result: Result<&Dict, _> = anyschema.try_into();
        assert!(result.is_ok());
    }
    #[test]
    fn try_from_anyschema_err() {
        let anyschema = &AnySchema::Bool(Bool::default());
        let result: Result<&Dict, _> = anyschema.try_into();
        assert!(result.is_err());
    }

    #[test]
    fn get_dynamic_keys_list_of_dicts() {
        let dynamic_key_schema: AnySchema = Bool::default().into();
        let dict_schema = Dict {
            dynamic_keys: Some(OrderMap::from_iter([(
                "outer.inner".to_string(),
                dynamic_key_schema.clone(),
            )])),
            ..Default::default()
        };
        let value: Value =
            json!({"outer": [ {"inner": "one"}, {"inner": "two"}, {"inner": "three"}]});
        let result = dict_schema.get_dynamic_keys(value.as_object().unwrap());
        assert_eq!(
            result,
            Some(OrderMap::from_iter([
                (
                    "one".to_string(),
                    DynamicKeyInfo {
                        dynamic_key_path: "outer.inner".to_string(),
                        schema: &dynamic_key_schema,
                    }
                ),
                (
                    "two".to_string(),
                    DynamicKeyInfo {
                        dynamic_key_path: "outer.inner".to_string(),
                        schema: &dynamic_key_schema,
                    }
                ),
                (
                    "three".to_string(),
                    DynamicKeyInfo {
                        dynamic_key_path: "outer.inner".to_string(),
                        schema: &dynamic_key_schema,
                    }
                ),
            ]))
        );
    }
    #[test]
    fn get_dynamic_keys_list_of_strings() {
        let dynamic_key_schema: AnySchema = Bool::default().into();
        let dict_schema = Dict {
            dynamic_keys: Some(OrderMap::from_iter([(
                "list".to_string(),
                dynamic_key_schema.clone(),
            )])),
            ..Default::default()
        };
        let value: Value = json!({"list": ["one", "two", "three"]});
        let result = dict_schema.get_dynamic_keys(value.as_object().unwrap());
        assert_eq!(
            result,
            Some(OrderMap::from_iter([
                (
                    "one".to_string(),
                    DynamicKeyInfo {
                        dynamic_key_path: "list".to_string(),
                        schema: &dynamic_key_schema,
                    }
                ),
                (
                    "two".to_string(),
                    DynamicKeyInfo {
                        dynamic_key_path: "list".to_string(),
                        schema: &dynamic_key_schema,
                    }
                ),
                (
                    "three".to_string(),
                    DynamicKeyInfo {
                        dynamic_key_path: "list".to_string(),
                        schema: &dynamic_key_schema,
                    }
                ),
            ]))
        );
    }
    #[test]
    fn get_dynamic_keys_from_schema_with_default_and_override() {
        let test_dict_schema = get_test_dict_schema();
        let dict_schema: &Dict = (&test_dict_schema).try_into().unwrap();
        let dynamic_key_schema = dict_schema
            .dynamic_keys
            .as_ref()
            .unwrap()
            .get("outer.inner")
            .unwrap();
        let value: Value = json!({"outer": [{"inner": "one"}, {"inner": "two"}]});
        let result = dict_schema.get_dynamic_keys(value.as_object().unwrap());
        assert_eq!(
            result,
            Some(OrderMap::from_iter([
                (
                    "one".to_string(),
                    DynamicKeyInfo {
                        dynamic_key_path: "outer.inner".to_string(),
                        schema: dynamic_key_schema,
                    }
                ),
                (
                    "two".to_string(),
                    DynamicKeyInfo {
                        dynamic_key_path: "outer.inner".to_string(),
                        schema: dynamic_key_schema,
                    }
                ),
            ]))
        );
    }

    #[test]
    fn get_dynamic_keys_from_schema_with_default_only() {
        let test_dict_schema = get_test_dict_schema();
        let dict_schema: &Dict = (&test_dict_schema).try_into().unwrap();
        let dynamic_key_schema = dict_schema
            .dynamic_keys
            .as_ref()
            .unwrap()
            .get("outer.inner")
            .unwrap();
        let value: Value = json!({"bool_key": true});
        let result = dict_schema.get_dynamic_keys(value.as_object().unwrap());
        assert_eq!(
            result,
            Some(OrderMap::from_iter([(
                "dyn_key1_int".to_string(),
                DynamicKeyInfo {
                    dynamic_key_path: "outer.inner".to_string(),
                    schema: dynamic_key_schema,
                }
            ),]))
        );
    }

    #[test]
    fn get_all_some() {
        let value: Value = json!({"outer": [{"inner": "one"}, {"inner": "two"}]});
        let result = Dict::get_all("outer.inner", value.as_object().unwrap());
        assert_eq!(result, Some(vec!["one".to_string(), "two".to_string()]));
    }

    #[test]
    fn get_all_none() {
        let value: Value = json!({"outer": [{"inner": "one"}, {"inner": "two"}]});
        let result = Dict::get_all("non_existing.inner", value.as_object().unwrap());
        assert!(result.is_none());
    }

    #[test]
    fn get_default_for_key_some() {
        let test_dict_schema = get_test_dict_schema();
        let dict_schema: &Dict = (&test_dict_schema).try_into().unwrap();
        let result = dict_schema.get_default_for_key("outer");
        assert_eq!(result, Some(json!([{"inner": "dyn_key1_int"}])))
    }

    #[test]
    fn get_default_for_key_none() {
        let test_dict_schema = get_test_dict_schema();
        let dict_schema: &Dict = (&test_dict_schema).try_into().unwrap();
        let result = dict_schema.get_default_for_key("str_key");
        assert!(result.is_none())
    }

    #[test]
    fn default_dynamic_keys_some() {
        let test_dict_schema = get_test_dict_schema();
        let dict_schema: &Dict = (&test_dict_schema).try_into().unwrap();
        let expected_result: DefaultDynamicKeys =
            OrderMap::from_iter([("outer.inner".to_string(), vec!["dyn_key1_int".to_string()])]);
        let result = dict_schema.default_dynamic_keys();
        assert_eq!(result, Some(&expected_result));
    }

    #[test]
    fn default_dynamic_keys_none() {
        let dict_schema = Dict::default();
        let result = dict_schema.default_dynamic_keys();
        assert_eq!(result, None);
    }

    #[test]
    fn resolve_dict_keys_static_dynamic_prefix_and_none() {
        let store = get_test_store();
        let schema = AnySchema::Dict(Dict {
            keys: Some(OrderMap::from_iter([
                ("static".into(), Str::default().into()),
                (
                    "dynamic".into(),
                    crate::list::List {
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
                ),
                (
                    "custom_prefixes".into(),
                    crate::list::List {
                        items: Some(Box::new(Str::default().into())),
                        ..Default::default()
                    }
                    .into(),
                ),
                ("prefix_schema".into(), Int::default().into()),
            ])),
            dynamic_keys: Some(OrderMap::from_iter([(
                "dynamic.key".into(),
                Int::default().into(),
            )])),
            prefix_keys: Some(vec![crate::dict::PrefixKeys {
                prefixes_key: Some("custom_prefixes".into()),
                prefixes: None,
                include_suffix_in_data: false,
                schema_ref: "eos_config#/keys/key1".into(),
            }]),
            allow_other_keys: Some(true),
            ..Default::default()
        });
        let dict_schema: &Dict = (&schema).try_into().unwrap();
        let value = json!({
            "static": "value",
            "dynamic": [{"key": "dyn_1"}],
            "custom_prefixes": ["custom_"],
            "custom_foo": "prefix-value"
        });

        let resolved_dict_keys = dict_schema.resolve_dict_keys(value.as_object().unwrap(), &store);

        assert!(matches!(
            resolved_dict_keys.resolve("static"),
            Some(DictKeyMatch::Static(_))
        ));
        assert!(matches!(
            resolved_dict_keys.resolve("dyn_1"),
            Some(DictKeyMatch::Dynamic(dynamic_key_info))
                if dynamic_key_info.dynamic_key_path == "dynamic.key"
        ));
        assert!(matches!(
            resolved_dict_keys.resolve("custom_foo"),
            Some(DictKeyMatch::Prefix(prefix_key_match))
                if prefix_key_match.prefixes_key.as_deref() == Some("custom_prefixes")
        ));
        assert!(resolved_dict_keys.resolve("missing").is_none());
    }

    #[test]
    fn resolve_dict_keys_prefix_with_suffix_and_default_prefix() {
        let store = Store::deserialize(json!({
            "myschema": {
                "type": "dict",
                "keys": {
                    "custom_prefixes": {
                        "type": "list",
                        "items": {
                            "type": "str"
                        },
                        "default": ["custom_"]
                    },
                    "prefix_schema": {
                        "type": "dict",
                        "keys": {
                            "foo": {"type": "str"},
                            "bar": {"type": "int"}
                        }
                    }
                },
                "prefix_keys": [{
                    "prefixes_key": "custom_prefixes",
                    "include_suffix_in_data": true,
                    "schema_ref": "myschema#/keys/prefix_schema"
                }]
            }
        }))
        .unwrap();
        let schema = store.get("myschema").unwrap();
        let dict_schema: &Dict = schema.try_into().unwrap();
        let value = json!({
            "custom_foo": "value"
        });

        let resolved_dict_keys = dict_schema.resolve_dict_keys(value.as_object().unwrap(), &store);

        assert!(matches!(
            resolved_dict_keys.resolve("custom_foo"),
            Some(DictKeyMatch::Prefix(prefix_key_match))
                if matches!(prefix_key_match.schema, AnySchema::Str(_))
        ));
        assert!(matches!(
            resolved_dict_keys.resolve("custom_bar"),
            Some(DictKeyMatch::Prefix(prefix_key_match))
                if matches!(prefix_key_match.schema, AnySchema::Int(_))
        ));
        assert!(matches!(
            resolved_dict_keys.resolve("custom_baz"),
            Some(DictKeyMatch::PrefixInvalidSuffix)
        ));
    }
}
