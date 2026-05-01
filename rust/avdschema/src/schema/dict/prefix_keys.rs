// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use ordermap::OrderMap;
use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;

use crate::{
    SchemaDataMapping, SchemaDataSequence, SchemaDataValue, Store, any::AnySchema,
    resolve::resolve_ref::resolve_ref,
};

use super::{Dict, DictKeyMatch};

/// PrefixKeys represents keys like custom_structured_configuration_*.
#[skip_serializing_none]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PrefixKeys {
    /// The key in the schema that defines the list of prefixes to look for.
    /// Ex. this could be "custom_structured_configuration_prefixes" which holds ["custom_structured_configuration_"] in input or by schema default.
    pub prefixes_key: Option<String>,
    /// Static list of prefixes to look for.
    /// If set, this takes precedence over `prefixes_key`.
    pub prefixes: Option<Vec<String>>,
    /// Whether to include the suffix (everything after the prefix part of the data key) in the resulting data being validated.
    pub include_suffix_in_data: bool,
    /// Schema reference to use for the dynamic key.
    pub schema_ref: String,
}

/// Helper struct for prefix matching - contains resolved schema and configuration
#[derive(Debug)]
pub struct ResolvedPrefixConfig<'a> {
    pub prefixes_key: Option<String>,
    pub schema_ref: String,
    pub prefixes: Vec<String>,
    pub schema: &'a AnySchema,
    pub include_suffix_in_data: bool,
    pub suffix_keys: Option<&'a OrderMap<String, AnySchema>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixKeyMatch<'a> {
    pub prefixes_key: Option<String>,
    pub schema: &'a AnySchema,
    pub schema_ref: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixMatchResult<'a> {
    Valid(PrefixKeyMatch<'a>),
    InvalidSuffix,
}

impl PrefixKeys {
    /// Resolve this prefix config with the given dict and store.
    /// Returns None if prefixes list is empty or schema cannot be resolved
    pub fn resolve<'a, 'input, M>(
        &self,
        dict: M,
        dict_schema: &'a Dict,
        store: &'a Store,
    ) -> Option<ResolvedPrefixConfig<'a>>
    where
        M: SchemaDataMapping<'input>,
    {
        // Get the list of prefixes from the static schema config or from input/defaults via prefixes_key.
        let prefixes = match &self.prefixes {
            Some(prefixes) => prefixes.clone(),
            None => match self.prefixes_key.as_deref() {
                Some(prefixes_key) => match dict.get(prefixes_key) {
                    Some(value) => Self::get_prefixes(value),
                    None => dict_schema
                        .get_default_for_key(prefixes_key)
                        .as_ref()
                        .map(Self::get_prefixes)
                        .unwrap_or_default(),
                },
                None => Vec::new(),
            },
        };

        if prefixes.is_empty() {
            return None;
        }

        // Resolve the schema reference
        let schema = resolve_ref(&self.schema_ref, store).ok()?;
        let suffix_keys = self
            .include_suffix_in_data
            .then_some(match schema {
                AnySchema::Dict(dict_schema) => dict_schema.keys.as_ref(),
                _ => None,
            })
            .flatten();

        Some(ResolvedPrefixConfig {
            prefixes_key: self.prefixes_key.clone(),
            schema_ref: self.schema_ref.clone(),
            prefixes,
            schema,
            include_suffix_in_data: self.include_suffix_in_data,
            suffix_keys,
        })
    }

    fn get_prefixes<'input>(value: impl SchemaDataValue<'input>) -> Vec<String> {
        let Some(sequence) = value.as_sequence() else {
            return Vec::new();
        };

        let mut prefixes = Vec::new();
        for item in SchemaDataSequence::iter(&sequence) {
            if let Some(prefix) = item.as_str() {
                prefixes.push(prefix.to_owned());
            }
        }
        prefixes
    }
}

impl<'a> ResolvedPrefixConfig<'a> {
    /// Check if a key matches any of the prefixes and return the appropriate schema
    pub fn resolve_match(&self, key: &str) -> Option<PrefixMatchResult<'a>> {
        // Quick return for the key given in prefixes_key since it often overlaps with the actual prefixes.
        if self.prefixes_key.as_deref() == Some(key) {
            return None;
        }

        for prefix in &self.prefixes {
            if let Some(suffix) = key.strip_prefix(prefix.as_str()) {
                // Get the schema - either the base schema or offset by suffix
                let (schema, schema_ref) = if self.include_suffix_in_data && !suffix.is_empty() {
                    match self
                        .suffix_keys
                        .and_then(|suffix_keys| suffix_keys.get(suffix))
                    {
                        Some(schema) => (schema, format!("{}/keys/{suffix}", self.schema_ref)),
                        None => return Some(PrefixMatchResult::InvalidSuffix),
                    }
                } else {
                    // Use the base schema directly
                    (self.schema, self.schema_ref.clone())
                };
                return Some(PrefixMatchResult::Valid(PrefixKeyMatch {
                    prefixes_key: self.prefixes_key.clone(),
                    schema,
                    schema_ref,
                }));
            }
        }
        None
    }
}

impl<'a> Dict {
    /// Resolve prefix key configs for the provided dict data.
    pub fn get_resolved_prefix_configs<'input, M>(
        &'a self,
        dict: M,
        store: &'a Store,
    ) -> Option<Vec<ResolvedPrefixConfig<'a>>>
    where
        M: SchemaDataMapping<'input>,
    {
        self.prefix_keys.as_ref().map(|prefix_configs| {
            prefix_configs
                .iter()
                .filter_map(|prefix_config| prefix_config.resolve(dict, self, store))
                .collect()
        })
    }

    /// Get map of prefix keys and their corresponding schema.
    /// Reads the prefix_keys definition in the schema and matches all keys in the dict against the prefixes.
    /// Returns a map of DynamicKeyInfo for each matching key.
    pub fn get_prefix_keys<'input, M>(
        &'a self,
        dict: M,
        store: &'a Store,
    ) -> Option<OrderMap<String, PrefixKeyMatch<'a>>>
    where
        M: SchemaDataMapping<'input>,
    {
        self.prefix_keys.as_ref()?;

        let resolved_dict_keys = self.resolve_dict_keys(dict, store);
        let result = dict
            .keys()
            .filter_map(|key| match resolved_dict_keys.resolve(key) {
                Some(DictKeyMatch::Prefix(prefix_key_match)) => {
                    Some((key.to_string(), prefix_key_match))
                }
                _ => None,
            })
            .collect::<OrderMap<_, _>>();

        (!result.is_empty()).then_some(result)
    }
}

#[cfg(test)]
mod tests {
    use ordermap::OrderMap;
    use serde_json::{Value, json};

    use crate::{Store, base::Base, int::Int, list::List, str::Str};

    use super::*;

    #[test]
    fn get_prefix_keys_basic() {
        use crate::utils::test_utils::get_test_store;

        let store = get_test_store();
        let dict_schema = Dict {
            prefix_keys: Some(vec![PrefixKeys {
                prefixes_key: Some("custom_structured_configuration_prefixes".to_string()),
                prefixes: None,
                include_suffix_in_data: false,
                schema_ref: "eos_cli_config_gen#/keys/key2".to_string(),
            }]),
            ..Default::default()
        };

        let value: Value = json!({
            "custom_structured_configuration_prefixes": ["custom_structured_configuration_"],
            "custom_structured_configuration_foo": "value1",
            "custom_structured_configuration_bar": "value2",
            "other_key": "value3"
        });
        let dict = value.as_object().unwrap();
        let result = dict_schema.get_prefix_keys(dict, &store);

        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.len(), 2);
        assert!(result.contains_key("custom_structured_configuration_foo"));
        assert!(result.contains_key("custom_structured_configuration_bar"));
        assert!(!result.contains_key("other_key"));

        let foo_match = result.get("custom_structured_configuration_foo").unwrap();
        assert_eq!(
            foo_match.prefixes_key.as_deref(),
            Some("custom_structured_configuration_prefixes")
        );
        // Verify the schema is a Str (key2 in the test store is a Str)
        let str_schema: Result<&Str, _> = foo_match.schema.try_into();
        assert!(str_schema.is_ok());
    }

    #[test]
    fn get_prefix_keys_with_suffix() {
        // Create a custom store with a dict schema that has keys for testing suffix navigation
        let custom_store = Store::deserialize(json!({
            "eos_cli_config_gen": {
                "type": "dict",
                "keys": {
                    "suffix_dict": {
                        "type": "dict",
                        "keys": {
                            "foo": {"type": "str"},
                            "bar": {"type": "int"}
                        }
                    }
                }
            },
            "eos_designs": {
                "type": "dict"
            }
        }))
        .unwrap();

        let dict_schema = Dict {
            prefix_keys: Some(vec![PrefixKeys {
                prefixes_key: None,
                prefixes: Some(vec!["custom_structured_configuration_".to_string()]),
                include_suffix_in_data: true,
                schema_ref: "eos_cli_config_gen#/keys/suffix_dict".to_string(),
            }]),
            ..Default::default()
        };

        let value: Value = json!({
            "custom_structured_configuration_foo": "value1",
            "custom_structured_configuration_bar": 42,
            "other_key": "value3"
        });
        let dict = value.as_object().unwrap();
        let result = dict_schema.get_prefix_keys(dict, &custom_store);

        assert!(result.is_some());
        let result = result.unwrap();
        // Should find both foo and bar
        assert_eq!(result.len(), 2);

        // Check that the schema is offset by the suffix
        let foo_match = result.get("custom_structured_configuration_foo").unwrap();
        let foo_schema: Result<&Str, _> = foo_match.schema.try_into();
        assert!(foo_schema.is_ok());

        let bar_match = result.get("custom_structured_configuration_bar").unwrap();
        let bar_schema: Result<&Int, _> = bar_match.schema.try_into();
        assert!(bar_schema.is_ok());
    }

    #[test]
    fn get_prefix_keys_none() {
        use crate::utils::test_utils::get_test_store;

        let store = get_test_store();
        let dict_schema = Dict::default();
        let value: Value = json!({"key": "value"});
        let dict = value.as_object().unwrap();
        let result = dict_schema.get_prefix_keys(dict, &store);
        assert!(result.is_none());
    }

    #[test]
    fn get_prefix_keys_with_default() {
        use crate::utils::test_utils::get_test_store;

        let store = get_test_store();
        let dict_schema = Dict {
            keys: Some(OrderMap::from_iter([(
                "custom_structured_configuration_prefixes".to_string(),
                List {
                    items: Some(Box::new(Str::default().into())),
                    base: Base {
                        default: Some(vec![json!("custom_structured_configuration_")]),
                        ..Default::default()
                    },
                    ..Default::default()
                }
                .into(),
            )])),
            prefix_keys: Some(vec![PrefixKeys {
                prefixes_key: Some("custom_structured_configuration_prefixes".to_string()),
                prefixes: None,
                include_suffix_in_data: false,
                schema_ref: "eos_cli_config_gen#/keys/key2".to_string(),
            }]),
            ..Default::default()
        };

        // Note: the input data does NOT contain the prefixes_key
        let value: Value = json!({
            "custom_structured_configuration_foo": "value1",
            "custom_structured_configuration_bar": "value2",
            "other_key": "value3"
        });
        let dict = value.as_object().unwrap();
        let result = dict_schema.get_prefix_keys(dict, &store);

        // Should still find the prefix keys using the default value
        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.len(), 2);
        assert!(result.contains_key("custom_structured_configuration_foo"));
        assert!(result.contains_key("custom_structured_configuration_bar"));
        assert!(!result.contains_key("other_key"));

        let foo_match = result.get("custom_structured_configuration_foo").unwrap();
        assert_eq!(
            foo_match.prefixes_key.as_deref(),
            Some("custom_structured_configuration_prefixes")
        );
        // Verify the schema is a Str (key2 in the test store is a Str)
        let str_schema: Result<&Str, _> = foo_match.schema.try_into();
        assert!(str_schema.is_ok());
    }

    #[test]
    fn get_prefix_keys_ignores_input_override() {
        use crate::utils::test_utils::get_test_store;

        let store = get_test_store();
        let dict_schema = Dict {
            prefix_keys: Some(vec![PrefixKeys {
                prefixes_key: None,
                prefixes: Some(vec!["custom_structured_configuration_".to_string()]),
                include_suffix_in_data: false,
                schema_ref: "eos_cli_config_gen#/keys/key2".to_string(),
            }]),
            ..Default::default()
        };

        let value: Value = json!({
            "custom_structured_configuration_prefixes": ["wrong_"],
            "custom_structured_configuration_foo": "value1",
            "wrong_bar": "value2"
        });
        let dict = value.as_object().unwrap();
        let result = dict_schema.get_prefix_keys(dict, &store).unwrap();

        assert!(result.contains_key("custom_structured_configuration_foo"));
        assert!(!result.contains_key("wrong_bar"));
    }
}
