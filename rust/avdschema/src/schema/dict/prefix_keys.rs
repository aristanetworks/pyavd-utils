use ordermap::OrderMap;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use serde_with::skip_serializing_none;

use crate::{Store, any::AnySchema, resolve::resolve_ref::resolve_ref};

use super::{Dict, DynamicKeyInfo};

/// PrefixKeys represents keys like custom_structured_configuration_*.
#[skip_serializing_none]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PrefixKeys {
    /// The key in the schema that defines the list of prefixes to look for.
    /// Ex. this could be "custom_structured_configuration_prefixes" which by default holds ["custom_structured_configuration_"]
    pub prefixes_key: String,
    /// Whether to include the suffix (everything after the prefix part of the data key) in the resulting data being validated.
    pub include_suffix_in_data: bool,
    /// Schema reference to use for the dynamic key.
    pub schema_ref: String,
}

/// Helper struct for prefix matching - contains resolved schema and configuration
pub struct ResolvedPrefixConfig<'a> {
    pub prefixes: Vec<String>,
    pub schema: &'a AnySchema,
    pub include_suffix_in_data: bool,
}

impl PrefixKeys {
    /// Resolve this prefix config with the given dict and store
    /// Returns None if prefixes list is empty or schema cannot be resolved
    pub fn resolve<'a>(
        &self,
        dict: &Map<String, Value>,
        dict_schema: &'a Dict,
        store: &'a Store,
    ) -> Option<ResolvedPrefixConfig<'a>> {
        // Get the list of prefixes from the dict, or use the default value from the schema
        let default_value = dict_schema.get_default_for_key(&self.prefixes_key);
        let prefixes = dict
            .get(&self.prefixes_key)
            .or(default_value.as_ref())
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        if prefixes.is_empty() {
            return None;
        }

        // Resolve the schema reference
        let schema = resolve_ref(&self.schema_ref, store).ok()?;

        Some(ResolvedPrefixConfig {
            prefixes,
            schema,
            include_suffix_in_data: self.include_suffix_in_data,
        })
    }
}

/// Result of matching a key against a prefix
pub enum PrefixMatchResult<'a> {
    /// Key matches and has a valid schema
    Valid(&'a AnySchema),
    /// Key matches but suffix is invalid (when include_suffix_in_data is true)
    InvalidSuffix,
}

impl<'a> ResolvedPrefixConfig<'a> {
    /// Check if a key matches any of the prefixes and return the appropriate schema
    pub fn match_key(&self, key: &str) -> Option<PrefixMatchResult<'a>> {
        for prefix in &self.prefixes {
            if let Some(suffix) = key.strip_prefix(prefix.as_str()) {
                // Get the schema - either the base schema or offset by suffix
                let schema = if self.include_suffix_in_data && !suffix.is_empty() {
                    // The schema should be a Dict with keys containing the suffix
                    // Navigate to keys/<suffix>
                    match self.schema {
                        AnySchema::Dict(dict_schema) => match &dict_schema.keys {
                            Some(keys) => match keys.get(suffix) {
                                Some(schema) => schema,
                                None => return Some(PrefixMatchResult::InvalidSuffix),
                            },
                            None => return Some(PrefixMatchResult::InvalidSuffix),
                        },
                        _ => return Some(PrefixMatchResult::InvalidSuffix),
                    }
                } else {
                    // Use the base schema directly
                    self.schema
                };
                return Some(PrefixMatchResult::Valid(schema));
            }
        }
        None
    }
}

impl<'a> Dict {
    /// Get map of prefix keys and their corresponding schema.
    /// Reads the prefix_keys definition in the schema and matches all keys in the dict against the prefixes.
    /// Returns a map of DynamicKeyInfo for each matching key.
    pub fn get_prefix_keys(
        &'a self,
        dict: &Map<String, Value>,
        store: &'a Store,
    ) -> Option<OrderMap<String, DynamicKeyInfo<'a>>> {
        let prefix_configs = self.prefix_keys.as_ref()?;

        // Build a map of matching keys to their DynamicKeyInfo
        let mut result = OrderMap::new();

        for prefix_config in prefix_configs {
            let resolved = prefix_config.resolve(dict, self, store)?;

            // Check each key in the dict against the prefixes
            for key in dict.keys() {
                if let Some(PrefixMatchResult::Valid(schema)) = resolved.match_key(key) {
                    result.insert(
                        key.to_string(),
                        DynamicKeyInfo {
                            dynamic_key_path: prefix_config.prefixes_key.clone(),
                            schema,
                        },
                    );
                }
                // Note: We skip InvalidSuffix here because get_prefix_keys is used for
                // collecting valid keys, not for validation
            }
        }

        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use ordermap::OrderMap;
    use serde_json::{Value, json};

    use crate::{any::AnySchema, base::Base, int::Int, list::List, str::Str, Store};

    use super::*;

    #[test]
    fn get_prefix_keys_basic() {
        use crate::utils::test_utils::get_test_store;

        let store = get_test_store();
        let dict_schema = Dict {
            prefix_keys: Some(vec![PrefixKeys {
                prefixes_key: "custom_structured_configuration_prefixes".to_string(),
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

        let foo_info = result.get("custom_structured_configuration_foo").unwrap();
        assert_eq!(
            foo_info.dynamic_key_path,
            "custom_structured_configuration_prefixes"
        );
        // Verify the schema is a Str (key2 in the test store is a Str)
        let str_schema: Result<&Str, _> = foo_info.schema.try_into();
        assert!(str_schema.is_ok());
    }

    #[test]
    fn get_prefix_keys_with_suffix() {
        use crate::utils::test_utils::get_test_store;

        let store = get_test_store();
        // Create a custom store with a dict schema that has keys for testing suffix navigation
        let custom_store = Store {
            eos_cli_config_gen: AnySchema::deserialize(json!({
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
            })).unwrap(),
            eos_designs: store.eos_designs.clone(),
        };

        let dict_schema = Dict {
            prefix_keys: Some(vec![PrefixKeys {
                prefixes_key: "custom_structured_configuration_prefixes".to_string(),
                include_suffix_in_data: true,
                schema_ref: "eos_cli_config_gen#/keys/suffix_dict".to_string(),
            }]),
            ..Default::default()
        };

        let value: Value = json!({
            "custom_structured_configuration_prefixes": ["custom_structured_configuration_"],
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
        let foo_info = result.get("custom_structured_configuration_foo").unwrap();
        let foo_schema: Result<&Str, _> = foo_info.schema.try_into();
        assert!(foo_schema.is_ok());

        let bar_info = result.get("custom_structured_configuration_bar").unwrap();
        let bar_schema: Result<&Int, _> = bar_info.schema.try_into();
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
                prefixes_key: "custom_structured_configuration_prefixes".to_string(),
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

        let foo_info = result.get("custom_structured_configuration_foo").unwrap();
        assert_eq!(
            foo_info.dynamic_key_path,
            "custom_structured_configuration_prefixes"
        );
        // Verify the schema is a Str (key2 in the test store is a Str)
        let str_schema: Result<&Str, _> = foo_info.schema.try_into();
        assert!(str_schema.is_ok());
    }
}
