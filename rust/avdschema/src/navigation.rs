// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Data-path traversal and dynamic- and prefix-key resolution over compiled schema views.
//!
//! Static keys and list items can be followed without input data. Dynamic dictionary keys are
//! different: their concrete names come from values elsewhere in the input document, schema
//! defaults, and optional caller overrides, so they are resolved for each operation rather than
//! archived in [`Store`].

use ordermap::OrderMap;
use schema_data::SchemaDataMapping;
use schema_data::SchemaDataSequence as _;
use schema_data::SchemaDataValue;

use crate::DeprecationView;
use crate::DictView;
use crate::DynamicKeyOverrides;
use crate::PrefixKeyView;
use crate::SchemaView;
use crate::Store;

/// Minimal metadata for a resolved schema node.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SchemaInfo<'a> {
    /// Schema type: `bool`, `int`, `str`, `list`, or `dict`.
    pub schema_type: &'static str,
    /// Primary key for a list schema, if configured; `None` for other types.
    pub primary_key: Option<&'a str>,
}

impl Store {
    /// Return minimal schema metadata at a data path.
    ///
    /// Path resolution is performed without caller-provided data or dynamic-key overrides.
    /// For `avd_design`, empty `node_type_keys`, `connected_endpoints_keys`, and
    /// `network_services_keys` lists suppress the corresponding dynamic-key defaults.
    /// Other schema-default resolution follows the existing navigation rules.
    /// Numeric path components traverse list items.
    pub fn get_schema_info(
        &self,
        schema_name: &str,
        data_path: &[String],
    ) -> Result<Option<SchemaInfo<'_>>, SchemaPathError> {
        let empty_data = if schema_name == "avd_design" {
            serde_json::json!({
                "node_type_keys": [],
                "connected_endpoints_keys": [],
                "network_services_keys": [],
            })
        } else {
            serde_json::Value::Object(serde_json::Map::new())
        };
        let Some(view) = self.get_schema_from_path(schema_name, data_path, &empty_data, None)?
        else {
            return Ok(None);
        };
        let (schema_type, primary_key) = match view {
            SchemaView::Bool(_) => ("bool", None),
            SchemaView::Int(_) => ("int", None),
            SchemaView::Str(_) => ("str", None),
            SchemaView::List(schema) => ("list", schema.primary_key()),
            SchemaView::Dict(_) => ("dict", None),
        };
        Ok(Some(SchemaInfo {
            schema_type,
            primary_key,
        }))
    }

    /// Return the effective schema covering a data path.
    ///
    /// Dynamic and prefix keys are resolved at the root dictionary from the supplied
    /// data and optional dynamic-key overrides. Nested dynamic and prefix keys are not
    /// supported, matching the existing path-helper contract.
    pub fn get_schema_from_path<'store, 'input, V>(
        &'store self,
        schema_name: &str,
        data_path: &[String],
        data_value: V,
        dynamic_key_overrides: Option<&DynamicKeyOverrides>,
    ) -> Result<Option<SchemaView<'store>>, SchemaPathError>
    where
        V: SchemaDataValue<'input>,
    {
        let Some(mut view) = self.get(schema_name) else {
            return Err(SchemaPathError::InvalidSchemaName(schema_name.to_owned()));
        };
        let mut path = data_path.iter();
        let Some(root_key) = path.next() else {
            return Ok(Some(view));
        };
        let SchemaView::Dict(root_schema) = view else {
            return Err(SchemaPathError::SchemaNotDict);
        };
        let input = data_value
            .as_mapping()
            .ok_or(SchemaPathError::ValueNotADict)?;
        view = if let Some(static_schema) = root_schema.key(root_key) {
            static_schema
        } else {
            let dynamic_keys = resolve_dynamic_keys(root_schema, input, dynamic_key_overrides);
            if let Some(dynamic_schema) = dynamic_keys.get(root_key).copied() {
                dynamic_schema
            } else if let Some(PrefixKeyResolution::Schema { schema, .. }) =
                resolve_prefix_key(root_schema, input, root_key)
            {
                schema
            } else {
                return Ok(None);
            }
        };

        for component in path {
            view = match view {
                SchemaView::Dict(schema) => {
                    let Some(child) = schema.key(component) else {
                        return Ok(None);
                    };
                    child
                }
                SchemaView::List(schema) if component.parse::<usize>().is_ok() => {
                    let Some(items) = schema.items() else {
                        return Ok(None);
                    };
                    items
                }
                SchemaView::List(_) => return Ok(None),
                SchemaView::Bool(_) | SchemaView::Int(_) | SchemaView::Str(_) => {
                    return Err(SchemaPathError::InvalidTraversal);
                }
            };
        }
        Ok(Some(view))
    }
}

/// Error encountered while mapping a data path to an effective schema.
#[derive(Clone, Debug, derive_more::Display)]
pub enum SchemaPathError {
    /// The requested schema root is not present.
    #[display("Schema name '{_0}' not found in the schema store")]
    InvalidSchemaName(String),
    /// The operation requires a dictionary root schema.
    #[display("Root schema is not a dictionary")]
    SchemaNotDict,
    /// The supplied root data is not a dictionary.
    #[display("Root data is not a dictionary")]
    ValueNotADict,
    /// A path attempts to traverse through a scalar schema.
    #[display("Data path cannot be traversed through this schema node")]
    InvalidTraversal,
}

/// Resolve the concrete keys covered by an effective dictionary's dynamic keys.
///
/// Input values take precedence over schema defaults. Caller overrides are
/// applied last. Removed dynamic-key schemas are excluded.
pub fn resolve_dynamic_keys<'store, 'input, M>(
    schema: DictView<'store>,
    input: M,
    overrides: Option<&DynamicKeyOverrides>,
) -> OrderMap<String, SchemaView<'store>>
where
    M: SchemaDataMapping<'input>,
{
    let mut resolved = OrderMap::new();
    for (path, dynamic_schema) in schema.dynamic_keys() {
        if dynamic_schema
            .deprecation()
            .is_some_and(DeprecationView::removed)
        {
            continue;
        }
        let values = dynamic_values_at_path(path, input).or_else(|| {
            schema
                .default_dynamic_keys(path)
                .map(|values| values.map(ToOwned::to_owned).collect())
        });
        for key in values.into_iter().flatten() {
            resolved.insert(key, dynamic_schema);
        }
    }
    if let Some(overrides) = overrides {
        for (concrete_key, path) in overrides {
            let Some(dynamic_schema) = schema.dynamic_key(path) else {
                continue;
            };
            if dynamic_schema
                .deprecation()
                .is_some_and(DeprecationView::removed)
            {
                continue;
            }
            resolved.insert(concrete_key.clone(), dynamic_schema);
        }
    }
    resolved
}

/// Result of matching one concrete input key against a dictionary's prefix-key configurations.
#[derive(Clone, Copy, Debug)]
pub enum PrefixKeyResolution<'store> {
    /// The input key matched a schema that should validate its value.
    Schema {
        /// Effective schema used to validate the matching value.
        schema: SchemaView<'store>,
        /// Whether required-key checks are disabled below this value.
        relaxed_validation: bool,
    },
    /// A prefix matched, but its suffix is not declared by the target dictionary.
    InvalidSuffix,
    /// A prefix matched and the target dictionary permits undeclared suffixes.
    AllowedOtherSuffix,
}

/// Resolve one concrete input key against an effective dictionary's prefix-key configurations.
///
/// A schema match from any configuration takes precedence over an invalid or permitted suffix
/// result from an earlier configuration. Static prefixes take precedence over prefixes read from
/// input data, and input data takes precedence over schema defaults.
pub fn resolve_prefix_key<'store, 'input, M>(
    schema: DictView<'store>,
    input: M,
    key: &str,
) -> Option<PrefixKeyResolution<'store>>
where
    M: SchemaDataMapping<'input>,
{
    let mut fallback = None;
    for config in schema.prefix_keys() {
        if config.prefixes_key() == Some(key) {
            continue;
        }
        for suffix in matching_suffixes(config, input, key) {
            let resolution = if config.include_suffix_in_data() {
                match config.schema() {
                    SchemaView::Dict(target) => target.key(suffix).map_or_else(
                        || {
                            if target.allow_other_keys() {
                                PrefixKeyResolution::AllowedOtherSuffix
                            } else {
                                PrefixKeyResolution::InvalidSuffix
                            }
                        },
                        |child_schema| PrefixKeyResolution::Schema {
                            schema: child_schema,
                            relaxed_validation: config.relaxed_validation(),
                        },
                    ),
                    SchemaView::Bool(_)
                    | SchemaView::Int(_)
                    | SchemaView::Str(_)
                    | SchemaView::List(_) => PrefixKeyResolution::InvalidSuffix,
                }
            } else {
                PrefixKeyResolution::Schema {
                    schema: config.schema(),
                    relaxed_validation: config.relaxed_validation(),
                }
            };
            if matches!(resolution, PrefixKeyResolution::Schema { .. }) {
                return Some(resolution);
            }
            fallback.get_or_insert(resolution);
        }
    }
    fallback
}

fn matching_suffixes<'store, 'input, 'key, M>(
    config: PrefixKeyView<'store>,
    input: M,
    key: &'key str,
) -> impl Iterator<Item = &'key str>
where
    M: SchemaDataMapping<'input>,
{
    let static_prefixes = config.prefixes();
    let input_prefixes = if static_prefixes.is_none() {
        config
            .prefixes_key()
            .and_then(|prefixes_key| input.get(prefixes_key))
    } else {
        None
    };
    let use_defaults = static_prefixes.is_none() && input_prefixes.is_none();
    static_prefixes
        .into_iter()
        .flatten()
        .filter_map(move |prefix| key.strip_prefix(prefix))
        .chain(
            input_prefixes
                .and_then(SchemaDataValue::as_sequence)
                .into_iter()
                .flat_map(|sequence| sequence.iter())
                .filter_map(SchemaDataValue::as_str)
                .filter_map(move |prefix| key.strip_prefix(prefix)),
        )
        .chain(
            config
                .default_prefixes()
                .filter(move |_| use_defaults)
                .filter_map(move |prefix| key.strip_prefix(prefix)),
        )
}

fn dynamic_values_at_path<'input, M>(key_path: &str, input: M) -> Option<Vec<String>>
where
    M: SchemaDataMapping<'input>,
{
    let mut path = key_path.split('.');
    path.next()
        .and_then(|root_key| input.get(root_key).map(|value| (root_key, value)))
        .map(|(root_key, value)| {
            value
                .walk(path, Some(&mut vec![root_key.to_owned()]))
                .into_values()
                .flat_map(|value| {
                    if let Some(string) = value.as_str() {
                        return vec![string.to_owned()];
                    }
                    value
                        .as_sequence()
                        .map(|items| {
                            items
                                .iter()
                                .filter_map(|item| item.as_str().map(ToOwned::to_owned))
                                .collect()
                        })
                        .unwrap_or_default()
                })
                .collect()
        })
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    fn navigation_store() -> Store {
        Store::from_json(
            &json!({
                "test": {
                    "type": "dict",
                    "keys": {"name": {"type": "str"}},
                    "dynamic_keys": {"network_services_keys.name": {"type": "bool"}},
                    "$defs": {"number": {"type": "int"}},
                    "prefix_keys": [{"prefixes": [""], "include_suffix_in_data": false, "schema_ref": "test#/$defs/number"}]
                }
            })
            .to_string(),
        )
        .expect("navigation schema should compile")
    }

    fn avd_design_navigation_store() -> Store {
        Store::from_json(
            &json!({
                "avd_design": {
                    "type": "dict",
                    "keys": {
                        "node_type_keys": {
                            "type": "list",
                            "primary_key": "key",
                            "default": [{"key": "l3leaf"}],
                            "items": {"type": "dict", "keys": {"key": {"type": "str"}}},
                        },
                        "connected_endpoints_keys": {
                            "type": "list",
                            "primary_key": "key",
                            "default": [{"key": "servers"}],
                            "items": {"type": "dict", "keys": {"key": {"type": "str"}}},
                        },
                        "network_services_keys": {
                            "type": "list",
                            "primary_key": "name",
                            "default": [{"name": "tenants"}],
                            "items": {"type": "dict", "keys": {"name": {"type": "str"}}},
                        },
                    },
                    "dynamic_keys": {
                        "node_type_keys.key": {
                            "type": "list",
                            "primary_key": "node",
                            "items": {"type": "dict", "keys": {"node": {"type": "str"}}},
                        },
                        "connected_endpoints_keys.key": {
                            "type": "list",
                            "primary_key": "name",
                            "items": {"type": "dict", "keys": {"name": {"type": "str"}}},
                        },
                        "network_services_keys.name": {
                            "type": "list",
                            "primary_key": "name",
                            "items": {"type": "dict", "keys": {"name": {"type": "str"}}},
                        },
                    },
                },
            })
            .to_string(),
        )
        .expect("AVD design navigation schema should compile")
    }

    #[test]
    fn schema_info_reports_all_types_and_unkeyed_lists() {
        let store = Store::from_json(
            &json!({
                "test": {
                    "type": "dict",
                    "keys": {
                        "enabled": {"type": "bool"},
                        "count": {"type": "int"},
                        "name": {"type": "str"},
                        "settings": {"type": "dict"},
                        "values": {"type": "list", "items": {"type": "str"}},
                        "keyed": {
                            "type": "list", "primary_key": "name",
                            "items": {"type": "dict", "keys": {"name": {"type": "str"}}}
                        }
                    }
                }
            })
            .to_string(),
        )
        .unwrap();
        for (path, schema_type, primary_key) in [
            (vec![], "dict", None),
            (vec!["enabled"], "bool", None),
            (vec!["count"], "int", None),
            (vec!["name"], "str", None),
            (vec!["settings"], "dict", None),
            (vec!["values"], "list", None),
            (vec!["values", "0"], "str", None),
            (vec!["keyed"], "list", Some("name")),
            (vec!["keyed", "0"], "dict", None),
            (vec!["keyed", "0", "name"], "str", None),
        ] {
            let path = path.into_iter().map(ToOwned::to_owned).collect::<Vec<_>>();
            assert_eq!(
                store.get_schema_info("test", &path).unwrap(),
                Some(SchemaInfo {
                    schema_type,
                    primary_key
                }),
                "path: {path:?}"
            );
        }
        for path in [vec!["missing"], vec!["keyed", "name"]] {
            let path = path.into_iter().map(ToOwned::to_owned).collect::<Vec<_>>();
            assert_eq!(store.get_schema_info("test", &path).unwrap(), None);
        }
        assert!(matches!(
            store.get_schema_info("missing", &[]),
            Err(SchemaPathError::InvalidSchemaName(_))
        ));
        assert!(matches!(
            store.get_schema_info("test", &["name".into(), "invalid".into()]),
            Err(SchemaPathError::InvalidTraversal)
        ));
    }

    #[test]
    fn schema_info_supports_avd_design_static_paths_without_dynamic_defaults() {
        let store = avd_design_navigation_store();
        assert_eq!(
            store
                .get_schema_info("avd_design", &["node_type_keys".into()])
                .unwrap(),
            Some(SchemaInfo {
                schema_type: "list",
                primary_key: Some("key")
            })
        );

        for dynamic_key in ["l3leaf", "servers", "tenants"] {
            assert_eq!(
                store
                    .get_schema_info("avd_design", &[dynamic_key.into()])
                    .unwrap(),
                None
            );
        }
    }

    #[test]
    fn schema_info_preserves_defaults_outside_original_blank_overrides() {
        let store = Store::from_json(
            &json!({
                "avd_design": {
                    "type": "dict",
                    "keys": {
                        "node_type_keys": {
                            "type": "list",
                            "default": [{"key": "custom_nodes"}],
                            "items": {"type": "dict", "keys": {"key": {"type": "str"}}}
                        },
                        "custom_node_type_keys": {
                            "type": "list", "$ref": "avd_design#/keys/node_type_keys"
                        }
                    },
                    "dynamic_keys": {
                        "custom_node_type_keys.key": {
                            "type": "list", "primary_key": "name",
                            "items": {"type": "dict", "keys": {"name": {"type": "str"}}}
                        }
                    }
                }
            })
            .to_string(),
        )
        .unwrap();
        assert_eq!(
            store
                .get_schema_info("avd_design", &["custom_nodes".into()])
                .unwrap(),
            Some(SchemaInfo {
                schema_type: "list",
                primary_key: Some("name")
            })
        );
    }

    #[test]
    fn path_navigation_resolves_keys_in_static_dynamic_prefix_order() {
        let store = navigation_store();
        let data = json!({
            "network_services_keys": [{"name": "dynamic_name"}],
            "name": "configured"
        });
        assert!(matches!(
            store
                .get_schema_from_path("test", &["dynamic_name".into()], &data, None)
                .unwrap(),
            Some(SchemaView::Bool(_))
        ));
        assert!(matches!(
            store
                .get_schema_from_path("test", &["name".into()], &data, None)
                .unwrap(),
            Some(SchemaView::Str(_))
        ));
        let overrides = DynamicKeyOverrides::from_iter([(
            "forced".into(),
            "network_services_keys.name".into(),
        )]);
        assert!(matches!(
            store
                .get_schema_from_path("test", &["forced".into()], &data, Some(&overrides))
                .unwrap(),
            Some(SchemaView::Bool(_))
        ));
    }

    #[test]
    fn path_navigation_prefers_a_valid_prefix_configuration() {
        let store = Store::from_json(&json!({"test": {
            "type": "dict", "keys": {"suffixes": {"type": "dict", "keys": {"name": {"type": "str"}}}, "number": {"type": "int"}},
            "prefix_keys": [
                {"prefixes": ["shared_"], "include_suffix_in_data": true, "schema_ref": "test#/keys/suffixes"},
                {"prefixes": ["shared_"], "include_suffix_in_data": false, "schema_ref": "test#/keys/number"}
            ]
        }}).to_string()).unwrap();
        assert!(matches!(
            store
                .get_schema_from_path("test", &["shared_unknown".into()], &json!({}), None)
                .unwrap(),
            Some(SchemaView::Int(_))
        ));
    }

    #[test]
    fn path_navigation_reports_invalid_roots_and_traversal() {
        let store = Store::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "keys": {
                        "scalar": {"type": "str"},
                        "nested": {"type": "dict", "keys": {}},
                        "empty_list": {"type": "list"},
                        "list": {"type": "list", "items": {"type": "bool"}}
                    }
                }
            }"#,
        )
        .unwrap();
        let mapping = json!({});

        assert!(matches!(
            store.get_schema_from_path("missing", &[], &mapping, None),
            Err(SchemaPathError::InvalidSchemaName(name)) if name == "missing"
        ));
        assert!(matches!(
            store.get_schema_from_path("test", &["scalar".into()], &json!([]), None),
            Err(SchemaPathError::ValueNotADict)
        ));
        assert!(matches!(
            store.get_schema_from_path(
                "test",
                &["nested".into(), "missing".into()],
                &mapping,
                None,
            ),
            Ok(None)
        ));
        assert!(matches!(
            store.get_schema_from_path("test", &["empty_list".into(), "0".into()], &mapping, None,),
            Ok(None)
        ));
        assert!(matches!(
            store.get_schema_from_path(
                "test",
                &["list".into(), "not_an_index".into()],
                &mapping,
                None,
            ),
            Ok(None)
        ));
        assert!(matches!(
            store.get_schema_from_path("test", &["scalar".into(), "child".into()], &mapping, None,),
            Err(SchemaPathError::InvalidTraversal)
        ));
    }

    #[test]
    fn dynamic_key_resolution_excludes_removed_and_invalid_overrides() {
        let store = Store::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "dynamic_keys": {
                        "node_type_keys.key": {"type": "bool"},
                        "custom_node_type_keys.key": {
                            "type": "bool",
                            "deprecation": {"warning": false, "removed": true}
                        }
                    }
                }
            }"#,
        )
        .unwrap();
        let Some(SchemaView::Dict(root)) = store.get("test") else {
            panic!("test root should be a dict")
        };
        let input = json!({
            "node_type_keys": [{"key": "live"}],
            "custom_node_type_keys": [{"key": "gone"}]
        });
        let overrides = DynamicKeyOverrides::from_iter([
            ("forced".into(), "node_type_keys.key".into()),
            ("forced_removed".into(), "custom_node_type_keys.key".into()),
            ("unknown".into(), "missing".into()),
        ]);

        let resolved = resolve_dynamic_keys(
            root,
            input.as_object().expect("input should be a mapping"),
            Some(&overrides),
        );
        assert_eq!(
            resolved.keys().map(String::as_str).collect::<Vec<_>>(),
            ["live", "forced"]
        );
    }

    #[test]
    fn overlapping_prefixes_consider_all_suffixes_from_every_source() {
        for source in ["static", "input", "default"] {
            let mut config = json!({"prefixes_key": "prefixes", "include_suffix_in_data": true, "schema_ref": "target#"});
            if source == "static" {
                config["prefixes"] = json!(["custom_", "custom_long_"]);
            }
            let store = Store::from_json(&json!({
                "test": {"type": "dict", "keys": {"prefixes": {"type": "list", "items": {"type": "str"}, "default": ["custom_", "custom_long_"]}}, "prefix_keys": [config]},
                "target": {"type": "dict", "keys": {"number": {"type": "int"}}}
            }).to_string()).unwrap();
            let data = if source == "static" {
                json!({"prefixes": ["input_"]})
            } else if source == "input" {
                json!({"prefixes": ["custom_", "custom_long_"]})
            } else {
                json!({})
            };
            assert!(
                matches!(
                    store
                        .get_schema_from_path("test", &["custom_long_number".into()], &data, None)
                        .unwrap(),
                    Some(SchemaView::Int(_))
                ),
                "{source}"
            );
            if source == "static" {
                assert!(
                    store
                        .get_schema_from_path("test", &["input_number".into()], &data, None)
                        .unwrap()
                        .is_none()
                );
            } else {
                for prefixes in [json!([]), json!(null), json!("custom_")] {
                    assert!(
                        store
                            .get_schema_from_path(
                                "test",
                                &["custom_long_number".into()],
                                &json!({"prefixes": prefixes}),
                                None
                            )
                            .unwrap()
                            .is_none()
                    );
                }
            }
            assert!(
                store
                    .get_schema_from_path("test", &["custom_".into()], &json!({}), None)
                    .unwrap()
                    .is_none()
            );
        }
    }

    #[test]
    fn prefix_defaults_follow_references_and_static_keys_take_precedence() {
        let store = Store::from_json(&json!({"test": {
            "type": "dict", "$defs": {"prefixes": {"type": "list", "items": {"type": "str"}, "default": ["custom_"]}},
            "keys": {"prefixes": {"type": "list", "$ref": "test#/$defs/prefixes"}, "custom_name": {"type": "str"}, "number": {"type": "int"}},
            "prefix_keys": [{"prefixes_key": "prefixes", "include_suffix_in_data": false, "schema_ref": "test#/keys/number"}]
        }}).to_string()).unwrap();
        assert!(matches!(
            store
                .get_schema_from_path("test", &["custom_name".into()], &json!({}), None)
                .unwrap(),
            Some(SchemaView::Str(_))
        ));
        assert!(matches!(
            store
                .get_schema_from_path("test", &["custom_other".into()], &json!({}), None)
                .unwrap(),
            Some(SchemaView::Int(_))
        ));
    }
}
