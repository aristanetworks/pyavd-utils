// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Data-path traversal and dynamic-key resolution over compiled schema views.
//!
//! Static keys and list items can be followed without input data. Dynamic dictionary keys are
//! different: their concrete names come from values elsewhere in the input document, schema
//! defaults, and optional caller overrides, so they are resolved for each operation rather than
//! archived in [`Store`].

use ordermap::OrderMap;

use crate::DeprecationView;
use crate::DictView;
use crate::DynamicKeyOverrides;
use crate::SchemaDataMapping;
use crate::SchemaDataSequence as _;
use crate::SchemaDataValue as _;
use crate::SchemaView;
use crate::Store;

impl Store {
    /// Return the primary key for the list schema at a data path.
    ///
    /// Path resolution is performed without caller-provided data or dynamic-key overrides.
    /// Schema-defined default dynamic keys remain available. Numeric path components traverse
    /// list items.
    pub fn get_list_primary_key(
        &self,
        schema_name: &str,
        data_path: &[String],
    ) -> Result<Option<&str>, SchemaPathError> {
        let empty_data = serde_json::Value::Object(serde_json::Map::new());
        let Some(view) = self.get_schema_from_path(schema_name, data_path, &empty_data, None)?
        else {
            return Ok(None);
        };
        Ok(match view {
            SchemaView::List(schema) => schema.primary_key(),
            SchemaView::Bool(_) | SchemaView::Int(_) | SchemaView::Str(_) | SchemaView::Dict(_) => {
                None
            }
        })
    }

    /// Return the effective schema covering a data path.
    ///
    /// Dynamic keys are resolved at the root dictionary from the supplied
    /// data and optional caller overrides. Nested dynamic keys are not
    /// supported, matching the existing path-helper contract.
    pub fn get_schema_from_path<'store, 'input, V>(
        &'store self,
        schema_name: &str,
        data_path: &[String],
        data_value: V,
        dynamic_key_overrides: Option<&DynamicKeyOverrides>,
    ) -> Result<Option<SchemaView<'store>>, SchemaPathError>
    where
        V: crate::SchemaDataValue<'input>,
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
            let Some(dynamic_schema) = dynamic_keys.get(root_key).copied() else {
                return Ok(None);
            };
            dynamic_schema
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
                    "dynamic_keys": {"names": {"type": "bool"}}
                }
            })
            .to_string(),
        )
        .expect("navigation schema should compile")
    }

    #[test]
    fn path_navigation_resolves_dynamic_keys_and_static_precedence() {
        let store = navigation_store();
        let data = json!({"names": ["dynamic_name"], "name": "configured"});
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
        let overrides = DynamicKeyOverrides::from_iter([("forced".into(), "names".into())]);
        assert!(matches!(
            store
                .get_schema_from_path("test", &["forced".into()], &data, Some(&overrides))
                .unwrap(),
            Some(SchemaView::Bool(_))
        ));
    }

    #[test]
    fn path_navigation_reports_invalid_roots_and_traversal() {
        let store = Store::from_json(
            r#"{
                "scalar_root": {"type": "str"},
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
            store.get_schema_from_path("scalar_root", &["child".into()], &mapping, None),
            Err(SchemaPathError::SchemaNotDict)
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
                        "active": {"type": "bool"},
                        "removed": {
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
        let input = json!({"active": ["live"], "removed": ["gone"]});
        let overrides = DynamicKeyOverrides::from_iter([
            ("forced".into(), "active".into()),
            ("forced_removed".into(), "removed".into()),
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
}
