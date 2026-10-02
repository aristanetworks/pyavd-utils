// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
use std::collections::HashMap;
#[cfg(feature = "dump_load_files")]
use std::path::PathBuf;

use serde::Deserialize;
use serde::Serialize;

use crate::dict::SourceRootDict;
use crate::dict::root::SourceRootSchema;
use crate::utils::dump::Dump;
use crate::utils::load::Load;
#[cfg(feature = "dump_load_files")]
use crate::utils::load::LoadError;
#[cfg(feature = "dump_load_files")]
use crate::utils::load::LoadFromFragments as _;

/// Source store containing named AVD schema roots.
///
/// Every named schema is a [`SourceRootDict`]. Recursive values below a root use
/// [`crate::any::SourceSchema`] and therefore cannot declare document metadata, reusable
/// definitions, or dynamic keys.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StoreSource {
    #[serde(flatten)]
    schemas: HashMap<String, SourceRootSchema>,
}

impl StoreSource {
    /// Return the schema names present in this store.
    pub fn schema_names(&self) -> Vec<&str> {
        let mut schema_names: Vec<_> = self.schemas.keys().map(String::as_str).collect();
        schema_names.sort_unstable();
        schema_names
    }

    pub fn get(&self, schema_name: &str) -> Result<&SourceRootDict, SchemaStoreError> {
        if let Some(schema) = self.schemas.get(schema_name) {
            return Ok(schema.as_dict());
        }
        // Either we have an invalid schema or we may be using an old schema name,
        // or tests using new schema names towards and old schema store.
        let schema_alias = match schema_name {
            "eos_designs" => "avd_design",
            "eos_cli_config_gen" => "eos_config",
            "avd_design" => "eos_designs",
            "eos_config" => "eos_cli_config_gen",
            _ => schema_name,
        };
        self.schemas
            .get(schema_alias)
            .map(SourceRootSchema::as_dict)
            .ok_or_else(|| SchemaStoreError::InvalidSchemaName(schema_name.to_owned()))
    }

    /// Create a new store instance based on the schema files in the given paths.
    /// If a path points to a directory, files matching `*.yml` are read in filename order and
    /// inherited into one root schema.
    /// If a path points to a single .yml or .json file it will be used directly.
    /// If a path points to a .gz file it will decompressed and the inner file,
    /// which must be a json file, will then be used.
    #[cfg(feature = "dump_load_files")]
    pub fn new_from_paths(schema_paths: HashMap<String, PathBuf>) -> Result<Self, LoadError> {
        let mut schemas = HashMap::new();
        for (schema_name, schema_path) in schema_paths {
            let schema = if schema_path.is_dir() {
                SourceRootSchema::from_fragments(&schema_path)?
            } else {
                SourceRootSchema::from_file(Some(&schema_path))?
            };
            schemas.insert(schema_name, schema);
        }
        Ok(StoreSource { schemas })
    }
}
impl Dump for StoreSource {}
impl Load for StoreSource {}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum SchemaStoreError {
    #[display("Schema name '{_0}' not found in the schema store.")]
    InvalidSchemaName(String),
}

#[cfg(test)]
mod tests {
    use super::Load as _;
    #[cfg(feature = "dump_load_files")]
    use crate::Dump as _;
    #[cfg(feature = "dump_load_files")]
    use crate::StoreSource;
    #[cfg(feature = "dump_load_files")]
    use crate::utils::test_utils::get_avd_store;
    use crate::utils::test_utils::get_test_store;
    #[cfg(feature = "dump_load_files")]
    use crate::utils::test_utils::get_tmp_file;

    #[test]
    #[cfg(feature = "dump_load_files")]
    fn dump_avd_store() {
        // Dumping uncompressed and compressed schema.
        let store = get_avd_store();

        let json_file_path = get_tmp_file("test_dump_avd_store_resolved.json");
        let json_result = store.to_file(Some(&json_file_path));
        assert!(json_result.is_ok());

        // Now dump as compressed file to see the size difference
        let gzip_file_path = get_tmp_file("test_dump_avd_store_resolved.gz");
        let gzip_result = store.to_file(Some(&gzip_file_path));
        assert!(gzip_result.is_ok());

        #[cfg(feature = "xz2")]
        {
            let xz_file_path = get_tmp_file("test_dump_avd_store_resolved.xz2");
            let xz_result = store.to_file(Some(&xz_file_path));
            assert!(xz_result.is_ok());
        }
    }

    #[test]
    #[cfg(feature = "dump_load_files")]
    fn load_avd_store() {
        dump_avd_store();
        let store = get_avd_store();

        // Now load the previously dumped files and compare
        let json_file_path = get_tmp_file("test_dump_avd_store_resolved.json");
        let json_result = StoreSource::from_file(Some(&json_file_path));
        assert!(json_result.is_ok());
        assert_eq!(json_result.unwrap(), *store);

        let gzip_file_path = get_tmp_file("test_dump_avd_store_resolved.gz");
        let gzip_result = StoreSource::from_file(Some(&gzip_file_path));
        assert!(gzip_result.is_ok());
        assert_eq!(gzip_result.unwrap(), *store);

        #[cfg(feature = "xz2")]
        {
            let xz_file_path = get_tmp_file("test_dump_avd_store_resolved.xz2");
            let xz_result = StoreSource::from_file(Some(&xz_file_path));
            assert!(xz_result.is_ok());
            assert_eq!(xz_result.unwrap(), *store);
        }
    }

    #[test]
    #[cfg(feature = "dump_load_files")]
    #[ignore = "Test only used for manual performance testing"]
    fn quick_load_avd_store_json() {
        //Depends on dump to be done before. This is just here to test the speed of loading from the file.
        let file_path = get_tmp_file("test_dump_avd_store_resolved.json");
        let result = StoreSource::from_file(Some(&file_path));
        assert!(result.is_ok());
    }

    #[test]
    #[cfg(feature = "dump_load_files")]
    #[ignore = "Test only used for manual performance testing"]
    fn quick_load_avd_store_gz() {
        //Depends on dump to be done before. This is just here to test the speed of loading from the file.
        let file_path = get_tmp_file("test_dump_avd_store_resolved.gz");
        let result = StoreSource::from_file(Some(&file_path));
        assert!(result.is_ok());
    }

    #[test]
    #[cfg(feature = "dump_load_files")]
    #[ignore = "Test only used for manual performance testing"]
    fn quick_load_avd_store_xz2() {
        //Depends on dump to be done before. This is just here to test the speed of loading from the file.
        let file_path = get_tmp_file("test_dump_avd_store_resolved.xz2");
        let result = StoreSource::from_file(Some(&file_path));
        assert!(result.is_ok());
    }

    #[test]
    fn schema_names_returns_sorted_store_keys() {
        let store = get_test_store();

        assert_eq!(
            store.schema_names(),
            ["avd_design", "cv_deploy", "eos_config"]
        );
    }

    #[test]
    fn source_store_requires_dictionary_roots() {
        for invalid_root in [r#"{"type":"str"}"#, r#"{"keys":{}}"#] {
            let json = format!(r#"{{"test":{invalid_root}}}"#);
            assert!(StoreSource::from_json(&json).is_err());
        }
    }

    #[test]
    fn root_only_properties_are_rejected_on_nested_dictionaries() {
        for (property, value) in [
            (
                "dynamic_keys",
                serde_json::json!({"names": {"type": "str"}}),
            ),
            ("$defs", serde_json::json!({"shared": {"type": "str"}})),
            ("$id", serde_json::json!("nested")),
            ("$schema", serde_json::json!("avd_meta_schema")),
        ] {
            let mut nested = serde_json::json!({"type": "dict"});
            nested
                .as_object_mut()
                .unwrap()
                .insert(property.to_owned(), value);
            let json = serde_json::json!({
                "test": {"type": "dict", "keys": {"nested": nested}}
            })
            .to_string();
            assert!(
                StoreSource::from_json(&json).is_err(),
                "accepted {property}"
            );
        }
    }

    #[test]
    fn root_dictionary_accepts_root_metadata_and_dynamic_keys() {
        let store = StoreSource::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "$id": "test",
                    "$schema": "avd_meta_schema",
                    "dynamic_keys": {"names": {"type": "str"}},
                    "$defs": {"shared": {"type": "int"}}
                }
            }"#,
        )
        .unwrap();
        let root = store.get("test").unwrap();
        assert_eq!(root.schema_id.as_deref(), Some("test"));
        assert_eq!(root.schema_schema.as_deref(), Some("avd_meta_schema"));
        assert_eq!(
            root.dynamic_keys.as_ref().map(ordermap::OrderMap::len),
            Some(1)
        );
        assert_eq!(
            root.schema_defs.as_ref().map(ordermap::OrderMap::len),
            Some(1)
        );
    }

    #[test]
    fn source_store_accepts_null_for_historical_optional_fields() {
        let store = StoreSource::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "description": null,
                    "required": null,
                    "keys": {"value": {"type": "str", "default": null}}
                }
            }"#,
        )
        .unwrap();

        let root = store.get("test").unwrap();
        assert!(root.base.description.is_none());
        assert!(root.base.required.is_none());
        assert!(matches!(
            root.keys.as_ref().and_then(|keys| keys.get("value")),
            Some(crate::any::SourceSchema::Str(value)) if value.base.default.is_none()
        ));
    }
}
