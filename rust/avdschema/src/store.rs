// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
use std::collections::HashMap;
#[cfg(feature = "dump_load_files")]
use std::path::PathBuf;

use serde::Deserialize;
use serde::Serialize;

use crate::resolve::errors::SchemaResolverError;
use crate::resolve_schema;
use crate::schema::any::AnySchema;
use crate::utils::dump::Dump;
use crate::utils::load::Load;
#[cfg(feature = "dump_load_files")]
use crate::utils::load::LoadError;

/// Schema store containing the AVD schemas.
/// The store is used as entrypoint for validation and when resolving a $ref pointing to a specific schema.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Store {
    #[serde(flatten)]
    schemas: HashMap<String, AnySchema>,
}

impl Store {
    /// Return the schema names present in this store.
    pub fn schema_names(&self) -> Vec<&str> {
        let mut schema_names: Vec<_> = self.schemas.keys().map(String::as_str).collect();
        schema_names.sort_unstable();
        schema_names
    }

    fn fetch<'a, 'b>(
        &'a self,
        schema_name: &'b str,
    ) -> Result<(&'b str, &'a AnySchema), SchemaStoreError> {
        if let Some(schema) = self.schemas.get(schema_name) {
            return Ok((schema_name, schema));
        }

        let alias = match schema_name {
            "eos_designs" => "avd_design",
            "eos_cli_config_gen" => "eos_config",
            "avd_design" => "eos_designs",
            "eos_config" => "eos_cli_config_gen",
            _ => schema_name,
        };

        let schema = self
            .schemas
            .get(alias)
            .ok_or_else(|| SchemaStoreError::InvalidSchemaName(schema_name.to_owned()))?;

        Ok((alias, schema))
    }

    pub fn get(&self, schema_name: &str) -> Result<&AnySchema, SchemaStoreError> {
        self.fetch(schema_name).map(|(_, schema)| schema)
    }

    pub fn canonical_schema_name<'a>(
        &self,
        schema_name: &'a str,
    ) -> Result<&'a str, SchemaStoreError> {
        self.fetch(schema_name)?;
        match schema_name {
            "eos_cli_config_gen" | "eos_config" => Ok("eos_config"),
            "eos_designs" | "avd_design" => Ok("avd_design"),
            _ => Ok(schema_name),
        }
    }

    pub fn as_resolved(mut self) -> Result<Self, SchemaResolverError> {
        // Clone each schema so we can resolve them while still being able to resolve $refs between them.
        let cloned_schemas = self.schemas.clone();
        for (schema_name, mut schema) in cloned_schemas {
            // Inplace resolve schema
            resolve_schema(&mut schema, &self)?;
            self.schemas.insert(schema_name, schema);
        }
        Ok(self)
    }

    /// Create a new store instance based on the schema files in the given paths.
    /// If a path points to a directory, files matching *.yml will be read and combined
    /// with a shallow merge, so avoid overlapping keys.
    /// If a path points to a single .yml or .json file it will be used directly.
    /// If a path points to a .gz file it will decompressed and the inner file,
    /// which must be a json file, will then be used.
    #[cfg(feature = "dump_load_files")]
    pub fn new_from_paths(schema_paths: HashMap<String, PathBuf>) -> Result<Self, LoadError> {
        let mut schemas = HashMap::new();
        for (schema_name, schema_path) in schema_paths {
            schemas.insert(schema_name, AnySchema::new_from_path(schema_path)?);
        }
        Ok(Store { schemas })
    }
}
impl Dump for Store {}
impl Load for Store {}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum SchemaStoreError {
    #[display("Schema name '{_0}' not found in the schema store.")]
    InvalidSchemaName(String),
}

#[cfg(test)]
mod tests {

    #[cfg(feature = "dump_load_files")]
    use super::Load as _;
    #[cfg(feature = "dump_load_files")]
    use crate::Dump as _;
    #[cfg(feature = "dump_load_files")]
    use crate::Store;
    #[cfg(feature = "dump_load_files")]
    use crate::utils::test_utils::get_avd_store;
    use crate::utils::test_utils::get_test_store;
    #[cfg(feature = "dump_load_files")]
    use crate::utils::test_utils::get_tmp_file;
    use serde::Deserialize as _;

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
        let json_result = Store::from_file(Some(&json_file_path));
        assert!(json_result.is_ok());
        assert_eq!(json_result.unwrap(), *store);

        let gzip_file_path = get_tmp_file("test_dump_avd_store_resolved.gz");
        let gzip_result = Store::from_file(Some(&gzip_file_path));
        assert!(gzip_result.is_ok());
        assert_eq!(gzip_result.unwrap(), *store);

        #[cfg(feature = "xz2")]
        {
            let xz_file_path = get_tmp_file("test_dump_avd_store_resolved.xz2");
            let xz_result = Store::from_file(Some(&xz_file_path));
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
        let result = Store::from_file(Some(&file_path));
        assert!(result.is_ok());
    }

    #[test]
    #[cfg(feature = "dump_load_files")]
    #[ignore = "Test only used for manual performance testing"]
    fn quick_load_avd_store_gz() {
        //Depends on dump to be done before. This is just here to test the speed of loading from the file.
        let file_path = get_tmp_file("test_dump_avd_store_resolved.gz");
        let result = Store::from_file(Some(&file_path));
        assert!(result.is_ok());
    }

    #[test]
    #[cfg(feature = "dump_load_files")]
    #[ignore = "Test only used for manual performance testing"]
    fn quick_load_avd_store_xz2() {
        //Depends on dump to be done before. This is just here to test the speed of loading from the file.
        let file_path = get_tmp_file("test_dump_avd_store_resolved.xz2");
        let result = Store::from_file(Some(&file_path));
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
    fn canonical_schema_name_returns_existing_schema_name() {
        let store = get_test_store();

        assert_eq!(
            store.canonical_schema_name("eos_config").unwrap(),
            "eos_config"
        );
        assert_eq!(
            store.canonical_schema_name("cv_deploy").unwrap(),
            "cv_deploy"
        );
    }

    #[test]
    fn canonical_schema_name_returns_alias_target() {
        let store = get_test_store();

        assert_eq!(
            store.canonical_schema_name("eos_cli_config_gen").unwrap(),
            "eos_config"
        );
        assert_eq!(
            store.canonical_schema_name("eos_designs").unwrap(),
            "avd_design"
        );
    }

    #[test]
    fn canonical_schema_name_returns_canonical_name_for_old_store_key() {
        let store = Store::deserialize(serde_json::json!({
            "eos_cli_config_gen": {
                "type": "dict",
                "keys": {}
            }
        }))
        .unwrap();

        assert_eq!(
            store.canonical_schema_name("eos_cli_config_gen").unwrap(),
            "eos_config"
        );
        assert_eq!(
            store.canonical_schema_name("eos_config").unwrap(),
            "eos_config"
        );
        assert!(store.get("eos_config").is_ok());
    }

    #[test]
    fn canonical_schema_name_invalid_schema_name_errors() {
        let store = get_test_store();

        assert!(matches!(
            store.canonical_schema_name("not_a_schema"),
            Err(super::SchemaStoreError::InvalidSchemaName(schema_name)) if schema_name == "not_a_schema"
        ));
    }
}
