// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
use serde::{Deserialize, Serialize};

use std::collections::HashMap;
#[cfg(feature = "dump_load_files")]
use std::path::PathBuf;

use crate::{
    resolve::errors::SchemaResolverError,
    resolve_schema,
    schema::any::AnySchema,
    utils::{dump::Dump, load::Load},
};

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
    pub fn get(&self, schema_name: &str) -> Result<&AnySchema, SchemaStoreError> {
        if let Some(schema) = self.schemas.get(schema_name) {
            return Ok(schema);
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
            .ok_or_else(|| SchemaStoreError::InvalidSchemaName(schema_name.to_string()))
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
    use {
        super::Load,
        crate::utils::test_utils::{get_avd_store, get_tmp_file},
        crate::{Dump as _, Store},
    };

    #[test]
    #[cfg(feature = "dump_load_files")]
    fn dump_avd_store() {
        // Dumping uncompressed and compressed schema.
        let store = get_avd_store();

        let file_path = get_tmp_file("test_dump_avd_store_resolved.json");
        let result = store.to_file(Some(&file_path));
        assert!(result.is_ok());

        // Now dump as compressed file to see the size difference
        let file_path = get_tmp_file("test_dump_avd_store_resolved.gz");
        let result = store.to_file(Some(&file_path));
        assert!(result.is_ok());

        #[cfg(feature = "xz2")]
        {
            let file_path = get_tmp_file("test_dump_avd_store_resolved.xz2");
            let result = store.to_file(Some(&file_path));
            assert!(result.is_ok());
        }
    }

    #[test]
    #[cfg(feature = "dump_load_files")]
    fn load_avd_store() {
        dump_avd_store();
        let store = get_avd_store();

        // Now load the previously dumped files and compare
        let file_path = get_tmp_file("test_dump_avd_store_resolved.json");
        let result = Store::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), *store);

        let file_path = get_tmp_file("test_dump_avd_store_resolved.gz");
        let result = Store::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), *store);

        #[cfg(feature = "xz2")]
        {
            let file_path = get_tmp_file("test_dump_avd_store_resolved.xz2");
            let result = Store::from_file(Some(&file_path));
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), *store);
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
}
