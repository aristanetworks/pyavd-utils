// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

use crate::{
    resolve_schema,
    schema::{any::AnySchema, dict::Dict},
    utils::{
        dump::Dump,
        load::{Load, LoadError},
    },
};

/// Schema store containing the AVD schemas.
/// The store is used as entrypoint for validation and when resolving a $ref pointing to a specific schema.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Store {
    pub eos_cli_config_gen: AnySchema,
    pub eos_designs: AnySchema,
}
impl Store {
    pub fn get(&self, schema: Schema) -> &AnySchema {
        match schema {
            Schema::EosDesigns => &self.eos_designs,
            Schema::EosCliConfigGen => &self.eos_cli_config_gen,
        }
    }
    pub fn as_resolved(mut self) -> Self {
        // Extract copies of each schema so we can resolve them.
        let mut eos_cli_config_gen_schema = self.eos_cli_config_gen.to_owned();
        let mut eos_designs_schema = self.eos_designs.to_owned();

        // Next resolve all $ref in each schema, updating the store as we go,
        // to avoid re-resolving nested refs many times.
        resolve_schema(&mut eos_cli_config_gen_schema, &self).unwrap();
        self.eos_cli_config_gen = eos_cli_config_gen_schema;
        resolve_schema(&mut eos_designs_schema, &self).unwrap();
        self.eos_designs = eos_designs_schema;

        self
    }

    /// Create a new store instance based on the schema files in the given paths.
    /// If a path points to a directory, files matching *.yml will be read and combined
    /// with a shallow merge, so avoid overlapping keys.
    /// If a path points to a single .yml or .json file it will be used directly.
    /// If a path points to a .gz file it will decompressed and the inner file,
    /// which must be a json file, will then be used.
    #[cfg(feature = "dump_load_files")]
    pub fn new_from_paths(
        eos_designs_schema_path: PathBuf,
        eos_cli_config_gen_schema_path: PathBuf,
    ) -> Result<Self, LoadError> {
        Ok(Store {
            eos_cli_config_gen: AnySchema::new_from_path(eos_cli_config_gen_schema_path)?,
            eos_designs: AnySchema::new_from_path(eos_designs_schema_path)?,
        })
    }
}
impl Dump for Store {}
impl Load for Store {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Schema {
    EosDesigns,
    EosCliConfigGen,
}

impl TryFrom<&str> for Schema {
    type Error = SchemaStoreError;

    /// Try to get the Schema Enum variant for the string.
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "eos_designs" => Ok(Self::EosDesigns),
            "eos_cli_config_gen" => Ok(Self::EosCliConfigGen),
            _ => Err(SchemaName::new(value.into()).into()),
        }
    }
}
impl From<Schema> for String {
    /// Get the schema name as string.
    fn from(value: Schema) -> Self {
        match value {
            Schema::EosDesigns => "eos_designs".to_string(),
            Schema::EosCliConfigGen => "eos_cli_config_gen".to_string(),
        }
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum SchemaStoreError {
    SchemaName(SchemaName),
}

#[derive(Debug, derive_more::Constructor, derive_more::Display)]
#[display("Schema name '{name}' not found in the schema store.")]
pub struct SchemaName {
    pub name: String,
}

#[cfg(test)]
mod tests {

    use super::Schema;

    use crate::Store;
    use crate::utils::test_utils::get_test_store;
    use serde::Deserialize as _;
    use serde_json::json;

    #[cfg(feature = "dump_load_files")]
    use super::Load;
    #[cfg(feature = "dump_load_files")]
    use crate::Dump as _;
    #[cfg(feature = "dump_load_files")]
    use crate::utils::test_utils::{get_avd_store, get_tmp_file};

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
