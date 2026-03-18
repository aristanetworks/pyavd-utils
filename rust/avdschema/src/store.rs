// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

use crate::{
    resolve_schema,
    schema::any::AnySchema,
    utils::{
        dump::Dump,
        load::{Load, LoadError},
    },
};

/// Schema store containing the AVD schemas.
/// The store is used as entrypoint for validation and when resolving a $ref pointing to a specific schema.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Store {
    #[serde(alias = "eos_cli_config_gen")]
    pub eos_config: AnySchema,
    #[serde(alias = "eos_designs")]
    pub avd_design: AnySchema,
    #[serde(default)]
    pub cv_deploy: Option<AnySchema>,
}
impl Store {
    /// Get the schema for the given schema type, returning an error if the schema is not available in the store.
    pub fn get(&self, schema: Schema) -> Result<&AnySchema, SchemaStoreError> {
        match schema {
            Schema::AVDDesign => Ok(&self.avd_design),
            Schema::EOSConfig => Ok(&self.eos_config),
            Schema::CVDeploy => self
                .cv_deploy
                .as_ref()
                .ok_or_else(|| SchemaNotAvailable::new("cv_deploy".into()).into()),
        }
    }
    pub fn as_resolved(mut self) -> Self {
        // Extract copies of each schema so we can resolve them.
        let mut eos_config_schema = self.eos_config.to_owned();
        let mut avd_design_schema = self.avd_design.to_owned();

        // Next resolve all $ref in each schema, updating the store as we go,
        // to avoid re-resolving nested refs many times.
        resolve_schema(&mut eos_config_schema, &self).unwrap();
        self.eos_config = eos_config_schema;
        resolve_schema(&mut avd_design_schema, &self).unwrap();
        self.avd_design = avd_design_schema;

        // Resolve cv_deploy only if it exists.
        if let Some(cv_deploy) = self.cv_deploy.as_ref() {
            let mut cv_deploy_schema = cv_deploy.to_owned();
            resolve_schema(&mut cv_deploy_schema, &self).unwrap();
            self.cv_deploy = Some(cv_deploy_schema);
        }

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
        avd_design_schema_path: PathBuf,
        eos_config_schema_path: PathBuf,
        cv_deploy_schema_path: Option<PathBuf>,
    ) -> Result<Self, LoadError> {
        Ok(Store {
            eos_config: AnySchema::new_from_path(eos_config_schema_path)?,
            avd_design: AnySchema::new_from_path(avd_design_schema_path)?,
            cv_deploy: cv_deploy_schema_path
                .map(AnySchema::new_from_path)
                .transpose()?,
        })
    }
}
impl Dump for Store {}
impl Load for Store {}

#[derive(Debug, Clone, Copy)]
pub enum Schema {
    AVDDesign,
    EOSConfig,
    CVDeploy,
}

impl TryFrom<&str> for Schema {
    type Error = SchemaStoreError;

    /// Try to get the Schema Enum variant for the string.
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "avd_design" => Ok(Self::AVDDesign),
            "eos_config" => Ok(Self::EOSConfig),
            "eos_designs" => Ok(Self::AVDDesign),
            "eos_cli_config_gen" => Ok(Self::EOSConfig),
            "cv_deploy" => Ok(Self::CVDeploy),
            _ => Err(SchemaName::new(value.into()).into()),
        }
    }
}
impl From<Schema> for String {
    /// Get the schema name as string.
    fn from(value: Schema) -> Self {
        match value {
            Schema::AVDDesign => "avd_design".to_string(),
            Schema::EOSConfig => "eos_config".to_string(),
            Schema::CVDeploy => "cv_deploy".to_string(),
        }
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum SchemaStoreError {
    SchemaName(SchemaName),
    SchemaNotAvailable(SchemaNotAvailable),
}

#[derive(Debug, derive_more::Constructor, derive_more::Display)]
#[display("Schema name '{name}' not found in the schema store.")]
pub struct SchemaName {
    pub name: String,
}

#[derive(Debug, derive_more::Constructor, derive_more::Display)]
#[display("Schema name '{name}' found in the schema store, but is not available.")]
pub struct SchemaNotAvailable {
    pub name: String,
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
