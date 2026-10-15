// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

#[cfg(feature = "dump_load_files")]
use std::ffi::OsStr;
#[cfg(feature = "dump_load_files")]
use std::fs::File;
#[cfg(feature = "dump_load_files")]
use std::io;
#[cfg(any(feature = "dump_load_files", feature = "gzip"))]
use std::io::BufReader;
#[cfg(feature = "dump_load_files")]
use std::path::Path;

use serde::de::DeserializeOwned;
#[cfg(feature = "dump_load_files")]
use walkdir::WalkDir;

#[cfg(feature = "dump_load_files")]
use crate::Inherit;

pub trait Load
where
    Self: DeserializeOwned,
{
    fn from_json(json: &str) -> Result<Self, LoadError> {
        Ok(serde_json::from_str(json)?)
    }

    #[cfg(feature = "dump_load_files")]
    fn from_file(input: Option<&Path>) -> Result<Self, LoadError> {
        // Read input from file / stdin
        match input {
            Some(path) => match path.extension().and_then(OsStr::to_str) {
                Some("yml" | "yaml") => Self::from_yaml_file(path),
                Some("json") => Self::from_json_file(path),
                #[cfg(feature = "xz2")]
                Some("xz2") => Self::from_xz2_file(path),
                Some("gz") => Self::from_gz_file(path),
                _ => Err(LoadError::InvalidExtension {}),
            },
            None => Self::from_stdin(),
        }
    }
    #[cfg(feature = "dump_load_files")]
    fn from_stdin() -> Result<Self, LoadError> {
        let reader = io::stdin();
        Ok(yaml_parser::serde::from_reader(reader)?)
    }
    #[cfg(feature = "dump_load_files")]
    fn from_yaml_file(path: &Path) -> Result<Self, LoadError> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        Ok(yaml_parser::serde::from_reader(reader)?)
    }
    #[cfg(feature = "xz2")]
    fn from_xz2_file(path: &Path) -> Result<Self, LoadError> {
        let file = File::open(path)?;
        let decompressor = xz2::read::XzDecoder::new(file);
        let reader = BufReader::new(decompressor);
        Ok(serde_json::from_reader(reader)?)
    }
    #[cfg(feature = "dump_load_files")]
    fn from_json_file(path: &Path) -> Result<Self, LoadError> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        Ok(serde_json::from_reader(reader)?)
    }
    #[cfg(feature = "xz2")]
    fn from_xz2_bytes(bytes: &[u8]) -> Result<Self, LoadError> {
        let decompressor = xz2::read::XzDecoder::new(bytes);
        let reader = BufReader::new(decompressor);
        Ok(serde_json::from_reader(reader)?)
    }
    #[cfg(feature = "dump_load_files")]
    fn from_gz_file(path: &Path) -> Result<Self, LoadError> {
        let file = File::open(path)?;
        let decompressor = flate2::read::GzDecoder::new(file);
        let reader = BufReader::new(decompressor);
        Ok(serde_json::from_reader(reader)?)
    }
    #[cfg(feature = "gzip")]
    fn from_gz_bytes(bytes: &[u8]) -> Result<Self, LoadError> {
        let decompressor = flate2::read::GzDecoder::new(bytes);
        let reader = BufReader::new(decompressor);
        Ok(serde_json::from_reader(reader)?)
    }
}

#[cfg(feature = "dump_load_files")]
pub trait LoadFromFragments
where
    Self: Load + Inherit + DeserializeOwned,
{
    fn from_fragments(glob: &Path) -> Result<Self, LoadError> {
        let mut glob_iter = WalkDir::new(glob)
            .max_depth(1)
            .sort_by_file_name()
            .into_iter()
            .filter_map(Result::ok)
            .filter_map(|entry| {
                Path::new(entry.file_name().to_str()?)
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("yml"))
                    .then_some(entry)
            });
        let first_file = glob_iter.next().ok_or(LoadError::NoFilesFound {})?;
        let mut combined_data = Self::from_file(Some(first_file.path()))?;
        for file in glob_iter {
            let file_data = Self::from_file(Some(file.path()))?;
            combined_data.inherit(&file_data);
        }
        Ok(combined_data)
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum LoadError {
    JsonError(serde_json::Error),
    #[cfg(feature = "yaml")]
    YamlError(yaml_parser::serde::DeError),
    #[cfg(feature = "dump_load_files")]
    IoError(io::Error),
    #[cfg(feature = "dump_load_files")]
    #[display("Invalid extension for input file.")]
    InvalidExtension {},
    #[cfg(feature = "dump_load_files")]
    #[display("No files found.")]
    NoFilesFound {},
}

#[cfg(all(test, feature = "dump_load_files"))]
mod tests {
    use super::Load as _;
    use crate::StoreSource;
    use crate::any::SourceSchema;
    use crate::utils::test_utils::get_test_dict_schema;
    use crate::utils::test_utils::get_test_store;
    use crate::utils::test_utils::get_tmp_file;

    #[test]
    fn load_yaml() {
        crate::utils::dump::tests::dump_yaml();
        let file_path = get_tmp_file("test_dump.yml");
        let schema = get_test_dict_schema();
        let result = SourceSchema::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), schema);
    }

    #[test]
    fn yaml_description_continuation_survives_file_load_and_compilation() {
        use crate::SchemaView;
        use crate::Store;

        let path = get_tmp_file("test_plain_description_continuation.yml");
        std::fs::write(
            &path,
            "probe:\n  type: dict\n  keys:\n    stateless:\n      type: bool\n      description: Take action, regardless of state. Should be set to\n        'true' for MSS-G.\n      default: true\n",
        ).unwrap();
        let source = StoreSource::from_yaml_file(&path).unwrap();
        let store = Store::compile(&source).unwrap();
        let SchemaView::Dict(root) = store.get("probe").unwrap() else {
            panic!("expected root dictionary");
        };
        assert_eq!(
            root.key("stateless").unwrap().description(),
            Some("Take action, regardless of state. Should be set to 'true' for MSS-G."),
        );
    }

    #[test]
    fn unsigned_integer_defaults_survive_recursive_yaml_load_and_compilation() {
        use crate::SchemaValueView;
        use crate::SchemaView;
        use crate::Store;

        let i64_max_u64 = u64::try_from(i64::MAX).unwrap();

        for number in [i64_max_u64 + 1, u64::MAX] {
            for scalar in [
                number.to_string(),
                format!("+{number}"),
                format!("0x{number:x}"),
                format!("0o{number:o}"),
                format!("!!int {number}"),
            ] {
                let yaml = format!(
                    "probe:\n  type: dict\n  keys:\n    child:\n      type: dict\n      default:\n        big: {scalar}\n"
                );
                let source: StoreSource = yaml_parser::serde::from_str(&yaml).unwrap();
                let store = Store::compile(&source).unwrap();
                let SchemaView::Dict(root) = store.get("probe").unwrap() else {
                    panic!("expected root dictionary");
                };
                let child = root.key("child").unwrap();
                let Some(SchemaValueView::Object(default)) = child.default() else {
                    panic!("expected dictionary default");
                };
                let value = default
                    .iter()
                    .find_map(|(key, value)| (key == "big").then_some(value));
                let Some(SchemaValueView::U64(value)) = value else {
                    panic!("expected unsigned compiled default for {scalar}, found {value:?}");
                };
                assert_eq!(value, number, "compiled default changed for {scalar}");
            }
        }
    }

    #[test]
    fn yaml_dump_roundtrips_empty_defaults() {
        use crate::Dump as _;

        for (name, input) in [
            ("empty_list", r#"{"type":"list","default":[]}"#),
            ("empty_dict", r#"{"type":"dict","default":{}}"#),
            (
                "nested_empty",
                r#"{"type":"dict","default":{"items":[],"settings":{}}}"#,
            ),
        ] {
            let schema = SourceSchema::from_json(input).unwrap();
            let path = get_tmp_file(&format!("test_dump_{name}_default.yml"));
            schema.to_yaml_file(&path).unwrap();
            let reloaded = SourceSchema::from_yaml_file(&path).unwrap();
            assert_eq!(reloaded, schema, "schema default changed for {name}");
        }
    }
    #[test]
    fn load_json() {
        crate::utils::dump::tests::dump_json();
        let file_path = get_tmp_file("test_dump.json");
        let schema = get_test_dict_schema();
        let result = SourceSchema::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), schema);
    }
    #[cfg(feature = "xz2")]
    #[test]
    fn load_xz2() {
        crate::utils::dump::tests::dump_xz2();
        let file_path = get_tmp_file("test_dump.xz2");
        let schema = get_test_dict_schema();
        let result = SourceSchema::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), schema);
    }
    #[test]
    fn load_gz() {
        crate::utils::dump::tests::dump_gz();
        let file_path = get_tmp_file("test_dump.gz");
        let schema = get_test_dict_schema();
        let result = SourceSchema::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), schema);
    }
    #[test]
    fn load_store_yaml() {
        crate::utils::dump::tests::dump_store_yaml();
        let file_path = get_tmp_file("test_dump_store.yml");
        let store = get_test_store();
        let result = StoreSource::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), store);
    }
}
