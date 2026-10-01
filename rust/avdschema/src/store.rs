// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Ownership, loading, and integrity of the compiled schema store.

#![allow(
    clippy::mem_forget,
    reason = "self_cell internally uses mem::forget to safely construct its self-referential owner"
)]

#[cfg(any(feature = "dump_load_files", feature = "mmap"))]
use std::path::Path;
use std::sync::OnceLock;

use fancy_regex::Regex;
use fancy_regex::RegexBuilder;
#[cfg(feature = "mmap")]
use mmap_guard::FileData;
use rkyv::rancor::Error as RkyvError;
use self_cell::self_cell;

use crate::Load as _;
use crate::SchemaView;
use crate::StoreSource;
use crate::compiled::ARCHIVE_FORMAT_VERSION;
use crate::compiled::ARCHIVE_HEADER_LENGTH;
use crate::compiled::ARCHIVE_MAGIC;
use crate::compiled::ArchivedCompiledStore;
use crate::compiled::ArchivedSchemaId;
use crate::compiled::CompiledStore;

enum ArchiveBytes {
    #[cfg(feature = "mmap")]
    Mapped(FileData),
    Owned(rkyv::util::AlignedVec),
}

impl AsRef<[u8]> for ArchiveBytes {
    fn as_ref(&self) -> &[u8] {
        match self {
            #[cfg(feature = "mmap")]
            Self::Mapped(data) => data.as_ref(),
            Self::Owned(data) => data.as_slice(),
        }
    }
}

struct ArchiveRoot<'a>(&'a ArchivedCompiledStore);

self_cell!(
    struct ArchiveCell {
        owner: ArchiveBytes,

        #[covariant]
        dependent: ArchiveRoot,
    }
);

/// Immutable compiled schema store plus process-local derived caches.
///
/// A store either owns aligned archive bytes compiled in this process or retains a native
/// memory mapping. Views borrow directly from those bytes without deserializing schema nodes into
/// separate Rust objects. The only mutable derived state is the thread-safe regular-expression
/// cache.
pub struct Store {
    archive: ArchiveCell,
    compiled_patterns: Vec<OnceLock<Result<Regex, String>>>,
}

impl std::fmt::Debug for Store {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Store")
            .field("roots", &self.archived().roots.len())
            .field("compiled_patterns", &self.compiled_patterns.len())
            .finish_non_exhaustive()
    }
}

impl Store {
    /// Load a JSON schema source and compile it into process-owned archived bytes.
    pub fn from_json(json: &str) -> Result<Self, StoreError> {
        let source = StoreSource::from_json(json)
            .map_err(|error| StoreError::InvalidSource(error.to_string()))?;
        Self::compile(&source)
    }

    /// Load gzip-compressed JSON schema source and compile it into process-owned archived bytes.
    #[cfg(feature = "gzip")]
    pub fn from_gz_bytes(bytes: &[u8]) -> Result<Self, StoreError> {
        let source = StoreSource::from_gz_bytes(bytes)
            .map_err(|error| StoreError::InvalidSource(error.to_string()))?;
        Self::compile(&source)
    }

    /// Memory-map and validate a compiled schema archive.
    ///
    /// The archive must use the format version supported by this crate. The returned store keeps
    /// the mapping alive for as long as any borrowed view can exist.
    ///
    /// The mapped file must not be modified or truncated in place while the returned store is
    /// alive. Publish archive updates by writing a separate file and replacing the path atomically,
    /// as the archive writer does, or by using a new path.
    #[cfg(feature = "mmap")]
    pub fn from_file(path: &Path) -> Result<Self, StoreError> {
        let bytes = mmap_guard::map_file(path)?;
        Self::from_bytes(ArchiveBytes::Mapped(bytes))
    }

    /// Compile a raw schema store into process-owned archived bytes.
    pub fn compile(source: &StoreSource) -> Result<Self, StoreError> {
        let bytes = CompiledStore::compile(source)?.to_bytes()?;
        Self::from_bytes(ArchiveBytes::Owned(bytes))
    }

    /// Compile one named root and its reachable schema nodes into owned bytes.
    pub fn compile_schema(source: &StoreSource, schema_name: &str) -> Result<Self, StoreError> {
        let bytes = CompiledStore::compile_schema(source, schema_name)?.to_bytes()?;
        Self::from_bytes(ArchiveBytes::Owned(bytes))
    }

    /// Compile a source store and atomically write its archived runtime representation.
    #[cfg(feature = "dump_load_files")]
    pub fn compile_to_file(source: &StoreSource, destination: &Path) -> Result<(), StoreError> {
        CompiledStore::compile_to_file(source, destination)?;
        Ok(())
    }

    fn from_bytes(bytes: ArchiveBytes) -> Result<Self, StoreError> {
        let archive = ArchiveCell::try_new(bytes, |bytes| {
            validate_header(bytes.as_ref())?;
            rkyv::access::<ArchivedCompiledStore, RkyvError>(bytes.as_ref())
                .map_err(|error| StoreError::InvalidArchive(error.to_string()))
                .and_then(|store| {
                    validate_integrity(store)?;
                    Ok(ArchiveRoot(store))
                })
        })?;
        let pattern_count = archive.borrow_dependent().0.strings.len();
        Ok(Self {
            archive,
            compiled_patterns: std::iter::repeat_with(OnceLock::new)
                .take(pattern_count)
                .collect(),
        })
    }

    /// Return the root schema view for a schema name, including AVD aliases.
    pub fn get(&self, schema_name: &str) -> Option<SchemaView<'_>> {
        let archived = self.archived();
        let id = archived.roots.get(schema_name).or_else(|| {
            let alias = match schema_name {
                "eos_designs" => "avd_design",
                "eos_cli_config_gen" => "eos_config",
                "avd_design" => "eos_designs",
                "eos_config" => "eos_cli_config_gen",
                _ => return None,
            };
            archived.roots.get(alias)
        })?;
        Some(crate::views::schema_view(self, *id))
    }

    pub(crate) fn archived(&self) -> &ArchivedCompiledStore {
        self.archive.borrow_dependent().0
    }

    pub(crate) fn pattern(&self, index: u32, pattern: &str) -> Result<&Regex, &str> {
        let Some(cell) = usize::try_from(index)
            .ok()
            .and_then(|index| self.compiled_patterns.get(index))
        else {
            return Err("string schema index is outside the pattern cache");
        };
        cell.get_or_init(|| {
            RegexBuilder::new(format!("^(?:{pattern})$").as_str())
                // Unicode mode keeps `.`, negated classes, and Perl shorthand complements valid
                // for Rust UTF-8 strings. Only the Unicode Perl tables are enabled in Cargo, so
                // broad properties and scripts remain unavailable.
                .unicode_mode(true)
                .build()
                .map_err(|error| error.to_string())
        })
        .as_ref()
        .map_err(String::as_str)
    }
}

/// Error returned while compiling, loading, or validating a compiled schema store.
#[derive(Debug, derive_more::Display)]
pub enum StoreError {
    /// The bytes do not carry the compiled-schema archive magic header.
    #[display("Input is not a compiled AVD schema archive")]
    NotArchive,
    /// The archive format version is not supported by this library.
    #[display("Unsupported compiled schema archive version {found}; expected {expected}")]
    UnsupportedVersion {
        /// Version encoded in the archive.
        found: u32,
        /// Version supported by this library.
        expected: u32,
    },
    /// The archive bytes or internal identifiers failed validation.
    #[display("Invalid compiled schema archive: {_0}")]
    InvalidArchive(String),
    /// The source schema store could not be deserialized.
    #[display("Invalid schema source: {_0}")]
    InvalidSource(String),
    /// A filesystem operation failed.
    Io(std::io::Error),
    /// Source-schema compilation or archive serialization failed.
    Compile(crate::compiled::CompileError),
}

impl From<std::io::Error> for StoreError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

impl From<crate::compiled::CompileError> for StoreError {
    fn from(error: crate::compiled::CompileError) -> Self {
        Self::Compile(error)
    }
}

fn validate_header(bytes: &[u8]) -> Result<(), StoreError> {
    if bytes.get(..ARCHIVE_MAGIC.len()) != Some(ARCHIVE_MAGIC) {
        return Err(StoreError::NotArchive);
    }
    if bytes.len() < ARCHIVE_HEADER_LENGTH {
        return Err(StoreError::InvalidArchive(
            "archive header is truncated".to_owned(),
        ));
    }
    let version_bytes: [u8; 4] = bytes
        .get(ARCHIVE_MAGIC.len()..ARCHIVE_MAGIC.len() + 4)
        .and_then(|bytes| bytes.try_into().ok())
        .ok_or(StoreError::NotArchive)?;
    let found = u32::from_le_bytes(version_bytes);
    if found != ARCHIVE_FORMAT_VERSION {
        return Err(StoreError::UnsupportedVersion {
            found,
            expected: ARCHIVE_FORMAT_VERSION,
        });
    }
    Ok(())
}

/// Validate every archived identifier before a public view can perform unchecked table lookup.
///
/// Keep this synchronized with every [`ArchivedSchemaId`] field added to the compiled model.
fn validate_integrity(store: &ArchivedCompiledStore) -> Result<(), StoreError> {
    for id in store.roots.values() {
        validate_schema_id(store, *id)?;
    }
    for schema in store.lists.iter() {
        if let Some(id) = schema.items.as_ref() {
            validate_schema_id(store, *id)?;
        }
    }
    for schema in store.dicts.iter() {
        for id in schema.keys.values().chain(schema.dynamic_keys.values()) {
            validate_schema_id(store, *id)?;
        }
    }
    Ok(())
}

fn validate_schema_id(
    store: &ArchivedCompiledStore,
    id: ArchivedSchemaId,
) -> Result<(), StoreError> {
    let valid = match id {
        ArchivedSchemaId::Bool(index) => {
            usize::try_from(index.to_native()).is_ok_and(|index| index < store.bools.len())
        }
        ArchivedSchemaId::Int(index) => {
            usize::try_from(index.to_native()).is_ok_and(|index| index < store.ints.len())
        }
        ArchivedSchemaId::Str(index) => {
            usize::try_from(index.to_native()).is_ok_and(|index| index < store.strings.len())
        }
        ArchivedSchemaId::List(index) => {
            usize::try_from(index.to_native()).is_ok_and(|index| index < store.lists.len())
        }
        ArchivedSchemaId::Dict(index) => {
            usize::try_from(index.to_native()).is_ok_and(|index| index < store.dicts.len())
        }
    };
    if valid {
        Ok(())
    } else {
        Err(StoreError::InvalidArchive(format!(
            "schema id {id:?} is outside its typed table"
        )))
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "gzip")]
    use std::io::Write as _;

    use indexmap::IndexMap;
    #[cfg(feature = "gzip")]
    use serde_json::json;

    use super::*;
    use crate::SchemaView;
    use crate::compiled::CompiledStore;
    use crate::compiled::SchemaId;
    #[test]
    fn constructors_reject_non_archives_versions_and_invalid_ids() {
        assert!(matches!(
            Store::from_bytes(ArchiveBytes::Owned(rkyv::util::AlignedVec::new())),
            Err(StoreError::NotArchive)
        ));

        let mut truncated = rkyv::util::AlignedVec::new();
        truncated.extend_from_slice(ARCHIVE_MAGIC);
        assert!(matches!(
            Store::from_bytes(ArchiveBytes::Owned(truncated)),
            Err(StoreError::InvalidArchive(_))
        ));

        let mut versioned = CompiledStore::default().to_bytes().unwrap();
        versioned[ARCHIVE_MAGIC.len()..ARCHIVE_MAGIC.len() + 4]
            .copy_from_slice(&(ARCHIVE_FORMAT_VERSION + 1).to_le_bytes());
        assert!(matches!(
            Store::from_bytes(ArchiveBytes::Owned(versioned)),
            Err(StoreError::UnsupportedVersion { .. })
        ));

        let invalid = CompiledStore {
            roots: IndexMap::from_iter([("invalid".into(), SchemaId::Dict(0))]),
            ..Default::default()
        }
        .to_bytes()
        .unwrap();
        assert!(matches!(
            Store::from_bytes(ArchiveBytes::Owned(invalid)),
            Err(StoreError::InvalidArchive(_))
        ));
    }

    #[cfg(feature = "gzip")]
    #[test]
    fn gzip_source_constructor_compiles_runtime_store() {
        let source = json!({"test": {"type": "bool"}}).to_string();
        let mut encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::fast());
        encoder.write_all(source.as_bytes()).unwrap();
        let bytes = encoder.finish().unwrap();
        let store = Store::from_gz_bytes(&bytes).unwrap();
        assert!(matches!(store.get("test"), Some(SchemaView::Bool(_))));
    }

    #[cfg(feature = "mmap")]
    #[test]
    fn file_constructor_maps_archives() {
        let archive =
            std::env::temp_dir().join(format!("archive-runtime-{}.rkyv", std::process::id()));

        let source_model = StoreSource::from_json(r#"{"test":{"type":"bool"}}"#).unwrap();
        let bytes = CompiledStore::compile(&source_model)
            .unwrap()
            .to_bytes()
            .unwrap();
        std::fs::write(&archive, bytes.as_slice()).unwrap();
        let mapped_store = Store::from_file(&archive).unwrap();
        assert!(matches!(
            mapped_store.get("test"),
            Some(SchemaView::Bool(_))
        ));

        std::fs::remove_file(archive).unwrap();
    }
}
