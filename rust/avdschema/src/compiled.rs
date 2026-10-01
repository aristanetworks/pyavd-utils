// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Compilation into immutable schema tables intended for zero-copy access.
//!
//! Compilation follows references and inherited layers, rejects incompatible types and cycles,
//! and interns identical effective nodes into typed tables. The resulting graph contains no
//! unresolved schema references. Concrete dynamic keys remain data-dependent and are therefore
//! resolved by the navigation API instead of this compiler.

use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
#[cfg(feature = "dump_load_files")]
use std::io::Write as _;
#[cfg(feature = "dump_load_files")]
use std::path::Path;
#[cfg(feature = "dump_load_files")]
use std::sync::atomic::AtomicU64;
#[cfg(feature = "dump_load_files")]
use std::sync::atomic::Ordering;

use indexmap::IndexMap;
use rkyv::Archive;
use rkyv::Deserialize;
use rkyv::Serialize;
use rkyv::rancor::Error as RkyvError;
use serde_json::Value;

use crate::StoreSource;
use crate::any::SourceSchema;
use crate::base::Deprecation;
use crate::resolve::resolve_ref::resolve_ref;
use crate::str::Format;

/// Identifies files produced by the AVD schema compiler before rkyv validation is attempted.
pub(crate) const ARCHIVE_MAGIC: &[u8; 8] = b"AVDSCHM\0";
/// Version of the complete archived layout and its interpretation.
///
/// Increment this whenever a previously generated archive cannot be read with exactly the same
/// semantics. The version is deliberately independent of the crate version; archives are not
/// otherwise promised to be portable between arbitrary pyavd-utils releases.
pub(crate) const ARCHIVE_FORMAT_VERSION: u32 = 1;
/// Bytes reserved for magic, version, and future header fields before the rkyv root.
pub(crate) const ARCHIVE_HEADER_LENGTH: usize = 16;

/// Stable identifier of a node in one of the typed schema tables.
#[derive(Archive, Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[rkyv(derive(Clone, Copy, Debug, PartialEq, Eq, Hash))]
pub enum SchemaId {
    /// Boolean schema table index.
    Bool(u32),
    /// Integer schema table index.
    Int(u32),
    /// String schema table index.
    Str(u32),
    /// List schema table index.
    List(u32),
    /// Dictionary schema table index.
    Dict(u32),
}

/// Validation-relevant schema properties shared by all node types.
#[derive(Archive, Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug))]
pub struct Common {
    pub required: bool,
    pub default: Option<CompiledValue>,
    pub display_name: Option<String>,
    pub description: Option<String>,
    pub deprecation: Option<CompiledDeprecation>,
    pub documentation_options: Option<CompiledDocumentationOptions>,
}

/// Schema default stored without depending on serde's owned value model.
///
/// AVD schemas do not have a floating-point model type, so numeric defaults are limited to exact
/// integers.
#[derive(Archive, Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug))]
#[rkyv(serialize_bounds(
    __S: rkyv::ser::Writer + rkyv::ser::Allocator,
    __S::Error: rkyv::rancor::Source,
))]
#[rkyv(deserialize_bounds(__D::Error: rkyv::rancor::Source))]
#[rkyv(bytecheck(bounds(__C: rkyv::validation::ArchiveContext)))]
pub enum CompiledValue {
    Null,
    Bool(bool),
    I64(i64),
    U64(u64),
    String(String),
    List(#[rkyv(omit_bounds)] Vec<CompiledValue>),
    Object(#[rkyv(omit_bounds)] Vec<(String, CompiledValue)>),
}

/// Documentation controls inherited onto an effective schema node.
#[derive(Archive, Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug))]
pub struct CompiledDocumentationOptions {
    pub table: Option<String>,
    pub hide_keys: bool,
}

/// String formats understood by AVD schema consumers.
#[derive(Archive, Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[rkyv(derive(Clone, Copy, Debug, PartialEq, Eq, Hash))]
pub enum CompiledStringFormat {
    Cidr,
    Ip,
    IpPool,
    Ipv4,
    Ipv4Cidr,
    Ipv4Pool,
    Ipv6,
    Ipv6Cidr,
    Ipv6Pool,
    Mac,
}

/// Archived form of schema deprecation metadata used in diagnostics.
#[derive(Archive, Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug))]
pub struct CompiledDeprecation {
    pub warning: bool,
    pub new_key: Option<String>,
    pub allow_with_new_key: bool,
    pub removed: bool,
    pub remove_in_version: Option<String>,
    pub remove_after_date: Option<String>,
    pub url: Option<String>,
    pub upgrade_handler: Option<String>,
}

impl From<&Deprecation> for CompiledDeprecation {
    fn from(value: &Deprecation) -> Self {
        Self {
            warning: value.warning,
            new_key: value.new_key.clone(),
            allow_with_new_key: value.allow_with_new_key.unwrap_or_default(),
            removed: value.removed.unwrap_or_default(),
            remove_in_version: value.remove_in_version.clone(),
            remove_after_date: value.remove_after_date.clone(),
            url: value.url.clone(),
            upgrade_handler: value.upgrade_handler.clone(),
        }
    }
}

#[derive(Archive, Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug))]
pub struct BoolSchema {
    pub common: Common,
}

#[derive(Archive, Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug))]
pub struct IntSchema {
    pub common: Common,
    pub min: Option<i64>,
    pub max: Option<i64>,
    pub valid_values: Option<Vec<i64>>,
    pub dynamic_valid_values: Option<Vec<String>>,
    pub convert_types: Option<Vec<String>>,
}

#[derive(Archive, Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug))]
pub struct StrSchema {
    pub common: Common,
    pub convert_to_lower_case: bool,
    pub min_length: Option<u64>,
    pub max_length: Option<u64>,
    pub pattern: Option<String>,
    pub valid_values: Option<Vec<String>>,
    pub dynamic_valid_values: Option<Vec<String>>,
    pub convert_types: Option<Vec<String>>,
    pub format: Option<CompiledStringFormat>,
}

#[derive(Archive, Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug))]
pub struct ListSchema {
    pub common: Common,
    pub items: Option<SchemaId>,
    pub min_length: Option<u64>,
    pub max_length: Option<u64>,
    pub primary_key: Option<String>,
    pub unique_keys: Option<Vec<String>>,
    pub allow_duplicate_primary_key: bool,
}

#[derive(Archive, Serialize, Deserialize, Clone, Debug)]
#[rkyv(derive(Debug))]
pub struct DictSchema {
    pub common: Common,
    pub keys: IndexMap<String, SchemaId>,
    pub dynamic_keys: IndexMap<String, SchemaId>,
    pub default_dynamic_keys: IndexMap<String, Vec<String>>,
    pub allow_other_keys: bool,
    pub begin_relaxed_validation: bool,
}

impl PartialEq for DictSchema {
    fn eq(&self, other: &Self) -> bool {
        self.common == other.common
            && self.keys.iter().eq(other.keys.iter())
            && self.dynamic_keys.iter().eq(other.dynamic_keys.iter())
            && self
                .default_dynamic_keys
                .iter()
                .eq(other.default_dynamic_keys.iter())
            && self.allow_other_keys == other.allow_other_keys
            && self.begin_relaxed_validation == other.begin_relaxed_validation
    }
}

impl Eq for DictSchema {}

impl Hash for DictSchema {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.common.hash(state);
        self.keys.iter().for_each(|entry| entry.hash(state));
        self.dynamic_keys.iter().for_each(|entry| entry.hash(state));
        self.default_dynamic_keys
            .iter()
            .for_each(|entry| entry.hash(state));
        self.allow_other_keys.hash(state);
        self.begin_relaxed_validation.hash(state);
    }
}

/// Fully resolved schema DAG stored as typed flat tables.
#[derive(Archive, Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct CompiledStore {
    pub roots: IndexMap<String, SchemaId>,
    pub bools: Vec<BoolSchema>,
    pub ints: Vec<IntSchema>,
    pub strings: Vec<StrSchema>,
    pub lists: Vec<ListSchema>,
    pub dicts: Vec<DictSchema>,
}

impl CompiledStore {
    /// Compile a raw schema store directly into resolved DAG tables.
    pub fn compile(store: &StoreSource) -> Result<Self, CompileError> {
        Compiler::new(store).compile()
    }

    /// Compile one named root and only the schema nodes reachable from it.
    pub fn compile_schema(store: &StoreSource, schema_name: &str) -> Result<Self, CompileError> {
        Compiler::new(store).compile_schema(schema_name)
    }

    /// Serialize this store into an rkyv-aligned byte buffer.
    pub fn to_bytes(&self) -> Result<rkyv::util::AlignedVec, CompileError> {
        let archived = rkyv::to_bytes::<RkyvError>(self)
            .map_err(|error| CompileError::Archive(error.to_string()))?;
        let mut bytes = rkyv::util::AlignedVec::with_capacity(
            ARCHIVE_HEADER_LENGTH.saturating_add(archived.len()),
        );
        bytes.extend_from_slice(ARCHIVE_MAGIC);
        bytes.extend_from_slice(&ARCHIVE_FORMAT_VERSION.to_le_bytes());
        bytes.extend_from_slice(&[0; 4]);
        bytes.extend_from_slice(archived.as_slice());
        Ok(bytes)
    }

    /// Compile a raw store and atomically write its archived representation.
    #[cfg(feature = "dump_load_files")]
    pub fn compile_to_file(store: &StoreSource, destination: &Path) -> Result<(), CompileError> {
        let bytes = Self::compile(store)?.to_bytes()?;
        write_atomically(destination, bytes.as_slice())?;
        Ok(())
    }
}

#[cfg(feature = "dump_load_files")]
static TEMPORARY_FILE_SEQUENCE: AtomicU64 = AtomicU64::new(0);

#[cfg(feature = "dump_load_files")]
fn write_atomically(destination: &Path, bytes: &[u8]) -> std::io::Result<()> {
    let parent = destination.parent().unwrap_or_else(|| Path::new("."));
    let file_name = destination.file_name().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "archive destination has no file name",
        )
    })?;
    let mut temporary = None;
    for _ in 0..100 {
        let sequence = TEMPORARY_FILE_SEQUENCE.fetch_add(1, Ordering::Relaxed);
        let candidate = parent.join(format!(
            ".{}.{}.{}.tmp",
            file_name.to_string_lossy(),
            std::process::id(),
            sequence
        ));
        match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&candidate)
        {
            Ok(file) => {
                temporary = Some((candidate, file));
                break;
            }
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(error) => return Err(error),
        }
    }
    let Some((temporary_path, mut file)) = temporary else {
        return Err(std::io::Error::new(
            std::io::ErrorKind::AlreadyExists,
            "unable to reserve a unique temporary archive file",
        ));
    };
    let result = file
        .write_all(bytes)
        .and_then(|()| file.flush())
        .and_then(|()| {
            drop(file);
            std::fs::rename(&temporary_path, destination)
        });
    if result.is_err() {
        let _ = std::fs::remove_file(&temporary_path);
    }
    result
}

/// A diagnostic describing an invalid schema encountered during compilation.
#[derive(Debug)]
pub enum SchemaDiagnostic {
    /// A schema reference could not be resolved.
    Reference {
        /// Schema path being compiled.
        schema_path: Vec<String>,
        /// Full reference string from the source schema.
        reference: String,
        /// Structured resolver failure.
        error: crate::SchemaResolverError,
    },
    /// Schema layers declare incompatible model types.
    TypeMismatch {
        /// Model type established by the first layer.
        expected: &'static str,
        /// Incompatible model type found in a subsequent layer.
        found: &'static str,
    },
    /// Following a reference would revisit a layer already being compiled.
    ReferenceCycle {
        /// Schema path being compiled.
        schema_path: Vec<String>,
        /// Reference that closes the cycle.
        reference: String,
    },
    /// Nested schema structure directly or indirectly contains itself.
    StructuralCycle {
        /// Schema path where the cycle was detected.
        schema_path: Vec<String>,
    },
    /// A schema default contains a number unsupported by AVD schema model types.
    UnsupportedDefaultNumber {
        /// Schema path whose effective default is invalid.
        schema_path: Vec<String>,
        /// Path from the default root to the invalid number.
        default_path: Vec<String>,
        /// Normalized JSON number representation.
        value: String,
    },
}

impl std::fmt::Display for SchemaDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reference {
                schema_path,
                reference,
                error,
            } => write!(
                f,
                "Unable to resolve schema reference '{reference}' while compiling '{}': {error}",
                schema_path.join("/")
            ),
            Self::TypeMismatch { expected, found } => write!(
                f,
                "Schema layering combines incompatible types: expected {expected}, found {found}"
            ),
            Self::ReferenceCycle {
                schema_path,
                reference,
            } => write!(
                f,
                "Schema reference '{reference}' forms a cycle while compiling '{}'",
                schema_path.join("/")
            ),
            Self::StructuralCycle { schema_path } => write!(
                f,
                "Schema contains a structural cycle while compiling '{}'",
                schema_path.join("/")
            ),
            Self::UnsupportedDefaultNumber {
                schema_path,
                default_path,
                value,
            } => {
                let location = if default_path.is_empty() {
                    "default".to_owned()
                } else {
                    format!("default/{}", default_path.join("/"))
                };
                write!(
                    f,
                    "Schema default at '{}/{}' contains unsupported number '{value}'; AVD schema defaults only support integers representable as i64 or u64",
                    schema_path.join("/"),
                    location
                )
            }
        }
    }
}

/// One or more structured schema diagnostics produced during compilation.
///
/// The container is intentionally plural so compilation passes can report multiple independent
/// problems without another API change. Individual compiler paths may still stop at their first
/// diagnostic; callers must not assume every problem in a source store is returned in one run.
#[derive(Debug)]
pub struct SchemaDiagnostics(Vec<SchemaDiagnostic>);

impl SchemaDiagnostics {
    fn single(diagnostic: SchemaDiagnostic) -> Self {
        Self(vec![diagnostic])
    }

    /// Iterate over structured diagnostics in reporting order.
    pub fn iter(&self) -> impl Iterator<Item = &SchemaDiagnostic> {
        self.0.iter()
    }
}

impl std::fmt::Display for SchemaDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, diagnostic) in self.0.iter().enumerate() {
            if index > 0 {
                f.write_str("\n")?;
            }
            diagnostic.fmt(f)?;
        }
        Ok(())
    }
}

/// Error raised while compiling or archiving a schema store.
#[derive(Debug, derive_more::Display)]
pub enum CompileError {
    /// One or more source-schema diagnostics prevented compilation.
    #[display("{_0}")]
    InvalidSchema(SchemaDiagnostics),
    /// A typed table cannot be represented by the archive's `u32` identifiers.
    #[display("Compiled schema table contains more than u32::MAX entries")]
    TableOverflow,
    /// Serialization into the archived representation failed.
    #[display("Unable to archive compiled schema: {_0}")]
    Archive(String),
    /// Writing a compiled archive failed.
    Io(std::io::Error),
}

impl From<SchemaDiagnostic> for CompileError {
    fn from(value: SchemaDiagnostic) -> Self {
        Self::InvalidSchema(SchemaDiagnostics::single(value))
    }
}

impl From<std::io::Error> for CompileError {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum NodeKey {
    Bool(BoolSchema),
    Int(IntSchema),
    Str(StrSchema),
    List(ListSchema),
    Dict(DictSchema),
}

struct Compiler<'a> {
    source: &'a StoreSource,
    output: CompiledStore,
    interned: HashMap<NodeKey, SchemaId>,
    memoized_layers: HashMap<Vec<*const SourceSchema>, SchemaId>,
    compiling_layers: HashMap<Vec<*const SourceSchema>, Vec<String>>,
}

impl<'a> Compiler<'a> {
    fn new(source: &'a StoreSource) -> Self {
        Self {
            source,
            output: CompiledStore::default(),
            interned: HashMap::new(),
            memoized_layers: HashMap::new(),
            compiling_layers: HashMap::new(),
        }
    }

    fn compile(mut self) -> Result<CompiledStore, CompileError> {
        for name in self.source.schema_names() {
            let root = self
                .source
                .get(name)
                .map_err(|error| SchemaDiagnostic::Reference {
                    schema_path: vec![name.to_owned()],
                    reference: format!("{name}#"),
                    error: error.into(),
                })?;
            let id = self.compile_layers(&[root], &[name.to_owned()])?;
            self.output.roots.insert(name.to_owned(), id);
        }
        Ok(self.output)
    }

    fn compile_schema(mut self, schema_name: &str) -> Result<CompiledStore, CompileError> {
        let root = self
            .source
            .get(schema_name)
            .map_err(|error| SchemaDiagnostic::Reference {
                schema_path: vec![schema_name.to_owned()],
                reference: format!("{schema_name}#"),
                error: error.into(),
            })?;
        let id = self.compile_layers(&[root], &[schema_name.to_owned()])?;
        self.output.roots.insert(schema_name.to_owned(), id);
        Ok(self.output)
    }

    fn compile_layers(
        &mut self,
        declared: &[&'a SourceSchema],
        schema_path: &[String],
    ) -> Result<SchemaId, CompileError> {
        let layers = self.expand_layers(declared, schema_path)?;
        let memo_key = layers
            .iter()
            .map(|schema| std::ptr::from_ref(*schema))
            .collect::<Vec<_>>();
        if let Some(id) = self.memoized_layers.get(&memo_key) {
            return Ok(*id);
        }
        if let Some(cycle_origin) = self.compiling_layers.get(&memo_key) {
            return Err(SchemaDiagnostic::StructuralCycle {
                schema_path: cycle_origin.clone(),
            }
            .into());
        }
        self.compiling_layers
            .insert(memo_key.clone(), schema_path.to_vec());

        let node_result = match layers.first().copied() {
            Some(SourceSchema::Bool(_)) => {
                Self::compile_bool(&layers, schema_path).map(NodeKey::Bool)
            }
            Some(SourceSchema::Int(_)) => Self::compile_int(&layers, schema_path).map(NodeKey::Int),
            Some(SourceSchema::Str(_)) => Self::compile_str(&layers, schema_path).map(NodeKey::Str),
            Some(SourceSchema::List(_)) => {
                self.compile_list(&layers, schema_path).map(NodeKey::List)
            }
            Some(SourceSchema::Dict(_)) => self
                .compile_dict(declared, &layers, schema_path)
                .map(NodeKey::Dict),
            None => {
                self.compiling_layers.remove(&memo_key);
                return Err(SchemaDiagnostic::TypeMismatch {
                    expected: "schema",
                    found: "no layers",
                }
                .into());
            }
        };
        self.compiling_layers.remove(&memo_key);
        let node_result = node_result?;
        let id = self.intern(node_result)?;
        self.memoized_layers.insert(memo_key, id);
        Ok(id)
    }

    fn expand_layers(
        &self,
        declared: &[&'a SourceSchema],
        schema_path: &[String],
    ) -> Result<Vec<&'a SourceSchema>, CompileError> {
        let Some(first_declared) = declared.first().copied() else {
            return Err(SchemaDiagnostic::TypeMismatch {
                expected: "schema",
                found: "no declared layers",
            }
            .into());
        };
        let mut result = Vec::new();
        for declared_layer in declared {
            let mut layer = *declared_layer;
            let mut chain = Vec::new();
            loop {
                if !same_type(first_declared, layer) {
                    return Err(SchemaDiagnostic::TypeMismatch {
                        expected: schema_type(first_declared),
                        found: schema_type(layer),
                    }
                    .into());
                }
                chain.push(std::ptr::from_ref(layer));
                result.push(layer);
                let Some(reference) = schema_ref(layer) else {
                    break;
                };
                layer = resolve_ref(reference, self.source).map_err(|error| {
                    SchemaDiagnostic::Reference {
                        schema_path: schema_path.to_vec(),
                        reference: reference.to_owned(),
                        error,
                    }
                })?;
                if chain.contains(&std::ptr::from_ref(layer)) {
                    return Err(SchemaDiagnostic::ReferenceCycle {
                        schema_path: schema_path.to_vec(),
                        reference: reference.to_owned(),
                    }
                    .into());
                }
            }
        }
        Ok(result)
    }

    fn compile_bool(
        layers: &[&SourceSchema],
        schema_path: &[String],
    ) -> Result<BoolSchema, CompileError> {
        Ok(BoolSchema {
            common: common(layers, schema_path)?,
        })
    }

    fn compile_int(
        layers: &[&SourceSchema],
        schema_path: &[String],
    ) -> Result<IntSchema, CompileError> {
        let schemas = layers.iter().filter_map(|schema| match schema {
            SourceSchema::Int(schema) => Some(schema),
            _ => None,
        });
        Ok(IntSchema {
            common: common(layers, schema_path)?,
            min: schemas.clone().find_map(|schema| schema.min),
            max: schemas.clone().find_map(|schema| schema.max),
            valid_values: schemas
                .clone()
                .find_map(|schema| schema.valid_values.valid_values.clone()),
            dynamic_valid_values: schemas
                .clone()
                .find_map(|schema| schema.valid_values.dynamic_valid_values.clone()),
            convert_types: schemas
                .clone()
                .find_map(|schema| schema.convert_types.convert_types.clone()),
        })
    }

    fn compile_str(
        layers: &[&SourceSchema],
        schema_path: &[String],
    ) -> Result<StrSchema, CompileError> {
        let schemas = layers.iter().filter_map(|schema| match schema {
            SourceSchema::Str(schema) => Some(schema),
            _ => None,
        });
        Ok(StrSchema {
            common: common(layers, schema_path)?,
            convert_to_lower_case: schemas
                .clone()
                .find_map(|schema| schema.convert_to_lower_case)
                .unwrap_or_default(),
            min_length: schemas.clone().find_map(|schema| schema.min_length),
            max_length: schemas.clone().find_map(|schema| schema.max_length),
            pattern: schemas.clone().find_map(|schema| {
                schema
                    .pattern
                    .as_ref()
                    .map(|pattern| pattern.pattern.clone())
            }),
            valid_values: schemas
                .clone()
                .find_map(|schema| schema.valid_values.valid_values.clone()),
            dynamic_valid_values: schemas
                .clone()
                .find_map(|schema| schema.valid_values.dynamic_valid_values.clone()),
            convert_types: schemas
                .clone()
                .find_map(|schema| schema.convert_types.convert_types.clone()),
            format: schemas
                .clone()
                .find_map(|schema| schema.format)
                .map(CompiledStringFormat::from),
        })
    }

    fn compile_list(
        &mut self,
        layers: &[&'a SourceSchema],
        schema_path: &[String],
    ) -> Result<ListSchema, CompileError> {
        let schemas = layers.iter().filter_map(|schema| match schema {
            SourceSchema::List(schema) => Some(schema),
            _ => None,
        });
        let item_layers = schemas
            .clone()
            .filter_map(|schema| schema.items.as_deref())
            .collect::<Vec<_>>();
        Ok(ListSchema {
            common: common(layers, schema_path)?,
            items: (!item_layers.is_empty())
                .then(|| self.compile_layers(&item_layers, &schema_path_with(schema_path, "items")))
                .transpose()?,
            min_length: schemas.clone().find_map(|schema| schema.min_length),
            max_length: schemas.clone().find_map(|schema| schema.max_length),
            primary_key: schemas
                .clone()
                .find_map(|schema| schema.primary_key.clone()),
            unique_keys: schemas
                .clone()
                .find_map(|schema| schema.unique_keys.clone()),
            allow_duplicate_primary_key: schemas
                .clone()
                .find_map(|schema| schema.allow_duplicate_primary_key)
                .unwrap_or_default(),
        })
    }

    fn compile_dict(
        &mut self,
        declared: &[&'a SourceSchema],
        layers: &[&'a SourceSchema],
        schema_path: &[String],
    ) -> Result<DictSchema, CompileError> {
        let schemas = layers.iter().filter_map(|schema| match schema {
            SourceSchema::Dict(schema) => Some(schema),
            _ => None,
        });
        let keys = self.compile_dict_children(
            schemas.clone().filter_map(|schema| schema.keys.as_ref()),
            &schema_path_with(schema_path, "keys"),
        )?;
        let dynamic_keys = self.compile_dict_children(
            schemas
                .clone()
                .filter_map(|schema| schema.dynamic_keys.as_ref()),
            &schema_path_with(schema_path, "dynamic_keys"),
        )?;
        let default_dynamic_keys = default_dynamic_keys(&dynamic_keys, self, layers, schema_path)?;
        let begin_relaxed_validation = matches!(
            declared.first(),
            Some(SourceSchema::Dict(schema))
                if schema.base.schema_ref.is_some()
                    && schema.relaxed_validation.unwrap_or_default()
        );
        Ok(DictSchema {
            common: common(layers, schema_path)?,
            keys,
            dynamic_keys,
            default_dynamic_keys,
            allow_other_keys: schemas
                .clone()
                .find_map(|schema| schema.allow_other_keys)
                .unwrap_or_default(),
            begin_relaxed_validation,
        })
    }

    fn compile_dict_children(
        &mut self,
        maps: impl Iterator<Item = &'a ordermap::OrderMap<String, SourceSchema>>,
        schema_path: &[String],
    ) -> Result<IndexMap<String, SchemaId>, CompileError> {
        let mut child_layers: IndexMap<&str, Vec<&SourceSchema>> = IndexMap::new();
        for map in maps {
            for (name, schema) in map {
                child_layers.entry(name).or_default().push(schema);
            }
        }
        child_layers
            .into_iter()
            .map(|(name, layers)| {
                self.compile_layers(&layers, &schema_path_with(schema_path, name))
                    .map(|id| (name.to_owned(), id))
            })
            .collect()
    }

    fn intern(&mut self, node: NodeKey) -> Result<SchemaId, CompileError> {
        if let Some(id) = self.interned.get(&node) {
            return Ok(*id);
        }
        let id = match &node {
            NodeKey::Bool(schema) => {
                let index = table_index(self.output.bools.len())?;
                self.output.bools.push(schema.clone());
                SchemaId::Bool(index)
            }
            NodeKey::Int(schema) => {
                let index = table_index(self.output.ints.len())?;
                self.output.ints.push(schema.clone());
                SchemaId::Int(index)
            }
            NodeKey::Str(schema) => {
                let index = table_index(self.output.strings.len())?;
                self.output.strings.push(schema.clone());
                SchemaId::Str(index)
            }
            NodeKey::List(schema) => {
                let index = table_index(self.output.lists.len())?;
                self.output.lists.push(schema.clone());
                SchemaId::List(index)
            }
            NodeKey::Dict(schema) => {
                let index = table_index(self.output.dicts.len())?;
                self.output.dicts.push(schema.clone());
                SchemaId::Dict(index)
            }
        };
        self.interned.insert(node, id);
        Ok(id)
    }

    fn is_removed(&self, id: SchemaId) -> bool {
        let deprecation = match id {
            SchemaId::Bool(index) => self
                .output
                .bools
                .get(usize::try_from(index).unwrap_or_default())
                .and_then(|schema| schema.common.deprecation.as_ref()),
            SchemaId::Int(index) => self
                .output
                .ints
                .get(usize::try_from(index).unwrap_or_default())
                .and_then(|schema| schema.common.deprecation.as_ref()),
            SchemaId::Str(index) => self
                .output
                .strings
                .get(usize::try_from(index).unwrap_or_default())
                .and_then(|schema| schema.common.deprecation.as_ref()),
            SchemaId::List(index) => self
                .output
                .lists
                .get(usize::try_from(index).unwrap_or_default())
                .and_then(|schema| schema.common.deprecation.as_ref()),
            SchemaId::Dict(index) => self
                .output
                .dicts
                .get(usize::try_from(index).unwrap_or_default())
                .and_then(|schema| schema.common.deprecation.as_ref()),
        };
        deprecation.is_some_and(|deprecation| deprecation.removed)
    }
}

fn schema_path_with(schema_path: &[String], segment: &str) -> Vec<String> {
    let mut child_path = schema_path.to_vec();
    child_path.push(segment.to_owned());
    child_path
}

fn common(layers: &[&SourceSchema], schema_path: &[String]) -> Result<Common, CompileError> {
    Ok(Common {
        required: layers
            .iter()
            .find_map(|schema| match schema {
                SourceSchema::Bool(schema) => schema.base.required,
                SourceSchema::Int(schema) => schema.base.required,
                SourceSchema::Str(schema) => schema.base.required,
                SourceSchema::List(schema) => schema.base.required,
                SourceSchema::Dict(schema) => schema.base.required,
            })
            .unwrap_or_default(),
        default: layers
            .iter()
            .find_map(|schema| schema_default(schema))
            .as_ref()
            .map(|value| compile_default_value(value, schema_path, &[]))
            .transpose()?,
        display_name: layers.iter().find_map(|schema| match schema {
            SourceSchema::Bool(schema) => schema.base.display_name.clone(),
            SourceSchema::Int(schema) => schema.base.display_name.clone(),
            SourceSchema::Str(schema) => schema.base.display_name.clone(),
            SourceSchema::List(schema) => schema.base.display_name.clone(),
            SourceSchema::Dict(schema) => schema.base.display_name.clone(),
        }),
        description: layers.iter().find_map(|schema| match schema {
            SourceSchema::Bool(schema) => schema.base.description.clone(),
            SourceSchema::Int(schema) => schema.base.description.clone(),
            SourceSchema::Str(schema) => schema.base.description.clone(),
            SourceSchema::List(schema) => schema.base.description.clone(),
            SourceSchema::Dict(schema) => schema.base.description.clone(),
        }),
        deprecation: layers
            .iter()
            .find_map(|schema| schema_deprecation(schema))
            .map(CompiledDeprecation::from),
        documentation_options: layers.iter().find_map(|schema| match schema {
            SourceSchema::Bool(schema) => {
                schema
                    .documentation_options
                    .as_ref()
                    .map(|options| CompiledDocumentationOptions {
                        table: options.table.clone(),
                        hide_keys: false,
                    })
            }
            SourceSchema::Int(schema) => {
                schema
                    .documentation_options
                    .as_ref()
                    .map(|options| CompiledDocumentationOptions {
                        table: options.table.clone(),
                        hide_keys: false,
                    })
            }
            SourceSchema::Str(schema) => {
                schema
                    .documentation_options
                    .as_ref()
                    .map(|options| CompiledDocumentationOptions {
                        table: options.table.clone(),
                        hide_keys: false,
                    })
            }
            SourceSchema::List(schema) => {
                schema
                    .documentation_options
                    .as_ref()
                    .map(|options| CompiledDocumentationOptions {
                        table: options.table.clone(),
                        hide_keys: false,
                    })
            }
            SourceSchema::Dict(schema) => {
                schema
                    .documentation_options
                    .as_ref()
                    .map(|options| CompiledDocumentationOptions {
                        table: options.table.clone(),
                        hide_keys: options.hide_keys.unwrap_or_default(),
                    })
            }
        }),
    })
}

fn compile_default_value(
    value: &Value,
    schema_path: &[String],
    default_path: &[String],
) -> Result<CompiledValue, CompileError> {
    match value {
        Value::Null => Ok(CompiledValue::Null),
        Value::Bool(value) => Ok(CompiledValue::Bool(*value)),
        Value::Number(value) => value
            .as_i64()
            .map(CompiledValue::I64)
            .or_else(|| value.as_u64().map(CompiledValue::U64))
            .ok_or_else(|| {
                SchemaDiagnostic::UnsupportedDefaultNumber {
                    schema_path: schema_path.to_vec(),
                    default_path: default_path.to_vec(),
                    value: value.to_string(),
                }
                .into()
            }),
        Value::String(value) => Ok(CompiledValue::String(value.clone())),
        Value::Array(values) => values
            .iter()
            .enumerate()
            .map(|(index, child)| {
                let child_path = schema_path_with(default_path, &index.to_string());
                compile_default_value(child, schema_path, &child_path)
            })
            .collect::<Result<Vec<_>, _>>()
            .map(CompiledValue::List),
        Value::Object(values) => values
            .iter()
            .map(|(key, child)| {
                let child_path = schema_path_with(default_path, key);
                compile_default_value(child, schema_path, &child_path)
                    .map(|compiled| (key.clone(), compiled))
            })
            .collect::<Result<Vec<_>, _>>()
            .map(CompiledValue::Object),
    }
}

impl From<Format> for CompiledStringFormat {
    fn from(value: Format) -> Self {
        match value {
            Format::Cidr => Self::Cidr,
            Format::Ip => Self::Ip,
            Format::IpPool => Self::IpPool,
            Format::Ipv4 => Self::Ipv4,
            Format::Ipv4Cidr => Self::Ipv4Cidr,
            Format::Ipv4Pool => Self::Ipv4Pool,
            Format::Ipv6 => Self::Ipv6,
            Format::Ipv6Cidr => Self::Ipv6Cidr,
            Format::Ipv6Pool => Self::Ipv6Pool,
            Format::Mac => Self::Mac,
        }
    }
}

fn default_dynamic_keys(
    dynamic_keys: &IndexMap<String, SchemaId>,
    compiler: &Compiler<'_>,
    layers: &[&SourceSchema],
    schema_path: &[String],
) -> Result<IndexMap<String, Vec<String>>, CompileError> {
    let mut result = IndexMap::new();
    for (path, dynamic_schema) in dynamic_keys {
        if compiler.is_removed(*dynamic_schema) {
            continue;
        }
        let Some(root_key) = path.split('.').next() else {
            continue;
        };
        let child_layers = layers
            .iter()
            .filter_map(|schema| match schema {
                SourceSchema::Dict(schema) => schema.keys.as_ref()?.get(root_key),
                _ => None,
            })
            .collect::<Vec<_>>();
        if child_layers.is_empty() {
            continue;
        }
        let child_schema_path = schema_path_with(&schema_path_with(schema_path, "keys"), root_key);
        let expanded_child_layers = compiler.expand_layers(&child_layers, &child_schema_path)?;
        let Some(default) = expanded_child_layers
            .iter()
            .find_map(|schema| schema_default(schema))
        else {
            continue;
        };
        if let Some(values) = values_at_path(&default, path.split('.').skip(1)) {
            result.insert(path.clone(), values);
        }
    }
    Ok(result)
}

fn values_at_path<'a>(
    root_value: &'a Value,
    path: impl Iterator<Item = &'a str>,
) -> Option<Vec<String>> {
    let mut current = vec![root_value];
    for key in path {
        let mut next = Vec::new();
        for current_value in current {
            match current_value {
                Value::Object(map) => {
                    if let Some(child) = map.get(key) {
                        next.push(child);
                    }
                }
                Value::Array(items) => {
                    for item in items {
                        if let Some(child) = item.as_object().and_then(|map| map.get(key)) {
                            next.push(child);
                        }
                    }
                }
                _ => {}
            }
        }
        if next.is_empty() {
            return None;
        }
        current = next;
    }
    Some(
        current
            .into_iter()
            .flat_map(|current_value| match current_value {
                Value::String(value) => vec![value.clone()],
                Value::Array(values) => values
                    .iter()
                    .filter_map(Value::as_str)
                    .map(ToOwned::to_owned)
                    .collect(),
                _ => Vec::new(),
            })
            .collect(),
    )
}

fn schema_ref(schema: &SourceSchema) -> Option<&str> {
    match schema {
        SourceSchema::Bool(schema) => schema.base.schema_ref.as_deref(),
        SourceSchema::Int(schema) => schema.base.schema_ref.as_deref(),
        SourceSchema::Str(schema) => schema.base.schema_ref.as_deref(),
        SourceSchema::List(schema) => schema.base.schema_ref.as_deref(),
        SourceSchema::Dict(schema) => schema.base.schema_ref.as_deref(),
    }
}

fn schema_default(schema: &SourceSchema) -> Option<Value> {
    match schema {
        SourceSchema::Bool(schema) => schema.base.default.map(Value::Bool),
        SourceSchema::Int(schema) => schema.base.default.map(|value| Value::Number(value.into())),
        SourceSchema::Str(schema) => schema.base.default.clone().map(Value::String),
        SourceSchema::List(schema) => schema.base.default.clone().map(Value::Array),
        SourceSchema::Dict(schema) => schema
            .base
            .default
            .clone()
            .map(|value| Value::Object(value.into_iter().collect())),
    }
}

fn schema_deprecation(schema: &SourceSchema) -> Option<&Deprecation> {
    match schema {
        SourceSchema::Bool(schema) => schema.base.deprecation.as_ref(),
        SourceSchema::Int(schema) => schema.base.deprecation.as_ref(),
        SourceSchema::Str(schema) => schema.base.deprecation.as_ref(),
        SourceSchema::List(schema) => schema.base.deprecation.as_ref(),
        SourceSchema::Dict(schema) => schema.base.deprecation.as_ref(),
    }
}

fn same_type(left: &SourceSchema, right: &SourceSchema) -> bool {
    matches!(
        (left, right),
        (SourceSchema::Bool(_), SourceSchema::Bool(_))
            | (SourceSchema::Int(_), SourceSchema::Int(_))
            | (SourceSchema::Str(_), SourceSchema::Str(_))
            | (SourceSchema::List(_), SourceSchema::List(_))
            | (SourceSchema::Dict(_), SourceSchema::Dict(_))
    )
}

fn schema_type(schema: &SourceSchema) -> &'static str {
    match schema {
        SourceSchema::Bool(_) => "bool",
        SourceSchema::Int(_) => "int",
        SourceSchema::Str(_) => "str",
        SourceSchema::List(_) => "list",
        SourceSchema::Dict(_) => "dict",
    }
}

fn table_index(length: usize) -> Result<u32, CompileError> {
    u32::try_from(length).map_err(|_conversion_error| CompileError::TableOverflow)
}

#[cfg(test)]
mod tests {
    use rkyv::rancor::Error as RkyvError;

    use super::ArchivedCompiledStore;
    use super::CompileError;
    use super::CompiledStore;
    use super::SchemaDiagnostic;
    use super::SchemaId;
    #[cfg(feature = "dump_load_files")]
    use super::TEMPORARY_FILE_SEQUENCE;
    #[cfg(feature = "dump_load_files")]
    use super::write_atomically;
    use crate::Load as _;
    use crate::SchemaResolverError;
    use crate::SchemaStoreError;
    use crate::StoreSource;
    use crate::utils::test_utils::get_test_store;

    #[test]
    fn compilation_is_deterministic_and_archivable() {
        let store = get_test_store();
        let first = CompiledStore::compile(&store).expect("test schema should compile");
        let second = CompiledStore::compile(&store).expect("test schema should compile");
        let first_bytes = first.to_bytes().expect("compiled schema should archive");
        let second_bytes = second.to_bytes().expect("compiled schema should archive");

        assert_eq!(first_bytes.as_slice(), second_bytes.as_slice());
        let archived = rkyv::access::<ArchivedCompiledStore, RkyvError>(&first_bytes)
            .expect("archive should pass byte validation");
        assert_eq!(archived.roots.len(), store.schema_names().len());
        assert!(archived.roots.get("eos_config").is_some());
    }

    #[test]
    fn repeated_inherited_layers_share_compiled_nodes() {
        let source = StoreSource::from_json(
            r#"{
                "base": {
                    "type": "dict",
                    "keys": {"shared": {"type": "str", "default": "value"}}
                },
                "first": {"type": "dict", "$ref": "base#"},
                "second": {"type": "dict", "$ref": "base#"}
            }"#,
        )
        .expect("inherited source schemas should deserialize");

        let compiled = CompiledStore::compile(&source).expect("inherited schemas should compile");
        let base = compiled.roots.get("base").expect("base root should exist");
        let first = compiled
            .roots
            .get("first")
            .expect("first root should exist");
        let second = compiled
            .roots
            .get("second")
            .expect("second root should exist");

        assert_eq!(first, base);
        assert_eq!(second, base);
        assert_eq!(compiled.dicts.len(), 1);
        assert_eq!(compiled.strings.len(), 1);
    }

    #[test]
    fn distinct_equivalent_nodes_share_an_interned_table_entry() {
        let source = StoreSource::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "keys": {
                        "first": {"type": "str", "default": "value"},
                        "second": {"type": "str", "default": "value"}
                    }
                }
            }"#,
        )
        .expect("equivalent source schemas should deserialize");

        let compiled =
            CompiledStore::compile(&source).expect("equivalent source schemas should compile");
        let Some(SchemaId::Dict(root_index)) = compiled.roots.get("test") else {
            panic!("test root should be a dictionary")
        };
        let root = compiled
            .dicts
            .get(usize::try_from(*root_index).expect("root index should fit usize"))
            .expect("root dictionary should exist");

        assert_eq!(root.keys.get("first"), root.keys.get("second"));
        assert_eq!(compiled.strings.len(), 1);
    }

    #[test]
    fn schema_compilation_errors_expose_typed_diagnostics() {
        let invalid_reference =
            StoreSource::from_json(r#"{"test":{"type":"str","$ref":"missing#"}}"#).unwrap();
        let CompileError::InvalidSchema(invalid_reference_diagnostics) =
            CompiledStore::compile(&invalid_reference).unwrap_err()
        else {
            panic!("invalid reference should return schema diagnostics")
        };
        let invalid_reference_diagnostic = invalid_reference_diagnostics.iter().next().unwrap();
        assert!(matches!(
            invalid_reference_diagnostic,
            SchemaDiagnostic::Reference {
                schema_path,
                reference,
                error: SchemaResolverError::SchemaStore(
                    SchemaStoreError::InvalidSchemaName(name)
                ),
            } if schema_path == &["test"] && reference == "missing#" && name == "missing"
        ));
        assert_eq!(
            invalid_reference_diagnostic.to_string(),
            "Unable to resolve schema reference 'missing#' while compiling 'test': Schema name 'missing' not found in the schema store."
        );

        let type_mismatch = StoreSource::from_json(
            r#"{"base":{"type":"bool"},"test":{"type":"str","$ref":"base#"}}"#,
        )
        .unwrap();
        let CompileError::InvalidSchema(type_mismatch_diagnostics) =
            CompiledStore::compile(&type_mismatch).unwrap_err()
        else {
            panic!("type mismatch should return schema diagnostics")
        };
        assert!(matches!(
            type_mismatch_diagnostics.iter().next(),
            Some(SchemaDiagnostic::TypeMismatch {
                expected: "str",
                found: "bool"
            })
        ));
        assert_eq!(
            type_mismatch_diagnostics.to_string(),
            "Schema layering combines incompatible types: expected str, found bool"
        );

        let reference_cycle = StoreSource::from_json(
            r#"{"a":{"type":"bool","$ref":"b#"},"b":{"type":"bool","$ref":"a#"}}"#,
        )
        .unwrap();
        let CompileError::InvalidSchema(reference_cycle_diagnostics) =
            CompiledStore::compile(&reference_cycle).unwrap_err()
        else {
            panic!("reference cycle should return schema diagnostics")
        };
        let reference_cycle_diagnostic = reference_cycle_diagnostics.iter().next().unwrap();
        assert!(matches!(
            reference_cycle_diagnostic,
            SchemaDiagnostic::ReferenceCycle {
                schema_path,
                reference,
            } if schema_path == &["a"] && reference == "a#"
        ));
        assert_eq!(
            reference_cycle_diagnostic.to_string(),
            "Schema reference 'a#' forms a cycle while compiling 'a'"
        );

        let structural_cycle = StoreSource::from_json(
            r#"{"root":{"type":"dict","keys":{"child":{"type":"dict","$ref":"root#"}}}}"#,
        )
        .unwrap();
        let CompileError::InvalidSchema(structural_cycle_diagnostics) =
            CompiledStore::compile(&structural_cycle).unwrap_err()
        else {
            panic!("structural cycle should return schema diagnostics")
        };
        let structural_cycle_diagnostic = structural_cycle_diagnostics.iter().next().unwrap();
        assert!(matches!(
            structural_cycle_diagnostic,
            SchemaDiagnostic::StructuralCycle { schema_path }
                if schema_path == &["root", "keys", "child"]
        ));
        assert_eq!(
            structural_cycle_diagnostic.to_string(),
            "Schema contains a structural cycle while compiling 'root/keys/child'"
        );
    }

    #[test]
    fn compilation_rejects_unsupported_numbers_in_nested_defaults() {
        for (number, expected_value, expected_path) in [
            ("1.5", "1.5", ["nested", "0"]),
            (
                "18446744073709551617",
                "18446744073709551617",
                ["nested", "0"],
            ),
            ("1e400", "1e+400", ["nested", "0"]),
        ] {
            let source = StoreSource::from_json(&format!(
                r#"{{"test":{{"type":"dict","default":{{"nested":[{number}]}}}}}}"#
            ))
            .expect("arbitrary-precision source number should deserialize");
            let CompileError::InvalidSchema(diagnostics) =
                CompiledStore::compile(&source).unwrap_err()
            else {
                panic!("unsupported default number should return schema diagnostics")
            };
            let Some(SchemaDiagnostic::UnsupportedDefaultNumber {
                schema_path,
                default_path,
                value,
            }) = diagnostics.iter().next()
            else {
                panic!("unsupported default number should return its typed diagnostic")
            };
            assert_eq!(schema_path, &["test"]);
            assert_eq!(default_path, &expected_path);
            assert_eq!(value, expected_value);
            assert_eq!(
                diagnostics.to_string(),
                format!(
                    "Schema default at 'test/default/nested/0' contains unsupported number '{expected_value}'; AVD schema defaults only support integers representable as i64 or u64"
                )
            );
        }
    }

    #[test]
    fn compilation_resolves_dynamic_keys_from_layered_defaults() {
        let source = StoreSource::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "keys": {
                        "selectors": {
                            "type": "dict",
                            "default": {"names": ["one", "two"]}
                        },
                        "groups": {
                            "type": "list",
                            "default": [{"name": "three"}, {"name": "four"}]
                        },
                        "single": {"type": "str", "default": "five"},
                        "without_default": {"type": "str"}
                    },
                    "dynamic_keys": {
                        "selectors.names": {"type": "bool"},
                        "groups.name": {"type": "bool"},
                        "single": {"type": "bool"},
                        "without_default": {"type": "bool"},
                        "missing": {"type": "bool"},
                        "removed": {
                            "type": "bool",
                            "deprecation": {"warning": false, "removed": true}
                        }
                    }
                }
            }"#,
        )
        .unwrap();
        let compiled = CompiledStore::compile(&source).unwrap();
        let Some(SchemaId::Dict(root_index)) = compiled.roots.get("test") else {
            panic!("test root should be a dictionary")
        };
        let root = &compiled.dicts[usize::try_from(*root_index).unwrap()];

        assert_eq!(
            root.default_dynamic_keys.get("selectors.names"),
            Some(&vec!["one".to_owned(), "two".to_owned()])
        );
        assert_eq!(
            root.default_dynamic_keys.get("groups.name"),
            Some(&vec!["three".to_owned(), "four".to_owned()])
        );
        assert_eq!(
            root.default_dynamic_keys.get("single"),
            Some(&vec!["five".to_owned()])
        );
        assert!(!root.default_dynamic_keys.contains_key("without_default"));
        assert!(!root.default_dynamic_keys.contains_key("missing"));
        assert!(!root.default_dynamic_keys.contains_key("removed"));
    }

    #[cfg(feature = "dump_load_files")]
    #[test]
    fn atomic_writer_reports_invalid_destinations() {
        let missing_name = write_atomically(std::path::Path::new(""), b"archive").unwrap_err();
        assert_eq!(missing_name.kind(), std::io::ErrorKind::InvalidInput);

        let sequence = TEMPORARY_FILE_SEQUENCE.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let missing_parent = std::env::temp_dir().join(format!(
            "avdschema-missing-parent-{}-{sequence}",
            std::process::id()
        ));
        let missing_parent_error =
            write_atomically(&missing_parent.join("schemas.rkyv"), b"archive").unwrap_err();
        assert_eq!(missing_parent_error.kind(), std::io::ErrorKind::NotFound);
    }

    #[cfg(feature = "dump_load_files")]
    #[test]
    fn atomic_writer_removes_temporary_file_when_replace_fails() {
        let sequence = TEMPORARY_FILE_SEQUENCE.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let destination = std::env::temp_dir().join(format!(
            "avdschema-existing-directory-{}-{sequence}",
            std::process::id()
        ));
        std::fs::create_dir_all(&destination).unwrap();

        let error = write_atomically(&destination, b"archive").unwrap_err();

        let temporary_prefix = format!(
            ".{}.{}.",
            destination.file_name().unwrap().to_string_lossy(),
            std::process::id()
        );
        let temporary_exists = std::fs::read_dir(destination.parent().unwrap())
            .unwrap()
            .filter_map(Result::ok)
            .any(|entry| {
                let file_name = entry.file_name();
                let file_name = file_name.to_string_lossy();
                file_name.starts_with(&temporary_prefix) && file_name.ends_with(".tmp")
            });
        std::fs::remove_dir(destination).unwrap();
        assert!(matches!(
            error.kind(),
            std::io::ErrorKind::IsADirectory
                | std::io::ErrorKind::PermissionDenied
                | std::io::ErrorKind::AlreadyExists
        ));
        assert!(!temporary_exists);
    }
}
