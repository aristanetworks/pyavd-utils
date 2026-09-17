// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Python bindings for the file-backed stage cache.
//!
//! These classes expose the Rust cache model without moving artifact payloads
//! through Python. A caller describes an invocation with [`PyStageRequest`],
//! asks [`PyFileStageCache`] for a reusable file handle, and calls its actual
//! producer only on a miss. After the producer has completely written the
//! selected file, registration records its identity and returns the same
//! [`PyCacheHit`] shape used by lookup.
//!
//! The cache deliberately leaves orchestration, serialization, encryption,
//! and artifact interpretation to its caller. Python receives file paths and
//! JSON metadata; artifact bytes remain in files owned according to each
//! target's retention policy.

use std::collections::BTreeMap;
use std::path::PathBuf;

use pyo3::exceptions::PyRuntimeError;
use pyo3::prelude::*;
use pyo3::types::PyModule;
use serde_json::Value;

use crate::ArtifactRetention;
use crate::ArtifactVerification;
use crate::CacheContext;
use crate::CacheHit;
use crate::FileArtifactHandle;
use crate::FileArtifactTarget;
use crate::FileStageCache;
use crate::StageRequest;

/// Canonical environmental factors required for safe cache reuse.
///
/// Context is separate from a stage's logical inputs. It represents execution
/// or storage details that can affect whether an existing artifact is reusable,
/// such as an installed package build, toolchain, schema, or encryption-key
/// identity. Factor names and meanings belong to the caller.
///
/// Use stable, namespaced keys such as `renderer.package`. Values should be small,
/// non-sensitive identities. Do not include credentials, encryption keys, or
/// other raw secrets; derive an opaque identity for such material instead. The
/// underlying map is canonically ordered, so insertion order does not alter a
/// request identity.
#[pyclass(
    module = "pyavd_utils._bindings._stage_cache",
    name = "CacheContext",
    frozen,
    skip_from_py_object
)]
#[derive(Clone, Debug)]
pub struct PyCacheContext {
    inner: CacheContext,
}

#[pymethods]
impl PyCacheContext {
    /// Construct context from stable, non-sensitive compatibility identities.
    ///
    /// An empty mapping is valid when a stage has no environmental reuse
    /// constraints. Every key and value must be non-empty and must not contain
    /// a NUL byte. The constructor validates those structural requirements but
    /// cannot determine whether the supplied factors are complete or secret.
    #[new]
    fn new(factors: BTreeMap<String, String>) -> PyResult<Self> {
        CacheContext::new(factors)
            .map(|inner| Self { inner })
            .map_err(runtime_error)
    }

    /// Return a copy of the compatibility factors in deterministic key order.
    #[getter]
    fn factors(&self) -> BTreeMap<String, String> {
        self.inner.factors().clone()
    }
}

/// Complete identity of one stage invocation.
///
/// `stage` and `entry_key` select the logical cache slot. A later successful
/// registration for that pair replaces the prior record. `behavior`, `inputs`,
/// and `context` identify the exact invocation stored in that slot; lookup is a
/// hit only when all request fields match and the recorded artifact passes its
/// verification policy.
///
/// Keep the three identity dimensions distinct:
///
/// * `behavior` identifies code or policy that changes output semantics;
/// * `inputs` identify logical data consumed by the stage;
/// * `context` identifies environmental or physical compatibility constraints.
///
/// The cache treats all identities as opaque strings. Their stability,
/// completeness, namespacing, and versioning remain caller responsibilities.
#[pyclass(
    module = "pyavd_utils._bindings._stage_cache",
    name = "StageCacheRequest",
    frozen,
    skip_from_py_object
)]
#[derive(Clone, Debug)]
pub struct PyStageRequest {
    inner: StageRequest,
}

#[pymethods]
impl PyStageRequest {
    /// Construct a complete stage cache request.
    ///
    /// The constructor does not validate or reinterpret stage, entry, input,
    /// or behavior identities. Changing any argument changes exact invocation
    /// identity, although only `stage` and `entry_key` select the record that
    /// a successful registration replaces.
    #[new]
    fn new(
        stage: String,
        entry_key: String,
        behavior: String,
        inputs: BTreeMap<String, String>,
        context: &PyCacheContext,
    ) -> Self {
        Self {
            inner: StageRequest::new(stage, entry_key, behavior, inputs, context.inner.clone()),
        }
    }

    /// Return the stable stage implementation name.
    #[getter]
    fn stage(&self) -> &str {
        &self.inner.stage
    }

    /// Return the stable logical entry key within the stage.
    #[getter]
    fn entry_key(&self) -> &str {
        &self.inner.entry_key
    }

    /// Return the code and policy behavior identity.
    ///
    /// Callers should change this value whenever identical logical inputs could
    /// produce a meaningfully different result because stage behavior changed.
    #[getter]
    fn behavior(&self) -> &str {
        &self.inner.behavior
    }

    /// Return a copy of the complete named logical input identities.
    ///
    /// Values should identify content or semantics, not merely mutable paths.
    /// Adding, removing, renaming, or changing an entry invalidates reuse.
    #[getter]
    fn inputs(&self) -> BTreeMap<String, String> {
        self.inner.inputs.clone()
    }

    /// Return the environmental compatibility factors required for reuse.
    #[getter]
    fn context(&self) -> PyCacheContext {
        PyCacheContext {
            inner: self.inner.context.clone(),
        }
    }
}

/// Destination and lifecycle policy selected before producing a stage result.
///
/// A target is an instruction to the producer. Constructing it does not create
/// the path, write bytes, or establish a cache record. The producer must finish
/// and close or flush the file before passing the target to
/// `FileStageCache.register`. The cache never copies or moves artifact
/// contents as part of registration.
#[pyclass(
    module = "pyavd_utils._bindings._stage_cache",
    name = "FileArtifactTarget",
    frozen,
    skip_from_py_object
)]
#[derive(Clone, Debug)]
pub struct PyFileArtifactTarget {
    inner: FileArtifactTarget,
}

#[pymethods]
impl PyFileArtifactTarget {
    /// Construct a caller-selected output target.
    ///
    /// `retention` must be `cache_managed` or `client_managed`.
    /// `verification` must be `trust_registered` or
    /// `verify_metadata_then_bytes`. A cache-managed caller-selected path must
    /// be below the cache's configured managed-artifact root when registered.
    #[new]
    fn new(path: String, retention: &str, verification: &str) -> PyResult<Self> {
        Ok(Self {
            inner: FileArtifactTarget::new(
                PathBuf::from(path),
                parse_retention(retention)?,
                parse_verification(verification)?,
            ),
        })
    }

    /// Return the path at which the producer must write the complete result.
    #[getter]
    fn path(&self) -> PathBuf {
        self.inner.path.clone()
    }

    /// Return the artifact ownership policy.
    ///
    /// `cache_managed` permits garbage collection after no current record
    /// references the path. `client_managed` leaves deletion entirely to the
    /// caller and is appropriate for final outputs and external temporary-file
    /// lifecycles.
    #[getter]
    fn retention(&self) -> &'static str {
        retention_name(self.inner.retention)
    }

    /// Return the lookup verification policy.
    ///
    /// `trust_registered` checks only that the path is a regular file.
    /// `verify_metadata_then_bytes` first compares size and modification time,
    /// hashing stored bytes only when either metadata value changed.
    #[getter]
    fn verification(&self) -> &'static str {
        verification_name(self.inner.verification)
    }
}

/// Path-based handle to a registered or reused artifact.
///
/// The handle contains no artifact payload. Later stages read the path directly
/// and retain responsibility for deserialization, decryption, and semantic
/// validation. Its content identity is derived from bytes as stored on disk;
/// for an encrypted or vaulted artifact it therefore identifies ciphertext,
/// not the decrypted logical value.
#[pyclass(
    module = "pyavd_utils._bindings._stage_cache",
    name = "FileArtifactHandle",
    frozen,
    skip_from_py_object
)]
#[derive(Clone, Debug)]
pub struct PyFileArtifactHandle {
    inner: FileArtifactHandle,
}

#[pymethods]
impl PyFileArtifactHandle {
    /// Return the path from which later stages can read the stored bytes.
    #[getter]
    fn path(&self) -> PathBuf {
        self.inner.path.clone()
    }

    /// Return the lowercase hexadecimal SHA-256 identity of stored file bytes.
    ///
    /// This identity can be used as an input identity in a downstream request.
    /// It does not describe decoded or decrypted content unless stored bytes
    /// are themselves that content.
    #[getter]
    fn content_identity(&self) -> &str {
        &self.inner.content_identity
    }

    /// Return `cache_managed` or `client_managed` artifact ownership.
    #[getter]
    fn retention(&self) -> &'static str {
        retention_name(self.inner.retention)
    }

    /// Return `trust_registered` or `verify_metadata_then_bytes` verification.
    #[getter]
    fn verification(&self) -> &'static str {
        verification_name(self.inner.verification)
    }
}

/// Reusable stage result containing a file handle and dependency metadata.
///
/// Successful registration and lookup return this same shape, allowing later
/// stages to consume newly produced and cached artifacts identically. Metadata
/// is caller-owned JSON data associated with the logical record. It does not
/// participate in exact request identity and the cache assigns it no meaning.
#[pyclass(
    module = "pyavd_utils._bindings._stage_cache",
    name = "StageCacheHit",
    frozen,
    skip_from_py_object
)]
#[derive(Clone, Debug)]
pub struct PyCacheHit {
    artifact: PyFileArtifactHandle,
    metadata_json: String,
}

#[pymethods]
impl PyCacheHit {
    /// Return the registered or reused file artifact handle.
    #[getter]
    fn artifact(&self) -> PyFileArtifactHandle {
        self.artifact.clone()
    }

    /// Return stage-specific dependency metadata as a JSON object string.
    ///
    /// The value always represents an object whose keys are deterministically
    /// ordered. An omitted metadata argument during registration is exposed as
    /// `{}`. Consumers are responsible for the schema and interpretation of
    /// values inside the object.
    #[getter]
    fn metadata_json(&self) -> &str {
        &self.metadata_json
    }
}

impl From<CacheHit> for PyCacheHit {
    fn from(value: CacheHit) -> Self {
        Self {
            artifact: PyFileArtifactHandle {
                inner: value.artifact,
            },
            metadata_json: metadata_json(&value.metadata),
        }
    }
}

/// File-backed stage cache with Rust-owned identity and persistence logic.
///
/// The cache keeps the latest record for each `(stage, entry_key)` pair in an
/// index below `root`. Artifact payloads remain separate files. Cache-managed
/// files live below `managed_root` and may be removed when no current record
/// references them; client-managed files are never deleted by the cache.
///
/// A cache instance is intended to have one coordinating writer. Saving uses
/// sibling-file replacement, but independent writers do not merge concurrent
/// in-memory updates. Rename-overwrite is atomic on supported platforms;
/// Windows may lose the reusable index if interrupted during replacement. Keep an instance alive for a build,
/// register completed outputs, then call `FileStageCache.save` to persist
/// records and collect unreferenced cache-managed artifacts.
#[pyclass(module = "pyavd_utils._bindings._stage_cache", name = "FileStageCache")]
#[derive(Debug)]
pub struct PyFileStageCache {
    inner: FileStageCache,
}

#[pymethods]
impl PyFileStageCache {
    /// Open an existing cache or create an empty in-memory cache view.
    ///
    /// `root` contains the durable cache index. When `managed_root` is omitted,
    /// managed artifacts are stored below a cache-owned directory under
    /// `root`; supplying it allows the caller to place those artifacts at a
    /// different controlled location. Opening may remove cache-managed files
    /// that are no longer referenced by the loaded index. Client-managed files
    /// are never removed.
    #[new]
    #[pyo3(signature = (root, managed_root=None))]
    fn new(py: Python<'_>, root: &str, managed_root: Option<&str>) -> PyResult<Self> {
        let root = PathBuf::from(root);
        let managed_root = managed_root.map(PathBuf::from);
        py.detach(|| {
            managed_root.map_or_else(
                || FileStageCache::open(root.clone()),
                |managed| FileStageCache::open_with_managed_root(root.clone(), managed),
            )
        })
        .map(|inner| Self { inner })
        .map_err(runtime_error)
    }

    /// Resolve an exact reusable invocation using its recorded file policy.
    ///
    /// Returns `None` when no record exists, request identity differs, the file
    /// is missing or not regular, or verification detects changed bytes.
    /// `trust_registered` avoids byte hashing. For
    /// `verify_metadata_then_bytes`, matching size and modification time return
    /// immediately; changed metadata triggers SHA-256 verification and matching
    /// bytes refresh the cached fingerprint.
    ///
    /// Deliberately changing bytes while preserving both recorded size and
    /// modification time can fool the metadata fast path. This is the explicit
    /// trust model for client-managed output files.
    fn lookup(&mut self, py: Python<'_>, request: &PyStageRequest) -> PyResult<Option<PyCacheHit>> {
        let request = request.inner.clone();
        py.detach(|| self.inner.lookup(&request))
            .map(|hit| hit.map(Into::into))
            .map_err(runtime_error)
    }

    /// Return prior metadata for the logical stage/entry slot as JSON.
    ///
    /// This intentionally ignores `behavior`, `inputs`, and `context`, and does
    /// not inspect the artifact. It supports incremental dependency discovery
    /// before the caller can form its next exact request. The result is `{}` if
    /// the slot has no prior record. It must not be interpreted as a cache hit.
    fn previous_metadata_json(&self, request: &PyStageRequest) -> String {
        metadata_json(&self.inner.previous_metadata(&request.inner))
    }

    /// Return the recorded stored-byte identity for an exact invocation.
    ///
    /// The request must match the complete recorded identity. Unlike `lookup`,
    /// this method does not check whether the artifact path exists and does not
    /// inspect its metadata or bytes. It is suitable for planning when the
    /// caller only needs the prior identity, not proof of reusable content.
    fn recorded_content_identity(&self, request: &PyStageRequest) -> PyResult<Option<String>> {
        self.inner
            .recorded_content_identity(&request.inner)
            .map_err(runtime_error)
    }

    /// Allocate a unique cache-managed target path.
    ///
    /// `extension` is a simple file extension without a leading dot, separators,
    /// or traversal components. `verification` accepts `trust_registered` or
    /// `verify_metadata_then_bytes`. Allocation chooses a path below the
    /// managed-artifact root but does not create or write the file; the caller
    /// must produce it before registration.
    fn allocate_managed(
        &self,
        py: Python<'_>,
        request: &PyStageRequest,
        extension: &str,
        verification: &str,
    ) -> PyResult<PyFileArtifactTarget> {
        let request = request.inner.clone();
        let extension = extension.to_owned();
        let verification = parse_verification(verification)?;
        py.detach(|| {
            self.inner
                .allocate_managed(&request, &extension, verification)
        })
        .map(|inner| PyFileArtifactTarget { inner })
        .map_err(runtime_error)
    }

    /// Register a completely written artifact for an exact invocation.
    ///
    /// The target file must already exist as a regular file and be complete and
    /// closed or flushed. Registration hashes its stored bytes, captures file
    /// metadata, and replaces the current record for the request's logical
    /// `(stage, entry_key)` slot. It does not persist the index; call `save`
    /// after all desired registrations.
    ///
    /// `metadata_json`, when supplied, must encode a JSON object. Its entries
    /// are returned by both this method and later lookups but do not participate
    /// in exact request identity. The returned hit is immediately usable by
    /// downstream stages.
    #[pyo3(signature = (request, target, metadata_json=None))]
    fn register(
        &mut self,
        py: Python<'_>,
        request: &PyStageRequest,
        target: &PyFileArtifactTarget,
        metadata_json: Option<&str>,
    ) -> PyResult<PyCacheHit> {
        let request = request.inner.clone();
        let target = target.inner.clone();
        let metadata = parse_metadata(metadata_json)?;
        py.detach(|| self.inner.register(&request, target, metadata))
            .map(Into::into)
            .map_err(runtime_error)
    }

    /// Persist the current index and collect unreferenced managed artifacts.
    ///
    /// The index is written through sibling-file replacement. After it is durable,
    /// cache-managed files no longer referenced by any current record are
    /// removed. Client-managed paths are not touched. This method does not merge
    /// changes made concurrently by another cache instance using the same root.
    fn save(&self, py: Python<'_>) -> PyResult<()> {
        py.detach(|| self.inner.save()).map_err(runtime_error)
    }
}

/// Register only the stage-cache classes on the shared Python extension module.
///
/// This is the cache crate's integration point with the wider bindings crate;
/// cache behavior and cache-specific Python types remain implemented here.
pub fn add_cache_classes(module: &Bound<'_, PyModule>) -> PyResult<()> {
    module.add_class::<PyCacheContext>()?;
    module.add_class::<PyStageRequest>()?;
    module.add_class::<PyFileArtifactTarget>()?;
    module.add_class::<PyFileArtifactHandle>()?;
    module.add_class::<PyCacheHit>()?;
    module.add_class::<PyFileStageCache>()?;
    Ok(())
}

/// Decode optional JSON-object metadata supplied by Python.
fn parse_metadata(value: Option<&str>) -> PyResult<BTreeMap<String, Value>> {
    value.map_or_else(
        || Ok(BTreeMap::new()),
        |value| {
            serde_json::from_str(value).map_err(|error| PyRuntimeError::new_err(error.to_string()))
        },
    )
}

/// Encode deterministically ordered metadata for Python consumption.
fn metadata_json(value: &BTreeMap<String, Value>) -> String {
    serde_json::to_string(value).unwrap_or_else(|_| "{}".to_owned())
}

/// Translate the cache's string errors to the binding's runtime exception.
fn runtime_error(error: String) -> PyErr {
    PyRuntimeError::new_err(error)
}

/// Parse the public snake-case retention value.
fn parse_retention(value: &str) -> PyResult<ArtifactRetention> {
    match value {
        "cache_managed" => Ok(ArtifactRetention::CacheManaged),
        "client_managed" => Ok(ArtifactRetention::ClientManaged),
        _ => Err(PyRuntimeError::new_err(
            "retention must be 'cache_managed' or 'client_managed'",
        )),
    }
}

/// Parse the public snake-case verification value.
fn parse_verification(value: &str) -> PyResult<ArtifactVerification> {
    match value {
        "trust_registered" => Ok(ArtifactVerification::TrustRegistered),
        "verify_metadata_then_bytes" => Ok(ArtifactVerification::VerifyMetadataThenBytes),
        _ => Err(PyRuntimeError::new_err(
            "verification must be 'trust_registered' or 'verify_metadata_then_bytes'",
        )),
    }
}

/// Return the stable Python spelling of a retention policy.
fn retention_name(value: ArtifactRetention) -> &'static str {
    match value {
        ArtifactRetention::CacheManaged => "cache_managed",
        ArtifactRetention::ClientManaged => "client_managed",
    }
}

/// Return the stable Python spelling of a verification policy.
fn verification_name(value: ArtifactVerification) -> &'static str {
    match value {
        ArtifactVerification::TrustRegistered => "trust_registered",
        ArtifactVerification::VerifyMetadataThenBytes => "verify_metadata_then_bytes",
    }
}
