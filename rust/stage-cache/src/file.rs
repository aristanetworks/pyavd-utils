// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fs;
use std::io::Read as _;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::UNIX_EPOCH;

use serde::Serialize;
use serde_json::Map;
use serde_json::Value;
use sha2::Digest as _;
use sha2::Sha256;

use crate::model::ArtifactRetention;
use crate::model::ArtifactVerification;
use crate::model::CacheHit;
use crate::model::CacheIndex;
use crate::model::CacheRecord;
use crate::model::FileArtifactHandle;
use crate::model::FileArtifactTarget;
use crate::model::FileFingerprint;
use crate::model::StageRequest;

const CACHE_SCHEMA: &str = "stage-cache.v1";
static UNIQUE_SEQUENCE: AtomicU64 = AtomicU64::new(0);

/// File-backed index and artifact lifecycle manager for explicit stage requests.
///
/// Each instance loads the latest record for every logical `(stage,
/// entry_key)` from `index.json`. Methods update an in-memory view until
/// [`Self::save`] publishes it through sibling-file replacement. Cache-managed artifacts live under a
/// separate managed root; client-managed files stay at caller-selected paths.
///
/// One cache root should have one coordinating writer. Instances do not lock
/// the index or merge concurrent changes made by other instances or processes.
#[derive(Debug)]
pub struct FileStageCache {
    root: PathBuf,
    managed: PathBuf,
    records: BTreeMap<String, CacheRecord>,
    reuse_enabled: bool,
}

impl FileStageCache {
    /// Load or initialize a cache whose index and managed files live below `root`.
    ///
    /// The index is read from `root/index.json`, and managed artifacts are
    /// stored below `root/managed`. A relative root is resolved against the
    /// process working directory at open time. Missing directories are created.
    /// Opening also removes managed files that are not referenced by the loaded
    /// index, including files abandoned before registration or save.
    ///
    /// # Errors
    ///
    /// Returns an error when paths cannot be resolved or created, the index
    /// cannot be read or decoded, its schema is unsupported, or initial garbage
    /// collection fails.
    pub fn open(root: PathBuf) -> Result<Self, String> {
        let managed = root.join("managed");
        Self::open_with_managed_root(root, managed)
    }

    /// Load or initialize a cache with a separate managed-artifact root.
    ///
    /// `root` contains the durable `index.json`; `managed` contains files whose
    /// retention is [`ArtifactRetention::CacheManaged`]. Both paths are resolved
    /// to absolute paths when this method runs. This form is useful when index
    /// metadata and potentially large or sensitive artifact bytes require
    /// different storage locations or filesystem policies.
    ///
    /// The same loading, schema validation, garbage collection, error behavior,
    /// and single-writer expectation as [`Self::open`] apply.
    pub fn open_with_managed_root(root: PathBuf, managed: PathBuf) -> Result<Self, String> {
        let root = absolute_path(root)?;
        let managed = absolute_path(managed)?;
        fs::create_dir_all(&root).map_err(|error| {
            format!(
                "failed to create stage-cache index directory {}: {error}",
                root.display()
            )
        })?;
        fs::create_dir_all(&managed).map_err(|error| {
            format!(
                "failed to create stage-cache artifact directory {}: {error}",
                managed.display()
            )
        })?;
        let root = canonical_directory(&root, "index")?;
        let managed = canonical_directory(&managed, "artifact")?;
        let index_path = root.join("index.json");
        let records = if index_path.is_file() {
            let bytes = fs::read(&index_path).map_err(|error| {
                format!(
                    "failed to read stage-cache index {}: {error}",
                    index_path.display()
                )
            })?;
            let index: CacheIndex = serde_json::from_slice(&bytes).map_err(|error| {
                format!(
                    "stage-cache index {} is invalid JSON: {error}",
                    index_path.display()
                )
            })?;
            if index.schema != CACHE_SCHEMA {
                return Err(format!(
                    "stage-cache index {} uses unsupported schema {}",
                    index_path.display(),
                    index.schema
                ));
            }
            index.records
        } else {
            BTreeMap::new()
        };
        validate_record_paths(&records)?;
        let cache = Self {
            root,
            managed,
            records,
            reuse_enabled: true,
        };
        cache.remove_unreferenced_managed()?;
        Ok(cache)
    }

    /// Resolve an exact reusable artifact for `request`.
    ///
    /// Lookup first selects the latest record for the request's `(stage,
    /// entry_key)` slot, then compares the complete request identity including
    /// behavior, inputs, and context. A missing slot, changed request, missing
    /// artifact, failed byte comparison, or disabled reuse returns `Ok(None)`.
    /// The producer should treat all of those outcomes as ordinary cache misses.
    ///
    /// Artifact verification follows the policy chosen at registration. A
    /// successful metadata fallback may update the in-memory file fingerprint;
    /// call [`Self::save`] to persist that refreshed fingerprint.
    ///
    /// # Errors
    ///
    /// Returns an error for request serialization failures or filesystem errors
    /// encountered while inspecting or hashing a candidate. Errors represent an
    /// unavailable or inconsistent cache, not an ordinary miss.
    pub fn lookup(&mut self, request: &StageRequest) -> Result<Option<CacheHit>, String> {
        if !self.reuse_enabled {
            return Ok(None);
        }
        let record_key = request.record_key();
        let Some(record) = self.records.get(&record_key).cloned() else {
            return Ok(None);
        };
        if record.request_identity != request_identity(request)? {
            return Ok(None);
        }
        let path = self.record_path(&record);
        if !path.is_file() {
            return Ok(None);
        }
        if record.verification == ArtifactVerification::VerifyMetadataThenBytes {
            let current_fingerprint = file_fingerprint(&path)?;
            if record.file_fingerprint.as_ref() != Some(&current_fingerprint) {
                let (content_identity, hashed_fingerprint) = hash_file_stable(&path)?;
                if content_identity != record.content_identity {
                    return Ok(None);
                }
                self.records
                    .get_mut(&record_key)
                    .ok_or_else(|| "stage-cache record disappeared during lookup".to_owned())?
                    .file_fingerprint = Some(hashed_fingerprint);
            }
        }
        Ok(Some(CacheHit {
            artifact: FileArtifactHandle {
                path,
                content_identity: record.content_identity.clone(),
                retention: record.retention,
                verification: record.verification,
            },
            metadata: record.metadata.clone(),
        }))
    }

    /// Return metadata from the latest record in the logical entry slot.
    ///
    /// Unlike [`Self::lookup`], this method intentionally does not require the
    /// complete request identity to match. It uses only `(stage, entry_key)` so
    /// a caller can recover metadata from the previous invocation while
    /// constructing the next request. This supports metadata such as dynamically
    /// discovered dependencies.
    ///
    /// Returns an empty map when the slot has no record or reuse is disabled.
    /// The artifact path and bytes are not inspected.
    #[must_use]
    pub fn previous_metadata(&self, request: &StageRequest) -> BTreeMap<String, Value> {
        if !self.reuse_enabled {
            return BTreeMap::new();
        }
        self.records
            .get(&request.record_key())
            .map_or_else(BTreeMap::new, |record| record.metadata.clone())
    }

    /// Return the stored-byte identity recorded for an exact invocation.
    ///
    /// This compares the complete request identity but deliberately does not
    /// check whether the artifact path exists or whether current bytes still
    /// match. It is useful for planning downstream reuse before paying to verify
    /// or load an intermediate artifact. Call [`Self::lookup`] before consuming
    /// the intermediate itself.
    ///
    /// Returns `None` for a missing or different invocation and while reuse is
    /// disabled.
    ///
    /// # Errors
    ///
    /// Returns an error if the request cannot be canonically serialized.
    pub fn recorded_content_identity(
        &self,
        request: &StageRequest,
    ) -> Result<Option<String>, String> {
        if !self.reuse_enabled {
            return Ok(None);
        }
        let Some(record) = self.records.get(&request.record_key()) else {
            return Ok(None);
        };
        Ok((record.request_identity == request_identity(request)?)
            .then(|| record.content_identity.clone()))
    }

    /// Choose a unique cache-managed output path for a stage attempt.
    ///
    /// The returned [`FileArtifactTarget`] has
    /// [`ArtifactRetention::CacheManaged`] and the requested verification policy.
    /// Its parent directory is created, but the artifact file itself is not. The
    /// caller must write the complete result to the returned path and register
    /// the target only after a successful stage execution.
    ///
    /// `extension` is written without a leading dot and must contain 1-16 ASCII
    /// letters, digits, or `-`. The generated name includes the process ID, the
    /// complete request identity, and a process-local sequence number.
    ///
    /// # Errors
    ///
    /// Returns an error for an invalid extension, request serialization failure,
    /// or failure to create the stage directory.
    pub fn allocate_managed(
        &self,
        request: &StageRequest,
        extension: &str,
        verification: ArtifactVerification,
    ) -> Result<FileArtifactTarget, String> {
        let extension = validate_extension(extension)?;
        let directory = self.managed.join(safe_component(&request.stage));
        fs::create_dir_all(&directory).map_err(|error| {
            format!(
                "failed to create stage-cache generation directory {}: {error}",
                directory.display()
            )
        })?;
        let sequence = UNIQUE_SEQUENCE.fetch_add(1, Ordering::Relaxed);
        let filename = format!(
            "{}-{}-{sequence}.{extension}",
            std::process::id(),
            request_identity(request)?
        );
        Ok(FileArtifactTarget::new(
            directory.join(filename),
            ArtifactRetention::CacheManaged,
            verification,
        ))
    }

    /// Finalize and record a successfully written target.
    ///
    /// The target file must already exist and must no longer be changing. This
    /// method hashes the stored bytes, captures size and modification time, and
    /// returns a [`CacheHit`] containing the resulting handle and supplied
    /// metadata. It does not write, move, copy, decrypt, or parse the artifact.
    ///
    /// Registration replaces the in-memory record for the request's `(stage,
    /// entry_key)` slot. The replacement becomes durable only after
    /// [`Self::save`]. When reuse is disabled, the method still validates and
    /// identifies the file so downstream code receives the normal handle, but
    /// it does not alter durable cache records.
    ///
    /// Cache-managed targets must be below the configured managed root.
    /// Client-managed targets may be anywhere accessible to the process and are
    /// stored as absolute paths. `metadata` is caller-owned JSON-compatible data
    /// and does not participate in request identity.
    ///
    /// # Errors
    ///
    /// Returns an error when a target path is invalid for its retention policy,
    /// the file cannot be inspected or hashed, the file changes repeatedly while
    /// being hashed, or the request cannot be serialized.
    pub fn register(
        &mut self,
        request: &StageRequest,
        target: FileArtifactTarget,
        metadata: BTreeMap<String, Value>,
    ) -> Result<CacheHit, String> {
        let requested_path = absolute_path(target.path)?;
        let (path, stored_path) = match target.retention {
            ArtifactRetention::CacheManaged => {
                let path = fs::canonicalize(&requested_path).map_err(|error| {
                    format!(
                        "failed to resolve cache-managed artifact {}: {error}",
                        requested_path.display()
                    )
                })?;
                let stored_path = path
                    .strip_prefix(&self.managed)
                    .map(Path::to_path_buf)
                    .map_err(|_strip_error| {
                        format!(
                            "cache-managed artifact {} is outside configured root {}",
                            path.display(),
                            self.managed.display()
                        )
                    })?;
                validate_managed_relative_path(&stored_path)?;
                (path, stored_path)
            }
            ArtifactRetention::ClientManaged => (requested_path.clone(), requested_path),
        };
        let (content_identity, file_fingerprint) = hash_file_stable(&path)?;
        let request_identity = request_identity(request)?;
        if self.reuse_enabled {
            self.records.insert(
                request.record_key(),
                CacheRecord {
                    request_identity,
                    retention: target.retention,
                    verification: target.verification,
                    content_identity: content_identity.clone(),
                    file_fingerprint: Some(file_fingerprint),
                    path: stored_path,
                    metadata: metadata.clone(),
                },
            );
        }
        Ok(CacheHit {
            artifact: FileArtifactHandle {
                path,
                content_identity,
                retention: target.retention,
                verification: target.verification,
            },
            metadata,
        })
    }

    /// Disable reuse for the remainder of this cache instance's lifetime.
    ///
    /// After this call, lookups and historical metadata queries behave as misses.
    /// Registration still returns a fully identified handle for newly produced
    /// files but does not replace loaded records. [`Self::save`] preserves the
    /// existing durable index and removes newly abandoned managed files.
    ///
    /// This is intended for execution environments whose compatibility identity
    /// cannot be established safely, such as an editable source installation.
    /// There is intentionally no matching enable method; open a new instance to
    /// resume durable reuse.
    pub fn disable_reuse(&mut self) {
        self.reuse_enabled = false;
    }

    /// Persist the in-memory index and collect unreferenced managed files.
    ///
    /// With reuse enabled, the complete index is canonically serialized and
    /// published through sibling-file replacement. Garbage collection then
    /// removes files below the managed root that are not referenced by any
    /// current cache-managed record. Client-managed files are never removed.
    ///
    /// With reuse disabled, the durable index is left untouched and garbage
    /// collection still runs against the records loaded when the cache opened.
    /// This cleans temporary managed outputs produced during a non-caching run
    /// without destroying the last reusable index.
    ///
    /// # Errors
    ///
    /// Returns an error if serialization, index publication, directory traversal,
    /// or managed-file removal fails. If publication succeeds but collection
    /// fails, the new index is already durable when the error is returned.
    pub fn save(&self) -> Result<(), String> {
        if self.reuse_enabled {
            let index = CacheIndex {
                schema: CACHE_SCHEMA.to_owned(),
                records: self.records.clone(),
            };
            atomic_write(&self.root.join("index.json"), &canonical_bytes(&index)?)?;
        }
        self.remove_unreferenced_managed()
    }

    fn remove_unreferenced_managed(&self) -> Result<(), String> {
        let live = self
            .records
            .values()
            .filter(|record| record.retention == ArtifactRetention::CacheManaged)
            .map(|record| self.managed.join(&record.path))
            .collect::<BTreeSet<_>>();
        remove_unreferenced_files(&self.managed, &live)
    }

    fn record_path(&self, record: &CacheRecord) -> PathBuf {
        match record.retention {
            ArtifactRetention::CacheManaged => self.managed.join(&record.path),
            ArtifactRetention::ClientManaged => record.path.clone(),
        }
    }
}

/// Serialize a value to deterministic compact JSON bytes.
///
/// Object keys are sorted recursively, while array order and scalar values are
/// preserved. Floating-point numbers and unsigned integers above `i64::MAX` are
/// rejected so callers cannot accidentally depend on representations outside
/// the cache's deliberately narrow canonical number model. The cache uses this
/// function before hashing [`StageRequest`] values and writing its index.
///
/// # Errors
///
/// Returns an error when Serde cannot convert or encode the value, or when a
/// number is outside the supported canonical model.
pub fn canonical_bytes<T: Serialize>(value: &T) -> Result<Vec<u8>, String> {
    let mut value = serde_json::to_value(value)
        .map_err(|error| format!("failed to encode canonical JSON: {error}"))?;
    normalize_value(&mut value)?;
    serde_json::to_vec(&value).map_err(|error| format!("failed to encode canonical JSON: {error}"))
}

/// Compute lowercase hexadecimal SHA-256 for an in-memory byte slice.
///
/// The returned string has 64 hexadecimal characters and no `sha256:` prefix.
#[must_use]
pub fn sha256(bytes: &[u8]) -> String {
    hex::encode(Sha256::digest(bytes))
}

/// Compute lowercase hexadecimal SHA-256 for the current bytes of a file.
///
/// The file is read in fixed-size chunks rather than loaded into one allocation.
/// This function does not compare metadata before and after reading, so callers
/// requiring a stable snapshot must ensure the file is not being modified.
/// [`FileStageCache::register`] adds that stability check around hashing.
///
/// # Errors
///
/// Returns an error when the file cannot be opened or read.
pub fn hash_file(path: &Path) -> Result<String, String> {
    let mut file = fs::File::open(path)
        .map_err(|error| format!("failed to open cache artifact {}: {error}", path.display()))?;
    let mut hasher = Sha256::new();
    let mut buffer = vec![0_u8; 64 * 1024].into_boxed_slice();
    loop {
        let read = file.read(&mut buffer).map_err(|error| {
            format!("failed to hash cache artifact {}: {error}", path.display())
        })?;
        if read == 0 {
            break;
        }
        let chunk = buffer
            .get(..read)
            .ok_or_else(|| "file read exceeded the hashing buffer".to_owned())?;
        hasher.update(chunk);
    }
    Ok(hex::encode(hasher.finalize()))
}

/// Read the cheap size and modification-time fingerprint for a file.
fn file_fingerprint(path: &Path) -> Result<FileFingerprint, String> {
    let metadata = fs::metadata(path).map_err(|error| {
        format!(
            "failed to inspect cache artifact {}: {error}",
            path.display()
        )
    })?;
    let modified = metadata
        .modified()
        .and_then(|modified| {
            modified
                .duration_since(UNIX_EPOCH)
                .map_err(|error| std::io::Error::other(error.to_string()))
        })
        .map_err(|error| {
            format!(
                "failed to inspect modification time of cache artifact {}: {error}",
                path.display()
            )
        })?;
    let modified_ns = u64::try_from(modified.as_nanos()).map_err(|_conversion_error| {
        format!(
            "modification time of cache artifact {} exceeds supported range",
            path.display()
        )
    })?;
    Ok(FileFingerprint {
        byte_length: metadata.len(),
        modified_ns,
    })
}

/// Hash a file only when its metadata remains stable around the read.
fn hash_file_stable(path: &Path) -> Result<(String, FileFingerprint), String> {
    for _ in 0..2 {
        let before = file_fingerprint(path)?;
        let content_identity = hash_file(path)?;
        let after = file_fingerprint(path)?;
        if before == after {
            return Ok((content_identity, after));
        }
    }
    Err(format!(
        "cache artifact {} changed while it was being hashed",
        path.display()
    ))
}

/// Canonically hash a complete stage request.
fn request_identity(request: &StageRequest) -> Result<String, String> {
    canonical_bytes(request).map(|bytes| sha256(&bytes))
}

/// Recursively sort objects and enforce the canonical number subset.
fn normalize_value(value: &mut Value) -> Result<(), String> {
    match value {
        Value::Null | Value::Bool(_) | Value::String(_) => Ok(()),
        Value::Number(number) => {
            if number.as_f64().is_some() && number.as_i64().is_none() && number.as_u64().is_none() {
                return Err("canonical JSON rejects floating point numbers".to_owned());
            }
            if number
                .as_u64()
                .is_some_and(|unsigned| i64::try_from(unsigned).is_err())
            {
                return Err(
                    "canonical JSON rejects unsigned integers above signed 64-bit range".to_owned(),
                );
            }
            Ok(())
        }
        Value::Array(items) => items.iter_mut().try_for_each(normalize_value),
        Value::Object(map) => {
            let mut entries = std::mem::take(map).into_iter().collect::<Vec<_>>();
            entries.sort_unstable_by(|(left, _), (right, _)| left.cmp(right));
            let mut sorted = Map::new();
            for (key, mut nested) in entries {
                normalize_value(&mut nested)?;
                sorted.insert(key, nested);
            }
            *map = sorted;
            Ok(())
        }
    }
}

/// Validate an extension used in a generated managed-artifact filename.
fn validate_extension(extension: &str) -> Result<&str, String> {
    if extension.is_empty()
        || extension.len() > 16
        || !extension
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
    {
        return Err(
            "cache artifact extension must be 1-16 ASCII letters, digits, or '-'".to_owned(),
        );
    }
    Ok(extension)
}

/// Convert a caller stage name into one portable directory component.
fn safe_component(value: &str) -> String {
    value
        .bytes()
        .map(|byte| {
            if byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_') {
                char::from(byte)
            } else {
                '_'
            }
        })
        .collect()
}

/// Resolve a relative caller path against the current working directory.
fn absolute_path(path: PathBuf) -> Result<PathBuf, String> {
    if path.is_absolute() {
        Ok(path)
    } else {
        std::env::current_dir()
            .map(|current| current.join(path))
            .map_err(|error| format!("failed to resolve borrowed cache path: {error}"))
    }
}

/// Canonicalize a cache directory after its creation.
fn canonical_directory(path: &Path, kind: &str) -> Result<PathBuf, String> {
    fs::canonicalize(path).map_err(|error| {
        format!(
            "failed to resolve stage-cache {kind} directory {}: {error}",
            path.display()
        )
    })
}

/// Reject persisted paths that could escape cache ownership boundaries.
fn validate_record_paths(records: &BTreeMap<String, CacheRecord>) -> Result<(), String> {
    for (record_key, record) in records {
        match record.retention {
            ArtifactRetention::CacheManaged => validate_managed_relative_path(&record.path)
                .map_err(|error| {
                    format!("stage-cache record {record_key:?} is invalid: {error}")
                })?,
            ArtifactRetention::ClientManaged if !record.path.is_absolute() => {
                return Err(format!(
                    "stage-cache record {record_key:?} has a relative client-managed path"
                ));
            }
            ArtifactRetention::ClientManaged => {}
        }
    }
    Ok(())
}

/// Require one non-empty relative path made only from ordinary components.
fn validate_managed_relative_path(path: &Path) -> Result<(), String> {
    if path.as_os_str().is_empty()
        || !path
            .components()
            .all(|component| matches!(component, Component::Normal(_)))
    {
        return Err(format!(
            "cache-managed artifact path {} must stay below its configured root",
            path.display()
        ));
    }
    Ok(())
}

/// Recursively remove files below `directory` that are absent from `live`.
fn remove_unreferenced_files(directory: &Path, live: &BTreeSet<PathBuf>) -> Result<(), String> {
    for entry in fs::read_dir(directory).map_err(|error| {
        format!(
            "failed to inspect stage-cache artifacts {}: {error}",
            directory.display()
        )
    })? {
        let entry =
            entry.map_err(|error| format!("failed to inspect a stage-cache artifact: {error}"))?;
        let file_type = entry.file_type().map_err(|error| {
            format!(
                "failed to inspect stage-cache artifact {}: {error}",
                entry.path().display()
            )
        })?;
        let path = entry.path();
        if file_type.is_dir() {
            remove_unreferenced_files(&path, live)?;
        } else if !live.contains(&path) {
            fs::remove_file(&path).map_err(|error| {
                format!(
                    "failed to remove unreferenced stage-cache artifact {}: {error}",
                    path.display()
                )
            })?;
        }
    }
    Ok(())
}

/// Publish complete bytes by replacing the destination with a sibling file.
///
/// Rename-overwrite is atomic where the platform supports it. Windows requires
/// removing an existing destination first, so interruption can discard the
/// cache index but cannot expose partially written index bytes.
fn atomic_write(path: &Path, content: &[u8]) -> Result<(), String> {
    let parent = path
        .parent()
        .ok_or_else(|| format!("stage-cache path {} has no parent", path.display()))?;
    fs::create_dir_all(parent).map_err(|error| {
        format!(
            "failed to create stage-cache directory {}: {error}",
            parent.display()
        )
    })?;
    let sequence = UNIQUE_SEQUENCE.fetch_add(1, Ordering::Relaxed);
    let temporary = parent.join(format!(
        ".{}.{}.{}.tmp",
        path.file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("index"),
        std::process::id(),
        sequence
    ));
    fs::write(&temporary, content).map_err(|error| {
        format!(
            "failed to write temporary stage-cache index {}: {error}",
            temporary.display()
        )
    })?;
    replace_file(&temporary, path).map_err(|error| {
        let _ = fs::remove_file(&temporary);
        format!(
            "failed to publish stage-cache index {} to {}: {error}",
            temporary.display(),
            path.display()
        )
    })
}

#[cfg(not(windows))]
/// Replace a destination atomically where the platform supports rename-overwrite.
fn replace_file(source: &Path, destination: &Path) -> std::io::Result<()> {
    fs::rename(source, destination)
}

#[cfg(windows)]
/// Replace a destination on Windows, where rename does not overwrite it.
fn replace_file(source: &Path, destination: &Path) -> std::io::Result<()> {
    if destination.exists() {
        fs::remove_file(destination)?;
    }
    fs::rename(source, destination)
}

#[cfg(test)]
mod tests {
    #![allow(
        clippy::panic_in_result_fn,
        reason = "assertions provide clearer failures in fallible cache tests"
    )]

    use super::*;

    fn root(label: &str) -> PathBuf {
        std::env::temp_dir().join(format!(
            "stage-cache-{label}-{}-{}",
            std::process::id(),
            UNIQUE_SEQUENCE.fetch_add(1, Ordering::Relaxed)
        ))
    }

    fn request() -> StageRequest {
        StageRequest::new(
            "compile".to_owned(),
            "documents/report".to_owned(),
            "compile.v1".to_owned(),
            BTreeMap::from([("source".to_owned(), "sha256:abc".to_owned())]),
            crate::CacheContext::default(),
        )
    }

    #[test]
    fn managed_target_is_reused_without_moving_the_producer_file() -> Result<(), String> {
        let root = root("managed");
        let mut cache = FileStageCache::open(root.clone())?;
        let request = request();
        let target =
            cache.allocate_managed(&request, "json", ArtifactVerification::TrustRegistered)?;
        fs::write(&target.path, br#"{"result":true}"#).map_err(|error| error.to_string())?;
        let path = target.path.clone();
        let registered = cache.register(&request, target, BTreeMap::new())?;
        assert_eq!(registered.artifact.path, path);
        cache.save()?;

        let mut reopened = FileStageCache::open(root.clone())?;
        assert_eq!(
            reopened.lookup(&request)?.map(|hit| hit.artifact.path),
            Some(path)
        );
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn verified_client_target_falls_back_to_bytes_after_metadata_changes() -> Result<(), String> {
        let root = root("verified-client");
        let target = root.join("target.txt");
        fs::create_dir_all(&root).map_err(|error| error.to_string())?;
        fs::write(&target, "first result\n").map_err(|error| error.to_string())?;
        let mut cache = FileStageCache::open(root.join("cache"))?;
        let request = request();
        cache.register(
            &request,
            FileArtifactTarget::new(
                target.clone(),
                ArtifactRetention::ClientManaged,
                ArtifactVerification::VerifyMetadataThenBytes,
            ),
            BTreeMap::new(),
        )?;
        cache
            .records
            .get_mut(&request.record_key())
            .ok_or_else(|| "registered cache record is missing".to_owned())?
            .file_fingerprint = None;
        assert!(cache.lookup(&request)?.is_some());
        assert!(
            cache
                .records
                .get(&request.record_key())
                .and_then(|record| record.file_fingerprint.as_ref())
                .is_some(),
            "successful byte fallback should refresh the cheap fingerprint"
        );

        fs::write(&target, "changed result\n").map_err(|error| error.to_string())?;
        assert!(cache.lookup(&request)?.is_none());
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn verified_client_target_trusts_matching_file_metadata() -> Result<(), String> {
        let root = root("verified-client-metadata");
        let target = root.join("target.txt");
        fs::create_dir_all(&root).map_err(|error| error.to_string())?;
        fs::write(&target, "first result\n").map_err(|error| error.to_string())?;
        let mut cache = FileStageCache::open(root.join("cache"))?;
        let request = request();
        cache.register(
            &request,
            FileArtifactTarget::new(
                target,
                ArtifactRetention::ClientManaged,
                ArtifactVerification::VerifyMetadataThenBytes,
            ),
            BTreeMap::new(),
        )?;
        cache
            .records
            .get_mut(&request.record_key())
            .ok_or_else(|| "registered cache record is missing".to_owned())?
            .content_identity = "not-the-file-hash".to_owned();

        assert_eq!(
            cache
                .lookup(&request)?
                .map(|hit| hit.artifact.content_identity),
            Some("not-the-file-hash".to_owned()),
            "matching metadata should avoid reading and hashing the file"
        );
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn trusted_client_target_is_not_rehashed() -> Result<(), String> {
        let root = root("trusted-client");
        let target = root.join("validated.json");
        fs::create_dir_all(&root).map_err(|error| error.to_string())?;
        fs::write(&target, "{}").map_err(|error| error.to_string())?;
        let mut cache = FileStageCache::open(root.join("cache"))?;
        let request = request();
        cache.register(
            &request,
            FileArtifactTarget::new(
                target.clone(),
                ArtifactRetention::ClientManaged,
                ArtifactVerification::TrustRegistered,
            ),
            BTreeMap::new(),
        )?;

        fs::write(&target, r#"{"changed":true}"#).map_err(|error| error.to_string())?;
        assert!(cache.lookup(&request)?.is_some());
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn disabled_reuse_neither_reads_nor_replaces_durable_records() -> Result<(), String> {
        let root = root("disabled-reuse");
        let original = request();
        let mut cache = FileStageCache::open(root.clone())?;
        let original_target =
            cache.allocate_managed(&original, "json", ArtifactVerification::TrustRegistered)?;
        fs::write(&original_target.path, "{}").map_err(|error| error.to_string())?;
        cache.register(&original, original_target, BTreeMap::new())?;
        cache.save()?;

        let mut disabled = FileStageCache::open(root.clone())?;
        disabled.disable_reuse();
        assert!(disabled.lookup(&original)?.is_none());
        assert!(disabled.previous_metadata(&original).is_empty());
        assert!(disabled.recorded_content_identity(&original)?.is_none());

        let mut changed = original.clone();
        changed.behavior = "compile.changed".to_owned();
        let temporary =
            disabled.allocate_managed(&changed, "json", ArtifactVerification::TrustRegistered)?;
        let temporary_path = temporary.path.clone();
        fs::write(&temporary_path, "{}").map_err(|error| error.to_string())?;
        disabled.register(&changed, temporary, BTreeMap::new())?;
        disabled.save()?;
        assert!(!temporary_path.exists());

        let mut reopened = FileStageCache::open(root.clone())?;
        assert!(reopened.lookup(&original)?.is_some());
        assert!(reopened.lookup(&changed)?.is_none());
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn changed_inputs_miss_but_preserve_previous_metadata() -> Result<(), String> {
        let root = root("metadata");
        let mut cache = FileStageCache::open(root.clone())?;
        let original = request();
        let target =
            cache.allocate_managed(&original, "json", ArtifactVerification::TrustRegistered)?;
        fs::write(&target.path, "{}").map_err(|error| error.to_string())?;
        cache.register(
            &original,
            target,
            BTreeMap::from([("dependencies".to_owned(), serde_json::json!(["source"]))]),
        )?;
        let mut changed = original.clone();
        changed
            .inputs
            .insert("source".to_owned(), "sha256:def".to_owned());
        assert!(cache.lookup(&changed)?.is_none());
        assert_eq!(
            cache.previous_metadata(&changed).get("dependencies"),
            Some(&serde_json::json!(["source"]))
        );
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn changed_cache_context_misses_the_recorded_invocation() -> Result<(), String> {
        let root = root("context");
        let mut cache = FileStageCache::open(root.clone())?;
        let original = request();
        let target =
            cache.allocate_managed(&original, "json", ArtifactVerification::TrustRegistered)?;
        fs::write(&target.path, "{}").map_err(|error| error.to_string())?;
        cache.register(&original, target, BTreeMap::new())?;

        let mut changed = original.clone();
        changed.context = crate::CacheContext::new(BTreeMap::from([(
            "runtime.package".to_owned(),
            "sha256:changed".to_owned(),
        )]))?;
        assert!(cache.lookup(&changed)?.is_none());
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn reopening_removes_an_abandoned_managed_target() -> Result<(), String> {
        let root = root("abandoned");
        let cache = FileStageCache::open(root.clone())?;
        let target =
            cache.allocate_managed(&request(), "json", ArtifactVerification::TrustRegistered)?;
        fs::write(&target.path, "{}").map_err(|error| error.to_string())?;
        assert!(target.path.is_file());
        drop(cache);

        let _cache = FileStageCache::open(root.clone())?;
        assert!(!target.path.exists());
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn cache_managed_target_cannot_escape_the_managed_root() -> Result<(), String> {
        let root = root("managed-boundary");
        let outside = root.join("outside.json");
        let mut cache = FileStageCache::open(root.clone())?;
        fs::write(&outside, "{}").map_err(|error| error.to_string())?;

        let result = cache.register(
            &request(),
            FileArtifactTarget::new(
                outside,
                ArtifactRetention::CacheManaged,
                ArtifactVerification::TrustRegistered,
            ),
            BTreeMap::new(),
        );

        assert!(result.is_err());
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[test]
    fn reopening_rejects_escaping_managed_record_path() -> Result<(), String> {
        let root = root("record-boundary");
        let mut cache = FileStageCache::open(root.clone())?;
        let request = request();
        let target =
            cache.allocate_managed(&request, "json", ArtifactVerification::TrustRegistered)?;
        fs::write(&target.path, "{}").map_err(|error| error.to_string())?;
        cache.register(&request, target, BTreeMap::new())?;
        cache
            .records
            .get_mut(&request.record_key())
            .ok_or_else(|| "registered cache record is missing".to_owned())?
            .path = PathBuf::from("../outside.json");
        cache.save()?;

        assert!(FileStageCache::open(root.clone()).is_err());
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }

    #[cfg(unix)]
    #[test]
    fn garbage_collection_removes_directory_symlink_without_following_it() -> Result<(), String> {
        use std::os::unix::fs::symlink;

        let root = root("managed-symlink");
        let external = root.join("external");
        let external_file = external.join("keep.txt");
        fs::create_dir_all(&external).map_err(|error| error.to_string())?;
        fs::write(&external_file, "keep").map_err(|error| error.to_string())?;
        let cache_root = root.join("cache");
        let cache = FileStageCache::open(cache_root.clone())?;
        let link = cache.managed.join("external-link");
        symlink(&external, &link).map_err(|error| error.to_string())?;
        drop(cache);

        let _reopened = FileStageCache::open(cache_root)?;
        assert!(!link.exists());
        assert!(external_file.is_file());
        fs::remove_dir_all(root).map_err(|error| error.to_string())
    }
}
