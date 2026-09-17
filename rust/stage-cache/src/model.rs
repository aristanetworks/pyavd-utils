// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::BTreeMap;
use std::path::PathBuf;

use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;

/// Environmental compatibility factors required for safe artifact reuse.
///
/// Context is deliberately separate from a stage's logical inputs. It captures
/// facts about the execution environment or physical representation that can
/// affect whether stored bytes are reusable without changing the logical input
/// data. Examples include an installed package build identity, a toolchain
/// identity, or an opaque identity for an encryption configuration.
///
/// Factors are caller-defined string pairs. Keys should be stable and
/// namespaced by their owner, for example `renderer.package` or
/// `storage.encryption_identity`. Values must be stable identities rather than raw
/// secrets or large payloads. [`BTreeMap`] ordering makes serialization and
/// request hashing deterministic regardless of insertion order. An empty
/// context is valid for stages without environmental compatibility factors.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct CacheContext {
    factors: BTreeMap<String, String>,
}

impl CacheContext {
    /// Construct context from caller-owned compatibility factors.
    ///
    /// This validates only structural requirements needed for unambiguous
    /// serialization. It cannot determine whether the caller included every
    /// factor affecting reuse or whether a value contains sensitive data.
    /// Callers should derive non-sensitive identities before construction and
    /// must never place raw credentials in the context.
    ///
    /// # Errors
    ///
    /// Returns an error if a key or value is empty or contains a NUL byte.
    pub fn new(factors: BTreeMap<String, String>) -> Result<Self, String> {
        for (key, value) in &factors {
            if key.is_empty() || key.contains('\0') {
                return Err(
                    "cache-context keys must be non-empty and contain no NUL bytes".to_owned(),
                );
            }
            if value.is_empty() || value.contains('\0') {
                return Err(format!(
                    "cache-context factor {key:?} must be non-empty and contain no NUL bytes"
                ));
            }
        }
        Ok(Self { factors })
    }

    /// Return the factors in deterministic key order.
    ///
    /// The map is borrowed from this immutable context. Clone it only when
    /// ownership is required by another API.
    #[must_use]
    pub fn factors(&self) -> &BTreeMap<String, String> {
        &self.factors
    }
}

/// Complete cache identity of one stage invocation.
///
/// `stage` and `entry_key` select the logical record slot. `behavior`,
/// `inputs`, and `context` participate in the exact request identity stored in
/// that slot. Lookup returns a hit only when every field matches the registered
/// invocation and its file satisfies the recorded verification policy.
///
/// The cache does not assign meaning to any string in this type. Callers own
/// naming, versioning, and the completeness of all supplied identities.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct StageRequest {
    /// Stable name of the stage implementation.
    ///
    /// Together with [`Self::entry_key`], this selects the record that a new
    /// registration replaces.
    pub stage: String,
    /// Stable logical entry key within the stage.
    ///
    /// Different outputs must use different keys even when their inputs and
    /// output bytes happen to be identical.
    pub entry_key: String,
    /// Identity of stage code and policy affecting the result.
    ///
    /// This is normally a caller-maintained algorithm or contract version. It
    /// should change whenever identical inputs could produce meaningfully
    /// different output because stage behavior changed.
    pub behavior: String,
    /// Complete named identities of logical inputs consumed by the stage.
    ///
    /// Values should identify input content or semantics, not merely mutable
    /// locations. Adding, removing, renaming, or changing an entry invalidates
    /// the recorded invocation.
    pub inputs: BTreeMap<String, String>,
    /// Environmental factors required to reuse the physical artifact safely.
    pub context: CacheContext,
}

impl StageRequest {
    /// Construct a complete stage request without interpreting its identities.
    ///
    /// Validation of stage names, entry keys, behavior identifiers, and input
    /// identities belongs to the caller. Context factors have already been
    /// validated by [`CacheContext::new`] or are empty through
    /// [`CacheContext::default`].
    #[must_use]
    pub fn new(
        stage: String,
        entry_key: String,
        behavior: String,
        inputs: BTreeMap<String, String>,
        context: CacheContext,
    ) -> Self {
        Self {
            stage,
            entry_key,
            behavior,
            inputs,
            context,
        }
    }

    /// Build the internal record-slot key for this logical stage entry.
    pub(crate) fn record_key(&self) -> String {
        format!("{}\0{}", self.stage, self.entry_key)
    }
}

/// Ownership policy governing removal of a registered artifact path.
///
/// Retention does not change how a producer writes or a consumer reads a file.
/// It only defines whether cache garbage collection may delete the path after
/// no durable cache record references it.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ArtifactRetention {
    /// The cache owns lifecycle cleanup for the artifact.
    ///
    /// The path must be below the configured managed-artifact root. The cache
    /// may remove it during open or save once no current record references it.
    /// Consumers must not retain the path beyond the cache lifecycle that keeps
    /// its record live.
    CacheManaged,
    /// The client owns the artifact lifetime.
    ///
    /// The cache records and verifies the path but never deletes it. This is
    /// suitable for final outputs, externally managed temporary files, and
    /// files whose permissions or retention are controlled elsewhere.
    ClientManaged,
}

/// Policy for checking a recorded file before returning a cache hit.
///
/// Verification applies to stored bytes at the artifact path. It does not
/// deserialize, decrypt, or validate the logical meaning of those bytes.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ArtifactVerification {
    /// Trust a registered artifact while its path remains a regular file.
    ///
    /// No size, timestamp, or byte hash is checked during lookup. This is the
    /// cheapest policy and is intended for immutable files whose path and
    /// lifetime are controlled by the cache or an equally trusted caller.
    TrustRegistered,
    /// Trust matching size and modification time, otherwise hash stored bytes.
    ///
    /// Registration stores SHA-256, byte length, and modification time. Lookup
    /// returns immediately when length and modification time still match. When
    /// either differs, it hashes the file: matching bytes produce a hit and
    /// refresh the metadata fingerprint, while changed bytes produce a miss.
    /// Deliberately changing bytes while preserving both recorded metadata
    /// values can fool this policy; callers choosing it accept that trust model.
    #[serde(alias = "verify_stored_bytes")]
    VerifyMetadataThenBytes,
}

/// Destination and file policy selected before a stage writes its result.
///
/// This value is an instruction to the producer, not proof that the path exists
/// or contains a complete result. The producer must finish writing and flush or
/// close the file before passing the target to
/// [`crate::FileStageCache::register`]. The cache does not create, copy, move,
/// or replace artifact contents.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileArtifactTarget {
    /// Path at which the producer must write the complete result.
    pub path: PathBuf,
    /// Component permitted to remove the path after registration.
    pub retention: ArtifactRetention,
    /// Verification policy applied by future lookups.
    pub verification: ArtifactVerification,
}

impl FileArtifactTarget {
    /// Construct a target for a caller-selected path and file policy.
    ///
    /// The path is not accessed or normalized until registration. Use
    /// [`crate::FileStageCache::allocate_managed`] when the cache should choose
    /// a unique path below its managed-artifact root.
    #[must_use]
    pub fn new(
        path: PathBuf,
        retention: ArtifactRetention,
        verification: ArtifactVerification,
    ) -> Self {
        Self {
            path,
            retention,
            verification,
        }
    }
}

/// Handle to registered stored bytes that may be passed to another stage.
///
/// A handle contains only a path and identities; it does not contain artifact
/// bytes. Consumers read directly from [`Self::path`] and remain responsible
/// for decoding, decrypting, and interpreting its content.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileArtifactHandle {
    /// Path from which the registered stored bytes can be consumed.
    pub path: PathBuf,
    /// Lowercase hexadecimal SHA-256 identity of the stored file bytes.
    ///
    /// For encrypted files this identifies ciphertext, not decrypted logical
    /// content. A caller needing a logical content identity should register it
    /// separately in stage metadata or derive it before encryption.
    pub content_identity: String,
    /// Component permitted to remove the path.
    pub retention: ArtifactRetention,
    /// Verification policy recorded for future lookups.
    pub verification: ArtifactVerification,
}

/// Result of registering a file or resolving an exact reusable invocation.
///
/// Registration returns the same shape as lookup so downstream scheduling does
/// not need separate representations for newly built and reused results.
#[derive(Clone, Debug, PartialEq)]
pub struct CacheHit {
    /// Handle to the registered or reused stored bytes.
    pub artifact: FileArtifactHandle,
    /// Caller-owned stage metadata associated with the logical record.
    ///
    /// Metadata does not participate in request identity. It can carry details
    /// needed to construct a later request, such as dynamically discovered
    /// dependencies. The cache stores JSON-compatible values without assigning
    /// them semantics.
    pub metadata: BTreeMap<String, Value>,
}

/// Persisted record for the latest invocation of one logical stage entry.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct CacheRecord {
    /// Hash of behavior, logical inputs, context, and slot identifiers.
    pub request_identity: String,
    /// Ownership policy captured when the artifact was registered.
    pub retention: ArtifactRetention,
    /// Lookup policy captured when the artifact was registered.
    pub verification: ArtifactVerification,
    /// SHA-256 identity of the stored file bytes.
    pub content_identity: String,
    /// Optional metadata fast-path identity for older compatible records.
    #[serde(default)]
    pub file_fingerprint: Option<FileFingerprint>,
    /// Absolute path to the registered artifact.
    pub path: PathBuf,
    /// Caller-owned dependency metadata associated with the slot.
    #[serde(default)]
    pub metadata: BTreeMap<String, Value>,
}

/// Cheap size and modification-time identity used before reading file bytes.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(crate) struct FileFingerprint {
    /// Stored file length in bytes.
    pub byte_length: u64,
    /// Modification time expressed as nanoseconds since the Unix epoch.
    pub modified_ns: u64,
}

/// Versioned on-disk index document.
#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct CacheIndex {
    /// On-disk schema identifier used to reject incompatible documents.
    pub schema: String,
    /// Latest record keyed by the encoded stage/entry slot.
    pub records: BTreeMap<String, CacheRecord>,
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::CacheContext;

    #[test]
    fn cache_context_rejects_empty_factors() {
        assert!(CacheContext::new(BTreeMap::from([(String::new(), "value".to_owned())])).is_err());
        assert!(CacheContext::new(BTreeMap::from([("key".to_owned(), String::new())])).is_err());
    }
}
