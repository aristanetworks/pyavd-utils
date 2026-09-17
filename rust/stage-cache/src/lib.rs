// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Reusable, file-backed caching for deterministic build stages.
//!
//! This crate separates cache decisions from the code that performs a build.
//! A caller describes one invocation with [`StageRequest`], asks a
//! [`FileStageCache`] for an exact match, and runs the stage only on a miss.
//! The producer chooses where it writes the result by using either a target
//! allocated by the cache or a [`FileArtifactTarget`] for an existing output
//! path. After the producer has finished writing, [`FileStageCache::register`]
//! records the result and returns a [`FileArtifactHandle`] that downstream
//! stages can consume directly. The cache never copies, moves, deserializes, or
//! interprets artifact contents.
//!
//! # Invocation identity
//!
//! A request keeps three kinds of identity separate:
//!
//! - `behavior` identifies the stage implementation and policy;
//! - `inputs` identifies the logical data consumed by the stage;
//! - [`CacheContext`] identifies environmental compatibility requirements such
//!   as installed package builds or an artifact encoding configuration.
//!
//! The cache stores one current record for each `(stage, entry_key)` pair.
//! Reuse requires the complete serialized request to match the identity stored
//! in that record. A changed behavior, input, or context factor is therefore a
//! miss and replaces the logical entry's record only after a new result is
//! successfully registered and saved.
//!
//! # Artifact ownership and verification
//!
//! [`ArtifactRetention::CacheManaged`] gives the cache permission to delete
//! unreferenced generations below its managed-artifact root.
//! [`ArtifactRetention::ClientManaged`] only records a reference to a path; the
//! cache never deletes that file. Verification is selected per target through
//! [`ArtifactVerification`]. Immutable cache-owned files can be trusted after
//! registration, while mutable client-owned outputs can use a cheap size and
//! modification-time check with byte hashing as a fallback.
//!
//! Stored bytes are opaque. They may contain JSON, text, encrypted data, or any
//! other format. Callers remain responsible for permissions, encryption,
//! decryption, parsing, and ensuring that [`CacheContext`] contains every
//! environmental factor needed to reuse those bytes safely.
//!
//! # Persistence and concurrency
//!
//! [`FileStageCache::save`] publishes the index through sibling-file replacement
//! and then removes unreferenced cache-managed files. Rename-overwrite is atomic
//! on supported platforms; Windows uses remove-then-rename and may lose the
//! reusable index if interrupted between those operations. Losing an index
//! discards reuse, not caller-managed outputs. Relative paths are resolved when
//! the cache or target is opened. A cache root is intended to have one
//! coordinating writer: this crate does not lock or merge writes from multiple
//! cache instances or processes.

mod file;
mod model;

pub use file::FileStageCache;
pub use file::canonical_bytes;
pub use file::hash_file;
pub use file::sha256;
pub use model::ArtifactRetention;
pub use model::ArtifactVerification;
pub use model::CacheContext;
pub use model::CacheHit;
pub use model::FileArtifactHandle;
pub use model::FileArtifactTarget;
pub use model::StageRequest;

#[cfg(feature = "python-bindings")]
pub mod python;
