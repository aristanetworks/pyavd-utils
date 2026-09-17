# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# The native extension API is documented here so static users do not need to inspect Rust sources.
# ruff: noqa: PYI021

from collections.abc import Mapping
from pathlib import Path
from typing import final

@final
class CacheContext:
    """
    Environmental compatibility factors required for safe cache reuse.

    Context is separate from a stage's logical inputs. It identifies execution
    or storage details that affect whether existing bytes are reusable, such as
    a package build, toolchain, schema, or encryption configuration. Names and
    meanings belong to the caller.

    Factor values should be small, stable, and non-sensitive. Credentials,
    encryption keys, and other secrets should be represented by opaque
    identities rather than included directly.
    """

    @property
    def factors(self) -> dict[str, str]:
        """Return a copy of the factors in deterministic key order."""

    def __new__(cls, factors: Mapping[str, str]) -> CacheContext:
        """
        Construct context from caller-owned compatibility identities.

        An empty mapping is valid. Keys and values must be non-empty and may
        not contain NUL bytes. The cache cannot determine whether the caller
        supplied every factor needed for safe reuse.
        """

@final
class StageCacheRequest:
    """
    Complete identity of one deterministic stage invocation.

    ``stage`` and ``entry_key`` select the logical cache slot. ``behavior``,
    ``inputs``, and ``context`` identify the exact invocation stored there.
    Reuse requires all fields to match and the registered artifact to satisfy
    its verification policy.
    """

    @property
    def stage(self) -> str:
        """Return the stable stage name."""

    @property
    def entry_key(self) -> str:
        """Return the stable logical entry key within the stage."""

    @property
    def behavior(self) -> str:
        """Return the identity of code and policy affecting the result."""

    @property
    def inputs(self) -> dict[str, str]:
        """Return a copy of all named logical input identities."""

    @property
    def context(self) -> CacheContext:
        """Return the environmental compatibility context."""

    def __new__(
        cls,
        stage: str,
        entry_key: str,
        behavior: str,
        inputs: Mapping[str, str],
        context: CacheContext,
    ) -> StageCacheRequest:
        """
        Construct a request from opaque caller-owned identities.

        Only ``stage`` and ``entry_key`` choose which durable record a later
        registration replaces. Every argument participates in exact invocation
        identity.
        """

@final
class FileArtifactTarget:
    """
    Destination and lifecycle policy selected before producing a stage result.

    Constructing a target does not create or write the path. The producer must
    completely write and close or flush the file before registration. The cache
    never copies or moves artifact contents during registration.
    """

    @property
    def path(self) -> Path:
        """Return the path at which the producer must write the result."""

    @property
    def retention(self) -> str:
        """
        Return ``cache_managed`` or ``client_managed`` ownership.

        Cache-managed files may be collected after no current record references
        them. Client-managed files are never deleted by the cache.
        """

    @property
    def verification(self) -> str:
        """
        Return ``trust_registered`` or ``verify_metadata_then_bytes``.

        Metadata verification compares size and modification time first and
        hashes bytes only when either value differs.
        """

    def __new__(cls, path: str, retention: str, verification: str) -> FileArtifactTarget:
        """Construct a caller-selected artifact target and file policy."""

@final
class FileArtifactHandle:
    """
    Path-based handle to registered or reused stored bytes.

    A handle contains identities and a path, not the artifact payload. Consumers
    read from that path and remain responsible for decoding, decryption, and
    semantic interpretation.
    """

    @property
    def path(self) -> Path:
        """Return the path from which stored bytes can be consumed."""

    @property
    def content_identity(self) -> str:
        """Return the lowercase hexadecimal SHA-256 identity of stored bytes."""

    @property
    def retention(self) -> str:
        """Return the recorded artifact ownership policy."""

    @property
    def verification(self) -> str:
        """Return the recorded lookup verification policy."""

@final
class StageCacheHit:
    """
    Reusable stage result containing a file handle and caller metadata.

    Registration and lookup return the same shape. Metadata does not participate
    in exact request identity and the cache does not assign it semantics.
    """

    @property
    def artifact(self) -> FileArtifactHandle:
        """Return the registered or reused artifact handle."""

    @property
    def metadata_json(self) -> str:
        """Return caller-owned metadata as a deterministically ordered JSON object."""

@final
class FileStageCache:
    """
    File-backed cache with Rust-owned identity, verification, and persistence.

    The cache keeps one current record per ``(stage, entry_key)`` below
    ``root``. Cache-managed artifacts may be collected when no current record
    references them; client-managed files are never deleted.

    A cache root is intended to have one coordinating writer. Index publication
    uses sibling-file replacement, but separate instances do not merge concurrent
    in-memory updates. Windows may lose the reusable index if interrupted during
    replacement; caller-managed outputs remain untouched.
    """

    def __new__(cls, root: str, managed_root: str | None = None) -> FileStageCache:
        """
        Open an existing cache or create an empty in-memory cache view.

        ``managed_root`` defaults to a cache-owned directory below ``root``.
        Opening may remove abandoned cache-managed files, but never removes
        client-managed paths.
        """

    def lookup(self, request: StageCacheRequest) -> StageCacheHit | None:
        """
        Resolve an exact reusable invocation using its recorded file policy.

        Returns ``None`` when the request differs, the file is unavailable, or
        verification detects changed bytes. Metadata verification trusts equal
        size and modification time and otherwise falls back to SHA-256.
        """

    def previous_metadata_json(self, request: StageCacheRequest) -> str:
        """
        Return metadata from the latest record in the logical slot.

        This ignores behavior, inputs, context, and artifact validity so callers
        can discover dependencies before forming an exact request. It is not a
        cache-hit decision.
        """

    def recorded_content_identity(self, request: StageCacheRequest) -> str | None:
        """
        Return the recorded byte identity for an exact request without checking its file.

        This is a planning API. Use ``lookup`` before consuming artifact bytes.
        """

    def allocate_managed(self, request: StageCacheRequest, extension: str, verification: str) -> FileArtifactTarget:
        """
        Allocate a unique cache-managed target below the managed-artifact root.

        The method selects a path but does not create or write the file.
        ``extension`` must be a simple extension without separators or a leading
        dot.
        """

    def register(
        self,
        request: StageCacheRequest,
        target: FileArtifactTarget,
        metadata_json: str | None = None,
    ) -> StageCacheHit:
        """
        Register a complete file and replace the request's logical slot.

        Registration hashes stored bytes and captures file metadata. It updates
        only the in-memory index; call ``save`` to publish it. Optional metadata
        must be a JSON object and does not participate in request identity.
        """

    def save(self) -> None:
        """
        Persist the index through sibling replacement and collect unreferenced managed artifacts.

        This does not merge changes from another cache instance using the same
        root.
        """
