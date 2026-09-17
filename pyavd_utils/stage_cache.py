# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""File-backed cache primitives for deterministic processing stages."""

from __future__ import annotations

# The native Rust module is not built in CI, so this suppression is required there.
from ._bindings import _stage_cache  # pyright: ignore[reportMissingModuleSource]

CacheContext = _stage_cache.CacheContext
FileArtifactHandle = _stage_cache.FileArtifactHandle
FileArtifactTarget = _stage_cache.FileArtifactTarget
FileStageCache = _stage_cache.FileStageCache
StageCacheHit = _stage_cache.StageCacheHit
StageCacheRequest = _stage_cache.StageCacheRequest

__all__ = [
    "CacheContext",
    "FileArtifactHandle",
    "FileArtifactTarget",
    "FileStageCache",
    "StageCacheHit",
    "StageCacheRequest",
]
