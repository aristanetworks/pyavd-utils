# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Tests for the Python-facing file stage cache."""

from pathlib import Path

from pyavd_utils.stage_cache import CacheContext, FileArtifactTarget, FileStageCache, StageCacheRequest


def test_cache_reuses_managed_artifact_without_moving_payload(tmp_path: Path) -> None:
    """Registration should return reusable file handles without payload copies."""
    cache = FileStageCache(str(tmp_path / "cache"))
    context = CacheContext({"renderer.package": "sha256:wheel"})
    request = StageCacheRequest(
        stage="compile",
        entry_key="documents/report",
        behavior="compile.v1",
        inputs={"source": "sha256:input"},
        context=context,
    )

    target = cache.allocate_managed(request, "json", "trust_registered")
    target.path.write_text('{"result":true}', encoding="utf-8")
    registered = cache.register(request, target, '{"dependencies":["source"]}')
    cache.save()

    assert request.entry_key == "documents/report"
    assert registered.artifact.path == target.path
    assert registered.artifact.retention == "cache_managed"
    assert registered.metadata_json == '{"dependencies":["source"]}'
    reused = cache.lookup(request)
    assert reused is not None
    assert reused.artifact.path == registered.artifact.path
    assert reused.artifact.content_identity == registered.artifact.content_identity


def test_client_artifact_uses_metadata_then_byte_verification(tmp_path: Path) -> None:
    """Changed client-owned bytes should invalidate an otherwise exact request."""
    cache = FileStageCache(str(tmp_path / "cache"))
    request = StageCacheRequest(
        "render",
        "documents/report",
        "render.v1",
        {"document": "sha256:document"},
        CacheContext({}),
    )
    output = tmp_path / "report.txt"
    output.write_text("first result\n", encoding="utf-8")

    cache.register(
        request,
        FileArtifactTarget(str(output), "client_managed", "verify_metadata_then_bytes"),
    )
    assert cache.lookup(request) is not None

    output.write_text("changed result\n", encoding="utf-8")
    assert cache.lookup(request) is None
