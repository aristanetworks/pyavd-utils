#!/usr/bin/env python
# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Download the pinned YAML test suite into rust/yaml-parser/tests/yaml-test-suite."""

from __future__ import annotations

import shutil
import sys
import tarfile
import tempfile
import urllib.request
from pathlib import Path

# TODO: Consider if we should always just pull the latest stable test suite.
PINNED_COMMIT = "6ad3d2c62885d82fc349026c136ef560838fdf3d"
ARCHIVE_URL = f"https://codeload.github.com/yaml/yaml-test-suite/tar.gz/{PINNED_COMMIT}"
REPO_ROOT = Path(__file__).resolve().parent.parent
TARGET_DIR = REPO_ROOT / "rust" / "yaml-parser" / "tests" / "yaml-test-suite"
PLACEHOLDERS = {"README.md", ".gitignore"}
# Upstream ships friendly alias trees as symlinks. The Rust tests already
# ignore them, and copying them breaks on Windows when the archive is unpacked
# without POSIX symlink semantics.
SKIPPED_TOP_LEVEL_DIRS = {"name", "tags"}


def clean_target_dir() -> None:
    TARGET_DIR.mkdir(parents=True, exist_ok=True)
    for child in TARGET_DIR.iterdir():
        if child.name in PLACEHOLDERS:
            continue
        if child.is_dir():
            shutil.rmtree(child)
        else:
            child.unlink()


def download_archive(destination: Path) -> None:
    # ARCHIVE_URL is a pinned HTTPS GitHub archive URL, so this controlled
    # use of urlopen is intentional here.
    with urllib.request.urlopen(ARCHIVE_URL) as response, destination.open("wb") as file:  # noqa: S310
        shutil.copyfileobj(response, file)


def extract_archive(archive_path: Path, extract_root: Path) -> Path:
    with tarfile.open(archive_path, "r:gz") as archive:
        archive.extractall(extract_root, filter="data")

    extracted_dirs = [path for path in extract_root.iterdir() if path.is_dir()]
    if len(extracted_dirs) != 1:
        msg = f"Expected one extracted directory in {extract_root}, got {len(extracted_dirs)}"
        raise RuntimeError(msg)
    return extracted_dirs[0]


def copy_suite_contents(source_dir: Path) -> None:
    for child in source_dir.iterdir():
        if child.name == ".git" or child.name in SKIPPED_TOP_LEVEL_DIRS:
            continue
        destination = TARGET_DIR / child.name
        if child.is_dir():
            shutil.copytree(child, destination)
        else:
            shutil.copy2(child, destination)


def verify_suite_present() -> None:
    if not any(path.is_dir() for path in TARGET_DIR.iterdir() if path.name not in PLACEHOLDERS):
        msg = f"No YAML test suite cases found under {TARGET_DIR}"
        raise RuntimeError(msg)


def main() -> None:
    sys.stdout.write(f"Downloading YAML test suite {PINNED_COMMIT} into {TARGET_DIR}\n")
    clean_target_dir()

    with tempfile.TemporaryDirectory() as temp_dir_name:
        temp_dir = Path(temp_dir_name)
        archive_path = temp_dir / "yaml-test-suite.tar.gz"
        download_archive(archive_path)
        extracted_root = extract_archive(archive_path, temp_dir)
        copy_suite_contents(extracted_root)

    verify_suite_present()
    sys.stdout.write("YAML test suite installed successfully.\n")


if __name__ == "__main__":
    main()
