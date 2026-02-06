# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""
Custom PEP 517 build backend that generates third-party licenses before building wheels.

This module wraps setuptools.build_meta and runs cargo-about to generate the
THIRD_PARTY_LICENSES.txt file before building distribution packages.

License generation is only performed for wheel builds (build_wheel), not for
editable installs, since licenses are only required for redistribution.
"""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path
from typing import Any

# Re-export all standard hooks from setuptools.build_meta
from setuptools.build_meta import (
    build_editable,
    build_sdist,
    get_requires_for_build_editable,
    get_requires_for_build_sdist,
    get_requires_for_build_wheel,
    prepare_metadata_for_build_editable,
    prepare_metadata_for_build_wheel,
)
from setuptools.build_meta import build_wheel as _build_wheel

# Re-export for PEP 517 compliance
__all__ = [
    "build_editable",
    "build_sdist",
    "build_wheel",
    "get_requires_for_build_editable",
    "get_requires_for_build_sdist",
    "get_requires_for_build_wheel",
    "prepare_metadata_for_build_editable",
    "prepare_metadata_for_build_wheel",
]


def _generate_licenses() -> None:
    """
    Generate the third-party licenses file using cargo-about.

    This function:
    1. Checks if cargo-about is installed, installs it if not
    2. Runs cargo-about to generate the license file
    3. Places the output in pyavd_utils/THIRD_PARTY_LICENSES.txt
    """
    project_root = Path(__file__).parent.parent.resolve()
    output_path = project_root / "pyavd_utils" / "THIRD_PARTY_LICENSES.txt"
    template_path = project_root / "about-text.hbs"
    config_path = project_root / "about.toml"

    # Check if template and config exist
    if not template_path.exists():
        print(f"Warning: Template file not found at {template_path}, skipping license generation")
        return
    if not config_path.exists():
        print(f"Warning: Config file not found at {config_path}, skipping license generation")
        return

    # Check if cargo is available
    cargo_path = shutil.which("cargo")
    if cargo_path is None:
        print("Warning: cargo not found in PATH, skipping license generation")
        return

    # Check if cargo-about is installed
    cargo_about_path = shutil.which("cargo-about")
    if cargo_about_path is None:
        print("cargo-about not found, attempting to install...")
        try:
            subprocess.run(
                [cargo_path, "install", "--locked", "cargo-about"],
                check=True,
                capture_output=True,
                text=True,
            )
            print("cargo-about installed successfully")
        except subprocess.CalledProcessError as e:
            print(f"Warning: Failed to install cargo-about: {e.stderr}")
            print("Skipping license generation")
            return

    # Generate the license file
    print("Generating third-party licenses...")
    try:
        result = subprocess.run(
            [cargo_path, "about", "generate", "-o", str(output_path), str(template_path)],
            check=True,
            capture_output=True,
            text=True,
            cwd=project_root,
        )
        print(f"Third-party licenses generated at {output_path}")
        if result.stdout:
            print(result.stdout)
    except subprocess.CalledProcessError as e:
        print(f"Warning: Failed to generate licenses: {e.stderr}")
        print("Build will continue without license file")


def build_wheel(
    wheel_directory: str,
    config_settings: dict[str, Any] | None = None,
    metadata_directory: str | None = None,
) -> str:
    """Build a wheel, generating third-party licenses first."""
    _generate_licenses()
    return _build_wheel(wheel_directory, config_settings, metadata_directory)
