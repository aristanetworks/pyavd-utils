# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

from pyavd_utils.validation import init_store_from_file


@pytest.fixture(scope="package")
def init_store() -> None:
    if schema_store_file := os.environ.get("TEST_SCHEMA_STORE_FILE"):
        init_store_from_file(Path(schema_store_file))
        return

    repo_root = Path(__file__).parents[2]
    tmp_file = Path(__file__).parent.joinpath("tmp", "test-schemas.json.gz")
    write_test_schema_store_with_cargo(repo_root, tmp_file)

    init_store_from_file(tmp_file)


def write_test_schema_store_with_cargo(repo_root: Path, output_file: Path) -> None:
    cargo = shutil.which("cargo")
    if cargo is None:
        msg = "cargo is required to build the local test schema store. Set TEST_SCHEMA_STORE_FILE to use an existing store file."
        raise RuntimeError(msg)

    output_file.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(  # noqa: S603
        [
            cargo,
            "run",
            "--locked",
            "-p",
            "test_schema_store",
            "--bin",
            "build_test_schemas",
            "--",
            "--output",
            str(output_file),
        ],
        check=True,
        cwd=repo_root,
    )
