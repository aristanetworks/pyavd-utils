# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

from pathlib import Path

import pytest

from pyavd_utils.schema_store import compile_schema_archive, init_store_from_file

ADV_SCHEMA_URL = "https://github.com/aristanetworks/avd/releases/download/v6.0.0-dev3/schemas.json.gz"


@pytest.fixture(scope="session")
def init_store(tmp_path_factory: pytest.TempPathFactory) -> None:
    from urllib.request import urlretrieve

    filename = Path(ADV_SCHEMA_URL).name
    schema_dir = tmp_path_factory.mktemp("schema_store")
    source_file = schema_dir / filename
    archive_file = schema_dir / "schemas.rkyv"
    urlretrieve(ADV_SCHEMA_URL, source_file)
    compile_schema_archive(source_file, archive_file)

    init_store_from_file(archive_file)
