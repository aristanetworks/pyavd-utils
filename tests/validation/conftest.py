# Copyright (c) 2025 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

from pathlib import Path

import pytest

from pyavd_utils.validation import init_store_from_file

ADV_SCHEMA_URL = "https://github.com/aristanetworks/avd/releases/download/v6.0.0-dev3/schemas.json.gz"


@pytest.fixture(scope="package")
def init_store() -> None:
    from urllib.request import urlretrieve

    filename = Path(ADV_SCHEMA_URL).name
    tmp_file = Path(__file__).parent.joinpath("tmp", filename)
    urlretrieve(ADV_SCHEMA_URL, tmp_file)  # noqa: S310

    init_store_from_file(tmp_file)
