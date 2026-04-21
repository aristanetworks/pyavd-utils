<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Test Suite Fixture

This directory is intentionally not a git submodule.

The `rust/yaml-parser/tests/test_suite.rs` integration tests require the
official YAML test suite to be downloaded into this directory before running.

From the repository root, run:

```bash
make rust-yaml-test-suite
```

CI installs the same pinned fixture by running:

```bash
python scripts/fetch_yaml_test_suite.py
```
