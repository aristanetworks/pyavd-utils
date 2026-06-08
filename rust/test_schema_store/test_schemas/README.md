<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Test Schemas

This directory contains the source YAML files for the `test_schema_store` crate.
Each `*.schema.yml` file becomes one schema in the store, using the file name
without `.schema.yml` as the schema name.

The compressed release artifact is built from these files:

```bash
cargo run --locked -p test_schema_store --bin build_test_schemas -- --output dist/test-schemas.json.gz
```

The crate builds an equivalent local `test-schemas.json.gz` from these same YAML
files, so other repositories can depend on the crate without running Python
schema-store build logic.

The installed-wheel tests consume this file through the `TEST_SCHEMA_STORE_FILE`
environment variable. Local source-tree pytest runs fall back to invoking the
Rust binary above.
