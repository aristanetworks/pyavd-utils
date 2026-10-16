<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Schema generation fixture

The source schemas are in [`../../schemas/schemas.json`](../../schemas/schemas.json),
shared with the Rust and Python validation and schema-store tests. The generation
schemas retain the accounting-methods definition reference, cross-schema
authentication model reference, and representative `interface_profiles` model.

The regeneration test deliberately writes `schemas.rkyv` and
`schema_generation_fixture.py.expected` back into this directory, including
the same Ruff formatting step used by AVD. A changed generator therefore leaves
the proposed output visible in `git diff` when the byte-for-byte comparison
fails. The `.expected` suffix keeps the generated oracle out of Python source
discovery because `pyavd` is intentionally not a dependency of pyavd-utils.

## Schema Explorer

`schema_explorer_fixture.json` exercises occurrence-based JSONL export: reference
overrides, hidden dictionaries and reference links, list primary keys, dictionary
item tables, defaults, deprecation, and dynamic-key paths. Tests inspect the JSONL
contract directly; SQLite persistence and browser integration belong to AVD.
