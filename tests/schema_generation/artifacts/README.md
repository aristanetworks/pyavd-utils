<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Schema generation fixture

`schemas.json` is a reduced schema store copied from the AVD schemas. It keeps
the resolved accounting-methods reference, the retained cross-schema
authentication model reference, and the representative `interface_profiles`
model while remaining small enough to make generated-source diffs useful
during the generator experiment.

The regeneration test deliberately writes `schemas.rkyv` and
`schema_generation_fixture.py.expected` back into this directory, including
the same Ruff formatting step used by AVD. A changed generator therefore leaves
the proposed output visible in `git diff` when the byte-for-byte comparison
fails. The `.expected` suffix keeps the generated oracle out of Python source
discovery because `pyavd` is intentionally not a dependency of pyavd-utils.
