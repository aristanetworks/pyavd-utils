<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Fragment inventories

`Inventory<YamlFragment>` and `Inventory<JsonFragment>` collect encoded variable
fragments for named devices. `add_fragment` decodes the supplied string immediately;
callers do not invoke a parser. The generic selects one input format and merge
backend for the complete inventory, rejecting mixed formats at compile time.

Each fragment supplies its text, an opaque source identifier, and a device list.
YAML must contain exactly one mapping document and no parser errors. JSON must
contain one object. Rejected insertions leave the inventory unchanged. Source
identifiers need not be unique. YAML nodes retain their source and spans; JSON
values do not have node-level sources or spans. The JSON fragment source remains
available for decoding and merge failures. Decoded values are owned, so the input
text is dropped after insertion.

## Collection and merge graph

Insertion records the device histories directly as a merge graph. For each
fragment, devices are grouped by their current head; one successor is created
per distinct parent and references the single decoded fragment. Device heads
move to those successors. Repeated device names within an insertion count once.
An empty device list affects no devices, including devices added later.

This shares identical ordered prefixes without comparing fragment contents or
deduplicating independent sources. Applying one fragment after different
histories creates different merge steps. Later insertions have higher precedence
under the default merge options. Shallow/deep modes, list strategies, and conflict
handling are provided by `schema-merge`.

## Build phases

`build(self, store, options) -> Result<(), BuildError>` consumes collection and
privately seals the inputs, merges, and validates. All devices use the schema,
merge settings, and validation settings selected by `BuildOptions`.

Merge steps execute once in dependency order. Expanded containers use the
merger's Arc sharing when histories branch. Intermediate roots are released
after their final child unless a device still references them; the final child
can consume an otherwise unused parent to preserve unique container ownership.
Only distinct terminal roots proceed to validation. A device may reference an
internal graph node while other devices continue into more specific fragments.

The sealed inputs own the parsed values. Merged and validated roots borrow those
inputs and stay local to the build function. Validation consumes the merged
wrappers and produces coerced `MergedValue` roots plus feedback. Coerced output
is enabled internally even if disabled in the supplied validation configuration.
The temporary public build drops these products and returns only `Ok(())`.
Validation findings remain feedback rather than becoming build failures; this
entrypoint does not expose them yet. Schema selection and merge/execution failures
are returned as build errors.

Publication, arena construction, rkyv/mmap output, Python bindings, parallel
execution, and validation caching across distinct roots are outside this crate's
current scope. Data-dependent dynamic-key merging retains the merger's existing
unsupported status. Ordinary validation can resolve dynamic definitions from an
assembled device root, but the preliminary merge needed for complete dynamic-key
merge semantics is not implemented.

## Verification

```sh
cargo test --locked -p avd-inventory
cargo clippy --locked -p avd-inventory --all-targets -- -D warnings
```

Tests exercise incremental graph sharing, divergent histories, per-device merge
equivalence in both formats, shared Arc subtrees, input rejection, coercion and
feedback provenance, and the borrowing boundary. Doctests verify the public build
and reject mixed formats and additions after build at compile time.
