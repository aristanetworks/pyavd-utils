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

Each fragment supplies its text, an opaque source identifier, a device list, and
`MergeOptions`. These options apply only when merging that incoming fragment,
including its recursive merges. Shallow updates, deep list append, and deep list
replacement can be mixed in the same inventory.
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

`build_to_json(self, store, options) -> Result<BuildOutput, BuildError>` consumes collection
and privately seals the inputs, merges, and validates. All devices use the schema
and validation settings selected by `BuildOptions`; merge behavior belongs to each
incoming fragment.

Merge steps execute once in dependency order. Expanded containers use the
merger's Arc sharing when histories branch. Intermediate roots are released
after their final child unless a device still references them; the final child
can consume an otherwise unused parent to preserve unique container ownership.
Only distinct terminal roots proceed to validation. A device may reference an
internal graph node while other devices continue into more specific fragments.

The sealed inputs own the parsed values. Merged and coerced roots borrow those
inputs and stay local to the build function. Validation and JSON encoding run in
parallel over distinct terminal roots, with one build-local Rayon pool.
`BuildOptions::validation_threads` accepts a nonzero worker limit, matching the
Python service's validation concurrency setting. The default uses available
parallelism; the pool never exceeds the number of distinct roots. A limit of one
runs on the calling thread. Results retain device insertion order regardless of
scheduling. There is no subtree validation cache.

`BuildOutput::devices` maps each device to an `Arc<DeviceOutput>` containing
validation errors, warnings and infos, plus `validated_data: Option<String>`.
Identical histories share their validation and JSON result. Invalid roots have
no JSON output. Call `BuildOutput::has_errors()` to gate the common build stage:
any device's validation errors prevent the caller from continuing the build.
Findings remain structured data; schema, merge, worker-pool and encoding failures
are returned as build errors.

The returned JSON and diagnostics own their data and outlive all parsed inputs.
YAML diagnostic sources and byte spans still refer to the original uncompressed
fragment text, rather than the returned JSON. The JSON adapter walks the effective
merged/coerced tree directly without constructing an intermediate JSON value tree
or adding serialization to `MergedValue`. Arbitrary-size integers retain numeric
precision. Non-string YAML keys and non-finite floats cannot be represented by
this JSON boundary and produce explicit encoding errors.

A Python binding can release the GIL around the complete consuming build, letting
Rust perform the parallel validation, and feed returned JSON into the existing
Python loader. Python bindings, service integration, arena construction,
rkyv/mmap publication, and validation caching are separate work.
Data-dependent dynamic-key merging retains the merger's existing
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
feedback provenance, per-fragment merge options, parallel/serial equivalence,
worker limits, JSON numeric precision, shared output, and the owning result boundary. Doctests verify the public build
and reject mixed formats and additions after build at compile time.
