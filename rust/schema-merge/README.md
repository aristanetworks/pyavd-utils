<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Schema-aware merging

`schema-merge` combines owned `serde_json::Value` trees or YAML `Node` trees through
`MergeableValue`, `MergeableEntry`, and `MergeableItem`. The merge engine does not
convert YAML to JSON. This crate is the merger layer for a future fragment inventory;
fragment/device selection and Python bindings are separate work.

```rust
use avdschema::Store;
use schema_merge::{merge, ListMerge, MergeOptions, StoreSchema};
use yaml_parser::parse_with_source;

let store = Store::from_json(r#"{"eos_config":{"type":"dict","keys":{
    "interfaces":{"type":"list","primary_key":"name","items":{"type":"dict"}}
}}}"#).expect("schema should compile");
let (mut base, base_errors) = parse_with_source(
    "interfaces: [{name: Ethernet1, mtu: 1500}]", Some("fixture-a".into()),
);
let (mut next, next_errors) = parse_with_source(
    "interfaces: [{name: Ethernet1, description: uplink}]", Some("fixture-b".into()),
);
assert!(base_errors.is_empty() && next_errors.is_empty());
let merged = merge(
    base.remove(0),
    [next.remove(0)],
    &StoreSchema::new(&store, "eos_config").expect("schema should exist"),
    MergeOptions { list_merge: ListMerge::AppendUnique, ..MergeOptions::default() },
).expect("fragments should merge");
// The merged interface has mtu from fixture-a and description from fixture-b.
```

Use `NoSchema` for ordinary list merging. `StoreSchema` delegates primary-key lookup
to `Store::get_schema_info`, the helper used by AVD's Python merger. `MergeSchema`
allows another metadata provider without coupling the algorithm to schema views.

## Behavior

- `MergeMode::Shallow` updates root keys like pipeline `dict.update()`: each incoming
  value replaces the complete previous value. Non-mapping roots are replaced.
- `MergeMode::Deep` recursively combines dictionaries. Scalars and type changes
  follow `SameKeyStrategy::{Override, UseExisting, MustMatch}`. `UseExisting` retains
  explicit null values as well.
- `ListMerge::Replace` replaces the complete list and bypasses primary keys.
- Other list strategies first merge matching schema primary keys recursively,
  retaining the base item's position. Remaining incoming items follow the list strategy.
  This also applies to `Keep`, matching AVD Python behavior.
- `Append` / `Prepend` retain all unmatched items. `AppendUnique` (`append_rp`)
  appends incoming items absent from the base. `PrependUnique` (`prepend_rp`)
  prepends incoming items and removes equivalent base items. Duplicates within
  the incoming list remain, matching Python's strategies.
- Present null primary keys match; missing primary keys use the ordinary list strategy.
  Incoming items match only the original base list, preserving unmatched duplicate
  primary keys for validation. `allow_duplicate_primary_key` does not disable merging,
  matching AVD's Python merger.
- Values move into the result. The public API consumes its inputs and returns the
  resulting root, including scalar replacements and root lists. Callers can clone
  inputs explicitly when they need to retain them. Incoming values are cloned only
  when matching multiple invalid duplicate primary keys in the base.

## Provenance and equality

A full replacement uses the incoming node's metadata. A partially merged dictionary
or list retains the base container's original source and span; its children keep
metadata from the nodes they came from. The container span therefore describes its
original declaration, not a combined range across files.

Incoming mapping keys and their structural pair metadata are retained when the key
already exists, independently of the merged value's metadata. Matched primary-key
list items retain the base item's structural metadata; appended/prepended items
retain their incoming item metadata. Spans, shared `Arc<str>` source identifiers,
properties, and comments survive without reconstruction from serialized values.
Duplicate YAML mapping keys remain visible for validation. Non-string YAML keys
are retained and compared by value, but have no schema path.

Metadata-free identities support primary-key indexes and unique-list strategies.
Dictionary order is ignored; list order is significant. Numeric identities support
Python equality between booleans, integers, and integral floats without rounding
large integers through floating point. Identities allocate only for indexed or
compared values; there is no root snapshot or document-wide prewalk.

Unlike the Python implementation, `PrependUnique` also handles dictionary/list items,
and invalid duplicate primary keys in the base do not trigger Python's removal-index
error. Semantic comparison verifies complete values rather than relying on
`deepmerge`'s hash-only comparisons. YAML non-finite floats use their bit representation
for identity.

## Limitations

Data-dependent dynamic-key schemas are unsupported. Dynamic-key definitions are not
pre-merged or resolved from fragment inputs; schema lookup retains the existing helper's
empty-input behavior. Unknown paths use ordinary list merging. A later change can merge
only the dynamic-key definition fields first, resolve schema paths, then merge the rest.

Merging does not coerce input types or validate inputs. Merge already-coerced values
when schema-driven type conversions should affect primary-key matching. Validation
can run directly on the resulting YAML tree to report each field's original source/span.
The crate does not install schema defaults or resolve YAML merge keys.

## Tests

```sh
cargo test --locked -p schema-merge
cargo bench --locked -p schema-merge --bench merge
```

Shared tests exercise both JSON and YAML backends. YAML tests additionally cover
source/span preservation, key and sequence metadata, shared source allocations,
semantic deduplication, invalid duplicate keys, and validation feedback after merging.
The nested ACL fixtures are copied from `aristanetworks/avd` at commit
`5af3f292dd6acba5bf1b1a42a6225c4d48f49353`
(`python-avd/tests/pyavd/utils/merge`, Apache-2.0).
