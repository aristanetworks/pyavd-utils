<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Schema-aware merging

`schema-merge` combines JSON values and YAML nodes. `merge_borrowed` produces a
`MergedValue` borrowing immutable fragments; `merge` also supports consuming owned
trees through `MergeableValue`, `MergeableEntry`, and `MergeableItem`. Neither API
converts YAML to JSON. This crate is the merger layer for a future fragment inventory;
fragment/device selection and Python bindings are separate work.

```rust
use avdschema::Store;
use schema_merge::{merge_borrowed, ListMerge, MergeOptions, StoreSchema};
use validation::{ValidatableSequence, ValidatableSequenceItem, ValidatableValue};
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
let base = base.remove(0);
let next = next.remove(0);
let merged = merge_borrowed(
    &base,
    [&next],
    &StoreSchema::new(&store, "eos_config").expect("schema should exist"),
    MergeOptions { list_merge: ListMerge::AppendUnique, ..MergeOptions::default() },
).expect("fragments should merge");
// The merged interface borrows mtu from fixture-a and description from fixture-b.
let interfaces = merged.get("interfaces").expect("interfaces should exist");
let sequence = interfaces.as_sequence().expect("interfaces should be a sequence");
let interface = sequence.iter().next().expect("interface should exist").value();
assert_eq!(interface.get("mtu").expect("mtu should exist").as_i64(), Some(1500));
assert_eq!(
    interface.get("description").expect("description should exist").as_str().as_deref(),
    Some("uplink"),
);
```

Use `NoSchema` for ordinary list merging. `StoreSchema` delegates primary-key lookup
to `Store::get_schema_info`, the helper used by AVD's Python merger. `MergeSchema`
allows another metadata provider without coupling the algorithm to schema views.

Use `merge_inputs(base, inputs, schema)` when each incoming input has its own
behavior. Each `MergeInput { value, options }` applies its options to that merge
only, including recursive calls. The value may be an owned backend tree or a
`MergedValue` borrowing a parsed fragment. The existing `merge` and
`merge_borrowed` functions remain convenient for uniform options.

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
- `merge_borrowed` retains immutable inputs. `merge` consumes its inputs and returns
  the resulting root, including scalar replacements and root lists. Both use the
  same merge algorithm.

## Borrowed merge results

`MergedValue` starts as one reference to a fragment. Merging expands participating
mappings and lists into owned vectors of wrappers, with borrowed keys and children.
Untouched subtrees remain single references. A list replacement simply switches
references without traversing or cloning either list. Expanded containers and generated
scalars use `Arc` storage. Their vectors are reused when uniquely owned; merging into a
shared container first copies its immediate entry/item wrappers, while child storage
remains shared. Primary-key indexes are currently rebuilt for each incoming fragment,
and tolerate unvalidated duplicate keys.

The original fragments must outlive all results borrowing them. They can be reused
for many devices. Cloning a result shares its expanded containers and generated
scalars without traversing them, retaining references to original subtrees. Further
merges leave the original result unchanged. `merge` can continue merging
`MergedValue` inputs without materializing a backend tree. `from_scalar` supports
owned generated scalars; its private representation excludes owned backend containers
whose children could not satisfy the fragment lifetime.

`ValidatableValue`, mapping/sequence adapters, and schema-data adapters support direct
validation and navigation. Child views borrow existing wrappers or own a temporary
wrapper around an original node, using the validation child-view contract.
Validation preserves each key/value's individual source and span. Coerced output
is another `MergedValue`: unchanged fragment scalars and keys remain borrowed,
converted or lowercased scalars are owned, and rebuilt containers retain their
original provenance and item metadata. Sequence iteration yields
`ValidatableSequenceItem` views whose `coerced_item` builder carries the original
item metadata with its value, preserving the association through filtering and
reordering. The output borrows the fragments rather
than the temporary merge wrappers, so it can outlive those wrappers. Unknown and
invalid values preserved by validation keep their existing borrowed subtrees.

`clone_to_coerced` shares the merge representation, retaining original subtree
references without copying expanded containers or generated scalars. Validation currently
rebuilds schema-traversed container wrappers even when all their children are unchanged;
avoiding those rebuilds is a separate optimization. Direct JSON/YAML validation retains
its existing owned output types.

`MergedValue` does not implement serialization. Consumers access merged data through
the validation and schema-data views.

The borrowing API works with non-static YAML nodes. It does not change the parser's
current ownership of decoded strings or implement the future inventory's input-buffer
storage. Those can evolve independently of the merge result lifetime.

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

Shared tests exercise owned and borrowed JSON and YAML backends, including borrowed
coercion and data access through validation views. Pointer checks verify subtree
sharing, copy-on-write forks of expanded containers, shared generated scalars, and
whole-list replacement without expansion. YAML tests additionally cover
source/span preservation, key and sequence metadata, shared source allocations,
semantic deduplication, invalid duplicate keys, and validation feedback after merging.
The nested ACL fixtures are copied from `aristanetworks/avd` at commit
`5af3f292dd6acba5bf1b1a42a6225c4d48f49353`
(`python-avd/tests/pyavd/utils/merge`, Apache-2.0).
