<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# avdschema

AVD schema source models, compiled runtime views, and schema artifact generation.

## Metaschema generation and deprecations

The `metaschema` feature exposes `generate_metaschema_json` for generating the authoring
metaschema from the Rust source-schema models.

The generated authoring metaschema marks `convert_types` as deprecated for integer and string
schemas using `"deprecated": true` and a description. Existing definitions remain valid, and
runtime conversion behavior is unchanged.

[JSON Schema deprecation](https://json-schema.org/understanding-json-schema/reference/annotations)
is an annotation standardized in Draft 2019-09, rather than a validation error. The generated
metaschema uses Draft 2020-12, with reusable definitions under `$defs`.
Editor warnings depend on support for the deprecation annotation. JSON Schema does not define replacement
fields or removal versions; those details belong in the description and release documentation.

Consumers should enable format validation explicitly to retain checks such as the syntax of
regular expressions declared with `format: "regex"`.

To deprecate another authoring property, add `#[schemars(extend("deprecated" = true))]` and a
descriptive doc comment to its schema field, gated by the `metaschema` feature when necessary.
For `convert_types`, the annotations belong on the schema-only `IntConvertTypes` and
`StrConvertTypes` fields. This keeps authoring deprecations separate from Rust API deprecations.

## Build-time authoring checks

These authoring checks target AVD 6.x source schemas. Compatibility with all older source
schema versions is not guaranteed. No new `convert_types` validation is introduced here.

With the `generation` feature, `StoreSource::validate_authoring()` checks every authored node,
including unused `$defs` and declarations overridden by references. The archive generator
`pyavd_utils_gen.schema_store.compile_schema_archive` runs this pass before compiling or
replacing its output. Invalid source data leaves any existing archive untouched.

The pass checks regex syntax with the same AVD dialect used at runtime, nonempty single-line
`display_name` values, nonempty descriptions, schema key names, `primary_key` and `unique_keys`
names. Static keys use snake case, with the existing `Vxlan1` and `MIB_family_name`
exceptions retained. Removing `MIB_family_name` is deferred pending a scope decision.

These are checks on authored declarations. Reference resolution and type compatibility are
still checked during compilation. This pass does not implement all JSON Schema constraints;
for example, optional `null` values have already been deserialized as absent fields.
Deserialization and direct runtime compilation retain their tolerance for historical source
schemas. Other native build tools can call `validate_authoring()` explicitly.
