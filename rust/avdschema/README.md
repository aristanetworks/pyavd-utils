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
