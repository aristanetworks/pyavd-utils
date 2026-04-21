<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# yaml-parser

`yaml-parser` is a YAML 1.2 parser for tooling and configuration workloads.
Its public surface is centered around:

- `parse` for an owned AST with spans, comments, anchors, and tags
- `emit_events` for event-stream consumers
- `writer::write_yaml_from_events` for YAML output from events
- `yaml_parser::serde` for optional serde integration

The authoritative user-facing API documentation lives in rustdoc on the public
items, especially:

- `src/lib.rs` for crate-level behavior and API selection guidance
- `src/value.rs` for the AST types
- `src/event.rs` for the event model
- `src/span.rs` for span and source-location types
- `src/writer.rs` for event-to-YAML output behavior
- `src/serde/mod.rs` for serde entry points

Implementation details are documented separately in [`ARCHITECTURE.md`](ARCHITECTURE.md).
