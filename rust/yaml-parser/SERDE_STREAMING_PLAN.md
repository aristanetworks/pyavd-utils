<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Parser: Serde / Streaming Plan (Temporary)

Status: **Draft / exploratory**
Last updated: 2026-03-12

This file captures the current rough plan for integrating `serde` support with the `yaml-parser` crate and moving toward a streaming deserializer design. It is intentionally temporary and may change as we learn more.

---

## Goals

1. Replace `serde_yaml` usage in the workspace with functionality built on `yaml-parser`.
2. Provide a `serde`-compatible deserializer API from `yaml-parser` (e.g. `from_str`, `from_reader`, `Deserializer`).
3. Move toward a design where the parser can act as a *streaming* source of values/events for `serde`, not just as an AST builder.

---

## Step 1 – Make `Parser` a streaming consumer of events

**Objective:** Decouple `parser::Parser` from a fixed `&[Event] + pos` model so it can consume events from any iterator-like source (e.g., directly from the `Emitter`), while keeping behaviour identical.

High-level tasks:

- Change `Parser` to consume events via a small streaming abstraction instead of indexing into a slice:
  - Today: `Parser` stores `&[Event<'input>]` and `pos: usize` and uses `peek()`/`advance()` on that slice.
  - Target: `Parser` stores an event *source* (likely an iterator with internal lookahead) plus a simple `events_consumed` counter used for progress checks.
- Update all internal methods (`parse`, `parse_node`, `parse_mapping`, `parse_sequence`, etc.) to use the new streaming interface while preserving semantics (including error recovery and span behaviour).
- Keep a backwards-compatible constructor for the common case used today:
  - `Parser::new(events: impl Iterator<Item = Event<'input>>)` or a thin wrapper that lets us pass `events.into_iter()` from the current `emit_events` output.
- Update call sites:
  - `lib.rs::parse`: construct `Parser` from `events.into_iter()`.
  - Tests in `parser::tests` that currently construct `Parser::new(&events)`.

This step **does not** change the public `emit_events` API or introduce serde yet; it only makes the internal parser more flexible and ready for streaming.

---

## Step 2 – Serde integration on top of the AST (non-streaming)

**Objective:** Implement a `serde` backend that operates on the final AST (`Node` / `Value`) produced by `parse`, as a low-risk replacement for `serde_yaml`.

High-level idea:

- Add a feature-gated `serde` module inside `yaml-parser` (e.g., `src/serde/mod.rs`, `src/serde/de.rs`) behind a `serde` feature in `Cargo.toml`.
- Implement `serde::de::Deserializer<'de>` backed by `Node<'de>` / `Value<'de>`:
  - Map `Value::Null/Bool/Int/Float/String/Sequence/Mapping` to the corresponding serde visitor methods.
  - Provide helpers like `from_str<T: DeserializeOwned>` and `from_reader<T: DeserializeOwned>` that:
    - Call `parse` internally.
    - Convert parse errors into an appropriate `serde::de::Error` type.
- Use this new API to replace `serde_yaml::from_str` / `from_reader` call sites in:
  - `rust/validation/src/validation/store.rs`.
  - `rust/avdschema/src/utils/load.rs`.

This gives us a working serde-backed YAML pipeline without changing the core parser logic.

---

## Step 3 – True streaming serde deserializer (optional / future)

**Objective:** If needed, evolve `yaml-parser` into a *true streaming* serde backend that does not materialize the full AST.

Possible direction:

- Factor the `Parser` so that its structural logic (handling mappings, sequences, scalars, anchors, tags, error recovery) is separated from *value construction*.
- Introduce a `ValueConsumer`-like abstraction that can either:
  - Build `Node` / `Value` (current behaviour), or
  - Drive serde visitors (`Visitor`, `SeqAccess`, `MapAccess`) directly.
- Implement a serde-oriented consumer and a `YamlDeserializer` type that owns lexer + emitter + parser state and implements `serde::de::Deserializer<'de>`.

This is a significantly larger refactor and is not required to replace `serde_yaml` in this repository; it is considered a stretch goal.

---

## Step 4 – Optional I/O streaming (`Read` / `BufRead`)

**Objective:** Allow parsing directly from `Read` / `BufRead` with bounded memory usage.

Sketch:

- Introduce an internal buffering layer for the lexer so that tokens/events/values can continue to borrow from a stable buffer owned by the parser/deserializer.
- Carefully manage buffer growth and compaction so that spans and `Cow<'input, str>` values remain valid.

This step is only needed if the project requires truly large-input streaming; for current use (config-sized YAML), reading into memory first is acceptable.
