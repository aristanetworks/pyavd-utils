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

---

## Serialization plan – events as the central layer

In addition to the deserialization/streaming work above, we want a serializer design that:

1. Uses **events** as the primary representation for emitting YAML, so we can eventually preserve comments, descriptions, and spacing.
2. Can be tested independently by round-tripping YAML through the existing **lexer + emitter + new event-based serializer**.
3. Aligns with a future fully streaming serde deserializer that also operates directly on events.

The current plan for serialization is:

### Step 1a – Initial YAML serializer from events

**Objective:** Implement a first-cut YAML emitter that takes a complete `Vec<Event<'_>>` (as produced by `emit_events`) and writes YAML text. This version will be conservative and block-style only, but structurally correct.

High-level tasks:

- Add an internal module (e.g. `writer` or `event_writer`) that exposes a function like:
  - `write_yaml_from_events<W: Write>(writer: W, events: &[Event<'_>]) -> io::Result<()>`.
- Implement a simple, deterministic mapping from events to YAML:
  - Ignore most presentation details initially (e.g., always use block mappings/sequences, conservative quoting for scalars, skip tags/anchors or handle them minimally).
  - Ensure output YAML parses back to an equivalent event stream for *simple* documents (single document, scalar keys, scalar/collection values).
- Keep the implementation intentionally straightforward, even if it does not yet handle all YAML 1.2 edge cases.

### Step 1b – Tighten the event-based serializer using roundtrip testing

**Objective:** Strengthen the initial event-based YAML serializer so it forms a solid foundation before building serde serialization on top.

High-level tasks:

- Add tests that:
  - Take input YAML from selected YAML Test Suite cases.
  - Run it through `emit_events`.
  - Serialize those events back to YAML using the new writer.
  - Re-run `emit_events` on the serializer output.
  - Compare the resulting events to the original ones using a *normalized* comparison that ignores spans and, initially, some presentation details (e.g. flow vs block style), but checks structural equivalence (stream/doc/map/seq boundaries, scalar values, anchors/aliases where supported).
- Iterate on the writer to:
  - Handle more YAML constructs correctly (anchors, aliases, flow collections where feasible, multiline scalars, etc.).
  - Reduce the set of ignored differences in the normalized comparison.
- Only move on to serde serialization once the event-based writer behaves robustly across a representative subset of the YAML Test Suite.

### Step 2 – Value/AST to events serializer (serde-ready)

**Objective:** Provide a reusable conversion from `Node` / `Value` to events, so both serde serialization and other tooling can emit events without going back through the low-level emitter.

High-level tasks:

- Add a small module that can traverse `Node<'input>` / `Value<'input>` and emit a structurally equivalent sequence of events:
  - `Value::Null/Bool/Int/Float/String` → `Scalar` events.
  - `Value::Sequence` → `SequenceStart` / items / `SequenceEnd`.
  - `Value::Mapping` → `MappingStart` / (key, value) pairs / `MappingEnd`.
  - `Value::Alias` → `Alias`.
  - Node properties (anchors/tags) → event `Properties` when appropriate.
- Decide on behaviour for `Value::Invalid` when serializing (likely an error surfaced at the serde layer).
- Ensure that running `parse` (AST) → value-to-events → event-based writer produces YAML that round-trips back into an equivalent AST for typical configuration inputs.

### Step 3 – Serde serializer via Value → events → YAML

**Objective:** Implement a `serde` serializer that reuses the Value → events converter and the event-based YAML writer, so serde-based code can emit YAML without depending on `serde_yaml`.

High-level tasks:

- Add a `serde` serialization module with:
  - A `SerError` type (mirroring `DeError` style: specific variants and `derive_more::Display`).
  - A `ValueSerializer` implementing `serde::Serializer` that builds a `Value<'static>` (or `Node<'static>`) from arbitrary `T: Serialize`.
  - Public helpers like:
    - `to_writer<W: Write, T: Serialize>(writer: W, value: &T) -> Result<(), SerError>`
    - `to_string<T: Serialize>(value: &T) -> Result<String, SerError>`
- Wire `to_writer` / `to_string` as:
  - `T: Serialize` → `ValueSerializer` → `Value<'static>` → Value → events → event-based YAML writer.
- Once stable, switch remaining `serde_yaml::to_writer` call sites (e.g. in `avdschema::utils::dump`) to use `yaml-parser`’s serde-based serializer instead, and then remove `serde_yaml` as a dumping dependency.
