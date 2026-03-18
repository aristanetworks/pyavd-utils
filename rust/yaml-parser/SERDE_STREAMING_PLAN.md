<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Parser: Serde / Streaming Plan

Status: **Design / implementation guide**
Last updated: 2026-03-17

This document describes how serde support in `yaml-parser` is structured today and
lays out a detailed plan for making everything **from the input string onward**
fully streaming (no mandatory `Vec<Event>` / `Vec<Node>` in the serde path).

We intentionally keep the current "read the whole input into a `String`" model
for now. Large-input `Read` / `BufRead` streaming can be revisited separately.

---

## 1. Current state (serde + parser)

**Parsing pipeline:**

- `Emitter<'input>` is a streaming `Iterator<Item = Event<'input>>` backed by the
  lexer. It pulls tokens on demand and produces events lazily.
- `Parser<'input, I>` operates on any `Iterator<Item = Event<'input>>`:

  - Holds `events: I`, `peeked: Option<Event<'input>>`, `events_consumed: usize`.
  - Implements `parse()` / `parse_next_document()` in terms of `peek`/`advance`
    over the iterator.

- Public `parse(&str)` builds a streaming emitter and parser directly:

  ```text
  &str -> Emitter<'input> (Iterator) -> Parser<Emitter<'input>> -> Vec<Node<'input>>
  ```

**Serde deserialization today:**

- `yaml_parser::serde::from_str<T>` in `src/serde/de.rs`:

  - Calls `parse(input)` to build a full `Vec<Node<'input>>` (AST).
  - Rejects parse errors and multi-document inputs.
  - Builds an `AnchorIndex<'de>` by walking the AST once.
  - Wraps the root `Value` in `ValueDeserializer` and drives serde visitors.

**Serde serialization today:**

- `yaml_parser::serde::to_string` / `to_writer`:

  - `T: Serialize` -> `ValueSerializer` -> `Value<'static>`.
  - `Value<'static>` -> events via `ast_events::node_to_events`.
  - Events -> YAML text via the event-based writer.

All of this is well-tested and in use; the missing piece is a fully streaming
serde deserializer built on the event iterator + parser.

---

## 2. Goals for Step 3 (streaming serde)

Within the constraint that we still read the entire YAML into a `String`:

1. **Eliminate mandatory `Vec<Event>`** – events are produced lazily via an
   iterator, not collected up front.
2. **Enable serde without mandatory `Vec<Node>`** – serde should be able to
   consume values directly from the parser's structural engine instead of
   always materializing a full AST per document.
3. **Preserve behaviour** – error recovery, spans, anchors/aliases, tags, and
   YAML test suite compatibility must stay intact.
4. **Incremental refactor** – keep existing `parse()` / `from_str()` working
   while we introduce streaming APIs and then migrate them to the new engine.

---

## 3. Phase 3.0 – EventStream: streaming events from a `&str`

**Objective:** Stop requiring `Vec<Event>` between the emitter and parser.

**Implementation status (2026-03-16):** Implemented. In the current codebase,
`Emitter<'input>` itself is the streaming `Iterator<Item = Event<'input>>`
backed by a `TokenCursor<'input>` over `Lexer<'input>`. Wherever this document
mentions `EventStream<'input>`, you can read it as "the streaming
`Emitter<'input>`".

### Tasks (original design, conceptually satisfied)

1. Ensure the emitter is exposed as a streaming iterator over `Event<'input>`:

   ```rust
   pub struct Emitter<'input> { /* emitter state */ }

   impl<'input> Iterator for Emitter<'input> {
       type Item = Event<'input>;
       fn next(&mut self) -> Option<Self::Item> { /* drive emitter */ }
   }
   ```

2. Change the internal implementation of `parse(&str)` to build a parser
   directly over the streaming emitter:

   ```rust
   pub fn parse(input: &str) -> (Vec<Node>, Vec<ParseError>) {
       let mut emitter = Emitter::new(input);
       let mut parser = Parser::new(&mut emitter);
       let docs = parser.parse();
       let mut errors = parser.take_errors();
       errors.extend(emitter.take_errors());
       (docs, errors)
   }
   ```

**Result:** From the moment we have an input `&str`, events are streamed from
the emitter into the parser. We still build a full AST, but `Vec<Event>` goes
away.

---

## 4. Phase 3.1 – Document-level streaming serde API (merged with 3.3)

**Objective:** Provide a streaming API over *documents* while still reusing the
existing AST-backed serde machinery per document (Option B), and do so on top of
the fully streaming emitter+parser pipeline.

**Implementation status (2026-03-17):** Implemented and merged with the
original Phase 3.3 design. Instead of introducing a separate
`StreamingDeserializer<'de, I>` with a bespoke consumer type, we:

- Added `Parser::parse_next_document` that parses a single document at a time
  from any `Iterator<Item = Event<'input>>`. Anchors are tracked internally in
  the parser and are **cleared between documents**, so alias validation is
  scoped per document.
- Introduced a streaming `StreamDeserializer<'de, T>` in `serde::de` that owns
  an `Emitter<'de>` and, on each `next()`, constructs a `Parser` over the
  emitter, parses the next document via `parse_next_document`, converts the
  resulting `Node<'de>` to `Node<'static>`, collects anchors, and then defers to
  the existing `ValueDeserializer`.
- Reimplemented single-document `from_str<T>(input: &str)` to use the same
  streaming pipeline (Emitter + Parser + `parse_next_document`) under the hood,
  while preserving its existing semantics (error if zero or multiple
  documents).
- Reimplemented `stream_from_str_docs<T>` to return the new
  `StreamDeserializer<'de, T>` instead of building a `Vec<Node>` up front.

### Public shape (as implemented)

```rust
pub fn from_str<T>(input: &str) -> Result<T, DeError>
where
    T: DeserializeOwned;

pub struct StreamDeserializer<'de, T> { /* owns Emitter<'de> + ParserContext */ }

impl<'de, T> Iterator for StreamDeserializer<'de, T>
where
    T: DeserializeOwned,
{
    type Item = Result<T, DeError>;
    fn next(&mut self) -> Option<Self::Item> { /* parse_next_document + serde */ }
}

pub fn stream_from_str_docs<'de, T>(
    input: &'de str,
) -> Result<StreamDeserializer<'de, T>, DeError>
where
    T: DeserializeOwned;
```

**Result:** Users can stream multiple documents from a single input string as
`T` values without ever constructing a `Vec<Node>` for all documents. For the
single-document case, the primary `from_str` entrypoint now also uses the
streaming emitter+parser pipeline under the hood instead of going through the
`parse()`/`Vec<Node>` API. Internally we still build a `Node` per document to
preserve existing serde semantics and anchor/alias behaviour, but the
emitter+parser pipeline itself is fully streaming and anchors are correctly
scoped per document.

---

## 5. Phase 3.2 – Consumer-based structural engine (experiment, rolled back)

**Objective (original):** Separate structural parsing (driving over `Event`s,
handling mappings/sequences/anchors/tags/errors) from "what to do with
values", so the same engine could either build an AST or drive serde visitors
via a generic `Consumer<'input>` trait.

**Status (2026-03-17):** Experimented with and then **rolled back**. A
`Consumer` / `ParserContext`-based parser was prototyped, but it turned out to
be an awkward fit for serde's pull-based `Visitor` / `DeserializeSeed` APIs and
added a lot of complexity without clear benefit. The main branch now uses the
simpler event-to-AST parser again, and this phase is kept here only as
historical context.

Key takeaways from the experiment:

- Sharing scalar type inference via callbacks was less important than keeping
  the parser simple and predictable.
- Serde wants to **pull** values of arbitrary types (`T`, `DeserializeSeed`),
  whereas the `Consumer` abstraction was **push-based** with a single monomorphic
  `Output` type.
- Mapping serde cleanly onto `Consumer` required additional layers of
  indirection (`MappingAccess`, `SequenceAccess`, custom contexts) that made the
  overall design harder to reason about and maintain.

Going forward, we instead plan to share scalar type inference logic via common
helpers/traits that can be reused both by the parser and by a future
event-based serde backend, without routing everything through a `Consumer`
callback layer.

---

## 6. Phase 3.3 – Direct events→serde backend (Option A)

**Objective:** Implement a fully streaming serde backend that consumes the
event stream (and a thin structural layer where needed) directly, without
building a general-purpose `Node` / `Value` AST for each document.

**Status (2026-03-18):** **COMPLETE.** The event-based backend is now the
default for all public serde APIs. The implementation includes full anchor/alias
support with optimized zero-copy event storage. The AST-backed implementation
is preserved as `from_str_ast_internal` for reference and testing.

Completed work for this phase:

1. **✅ Design an event-based serde deserializer**

   - Implemented `serde::event_de::EventStream<'de>` with
     `impl<'de> Deserializer<'de> for &mut EventStream<'de>`, handling scalars,
     sequences, mappings, options, and enums.
   - Added Criterion benchmarks comparing event-based vs AST-based approaches.
   - **Result:** Event-based backend is 10-30% faster than AST backend for most
     workloads.

2. **✅ Add streaming anchor/alias handling based on events**

   - Anchors are stored as event subsequences in `HashMap<String, Vec<Event<'de>>>`.
   - When an alias is encountered, the stored event sequence is replayed through
     the deserializer.
   - **Optimization:** Events are stored with `'de` lifetime (borrowed from input)
     rather than `'static` (owned), avoiding string content cloning. This
     eliminated ~10% overhead on anchor/alias workloads.
   - Anchor/alias logic is isolated in `EventStream`, making it transparent to
     all deserialize methods.

3. **✅ Migrate public serde APIs once parity is reached**

   - `from_str<T>` now calls `event_de::from_str_events_internal`.
   - `StreamDeserializer<'de, T>` is now a type alias for
     `event_de::EventStreamDeserializer<'de, T>`.
   - `stream_from_str_docs<T>` returns the event-based streaming iterator.
   - AST-based implementation preserved as `from_str_ast_internal` for reference.
   - **All 169 tests passing** with the event-based backend.

---

## 7. Phase 3.4 – API integration and future work (public surface)

**Objective:** Keep the public API surface small and coherent while the
internals evolve towards a fully streaming serde backend.

**Status (2026-03-18):** **COMPLETE.** The public API surface is clean and
coherent, with all serde APIs now using the event-based backend internally.

### Current public shape

- `yaml_parser::serde::from_str` — single-document entrypoint, implemented
  using the event-based backend (`event_de::from_str_events_internal`).
- `yaml_parser::serde::stream_from_str_docs` — multi-document streaming
  entrypoint returning `StreamDeserializer<'de, T>` (type alias for
  `event_de::EventStreamDeserializer<'de, T>`).
- Existing serialization helpers remain unchanged:

 ```rust
 pub use de::{DeError, from_reader, from_str, StreamDeserializer, stream_from_str_docs};
 pub use ser::{SerError, to_string, to_writer};
 ```

### Future work (not in scope for this step)

- Streaming directly from `Read` / `BufRead` with a lifetime-aware buffer for
  the lexer.
- Further performance optimizations to close the remaining gap with `serde_yaml`
  (currently at 70-85% of serde_yaml speed, target is 80%+).

---

## 8. Summary

The streaming serde backend is now complete and production-ready:

- **Event-based deserialization** drives serde visitors directly from the event
  stream without building an intermediate AST.
- **Zero-copy anchor/alias handling** stores events with `'de` lifetime,
  avoiding string content cloning. Anchors are indexed incrementally as events
  are encountered, and aliases trigger on-demand replay of stored event
  sequences.
- **Performance** is 10-30% faster than the AST-backed approach and achieves
  70-85% of `serde_yaml` speed across various workloads.
- **All 169 tests pass** with the event-based backend, ensuring full
  compatibility with the existing AST-backed implementation.
