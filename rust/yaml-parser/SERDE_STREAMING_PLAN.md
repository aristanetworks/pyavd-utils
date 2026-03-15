<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Parser: Serde / Streaming Plan

Status: **Design / implementation guide**
Last updated: 2026-03-15

This document describes how serde support in `yaml-parser` is structured today and
lays out a detailed plan for making everything **from the input string onward**
fully streaming (no mandatory `Vec<Event>` / `Vec<Node>` in the serde path).

We intentionally keep the current "read the whole input into a `String`" model
for now. Large-input `Read` / `BufRead` streaming can be revisited separately.

---

## 1. Current state (serde + parser)

**Parsing pipeline:**

- `emit_events(input: &str)` (internal) lexes + emits a `Vec<Event<'input>>`.
- `Parser<'input, I>` already operates on any `Iterator<Item = Event<'input>>`:

  - Holds `events: I`, `peeked: Option<Event<'input>>`, `events_consumed: usize`.
  - Implements `parse()` in terms of `peek`/`advance` over the iterator.

- Public `parse(&str)` currently does:

  ```text
  &str -> Vec<Event> -> Parser<Vec<Event>.into_iter()> -> Vec<Node<'input>>
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

### Tasks

1. Introduce an internal `EventStream<'input>` iterator that encapsulates the
   current emitter state machine:

   ```rust
   pub struct EventStream<'input> { /* emitter state */ }

   impl<'input> Iterator for EventStream<'input> {
       type Item = Event<'input>;
       fn next(&mut self) -> Option<Self::Item> { /* drive emitter */ }
   }

   pub fn event_stream(input: &'input str) -> EventStream<'input> { ... }
   ```

2. Change the internal implementation of `parse(&str)` to:

   ```rust
   pub fn parse(input: &str) -> (Vec<Node>, Vec<ParseError>) {
       let events = event_stream(input);
       let mut parser = Parser::new(events);
       let docs = parser.parse();
       let errors = parser.take_errors();
       (docs, errors)
   }
   ```

**Result:** From the moment we have an input `&str`, events are streamed from
the emitter into the parser. We still build a full AST, but `Vec<Event>` goes
away.

---

## 4. Phase 3.1 – Document-level streaming serde API

**Objective:** Provide a streaming API over *documents* without yet changing
how each document is deserialized internally.

### Tasks

1. Introduce a `StreamDeserializer<'de, I>` type that owns a `Parser<'de, I>`:

   ```rust
   pub struct StreamDeserializer<'de, I> {
       parser: Parser<'de, I>,
   }
   ```

2. Implement an iterator-like API that yields one document at a time:

   ```rust
   impl<'de> StreamDeserializer<'de, EventStream<'de>> {
       pub fn from_str(input: &'de str) -> Self { /* event_stream + Parser */ }
   }

   impl<'de, I> StreamDeserializer<'de, I>
   where
       I: Iterator<Item = Event<'de>>,
   {
       pub fn next_value<T>(&mut self) -> Result<Option<T>, DeError>
       where
           T: DeserializeOwned,
       {
           // For now: parse a single Node via the existing AST path,
           // then run AST-backed serde on that Node and drop it.
       }
   }
   ```

3. Expose a helper in `yaml_parser::serde`:

   ```rust
   pub fn stream_from_str_docs<T>(input: &str)
       -> impl Iterator<Item = Result<T, DeError>>
   where
       T: DeserializeOwned;
   ```

**Result:** Users can stream multiple documents from a single input string as
`T` values, even though each document still goes through the AST internally.
This also defines the public "streaming" surface we will later back with a more
aggressive implementation.

---

## 5. Phase 3.2 – Factor parser into structural engine + consumer

**Objective:** Separate structural parsing (driving over `Event`s, handling
mappings/sequences/anchors/tags/errors) from "what to do with values", so the
same engine can either build an AST or drive serde visitors.

### Tasks

1. Define a `Consumer<'input>` trait in `parser`:

   ```rust
   trait Consumer<'input> {
       type Output;
       type Error;

       fn scalar(&mut self, ...) -> Result<Self::Output, Self::Error>;
       fn mapping(&mut self, ...) -> Result<Self::Output, Self::Error>;
       fn sequence(&mut self, ...) -> Result<Self::Output, Self::Error>;
       fn alias(&mut self, ...) -> Result<Self::Output, Self::Error>;
   }
   ```

   The mapping/sequence methods receive closures or lightweight builder types
   (`MappingBuilder`, `SequenceBuilder`) that iterate over entries/items using
   the parser's existing loops, but deliver each key/value/item to the
   `Consumer`.

2. Implement `NodeConsumer<'input>` as the first `Consumer`:

   - `Output = Node<'input>`, `Error = ParseError`.
   - `scalar` builds `Node::new(Value::..., span)` (with type inference).
   - `mapping` and `sequence` internally collect `Vec<(Node, Node)>` /
     `Vec<Node>` by driving the builder.
   - `alias` builds `Node::new(Value::Alias, span)` and runs anchor checks.

3. Refactor `Parser` internals to be parameterised by a `Consumer`:

   - Introduce `parse_node_with<C: Consumer>(&mut self, consumer: &mut C)` that
     runs one node worth of structural logic and delegates node construction to
     the consumer.
   - Rewrite `parse()` to use a `NodeConsumer` instance under the hood so that
     existing `parse()` semantics remain unchanged.

**Result:** The parser becomes a reusable structural engine. AST construction is
just one particular `Consumer` implementation.

---

## 6. Phase 3.3 – Streaming serde consumer and deserializer (engine)

**Objective:** Implement a `Consumer` that drives serde visitors directly,
giving us a fully streaming serde backend from the emitter onward.

### Tasks

1. Define `SerdeConsumer<'de>` implementing `Consumer<'de>`:

   - Holds the current serde `Visitor<'de>` and mode (scalar vs map vs seq).
   - Has access to an `AnchorIndex<'de>` for alias resolution and to the
     parser's error collection mechanism.
   - `scalar` mirrors `ValueDeserializer::deserialize_any` but works directly
     from scalar events.
   - `mapping` constructs a `MapAccess` adaptor that pulls keys/values by
     driving the mapping body via the parser's structural engine.
   - `sequence` constructs a `SeqAccess` adaptor over sequence items.
   - `alias` resolves via `AnchorIndex` and re-enters the consumer on the
     target value (initially via a small in-memory representation if needed).

2. Implement `StreamingDeserializer<'de, I>` that owns a `Parser<'de, I>` and
   an `AnchorIndex<'de>`:

   - Implements `serde::de::Deserializer<'de>` by creating a `SerdeConsumer`
     and delegating to `Parser::parse_node_with`.
   - Initially supports single-document inputs (matching `from_str` today);
     multi-document streaming is handled by `StreamDeserializer` from Phase 3.1.

3. Expose new serde entrypoints backed by the streaming engine:

   - `from_str_streaming<T>(input: &str) -> Result<T, DeError>` that uses
     `EventStream` + `Parser` + `StreamingDeserializer`.
   - Optionally, `stream_from_str_docs<T>` wired to the same engine for
     multi-document streaming.

**Result:** For typical inputs, serde deserialization no longer needs to build
`Vec<Node>`; values can be deserialized directly from the event stream driven
by the parser's structural logic. Anchor/alias handling may remain partially
AST-backed at first and can be refined later.

---

## 7. Phase 3.4 – API integration and future work (public surface)

**Objective:** Integrate the streaming deserializer into the public API and
decide how it relates to the existing AST-backed `from_str`.

### Tasks

1. Add public re-exports and helpers in `yaml_parser::serde`:

   - Keep existing:

     ```rust
     pub use de::{DeError, from_reader, from_str};
     pub use ser::{SerError, to_string, to_writer};
     ```

   - Add streaming-specific APIs once implemented:

     ```rust
     pub use de::{from_str_streaming, StreamDeserializer};
     ```

2. After the streaming path is battle-tested, consider reimplementing
   `from_str` in terms of `from_str_streaming` for the single-document case so
   that the default entrypoint also benefits from streaming internals.

3. Future work (not in scope for this step):

   - Streaming directly from `Read` / `BufRead` with a lifetime-aware buffer
     for the lexer.
   - More sophisticated streaming anchor/alias handling that does not require
     pre-building any AST even for complex alias patterns.

---

## 8. Notes on anchors and aliases (future refinements)

The plan above deliberately treats anchor/alias handling conservatively:

- Initially, we may still build enough in-memory representation to support
  alias resolution for streaming serde, especially for complex alias graphs.
- Longer-term, we can refine `SerdeConsumer` and the parser so that anchors are
  indexed and aliases resolved incrementally, without needing a pre-pass over
  the full AST.

This allows us to get a working streaming serde backend in place while keeping
behaviour identical to the existing AST-backed implementation.
   (`MappingBuilder`, `SequenceBuilder`) that iterate over entries/items using
   the parser's existing loops, but deliver each key/value/item to the
   `Consumer`.

1. Implement `NodeConsumer<'input>` as the first `Consumer`:

   - `Output = Node<'input>`, `Error = ParseError`.
   - `scalar` builds `Node::new(Value::..., span)` (with type inference).
   - `mapping` and `sequence` internally collect `Vec<(Node, Node)>` /
     `Vec<Node>` by driving the builder.
   - `alias` builds `Node::new(Value::Alias, span)` and runs anchor checks.

2. Refactor `Parser` internals to be parameterised by a `Consumer`:

   - Introduce `parse_node_with<C: Consumer>(&mut self, consumer: &mut C)` that
     runs one node worth of structural logic and delegates node construction to
     the consumer.
   - Rewrite `parse()` to use a `NodeConsumer` instance under the hood so that
     existing `parse()` semantics remain unchanged.

**Result:** The parser becomes a reusable structural engine. AST construction is
just one particular `Consumer` implementation.

---

## 6. Phase 3.3 – Streaming serde consumer and deserializer

**Objective:** Implement a `Consumer` that drives serde visitors directly,
giving us a fully streaming serde backend from the emitter onward.

### Tasks

1. Define `SerdeConsumer<'de>` implementing `Consumer<'de>`:

   - Holds the current serde `Visitor<'de>` and mode (scalar vs map vs seq).
   - Has access to an `AnchorIndex<'de>` for alias resolution and to the
     parser's error collection mechanism.
   - `scalar` mirrors `ValueDeserializer::deserialize_any` but works directly
     from scalar events.
   - `mapping` constructs a `MapAccess` adaptor that pulls keys/values by
     driving the mapping body via the parser's structural engine.
   - `sequence` constructs a `SeqAccess` adaptor over sequence items.
   - `alias` resolves via `AnchorIndex` and re-enters the consumer on the
     target value (initially via a small in-memory representation if needed).

2. Implement `StreamingDeserializer<'de, I>` that owns a `Parser<'de, I>` and
   an `AnchorIndex<'de>`:

   - Implements `serde::de::Deserializer<'de>` by creating a `SerdeConsumer`
     and delegating to `Parser::parse_node_with`.
   - Initially supports single-document inputs (matching `from_str` today);
     multi-document streaming is handled by `StreamDeserializer` from Phase 3.1.

3. Expose new serde entrypoints backed by the streaming engine:

   - `from_str_streaming<T>(input: &str) -> Result<T, DeError>` that uses
     `EventStream` + `Parser` + `StreamingDeserializer`.
   - Optionally, `stream_from_str_docs<T>` wired to the same engine for
     multi-document streaming.

**Result:** For typical inputs, serde deserialization no longer needs to build
`Vec<Node>`; values can be deserialized directly from the event stream driven
by the parser's structural logic. Anchor/alias handling may remain partially
AST-backed at first and can be refined later.

---

## 7. Phase 3.4 – API integration and future work

**Objective:** Integrate the streaming deserializer into the public API and
decide how it relates to the existing AST-backed `from_str`.

### Tasks

1. Add public re-exports and helpers in `yaml_parser::serde`:

   - Keep existing:

     ```rust
     pub use de::{DeError, from_reader, from_str};
     pub use ser::{SerError, to_string, to_writer};
     ```

   - Add streaming-specific APIs once implemented:

     ```rust
     pub use de::{from_str_streaming, StreamDeserializer};
     ```

2. After the streaming path is battle-tested, consider reimplementing
   `from_str` in terms of `from_str_streaming` for the single-document case so
   that the default entrypoint also benefits from streaming internals.

3. Future work (not in scope for this step):

   - Streaming directly from `Read` / `BufRead` with a lifetime-aware buffer
     for the lexer.
   - More sophisticated streaming anchor/alias handling that does not require
     pre-building any AST even for complex alias patterns.

---

## 8. Notes on anchors and aliases

The plan above deliberately treats anchor/alias handling conservatively:

- Initially, we may still build enough in-memory representation to support
  alias resolution for streaming serde, especially for complex alias graphs.
- Longer-term, we can refine `SerdeConsumer` and the parser so that anchors are
  indexed and aliases resolved incrementally, without needing a pre-pass over
  the full AST.

This allows us to get a working streaming serde backend in place while keeping
behaviour identical to the existing AST-backed implementation.
