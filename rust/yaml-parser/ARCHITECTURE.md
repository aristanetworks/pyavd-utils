<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Parser Architecture

**Date:** 2026-03-21
**Status:** Current workspace design
**Scope:** Internal architecture and public entrypoints

## Overview

`yaml-parser` is a YAML 1.2 parser with three main goals:

- recover from syntax and structure errors without stopping at the first issue
- preserve source spans throughout the pipeline
- keep the hot path allocation-light and zero-copy where possible

The crate exposes two main user-facing parsing styles:

- AST parsing via [`parse`](src/lib.rs)
- serde deserialization via [`yaml_parser::serde`](src/serde/mod.rs)

Both share the same lexer and emitter.

## Pipeline

The public pipeline is intentionally simple:

```text
AST path:    Lexer -> Emitter -> AstEvent adapter -> Parser
serde path:  Lexer -> Emitter -> event_de
```

That means:

- lexing happens once
- structural YAML decisions live in the emitter
- AST construction and serde deserialization are consumer layers on top of the same event stream
- the AST path adds a thin adapter layer where structural comment trivia can be attached

## Layer 1: Lexer

The lexer lives in [`src/lexer/mod.rs`](src/lexer/mod.rs) and implements
`Iterator<Item = RichToken<'input>>`.

Key responsibilities:

- tokenize a full YAML stream, including multi-document input
- emit `LineStart(indent)` as the indentation signal
- track flow context so characters such as `,`, `[`, `]`, `{`, `}` and `:`
  are interpreted correctly
- handle directives, anchors, aliases, tags, block scalar headers, and quoted strings
- collect lexer errors without aborting iteration

Important design points:

- string content uses `Cow<'input, str>` for zero-copy when possible
- fixed-width ASCII structural probes use byte-oriented helpers instead of repeated
  `chars().nth(n)` walks
- comments and inline whitespace remain visible as real tokens because emitter
  behavior depends on them

Related files:

- [`src/lexer/token.rs`](src/lexer/token.rs)
- [`src/lexer/rich_token.rs`](src/lexer/rich_token.rs)

## Layer 2: Emitter

The emitter lives in [`src/emitter/mod.rs`](src/emitter/mod.rs) and is the
structural core of the crate.

It consumes lexer output and emits YAML serialization-tree [`Event`](src/event.rs)s.
For AST consumers it also exposes an internal AST-oriented stream that can carry
extra structural metadata on top of plain events.

Key responsibilities:

- own block/flow structure handling
- interpret indentation using `LineStart(indent)` plus emitter state
- collect and attach anchors and tags
- handle plain, quoted, and block scalar behavior
- manage error recovery for malformed structure

### State machine

The emitter is explicitly state-based. The stack in `ParseState` tracks active
constructs such as:

- block sequences and mappings
- flow sequences and mappings
- property collection
- multiline scalar continuation

This state-based design is kept intentionally. It is more robust and easier to
debug than the earlier helper-island style, even though it required dedicated
performance cleanup work.

### Token access

The emitter reads through [`TokenCursor`](src/emitter/cursor.rs), which provides:

- cheap kind-only lookahead
- owned consume for committed tokens
- borrowed lookahead windows for scanner-style helpers

This keeps the lexer/emitter boundary thin without caching redundant token data
in every `RichToken`.

### Property storage

Emitter-side anchor/tag state uses the sparse `EmitterProperties` wrapper in
[`src/emitter/states.rs`](src/emitter/states.rs).

That design keeps the common no-properties case small:

- emitter states carry `None` instead of materializing empty property structs
- public `Event` and AST APIs still expose optional boxed properties where needed

### Comment handling

Comments stay visible as lexer tokens because they affect scalar termination and,
for the AST path, some comments are attached to parsed structures.

The current attachment policy is intentionally small:

- same-line comments after a scalar/alias become `Node.trailing_comment`
- comments after `key:` before a nested block value become `MappingPair.header_comment`
- comments after `-` before a nested block item become `SequenceItem.header_comment`

Leading comments that do not clearly belong to one of those structural cases are
transported through the internal AST event layer only and are not preserved on
the final AST.

## Layer 3A: Event-to-AST Parser

The AST parser lives in [`src/parser/mod.rs`](src/parser/mod.rs).

It consumes the event stream and builds:

- [`Node`](src/value.rs)
- [`Value`](src/value.rs)
- [`Stream`](src/stream.rs)

Key responsibilities:

- rebuild tree structure from event pairs
- apply scalar type inference
- scope anchors per document and validate aliases
- preserve spans on AST nodes
- attach supported comment metadata to nodes, mapping pairs, and sequence items

The AST layer is intentionally simpler than the emitter because the emitter has
already resolved structure.

## Layer 3B: Serde Deserializer

The serde path lives in:

- [`src/serde/de.rs`](src/serde/de.rs)
- [`src/serde/event_de.rs`](src/serde/event_de.rs)

This path deserializes directly from the event stream and does not build the AST
first. That keeps the public serde API aligned with the same structural behavior
as the AST parser while avoiding an intermediate `Vec<Node>`.

Current entrypoints:

- `yaml_parser::serde::from_str`
- `yaml_parser::serde::from_reader`
- `yaml_parser::serde::stream_from_str_docs`

`from_reader` is currently convenience-oriented and reads the full input into a
`String` before deserializing.

## Event Writer and Serialization

Two writer-related layers exist:

- [`src/writer.rs`](src/writer.rs) writes YAML from an event stream
- [`src/serde/ser.rs`](src/serde/ser.rs) builds on AST-to-events conversion plus
  the writer to implement serde serialization helpers

These layers prioritize structurally correct output and round-tripping over
preserving original source formatting.

## Error Model

Errors are collected rather than thrown immediately.

Key pieces:

- [`ParseError`](src/error.rs) stores kind, span, and document offset
- lexer, emitter, and parser each contribute errors
- multi-document parsing keeps spans document-relative internally and exposes
  helpers to convert to global coordinates

This lets the crate support editor tooling, batch validation, and partial parse
results from malformed YAML.

## Span Model

Source locations live in [`src/span.rs`](src/span.rs).

Important details:

- byte offsets use `u32`
- indentation/column tracking uses `u16`
- spans are attached to tokens, events, AST nodes, and parse errors

That keeps location tracking cheap while still covering realistic YAML inputs.

## Performance Notes

The current performance work has focused on repeated touching in hot paths:

- reducing token cloning at the lexer/emitter boundary
- preferring kind-only peeks where payload data is not needed
- adding same-line fast paths for common sequence-entry shapes
- keeping emitter property state sparse

The benchmark reference document is [`BENCHMARKS.md`](BENCHMARKS.md).

## Testing

The crate is validated through several layers:

- unit tests for lexer, emitter, parser, serde, spans, and writer
- integration tests for edge cases and benchmark corpora
- YAML test suite coverage in [`tests/test_suite.rs`](tests/test_suite.rs)
- benchmark corpora checked for parse health and selected serde equivalence

## Current Tradeoffs

The crate intentionally prefers:

- explicit state over clever hidden control flow
- shared lexer/emitter infrastructure across AST and serde APIs
- span preservation and error recovery over the absolute minimum parser shape

That tradeoff has proven worth keeping; current optimization work is focused on
hot-path tuning inside this architecture rather than redesigning it.

## Future Work

### Reduce serde alias replay cloning

The serde event deserializer currently clones the full recorded event buffer
each time an alias is replayed. That keeps the implementation simple, but it
scales linearly with the size of the anchored subtree for every alias use.

If alias-heavy documents become a real workload, this could be revisited with a
shared backing store for recorded events so alias replay can borrow or share
the buffered sequence instead of cloning it per dereference.

### String-only schema key matching in validation

One possible follow-up in the validation layer is to stop coercing YAML scalar
mapping keys (`123`, `true`, `1.5`) to strings when matching schema-defined
keys or walking schema-driven paths.

Rationale:

- AVD schemas define keys as identifier-like strings.
- Dynamic keys are also string-derived.
- Treating non-string YAML keys as schema-matchable creates extra normalization
  rules in `get()` and path navigation that the schema language does not need.

If adopted, this should be implemented consistently across validation:

- strict string-only matching for YAML `get()` / mapping lookup
- matching iterator behavior left unchanged so diagnostics can still report the
  original input accurately
- explicit review of unexpected-key and path-walking behavior so the policy is
  changed everywhere at once, not piecemeal
