<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Parser Architecture Documentation

**Version:** workspace version (`yaml-parser`)
**Date:** 2026-03-20
**Status:** Production-ready, YAML 1.2 compliant
**Dependencies:** Minimal dependency surface (`derive_more`; optional `serde`)

---

## Table of Contents

1. [Overview](#overview)
2. [High-Level Architecture](#high-level-architecture)
3. [Module Structure](#module-structure)
4. [Data Flow](#data-flow)
5. [Core Components](#core-components)
6. [Key Design Decisions](#key-design-decisions)
7. [Error Recovery Strategy](#error-recovery-strategy)
8. [Testing Strategy](#testing-strategy)
9. [Known Limitations](#known-limitations)
10. [Future Improvements](#future-improvements)

---

## Overview

This is a YAML 1.2 parser written in Rust with the following key features:

- **Error Recovery**: Continues parsing after errors, collecting multiple errors in a single pass
- **Span Tracking**: Every parsed value includes its source location (byte range)
- **YAML 1.2 Compliance**: Passes all 496 unique tests from the YAML Test Suite (402 positive + 94 negative)
- **Minimal Dependencies**: Custom span/error handling with a small dependency surface
- **Zero-Copy Design**: Uses `Cow<'input, str>` throughout to minimize allocations
- **Layered Architecture**: Separates tokenization, event emission, and AST construction
- **Context-Aware Lexing**: Tracks flow depth and quote state to correctly tokenize context-dependent characters
- **State-Driven Emission**: The emitter owns indentation, multiline scalar continuation, and deferred event bridging through explicit states
- **High Performance**: Tuned for low-allocation parsing with benchmark results tracked in `BENCHMARKS.md`

### Key Capabilities

- ✅ Block and flow collections (sequences and mappings)
- ✅ Plain, single-quoted, double-quoted, literal (`|`), and folded (`>`) scalars
- ✅ Anchors (`&name`) and aliases (`*name`)
- ✅ Tags (`!tag`, `!!type`, `!<uri>`)
- ✅ Document markers (`---`, `...`)
- ✅ Directives (`%YAML`, `%TAG`)
- ✅ Multi-document streams
- ✅ Error recovery with partial output
- ✅ Indentation validation driven by `LineStart(indent)` + emitter state
- ✅ Flow context column tracking

---

## High-Level Architecture

The parser uses a **three-layer architecture** with unified streaming tokenization and an explicit event layer:

```txt
┌─────────────────────────────────────────────────────────────┐
│                      Input (YAML String)                    │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 1: Unified Lexer (lexer::Lexer)                      │
│  - Tokenizes entire YAML stream (single + multi-document)   │
│  - Handles directives (%YAML, %TAG) inline                  │
│  - Tracks document boundaries (---, ...)                    │
│  - Tracks flow depth (nested {}/[])                         │
│  - Emits `LineStart(indent)` as the indentation signal      │
│  - Context-aware: ,[]{} are delimiters in flow,             │
│                   part of plain scalars in block            │
│  - Phase tracking: DirectivePrologue vs InDocument          │
│  - Inline error detection stored by the lexer (via          │
│    `Lexer::take_errors`)                                    │
│  - Implements Iterator<Item = RichToken<'input>> for        │
│    streaming tokenization                                   │
│  Output: Iterator<Item = RichToken<'input>>                 │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 2: Event Emitter (emitter::Emitter)                  │
│  - Consumes a buffered token cursor over the lexer          │
│  - Emits YAML events (StreamStart, Scalar, MappingStart,    │
│    SequenceStart, Alias, etc.)                              │
│  - Handles YAML structure (indentation, flow vs block,      │
│    explicit/implicit keys, block scalars, properties)       │
│  - Performs structural validation and error recovery        │
│  Output: Iterator<Item = Event<'input>>                     │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 3: Event-to-AST Parser (parser::Parser)              │
│  - Consumes a streaming event source with 1-element peek    │
│  - Reconstructs AST (Vec<Node<'input>>) from event stream   │
│  - Applies scalar type inference                            │
│  - Resolves anchors and validates aliases                   │
│  Output: Vec<Node<'input>> (Stream<'input>)                 │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│          Output: (Stream<'static>, Vec<ParseError>)         │
└─────────────────────────────────────────────────────────────┘
```

---

## Module Structure

### Core Modules

#### `lib.rs` - Public API

- **Main entry points**:
  - `parse(input: &str) -> (Stream<'static>, Vec<ParseError>)` – full parse to AST
  - `emit_events(input: &str) -> (Vec<Event<'_>>, Vec<ParseError>)` – event-only API for tests/tooling
- **Exports**: Public high-level API (`parse`, `emit_events`) plus core types:
  - AST: `Node`, `Value`, `Properties`, `Stream`
  - Errors: `ParseError`, `ErrorKind`
  - Spans: `Span`, `Spanned`, `Position`, `SourceMap`
  - Events: `Event`, `ScalarStyle`, `CollectionStyle`
- **Module declarations**: `lexer`, `emitter`, `event`, `parser`, `error`, `span`, `stream`, `value`

#### `lexer/mod.rs` - Lexer Module Root

- **Purpose**: Organize all lexing-related components
- **Structure**:

  ```txt
  src/lexer/
  ├── mod.rs           # Module root, unified streaming lexer (handles everything)
  ├── token.rs         # Token type definitions
  └── rich_token.rs    # Token wrapper with span
  ```

- **Exports**: `Lexer<'input>`, `RichToken<'input>`, `Token<'input>`, and related types.

#### `span.rs` - Source Location Tracking

- **Type Aliases** (for consistency and memory optimization):
  - **`BytePosition`**: Type alias for `u32`, used for all byte offsets in source text
    - `pos_to_usize(pos)`: Convert `BytePosition` to `usize` for string indexing
    - `usize_to_pos(n)`: Convert `usize` to `BytePosition` (saturating at `u32::MAX`)
  - **`IndentLevel`**: Type alias for `u16`, used for indentation levels and column positions
    - `indent_to_usize(level)`: Convert to `usize` for array indexing and comparisons
    - `usize_to_indent(n)`: Convert `usize` to `IndentLevel` (saturating at `u16::MAX`)
- **`Span`**: Custom struct with `start: BytePosition` and `end: BytePosition` (byte offsets, max 4GB)
  - `new(range)`: Create from a `Range<BytePosition>`
  - `from_usize_range(range)`: Create from a `Range<usize>` (saturating conversion)
  - `at(pos)`: Create a zero-width span at a `BytePosition`
  - `at_usize(pos)`: Create a zero-width span at a `usize` position (saturating conversion)
  - `start_usize()`, `end_usize()`: Get offsets as `usize` for string indexing
  - `len()`: Get span length in bytes
  - `union(other)`: Create span encompassing both spans
  - `to_range()`: Convert back to `Range<usize>`
- **`Spanned<T>`**: Type alias for `(T, Span)` tuple
- **`Position`**: 1-based line and column numbers for human-readable positions
- **`SourceMap`**: Converts byte offsets to line/column positions using binary search
  - `position(byte_offset) -> Position`: Get line/column for a byte offset
  - `line_range(line) -> Option<Range<usize>>`: Get byte range for a line
- Every token and node includes its source location

#### `error.rs` - Error Types

- **`ParseError`**: Contains `kind`, `span`, `span_offset`
  - `global_span()`: Convert document-relative span to input-relative coordinates
  - `suggestion()`: Delegates to `ErrorKind::suggestion()`
- **`ErrorKind`**: 27 error variants organized by category:

  **Syntax Errors:**
  - `UnexpectedEof`, `UnexpectedToken` (generic fallback, currently unused)
  - `TrailingContent` - content after a value where none allowed
  - `MissingSeparator` - missing comma in flow collections
  - `UnmatchedBracket` - extra closing bracket/brace
  - `ContentOnSameLine` - invalid content on same line as entry
  - `UnexpectedColon` - colon in invalid position
  - `MissingColon` - mapping key not followed by `:` (e.g., `key\n  value`)
  - `InvalidComment` - comment `#` without preceding whitespace

  **Context Errors:**
  - `BlockIndicatorInFlow` - block indicators `-`, `?` inside flow collections
  - `DocumentMarkerInFlow` - `---` or `...` inside flow collections

  **Indentation Errors:**
  - `InvalidIndentation`, `InvalidIndentationContext { expected, found }`
  - `TabInIndentation` - tabs not allowed for indentation

  **String Errors:**
  - `UnterminatedString` - missing closing quote
  - `InvalidEscape(char)` - invalid escape sequence

  **Anchor/Alias/Tag Errors:**
  - `DuplicateAnchor`, `InvalidAnchor`, `UndefinedAlias`
  - `DuplicateTag`, `InvalidTag`, `PropertiesOnAlias`
  - `UndefinedTagHandle`
  - `MultilineImplicitKey` - implicit keys must be single line
  - `OrphanedProperties` - anchor/tag without following value

  **Directive Errors:**
  - `DuplicateDirective`, `InvalidDirective`
  - `InvalidNumber`, `InvalidBlockScalar`

- **Error suggestions**: `ErrorKind::suggestion()` returns fix hints for 20+ error types

#### `lexer/rich_token.rs` - Wrapper for tokens

- **`RichToken`**: Token wrapper (trivia not currently attached)

  ```rust
  pub struct RichToken {
      pub token: Token,
      pub span: Span,
  }
  ```

- **Trivia Handling**:
  - Comments are real `Token::Comment(_)` tokens in the stream
  - Whitespace is represented as `Token::Whitespace` / `Token::WhitespaceWithTabs`
  - Reason: comments and inline whitespace affect plain-scalar termination and must
    be visible to the emitter for correct multiline/plain-scalar behavior
  - `RichToken` keeps tokens + spans simple; higher-level tools can group/filter
    tokens as needed for IDE-style features

#### `value.rs` - AST Types

- **`Properties<'input>`**: Optional node-level properties (anchor, tag)

  ```rust
  pub struct Properties<'input> {
      pub anchor: Option<Cow<'input, str>>,  // &name (zero-copy)
      pub tag: Option<Cow<'input, str>>,     // !tag (zero-copy)
  }
  ```

  Properties are boxed inside `Node` to keep the common case (no properties)
  small while still supporting anchors and tags on any node.

- **`Node<'input>`**: The core AST node type with zero-copy support

  ```rust
  pub struct Node<'input> {
      pub properties: Option<Box<Properties<'input>>>,
      pub value: Value<'input>,  // The actual content
      pub span: Span,            // Source location
  }
  ```

- **`Number<'input>`**: Flexible integer representation

  ```rust
  pub enum Number<'input> {
      I64(i64),
      U64(u64),
      I128(i128),
      U128(u128),
      BigIntStr(Cow<'input, str>), // very large decimal integers
  }
  ```

  `BigIntStr` preserves decimal integers that do not fit in the native
  `i128` / `u128` ranges as their original textual representation while still
  allowing them to be re-emitted as numeric scalars in YAML.

- **`Value<'input>`**: The actual YAML value with zero-copy support

  ```rust
  pub enum Value<'input> {
      Null,
      Bool(bool),
      Int(Number<'input>),
      Float(f64),
      String(Cow<'input, str>),       // Zero-copy when possible
      Sequence(Vec<Node<'input>>),
      Mapping(Vec<(Node<'input>, Node<'input>)>),
      Alias(Cow<'input, str>),        // Zero-copy when possible
  }
  ```

- **Zero-Copy Design**: Uses `Cow<'input, str>` (Copy-on-Write) for string and
  some numeric content
  - `Cow::Borrowed(&str)` - zero allocation, borrows from input
  - `Cow::Owned(String)` - allocated when content is transformed (escapes, multiline)
  - `into_owned()` methods convert to `'static` lifetime when needed

- **Key Design**: Anchors and tags are **node properties**, not value wrappers
  - This matches the YAML spec's data model
  - Allows tags/anchors on any value type

### Layer 1: Unified Lexer

#### `lexer/mod.rs`

- **Purpose**: Unified streaming lexer that tokenizes the entire YAML stream (including directives, document boundaries, and content)
- **Key Types**:
  - `Lexer<'input>`: Main lexer state machine with zero-copy tokenization, implements `Iterator<Item = RichToken<'input>>`
  - `LexMode`: `Block` or `Flow` context
  - `LexerPhase`: `DirectivePrologue` (before document content) or `InDocument` (processing content)
- **State Tracking**:
  - `flow_depth`: Nesting level of `{}`/`[]`
  - `in_quoted_string`: Inside quoted string?
  - `prev_was_json_like`: For colon detection after JSON-like values
  - `prev_was_separator`: For comment validation (# after whitespace)
  - `byte_pos`: Current position in input (direct string slicing, no Vec<char>)
  - `pending_tokens`: Queue for multi-token constructs
  - `phase_state`: Tracks whether directives are valid at current position
  - `has_yaml_directive`: Tracks if `%YAML` was seen (for duplicate detection)
  - `has_directive_in_prologue`: For "directive without document" error
- **Unified Stream Handling**:
  - Handles directives (`%YAML`, `%TAG`, reserved) inline via `try_lex_directive()`
  - Emits `YamlDirective`, `TagDirective`, `ReservedDirective` tokens
  - Validates directive position (only valid before document content)
  - Detects `%` at column 0 in document context as error
- **Key Innovation**: Context-aware character interpretation
  - In **block context**: `,[]{}` are part of plain scalars
  - In **flow context**: `,[]{}` are delimiters
- **Indentation Signal**: `LineStart(indent)` is the only structural indentation token
  - The lexer reports the indentation width of each new logical line
  - The emitter owns collection lifetime and dedent decisions using `current_indent`,
    `indent_stack`, and explicit parser states
- **Modular Token Lexing**: The main `next_token()` method delegates to helper methods:
  - `try_lex_directive()` - Directives (`%YAML`, `%TAG`, reserved)
  - `try_lex_document_marker()` - Document start/end markers (`---`, `...`)
  - `try_lex_newline()` - Newline and indentation handling
  - `try_lex_whitespace()` - Inline whitespace
  - `try_lex_comment()` - Comments
  - `try_lex_flow_indicator()` - Flow indicators `{}[],`
  - `try_lex_block_indicator()` - Block indicators `-`, `?`
  - `try_lex_colon()` - Colon handling with context awareness
  - `try_lex_anchor_or_alias()` - Anchors `&` and aliases `*`
  - `try_lex_block_scalar_header()` - Block scalar headers `|`, `>`
  - `try_lex_quoted_scalar()` - Quoted scalars `'...'` and `"..."`
- **Shared Helpers for Quoted Strings**:
  - `handle_quoted_newline()` - Shared newline handling for both quote styles
  - `finalize_quoted_string()` - Shared end-of-string handling

#### `lexer/token.rs`

- **Purpose**: Token type definitions with zero-copy support
- **Key Types**:
  - `Token<'input>`: 23 variants using `Cow<'input, str>` for content
  - `QuoteStyle`: `Single` or `Double`
  - `BlockScalarHeader`: For `|` and `>` scalars
  - `Chomping`: `Strip`, `Clip`, or `Keep` trailing newlines
- **Helper Methods**:
  - `is_scalar()`: Check if token is a scalar type
  - `is_flow_indicator()`: Check if token is a flow indicator
- **Display Implementation**: Human-readable token formatting for errors

#### `lexer/rich_token.rs`

- **Purpose**: Token wrapper with span information.
- **Key Type**: `RichToken<'input>` - combines `Token<'input>` with `Span`.
- **Fields**:
  - `token: Token<'input>` - The token variant.
  - `span: Span` - Source location.
- **Error Handling**: Errors during lexing are collected by the lexer itself
  (`Lexer::take_errors`) rather than attached to individual `RichToken`s.
- **Usage**: The lexer implements `Iterator<Item = RichToken<'input>>`. The
  emitter reads from it lazily through `TokenCursor<'input>`, so neither
  `parse()` nor `emit_events()` needs to materialize a `Vec<RichToken<'_>>`.

### Layer 2: Event Emitter

- **Purpose**: Convert lexer tokens into YAML serialization-tree events while
  owning indentation, block/flow structure, properties, and error recovery.
- **Key Types**:
  - `Emitter<'input>`: Streaming iterator over `Event<'input>`
  - `TokenCursor<'input>`: Buffered token source over the lexer, providing cheap
    `peek`/`take` access without forcing broad rescans
  - `ParseState<'input>`: Explicit state machine for values, collections,
    properties, and multiline scalar continuations
  - `EmitterProperties<'input>`: Sparse emitter-only property carrier that keeps
    the common no-anchor/no-tag case off the hot state payload
  - `ValueContext`: Carries `min_indent`, `content_column`, key/value kind, and
    implicit-mapping permissions between value states
- **Indentation Model**:
  - `LineStart(indent)` is the authoritative indentation signal
  - `Emitter` tracks `current_indent`, `last_line_start_span`, and an emitter-owned
    `indent_stack` for active block structure levels
  - Block collections are advanced by explicit phases such as
    `BlockSeqPhase::BeforeEntryScan` / `BeforeEntryDispatch` and
    `BlockMapPhase::BeforeKeyScan` / `BeforeKeyDispatch`
- **Value-State Model**:
  - Value parsing is split into `Value`, `ValueCollectProperties`,
    `ValueDispatch`, and `ValueDispatchToken`
  - Multiline scalar slow paths are resumable states:
    `PlainScalarBlock`, `PlainScalarFlow`, `QuotedScalar`, and `BlockScalar`
  - Explicit `?` keys and implicit keys are distinguished in `ValueKind`, so
    only implicit keys enforce single-line key restrictions
- **Deferred Emission Model**:
  - `Emitter` has a single `pending_event: Option<Event<'input>>` slot for the
    hot transitions that must emit two events across consecutive `next()` calls
  - This replaced the old standalone `Emit*` parse states for value/collection
    bridging, while document start/end emission remains in `DocState`
- **Usage**:
  - `parse()` wires `Emitter` directly into `Parser`
  - `emit_events()` simply collects the streaming `Event<'input>` iterator

### Layer 3: Event Parser

#### `event.rs`

- **Purpose**: Define SAX-style event types for YAML parsing
- **Key Types**:
  - `Event<'input>`: 10 event variants for stream structure
    - `StreamStart`, `StreamEnd` - Stream boundaries
    - `DocumentStart`, `DocumentEnd` - Document boundaries with explicit flag
    - `MappingStart`, `MappingEnd` - Mapping boundaries with style
    - `SequenceStart`, `SequenceEnd` - Sequence boundaries with style
    - `Scalar` - Scalar value with style and boxed event-level properties
    - `Alias` - Reference to anchor
  - `CollectionStyle`: `Block` or `Flow`
  - `ScalarStyle`: `Plain`, `SingleQuoted`, `DoubleQuoted`, `Literal`, `Folded`
- **Usage**: Emitter emits events; `Parser` reconstructs AST
- **Note**: Consolidated from `event/` directory into single file (416 lines) for better maintainability

#### `parser/mod.rs`

- **Purpose**: Reconstruct AST from event stream
- **Key Types**:
  - `Parser<'input, I>`: Event-to-AST parser over any `Iterator<Item = Event<'input>>`
  - One-element lookahead buffer for streaming consumption
- **Main Functions**:
  - `Parser::parse() -> Vec<Node<'input>>`
  - `Parser::parse_next_document() -> Option<Node<'input>>`
- **Responsibilities**:
  - Build `Node` tree from flat event stream
  - Track anchor names for alias validation
  - Scope anchors per document
  - Validate structure (balanced start/end events)
  - Apply type inference to scalars (bool, int, float, null)
- **Error Handling**: Reports `UndefinedAlias` for unknown anchor references

---

## Data Flow

### Example: Parsing a Simple Document

**Input:**

```yaml
---
name: Alice
age: 30
```

**Layer 1 (Unified Lexer) Output:**

```rust
[
  (DocStart, 0..3),           // ---
  (LineStart(0), 3..4),
  (Plain("name"), 4..8),
  (Colon, 8..9),
  (Whitespace, 9..10),
  (Plain("Alice"), 10..15),
  (LineStart(0), 15..16),
  (Plain("age"), 16..19),
  (Colon, 19..20),
  (Whitespace, 20..21),
  (Plain("30"), 21..23),
  (LineStart(0), 23..24),
]
```

**Layer 2 (Event Emitter) Output:**

```rust
[
  StreamStart,
  DocumentStart { explicit: true, span: ... },
  MappingStart { style: Block, properties: Box::default(), span: ... },
  Scalar { value: "name", style: Plain, ... },
  Scalar { value: "Alice", style: Plain, ... },
  Scalar { value: "age", style: Plain, ... },
  Scalar { value: "30", style: Plain, ... },
  MappingEnd,
  DocumentEnd { explicit: false, span: ... },
  StreamEnd,
]
```

**Layer 3 (Event-to-AST Parser) Output:**

```rust
Node {
  properties: None,
  value: Mapping([
    (
      Node { value: String("name"), ... },
      Node { value: String("Alice"), ... }
    ),
    (
      Node { value: String("age"), ... },
      Node { value: Int(Number::U64(30)), ... }
    )
  ]),
  span: 4..24
}
```

### Flow Context Example

**Input:**

```yaml
items: [apple, banana, {color: red}]
```

**Lexer Behavior:**

- Outside `[...]`: Block context
  - `items` is a plain scalar
  - `:` is a colon indicator
- Inside `[...]`: Flow context (flow_depth = 1)
  - `,` is a comma delimiter (not part of scalar)
  - `[` and `]` are flow sequence delimiters
- Inside `{...}`: Nested flow context (flow_depth = 2)
  - `{` and `}` are flow mapping delimiters
  - `:` is a colon indicator

**Token Stream:**

```rust
[
  (Plain("items"), ...),
  (Colon, ...),
  (Whitespace, ...),
  (FlowSeqStart, ...),      // flow_depth: 0 → 1
  (Plain("apple"), ...),
  (Comma, ...),
  (Whitespace, ...),
  (Plain("banana"), ...),
  (Comma, ...),
  (Whitespace, ...),
  (FlowMapStart, ...),      // flow_depth: 1 → 2
  (Plain("color"), ...),
  (Colon, ...),
  (Whitespace, ...),
  (Plain("red"), ...),
  (FlowMapEnd, ...),        // flow_depth: 2 → 1
  (FlowSeqEnd, ...),        // flow_depth: 1 → 0
]
```

### LineStart-Based Indentation Example

**Input:**

```yaml
parent:
  child1: value1
  child2: value2
sibling: value3
```

**Token Stream (simplified):**

```rust
[
  (LineStart(0), ...),
  (Plain("parent"), ...),
  (Colon, ...),
  (LineStart(2), ...),      // Nested block content begins at indent 2
  (Plain("child1"), ...),
  (Colon, ...),
  (Plain("value1"), ...),
  (LineStart(2), ...),      // Same indentation
  (Plain("child2"), ...),
  (Colon, ...),
  (Plain("value2"), ...),
  (LineStart(0), ...),      // Dedent back to the parent line
  (Plain("sibling"), ...),
  (Colon, ...),
  (Plain("value3"), ...),
]
```

The emitter uses the `LineStart(indent)` tokens, plus its own collection state,
to decide when block collections continue, terminate, or report orphan-indent
errors.

---

## Core Components

### Token Types

The `Token` enum (defined in `token.rs`) has 20+ variants:

**Structure Indicators:**

- `BlockSeqIndicator` - `-` (block sequence item)
- `MappingKey` - `?` (explicit mapping key)
- `Colon` - `:` (mapping value indicator)

**Flow Indicators:**

- `FlowMapStart` / `FlowMapEnd` - `{` / `}`
- `FlowSeqStart` / `FlowSeqEnd` - `[` / `]`
- `Comma` - `,` (flow separator)

**Document Markers:**

- `DocStart` - `---`
- `DocEnd` - `...`

**Scalars:**

- `Plain(String)` - Plain scalar content
- `StringStart(QuoteStyle)` / `StringEnd(QuoteStyle)` - Quoted string delimiters
- `StringContent(String)` - Quoted string content
- `LiteralBlockHeader(BlockScalarHeader)` - `|` with chomping/indent
- `FoldedBlockHeader(BlockScalarHeader)` - `>` with chomping/indent

**Anchors and Aliases:**

- `Anchor(String)` - `&name`
- `Alias(String)` - `*name`

**Tags:**

- `Tag(String)` - `!tag`, `!!type`, `!<uri>`

**Directives:**

- `YamlDirective(String)` - `%YAML 1.2`
- `TagDirective(String)` - `%TAG !prefix! uri`
- `ReservedDirective(String)` - `%RESERVED`

**Whitespace and Structure:**

- `LineStart(IndentLevel)` - Newline + indentation count (spaces only, not tabs)
- `Whitespace` - Inline whitespace (spaces only)
- `WhitespaceWithTabs` - Inline whitespace containing at least one tab
- `Comment(String)` - `# comment`

**Note on Whitespace Tokens**: The `Whitespace` / `WhitespaceWithTabs` split enables O(1) tab detection in the parser. YAML forbids tabs for indentation but allows them for separation. The parser checks for `WhitespaceWithTabs` after `LineStart` to detect invalid tab indentation.

**Error Recovery:**

- The lexer records `ParseError`s and keeps producing tokens from the next safe
  point; it does not emit a dedicated `Invalid` token.

### Node and Value Types

**`Node<'input>` Structure:**

```rust
pub struct Node<'input> {
    pub properties: Option<Box<Properties<'input>>>,
    pub value: Value<'input>,              // The actual YAML value
    pub span: Span,                        // Source location (byte range)
}
```

**`Value<'input>` Enum:**

```rust
pub enum Value<'input> {
    Null,                                  // null, ~, or empty
    Bool(bool),                            // true, false
    Int(Number<'input>),                   // 42, -17, 0o77, 0xFF, very large ints
    Float(f64),                            // 3.14, -0.5, .inf, .nan
    String(Cow<'input, str>),              // Any string content (zero-copy)
    Sequence(Vec<Node<'input>>),           // Array/list of nodes
    Mapping(Vec<(Node<'input>, Node<'input>)>),  // Key-value pairs
    Alias(Cow<'input, str>),               // *anchor_name (zero-copy)
}
```

**Zero-Copy API:**

- `parse(input: &str) -> (Stream<'static>, Vec<ParseError>)` - Convenience API, returns owned data
- `emit_events(input: &str) -> (Vec<Event<'_>>, Vec<ParseError>)` - Event-level API that borrows from the input
- `Node::into_owned()` / `Value::into_owned()` - Convert borrowed to owned

**Design Note:** Mappings use `Vec<(Node, Node)>` instead of `HashMap` because:

1. YAML allows duplicate keys (last one wins, but all are preserved)
2. YAML allows non-hashable keys (e.g., sequences or mappings as keys)
3. Preserves insertion order (important for round-tripping)

---

## Key Design Decisions

### 1. Unified Streaming Architecture

**Why a unified lexer with event-based parsing?**

- **Separation of Concerns**: Each layer has a single responsibility
  - Unified lexer: Tokenizes entire stream (directives, markers, content)
  - Event emitter: Interprets tokens and emits YAML events, validates structure
  - Event-to-AST parser: Reconstructs AST from events, infers scalar types,
    resolves anchors and aliases
- **Streaming Design**: `Lexer<'input>` implements `Iterator<Item = RichToken<'input>>`
  - Enables lazy tokenization and potential streaming support
  - Simplifies document boundary handling (directives handled inline)
- **Phase Tracking**: `LexerPhase` (DirectivePrologue vs InDocument) determines valid tokens
  - Directives only valid before document content
  - Errors detected at lexer level with inline error attachment
- **Testability**: Each layer can be tested independently
- **Maintainability**: Single lexer pass reduces complexity

**Previous Design (removed):** Earlier revisions experimented with a separate stream-level pass, but
the current design uses a single unified lexer that handles directives and document markers inline.

### 2. LineStart-Driven Indentation

**Why keep indentation ownership in the emitter?**

- **Fewer repeated rescans**: collection phases can carry line-transition state
  instead of rediscovering indentation by repeated broad lookahead
- **Clear structure ownership**: the lexer reports line starts; the emitter owns
  block collection lifetime and indentation validation
- **Better explicit state transitions**: block sequences, block mappings, values,
  and multiline scalars each make their own line-ownership decisions
- **Matches current implementation**: `Indent` and `Dedent` have been removed

**How it works:**

1. The lexer emits `LineStart(indent)` for each logical line in block context.
2. The emitter updates `current_indent` when consuming `LineStart`.
3. Block collection phases compare `current_indent` with their stored collection
   indent to decide whether to continue, end, or report an orphan-indent error.
4. Value states carry `min_indent` and `content_column` through explicit states
   rather than inferring ownership from ad-hoc indentation helpers.

### 3. Context-Aware Lexing

**Why track flow depth during lexing?**

YAML has context-dependent syntax:

- In **block context**: `,[]{}` are regular characters (part of plain scalars)
- In **flow context**: `,[]{}` are structural delimiters

**Example:**

```yaml
# Block context - comma is part of the scalar
plain: hello, world

# Flow context - comma is a separator
flow: [hello, world]
```

The lexer tracks `flow_depth` to determine which interpretation to use.

### 4. Node Properties Architecture

**Why are anchors and tags node properties instead of value wrappers?**

This matches the YAML 1.2 specification's data model:

- Anchors and tags are **properties of nodes**, not part of the value
- Any value can have an anchor and/or tag
- This allows proper handling of cases like:

  ```yaml
  &anchor !!str "value"
  ```

**Alternative Considered:** Wrapper types like `Tagged(tag, value)` or `Anchored(anchor, value)`

- **Rejected because**: Doesn't match YAML spec, complicates type handling

### 5. Sparse Emitter Properties

**Why does the emitter use a separate sparse property carrier?**

- Anchors and tags are rare on most real inputs, but emitter parse states are on
  the hot path for every value and collection transition.
- Keeping full event-layer `Properties` inline in many `ParseState` variants
  makes the state stack larger and increases clone/merge pressure even when no
  properties are present.
- `EmitterProperties<'input>` keeps the empty case as `None` and only
  materializes a boxed property payload when an anchor or tag is actually seen.

**Result:** The public `Event` API stays unchanged, while the emitter can pass
around a smaller hot-path payload and convert back to boxed event properties only
at emission boundaries.

### 6. Hand-Written Parser (No Parser-Combinator Dependency)

**Why not use a parser combinator library like chumsky?**

After analysis, we chose a hand-written recursive descent parser because:

- **Context Sensitivity**: YAML's grammar is highly context-sensitive (flow vs block context, indentation-based structure). Parser combinators struggle with stateful parsing.
- **Performance**: Direct control over memory allocation and iteration (zero-copy `Cow<'input, str>`)
- **Error Recovery**: Custom recovery logic tailored to YAML's specific error patterns
- **Tight Dependency Surface**: The parsing core stays hand-written and does not depend on parser-combinator or YAML runtime libraries

**Error Recovery Strategy:**

- Parser continues after errors, collecting all errors in a single pass
- Partial output is still produced whenever the remaining structure can be recovered
- Missing or malformed values typically recover as empty/null-like scalar events at the emitter layer
- Specific error kinds provide actionable error messages

### 7. Span Tracking

**Why track source locations for every token and node?**

- **Error Messages**: Can show exactly where errors occurred
- **IDE Features**: Enables go-to-definition, hover, etc.
- **Source Mapping**: Can map parsed values back to original source
- **Debugging**: Easier to debug parser issues

Every `Token` and `Node` includes a `Span` (byte range in the input).

---

## Error Recovery Strategy

### Philosophy

The parser is designed to **continue parsing after errors** and collect multiple errors in a single pass. This is crucial for:

- **IDE Integration**: Show all errors at once, not just the first one
- **Better User Experience**: Users can fix multiple errors at once
- **Partial Output**: Even with errors, produce a partial AST

### Error Collection

Errors are collected at **all three layers** and merged by the public APIs:

- Lexer: collects lexing errors internally (`Lexer::take_errors`).
- Emitter: collects structural/event-level errors (`Emitter::take_errors`).
- Parser: collects AST-level errors (`Parser::take_errors`).

All of these use the shared `ParseError` type:

```rust
pub struct ParseError {
    pub kind: ErrorKind,
    pub span: Span,           // Document-relative byte range
    pub span_offset: usize,   // Add to span for global coordinates
}
```

The `span_offset` field enables accurate error positioning in multi-document streams.
Use `error.global_span()` to get the span relative to the original input.

Context information (expected values, indentation levels) is embedded directly in `ErrorKind` variants
where needed (e.g., `InvalidIndentationContext { expected: u16, found: u16 }`).

### Recovery Strategies

1. **Lexer-level errors**: When the lexer encounters invalid input, it records a `ParseError`
   and attempts to continue tokenizing from the next safe point.
2. **Continue Parsing**: After an error, the parser skips to the next reasonable boundary and continues.
3. **Partial Collections**: Collections with errors still include valid elements.

### Example

Input with error:

```yaml
name: Alice
age: [30, 40  # Missing closing bracket
city: Boston
```

Output:

- **Errors**: `[UnterminatedFlowSequence at span 15..21]`
- **Partial AST**: Still includes `name: Alice` and attempts to parse `city: Boston`

---

## Testing Strategy

### YAML 1.2 Test Suite

The parser is tested against the official YAML 1.2 test suite:

- **496 unique tests** covering all YAML 1.2 features
  - 402 positive tests (valid YAML that should parse successfully)
  - 94 negative tests (invalid YAML that should produce errors)
- **100% pass rate** - All tests passing
- Tests are in `tests/test_suite.rs`
- Note: The test suite directory contains 842 total test instances due to symlinks in `name/` and `tags/` directories that organize the same tests in different ways

### Test Suite Format

Tests use the YAML test suite event format:

```txt
=VAL :value
=ALI *anchor
+MAP
+SEQ
-MAP
-SEQ
```

The test runner (`run_test_suite()`) parses these events and compares them to the parser's output.

### Unit Tests

Each layer has unit tests for specific functionality:

- Lexer tests: tokenization, flow depth tracking, directives, line starts
- Emitter tests: event generation, structural validation, block scalar handling,
  explicit/implicit key behaviour
- Parser tests: AST building from events, error recovery, anchor resolution

### Test Organization

Following user preference:

- **Data-driven tests**: Loop over predefined test sets
- **Separate datasets**: Positive and negative test cases in different datasets
- **Not many individual test functions**: Use parameterized tests

---

## Known Limitations

Key limitations:

### 1. Performance

- The crate is tuned for low-allocation parsing and good event/AST throughput,
  but benchmark leadership depends heavily on corpus shape.
- Current benchmark results are tracked in `BENCHMARKS.md`; recent runs show a
  mixed picture rather than a blanket win over `saphyr` or `serde_yaml`.
- Performance-sensitive areas today are block scalars, large flow-heavy inputs,
  and indentation-heavy block structure handling.

### 2. Memory Usage

- Main `parse()` API returns owned `Node<'static>` values for convenience
- Use `emit_events()` when you only need the event stream and want to borrow from the input
- Arena allocation could provide additional performance wins for very large documents

### 3. Memory Optimization Trade-offs

The parser uses optimized type sizes to reduce memory footprint. This introduces the following limitations:

| Type | Optimization | Limitation |
| ---- | ------------ | ---------- |
| `Span` | Uses `BytePosition` (`u32`) for `start` and `end` | **Maximum file size: 4GB** (2³² bytes). Files larger than 4GB will have truncated span offsets. |
| `IndentLevel` | Uses `u16` for all indentation values | **Maximum indentation level: 65,535**. Indentation values above 65,535 will saturate. |
| `Node` | Anchors and tags stored in `Option<Box<Properties>>` | **Heap allocation for anchors/tags**: Nodes with anchors or tags incur one additional heap allocation. This is a trade-off for reducing base `Node` size from 96 to 48 bytes. |
| `EmitterProperties` | Stores event properties as `Option<Box<...>>` internally | **Rare-property heap allocation**: emitter-side anchors/tags allocate only when present, trading a tiny rare-case cost for a smaller hot-path state stack. |

**Size reductions achieved:**

| Type | Before | After | Savings |
| ---- | ------ | ----- | ------- |
| `Span` | 16 bytes | 8 bytes | 50% |
| `Node` | 96 bytes | 48 bytes | 50% |
| `ErrorKind` | 24 bytes | 8 bytes | 67% |
| `ParseError` | 48 bytes | 24 bytes | 50% |

**Practical impact:** These limits are rarely encountered in practice:

- 4GB is significantly larger than any typical YAML file
- 65,535 levels of indentation exceeds any reasonable use case
- Anchors and tags are uncommon in most YAML documents

### 4. Unicode Handling

- Basic Unicode support, but not fully tested
- Could improve handling of Unicode edge cases

### 5. Streaming

- The lexer, emitter, and event-to-AST parser are already streaming over an
  in-memory `&str`
- The public `parse()` API still returns an owned `Stream<'static>` for
  convenience
- True `Read`/`BufRead` streaming for arbitrarily large inputs is still future work

---

## Future Improvements

### Short-Term (Performance & Testing)

1. **Block Scalar Pipeline Refinement (Lexer vs Emitter responsibilities)**
   - **Current State**: Block scalar headers (`|`, `>`) are recognized in the lexer, but line collection, folding, and chomping are handled in the emitter's `BlockScalar` state using lexer tokens (`LineStart`, `Plain`, `Whitespace`, `Comment`, flow tokens).
   - **Observation**: Recent optimizations made `parse_block_scalar` a streaming builder (single `String` plus small per-line scratch) and brought block-scalar throughput close to Saphyr while preserving the layered architecture.
   - **Potential Future Direction**:
     - Evaluate whether some block-scalar concerns (e.g., more precise comment vs content handling, or preclassification of block-scalar content lines) should move into the lexer to further simplify the emitter.
     - Alternatively, factor the block-scalar builder logic into a shared helper that both the emitter and any future scanner-level implementation can reuse, so semantics remain consistent if responsibilities shift between layers.
   - **Goal**: Preserve the current lexer+emitter layering while keeping the option open to move more block-scalar work into the lexer if we need additional performance headroom on scalar-heavy workloads.

### Short-Term (Feature Additions)

1. **Pretty Printing**
   - Format YAML output
   - Preserve comments and formatting

---

## References

- **YAML 1.2 Specification**: https://yaml.org/spec/1.2/spec.html
- **YAML Test Suite**: https://github.com/yaml/yaml-test-suite
  - Internal technical debt is tracked in the issue tracker rather than in a
    separate document in this crate.

---

## Conclusion

This YAML parser achieves **full YAML 1.2 compliance** through a carefully designed three-layer architecture with a **small, non-parser-specific dependency surface**:

1. **Unified Lexer**: Tokenizes entire stream with phase tracking (directives, markers, content)
2. **Event Emitter**: Emits SAX-style events, owns indentation and structural validation
3. **AST Builder**: Reconstructs typed AST from events, resolves anchors and aliases

**Key achievements:**

- **Minimal dependencies**: Hand-written lexer/emitter/parser pipeline with custom `Span` implementation
- **Zero-copy parsing**: `Cow<'input, str>` throughout for minimal allocations
- **Streaming pipeline**: Lexer, emitter, and parser can all operate lazily over an in-memory `&str`
- **Actionable errors**: Rich `ErrorKind` variants with specific suggestions (no generic fallback errors)
- **Structured error collection**: Lexer, emitter, and parser all report `ParseError` values that are merged by `emit_events`/`parse`.
- **State-machine indentation model**: `LineStart(indent)` plus emitter-owned phases now fully replace lexer `Indent`/`Dedent` tokens
- **Measured performance**: Benchmark results are tracked in `BENCHMARKS.md`, with the crate optimized around spans, error recovery, and event/AST throughput

The architecture achieves both **correctness** and **performance**, making it suitable for IDE integration and user-facing tools where helpful error messages are crucial, as well as for high-throughput parsing scenarios.

The codebase is clean of clippy warnings and ready for production use.
