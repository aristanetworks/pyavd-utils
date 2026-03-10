<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Parser Architecture Documentation

**Version:** 0.0.3
**Date:** 2026-03-10
**Status:** Production-ready, YAML 1.2 compliant
**Dependencies:** Zero external dependencies

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
- **Zero Dependencies**: Self-contained with custom span/error handling
- **Zero-Copy Design**: Uses `Cow<'input, str>` throughout to minimize allocations
- **Layered Architecture**: Separates tokenization, event emission, and AST construction
- **Context-Aware Lexing**: Tracks flow depth and quote state to correctly tokenize context-dependent characters
- **High Performance**: Competitive with or faster than saphyr in most benchmarks (see `BENCHMARKS.md`)

### Key Capabilities

- ✅ Block and flow collections (sequences and mappings)
- ✅ Plain, single-quoted, double-quoted, literal (`|`), and folded (`>`) scalars
- ✅ Anchors (`&name`) and aliases (`*name`)
- ✅ Tags (`!tag`, `!!type`, `!<uri>`)
- ✅ Document markers (`---`, `...`)
- ✅ Directives (`%YAML`, `%TAG`)
- ✅ Multi-document streams
- ✅ Error recovery with partial output
- ✅ Indentation validation (INDENT/DEDENT tokens)
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
│  - Emits INDENT/DEDENT tokens                               │
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
│  - Consumes an iterator of RichToken<'input> + &str         │
│  - Emits YAML events (StreamStart, Scalar, MappingStart,    │
│    SequenceStart, Alias, etc.)                              │
│  - Handles YAML structure (indentation, flow vs block,      │
│    complex keys, block scalars, properties)                 │
│  - Performs structural validation and error recovery        │
│  Output: Iterator<Item = Event<'input>>                     │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 3: Event-to-AST Parser (parser::Parser)              │
│  - Consumes &[Event<'input>]                                │
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

- **`Node<'input>`**: The core AST node type with zero-copy support

  ```rust
  pub struct Node<'input> {
      pub anchor: Option<Cow<'input, str>>,  // &name (zero-copy)
      pub tag: Option<Cow<'input, str>>,     // !tag (zero-copy)
      pub value: Value<'input>,              // The actual content
      pub span: Span,                        // Source location
  }
  ```

- **`Value<'input>`**: The actual YAML value with zero-copy support

  ```rust
  pub enum Value<'input> {
      Null,
      Bool(bool),
      Int(i64),
      Float(f64),
      String(Cow<'input, str>),       // Zero-copy when possible
      Sequence(Vec<Node<'input>>),
      Mapping(Vec<(Node<'input>, Node<'input>)>),
      Alias(Cow<'input, str>),        // Zero-copy when possible
      Invalid,
  }
  ```

- **Zero-Copy Design**: Uses `Cow<'input, str>` (Copy-on-Write) for string content
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
  - `indent_stack`: Stack of indentation levels
  - `in_quoted_string`: Inside quoted string?
  - `prev_was_json_like`: For colon detection after JSON-like values
  - `prev_was_separator`: For comment validation (# after whitespace)
  - `byte_pos`: Current position in input (direct string slicing, no Vec<char>)
  - `pending_tokens`: Queue for multi-token constructs
  - `lexer_phase`: Tracks whether directives are valid at current position
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
- **INDENT/DEDENT Tokens**: Python-style indentation tracking
  - Emitted after `LineStart` in block context
  - Used by the emitter to determine block structure boundaries when producing events
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
  high-level `emit_events` API collects these into a `Vec<RichToken<'_>>`
  before passing them to the emitter.

### Layer 2: Event Emitter

> NOTE: The current implementation uses the `emitter` module as the second
> layer between the lexer and the AST parser.

### Layer 3: Event Parser

#### `event.rs`

- **Purpose**: Define SAX-style event types for YAML parsing
- **Key Types**:
  - `Event<'input>`: 13 event variants for stream structure
    - `StreamStart`, `StreamEnd` - Stream boundaries
    - `DocumentStart`, `DocumentEnd` - Document boundaries with explicit flag
    - `MappingStart`, `MappingEnd` - Mapping boundaries with style
    - `SequenceStart`, `SequenceEnd` - Sequence boundaries with style
    - `Scalar` - Scalar value with style, anchor, tag
    - `Alias` - Reference to anchor
  - `CollectionStyle`: `Block` or `Flow`
  - `ScalarStyle`: `Plain`, `SingleQuoted`, `DoubleQuoted`, `Literal`, `Folded`
- **Usage**: Emitter emits events; `Parser` reconstructs AST
- **Note**: Consolidated from `event/` directory into single file (416 lines) for better maintainability

#### `parser/mod.rs`

- **Purpose**: Reconstruct AST from event stream
- **Key Types**:
  - `Parser<'input>`: Stack-based parser for events
  - Tracks in-progress collections (mapping key/value, sequence)
- **Main Function**: `Parser::parse() -> Vec<Node>`
- **Responsibilities**:
  - Build `Node` tree from flat event stream
  - Track anchors via `HashSet<String>`
  - Resolve aliases by cloning anchored nodes
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

**Layer 2 (Event-Based Parser) Output:**

```rust
[
  StreamStart,
  DocumentStart { explicit: true, version: None },
  MappingStart { anchor: None, tag: None, style: Block },
  Scalar { value: "name", style: Plain, ... },
  Scalar { value: "Alice", style: Plain, ... },
  Scalar { value: "age", style: Plain, ... },
  Scalar { value: "30", style: Plain, ... },
  MappingEnd,
  DocumentEnd { explicit: false },
  StreamEnd,
]
```

**Layer 3 (Event Parser) Output:**

```rust
Node {
  anchor: None,
  tag: None,
  value: Mapping([
    (
      Node { value: String("name"), ... },
      Node { value: String("Alice"), ... }
    ),
    (
      Node { value: String("age"), ... },
      Node { value: Int(30), ... }
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

### INDENT/DEDENT Example

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
  (LineStart(2), ...),      // Indentation increased from 0 to 2
  (Indent(2), ...),         // INDENT token emitted
  (Plain("child1"), ...),
  (Colon, ...),
  (Plain("value1"), ...),
  (LineStart(2), ...),      // Same indentation
  (Plain("child2"), ...),
  (Colon, ...),
  (Plain("value2"), ...),
  (LineStart(0), ...),      // Indentation decreased from 2 to 0
  (Dedent, ...),            // DEDENT token emitted
  (Plain("sibling"), ...),
  (Colon, ...),
  (Plain("value3"), ...),
]
```

The parser uses `Indent` and `Dedent` tokens to determine when block collections end.

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

- `LineStart(usize)` - Newline + indentation count (spaces only, not tabs)
- `Whitespace` - Inline whitespace (spaces only)
- `WhitespaceWithTabs` - Inline whitespace containing at least one tab
- `Comment(String)` - `# comment`
- `Indent(usize)` - INDENT token (emitted by the lexer)
- `Dedent` - DEDENT token (emitted by the lexer)

**Note on Whitespace Tokens**: The `Whitespace` / `WhitespaceWithTabs` split enables O(1) tab detection in the parser. YAML forbids tabs for indentation but allows them for separation. The parser checks for `WhitespaceWithTabs` after `LineStart` to detect invalid tab indentation.

**Error Recovery:**

- `Invalid` - Invalid token (for error recovery)

### Node and Value Types

**`Node<'input>` Structure:**

```rust
pub struct Node<'input> {
    pub anchor: Option<Cow<'input, str>>,  // Optional anchor name (&name)
    pub tag: Option<Cow<'input, str>>,     // Optional tag (!tag)
    pub value: Value<'input>,              // The actual YAML value
    pub span: Span,                        // Source location (byte range)
}
```

**`Value<'input>` Enum:**

```rust
pub enum Value<'input> {
    Null,                                  // null, ~, or empty
    Bool(bool),                            // true, false
    Int(i64),                              // 42, -17, 0o77, 0xFF
    Float(f64),                            // 3.14, -0.5, .inf, .nan
    String(Cow<'input, str>),              // Any string content (zero-copy)
    Sequence(Vec<Node<'input>>),           // Array/list of nodes
    Mapping(Vec<(Node<'input>, Node<'input>)>),  // Key-value pairs
    Alias(Cow<'input, str>),               // *anchor_name (zero-copy)
    Invalid,                               // Error recovery placeholder
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

### 2. INDENT/DEDENT Tokens

**Why Python-style indentation tokens?**

- **Simplifies Parser**: Parser doesn't need to track indentation itself
- **Clear Structure Boundaries**: DEDENT tokens explicitly mark where block collections end
- **Error Recovery**: Easier to recover from indentation errors
- **Matches YAML Semantics**: YAML's block structure is indentation-based

**How it works:**

1. The lexer maintains an `indent_stack`
2. After each `LineStart(n)` token in block context:
   - If `n > current_indent`: Emit `Indent(n)`, push to stack
   - If `n < current_indent`: Emit `Dedent` for each popped level
   - If `n == current_indent`: No change
3. The emitter uses `Dedent` to know when to stop a block collection when emitting events

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

### 5. Hand-Written Parser (No External Dependencies)

**Why not use a parser combinator library like chumsky?**

After analysis, we chose a hand-written recursive descent parser because:

- **Context Sensitivity**: YAML's grammar is highly context-sensitive (flow vs block context, indentation-based structure). Parser combinators struggle with stateful parsing.
- **Performance**: Direct control over memory allocation and iteration (zero-copy `Cow<'input, str>`)
- **Error Recovery**: Custom recovery logic tailored to YAML's specific error patterns
- **Zero Dependencies**: The entire crate has zero external dependencies, simplifying auditing and deployment

**Error Recovery Strategy:**

- Parser continues after errors, collecting all errors in a single pass
- Invalid nodes are marked with `Value::Invalid`
- Partial output is still produced (useful for IDE features)
- Specific error kinds (23 variants) provide actionable error messages

### 6. Span Tracking

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
2. **Invalid Nodes**: The parser produces `Value::Invalid` for unparsable structures.
3. **Continue Parsing**: After an error, the parser skips to the next reasonable boundary and continues.
4. **Partial Collections**: Collections with errors still include valid elements.

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

- Lexer tests: tokenization, flow depth tracking, INDENT/DEDENT
- Emitter tests: event generation, structural validation, block scalar handling
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

The parser is competitive with or faster than `saphyr` (a mature Rust YAML parser) in most benchmarks:

- **Mappings**: 34-49% faster
- **Sequences**: 14% faster
- **Flow collections**: 269% faster
- **Latency**: 16-40% faster for small/medium/large documents
- **Block scalars**: ~6% slower (due to token-based architecture overhead)

See `BENCHMARKS.md` for detailed performance data and instructions.

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

- Currently parses entire input at once
- Could support streaming for large files

---

## Future Improvements

### Short-Term (Performance & Testing)

1. **Block Scalar Pipeline Refinement (Lexer vs Emitter responsibilities)**
   - **Current State**: Block scalar headers (`|`, `>`) are recognized in the lexer, but line collection, folding, and chomping are handled in the emitter's `parse_block_scalar` using lexer tokens (`LineStart`, `Indent`/`Dedent`, `Plain`, `Whitespace`, `Comment`, flow tokens).
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

This YAML parser achieves **full YAML 1.2 compliance** through a carefully designed three-layer architecture with **zero external dependencies**:

1. **Unified Lexer**: Tokenizes entire stream with phase tracking (directives, markers, content)
2. **Event-Based Parser**: Emits SAX-style events, validates structure and indentation
3. **AST Builder**: Reconstructs typed AST from events, resolves anchors and aliases

**Key achievements:**

- **Zero dependencies**: Self-contained crate with custom `Span` implementation
- **Zero-copy parsing**: `Cow<'input, str>` throughout for minimal allocations
- **Streaming-ready lexer**: `Lexer<'input>` implements `Iterator<Item = RichToken<'input>>` for lazy tokenization
- **Actionable errors**: Rich `ErrorKind` variants with specific suggestions (no generic fallback errors)
- **Structured error collection**: Lexer, emitter, and parser all report `ParseError` values that are merged by `emit_events`/`parse`.
- **High performance**: Faster than saphyr in most benchmarks while always providing spans

The architecture achieves both **correctness** and **performance**, making it suitable for IDE integration and user-facing tools where helpful error messages are crucial, as well as for high-throughput parsing scenarios.

The codebase is clean of clippy warnings and ready for production use.
