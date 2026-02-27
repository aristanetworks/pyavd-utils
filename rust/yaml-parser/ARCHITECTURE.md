<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Parser Architecture Documentation

**Version:** 0.0.2
**Date:** 2026-02-27
**Status:** 99.4% YAML 1.2 Test Suite Compliance (331/333 tests passing)
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
- **99.4% YAML 1.2 Compliance**: Passes 331/333 tests from the official YAML test suite
- **Zero Dependencies**: Self-contained with custom span/error handling
- **Zero-Copy Design**: Uses `Cow<'input, str>` throughout to minimize allocations
- **Layered Architecture**: Separates stream-level parsing from document-level parsing
- **Context-Aware Lexing**: Tracks flow depth and quote state to correctly tokenize context-dependent characters

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

The parser uses a **three-layer architecture**:

```txt
┌─────────────────────────────────────────────────────────────┐
│                      Input (YAML String)                    │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 1: Stream Lexer (stream_lexer.rs)                    │
│  - Splits input into raw documents                          │
│  - Extracts directives (%YAML, %TAG)                        │
│  - Identifies document boundaries (---, ...)                │
│  Output: Vec<RawDocument>                                   │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 2: Context Lexer (context_lexer.rs)                  │
│  - Tokenizes document content                               │
│  - Tracks flow depth (nested {}/[])                         │
│  - Emits INDENT/DEDENT tokens                               │
│  - Context-aware: ,[]{}  are delimiters in flow,            │
│                   part of plain scalars in block            │
│  Output: Vec<Spanned<Token>>                                │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 3: Parser (parser/mod.rs, block.rs, flow.rs,         │
│                   scalar.rs)                                │
│  - Parses tokens into AST (Node tree)                       │
│  - Validates structure and indentation                      │
│  - Resolves anchors and aliases                             │
│  - Validates tag handles                                    │
│  Output: Vec<Node> (Stream)                                 │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│              Output: (Stream, Vec<ParseError>)              │
└─────────────────────────────────────────────────────────────┘
```

---

## Module Structure

### Core Modules

#### `lib.rs` - Public API

- **Main entry point**: `parse(input: &str) -> (Stream, Vec<ParseError>)`
- **Exports**: All public types and functions

#### `span.rs` (~275 lines) - Source Location Tracking

- **`Span`**: Custom struct with `start: usize` and `end: usize` (byte offsets)
  - `new(range)`: Create from a `Range<usize>`
  - `at(pos)`: Create a zero-width span at a position
  - `len()`: Get span length in bytes
  - `union(other)`: Create span encompassing both spans
  - `to_range()`: Convert back to `Range<usize>`
- **`Spanned<T>`**: Type alias for `(T, Span)` tuple
- **`Position`**: 1-based line and column numbers for human-readable positions
- **`SourceMap`**: Converts byte offsets to line/column positions using binary search
  - `position(byte_offset) -> Position`: Get line/column for a byte offset
  - `line_range(line) -> Option<Range<usize>>`: Get byte range for a line
- Every token and node includes its source location

#### `error.rs` (~375 lines) - Error Types

- **`ParseError`**: Contains `kind`, `span`, `span_offset`, `expected`, `found`
  - `global_span()`: Convert document-relative span to input-relative coordinates
  - `suggestion()`: Delegates to `ErrorKind::suggestion()`
- **`ErrorKind`**: 23 error variants organized by category:

  **Syntax Errors:**
  - `UnexpectedEof`, `UnexpectedToken` (generic fallback)
  - `TrailingContent` - content after a value where none allowed
  - `MissingSeparator` - missing comma in flow collections
  - `UnmatchedBracket` - extra closing bracket/brace
  - `ContentOnSameLine` - invalid content on same line as entry
  - `UnexpectedColon` - colon in invalid position

  **Indentation Errors:**
  - `InvalidIndentation`, `InvalidIndentationContext { expected, found }`
  - `TabInIndentation` - tabs not allowed for indentation

  **String Errors:**
  - `UnterminatedString`, `UnterminatedQuotedString { double_quoted }`
  - `InvalidEscape(char)` - invalid escape sequence

  **Anchor/Alias/Tag Errors:**
  - `DuplicateAnchor`, `InvalidAnchor`, `UndefinedAlias`
  - `DuplicateTag`, `InvalidTag`, `PropertiesOnAlias`
  - `UndefinedTagHandle`
  - `MultilineImplicitKey` - implicit keys must be single line

  **Directive Errors:**
  - `DuplicateDirective`, `InvalidDirective`
  - `InvalidNumber`, `InvalidBlockScalar`

- **Error suggestions**: `ErrorKind::suggestion()` returns fix hints for 16+ error types

#### `rich_token.rs` - Wrapper for tokens

- **`RichToken`**: Token wrapper (trivia not currently attached)

  ```rust
  pub struct RichToken {
      pub token: Token,
      pub span: Span,
  }
  ```

- **Design Decision (2026-02-24)**: Trivia attachment was **reverted**
  - Comments remain as real `Token::Comment(_)` tokens in the stream
  - Whitespace remains as real `Token::Whitespace` / `Token::WhitespaceWithTabs` tokens
  - **Reason**: Comments have semantic meaning in YAML - they terminate plain scalars
  - The parser needs to see comments directly to correctly parse multiline plain scalars
  - `RichToken` structure is preserved for future IDE features
  - IDE features can filter/group tokens as needed post-parsing

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

### Layer 1: Stream Lexer

#### `stream_lexer.rs` (~468 lines)

- **Purpose**: Split YAML stream into raw documents
- **Key Types**:
  - `RawDocument<'input>`: Contains directives, content (slice of input), and content span
  - `Directive`: Enum for `%YAML`, `%TAG`, and reserved directives
- **Main Function**: `parse_stream(input) -> (Vec<RawDocument>, Vec<ParseError>)`
- **Responsibilities**:
  - Detect document boundaries (`---`, `...`)
  - Extract directives from document headers
  - Handle multi-document streams
  - Preserve raw content (zero-copy) for next layer
- **Error Handling**: Emits `TrailingContent` errors for invalid content after directives

### Layer 2: Context Lexer

#### `context_lexer.rs` (~1,447 lines)

- **Purpose**: Context-aware tokenization
- **Key Types**:
  - `ContextLexer<'input>`: Main lexer state machine with zero-copy tokenization
  - `LexMode`: `Block` or `Flow` context
- **State Tracking**:
  - `flow_depth`: Nesting level of `{}`/`[]`
  - `indent_stack`: Stack of indentation levels
  - `in_quoted_string`: Inside quoted string?
  - `prev_was_json_like`: For colon detection after JSON-like values
  - `prev_was_separator`: For comment validation (# after whitespace)
  - `byte_pos`: Current position in input (direct string slicing, no Vec<char>)
  - `pending_tokens`: Queue for multi-token constructs
- **Key Innovation**: Context-aware character interpretation
  - In **block context**: `,[]{}` are part of plain scalars
  - In **flow context**: `,[]{}` are delimiters
- **INDENT/DEDENT Tokens**: Python-style indentation tracking
  - Emitted after `LineStart` in block context
  - Used by parser to determine block structure boundaries
- **Modular Token Lexing**: The main `next_token()` method delegates to helper methods:
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

#### `token.rs` (~188 lines)

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

#### `rich_token.rs` (~36 lines)

- **Purpose**: Token wrapper with span information
- **Key Type**: `RichToken<'input>` - combines `Token<'input>` with `Span`
- **Usage**: The context lexer produces `Vec<RichToken>` for the parser

### Layer 3: Parser

#### `parser/mod.rs` (~1,419 lines)

- **Purpose**: Main parser orchestration
- **Key Types**:
  - `Parser<'tokens, 'input>`: Main parser state with two lifetimes for zero-copy
  - `NodeProperties<'input>`: Temporary storage for anchor/tag before value
  - `Stream<'input>`: Type alias for `Vec<Node<'input>>`
- **Parser State**:
  - `tokens`: Reference to token slice (`&'tokens [RichToken<'input>]`)
  - `input`: Reference to input string (`&'input str`)
  - `pos`: Current position in token stream
  - `errors`: Collected errors
  - `anchors`: Map of anchor names to nodes
  - `flow_depth`: Current flow nesting
  - `flow_context_columns`: Stack of flow collection start columns
  - `indent_stack`: Stack of indentation levels
  - `tag_handles`: Map of tag prefixes (from %TAG directives)
- **Main Functions**:
  - `parse_tokens()`: Entry point
  - `parse_single_document()`: Parse one document with zero-copy support
  - `parse_value()`: Dispatch to appropriate parser
  - `collect_node_properties()`: Collect anchor/tag before value
- **Helper Methods**:
  - `handle_flow_collection_as_value()`: Flow mapping/sequence as value
  - `handle_anchor_in_value()`: Anchor property handling
  - `handle_tag_in_value()`: Tag property handling
- **Error Methods**:
  - `error()`: Basic error reporting with specific `ErrorKind`
  - `error_expected()`: Error with expected tokens list
- **Validation**:
  - Indentation rules (uses `InvalidIndentation`, `InvalidIndentationContext`)
  - Flow context column tracking
  - Anchor/alias resolution (`DuplicateAnchor`, `UndefinedAlias`)
  - Tag handle validation (`UndefinedTagHandle`)

#### `parser/scalar.rs` (~1,014 lines)

- **Purpose**: Parse all scalar types
- **Functions**:
  - `parse_scalar()`: Dispatch to appropriate scalar parser
  - `parse_quoted_string()`: Single and double quoted
  - `parse_block_scalar()`: Literal (`|`) and folded (`>`)
  - `parse_plain_multiline()`: Multiline plain scalars
- **Error Handling**: Emits `ContentOnSameLine`, `UnexpectedColon` for specific errors
- **Complexity**: Multiline string handling with indentation validation

#### `parser/flow.rs` (~400 lines)

- **Purpose**: Parse flow collections
- **Functions**:
  - `parse_flow_mapping()`: `{ key: value, ... }`
  - `parse_flow_sequence()`: `[ item, ... ]`
- **Helper Methods**:
  - `enter_flow_collection()` / `exit_flow_collection()`: Depth tracking
  - `handle_flow_comma()`: Consecutive comma detection
  - `handle_flow_entry_end()`: Entry delimiter handling
- **Error Handling**: Emits `MissingSeparator` for missing commas
- **Features**:
  - Tracks flow context columns for indentation validation
  - Handles nested flow collections
  - Validates continuation line indentation

#### `parser/block.rs` (~780 lines)

- **Purpose**: Parse block structures
- **Functions**:
  - `parse_block_sequence()`: Block sequences with `-`
  - `parse_block_mapping()`: Block mappings with `key: value`
- **Key Feature**: Uses INDENT/DEDENT tokens to determine structure boundaries
- **Error Handling**: Emits `TrailingContent`, `MultilineImplicitKey` for specific errors
- **Complexity**: Handles complex indentation rules

---

## Data Flow

### Example: Parsing a Simple Document

**Input:**

```yaml
---
name: Alice
age: 30
```

**Layer 1 (Stream Lexer) Output:**

```rust
RawDocument {
  directives: [],
  content: "name: Alice\nage: 30\n",
  content_span: 4..27
}
```

**Layer 2 (Context Lexer) Output:**

```rust
[
  (LineStart(0), 4..4),
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

**Layer 3 (Parser) Output:**

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

**Context Lexer Behavior:**

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
- `Indent(usize)` - INDENT token (emitted by context lexer)
- `Dedent` - DEDENT token (emitted by context lexer)

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

- `parse(input) -> (Vec<Node<'static>>, ...)` - Convenience API, returns owned data
- `parse_single_document(tokens, input, ...) -> (Option<Node<'input>>, ...)` - Zero-copy API
- `Node::into_owned()` / `Value::into_owned()` - Convert borrowed to owned

**Design Note:** Mappings use `Vec<(Node, Node)>` instead of `HashMap` because:

1. YAML allows duplicate keys (last one wins, but all are preserved)
2. YAML allows non-hashable keys (e.g., sequences or mappings as keys)
3. Preserves insertion order (important for round-tripping)

---

## Key Design Decisions

### 1. Three-Layer Architecture

**Why separate layers?**

- **Separation of Concerns**: Each layer has a single responsibility
  - Stream lexer: Document boundaries
  - Context lexer: Tokenization with context awareness
  - Parser: Structure validation and AST building
- **Testability**: Each layer can be tested independently
- **Maintainability**: Changes to one layer don't affect others
- **Clarity**: Easier to understand and debug

**Alternative Considered:** Single-pass lexer+parser (like `parse_legacy()`)

- **Rejected because**: Context-dependent tokenization is too complex to handle in a single pass
- The legacy lexer is kept for comparison and benchmarking

### 2. INDENT/DEDENT Tokens

**Why Python-style indentation tokens?**

- **Simplifies Parser**: Parser doesn't need to track indentation itself
- **Clear Structure Boundaries**: DEDENT tokens explicitly mark where block collections end
- **Error Recovery**: Easier to recover from indentation errors
- **Matches YAML Semantics**: YAML's block structure is indentation-based

**How it works:**

1. Context lexer maintains an `indent_stack`
2. After each `LineStart(n)` token in block context:
   - If `n > current_indent`: Emit `Indent(n)`, push to stack
   - If `n < current_indent`: Emit `Dedent` for each popped level
   - If `n == current_indent`: No change
3. Parser uses `Dedent` to know when to stop parsing a block collection

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

The context lexer tracks `flow_depth` to determine which interpretation to use.

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
- **Maintainability**: ~7,200 lines of readable, imperative Rust code

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

Errors are collected in `Parser.errors: Vec<ParseError>`:

```rust
pub struct ParseError {
    pub kind: ErrorKind,
    pub span: Span,           // Document-relative byte range
    pub span_offset: usize,   // Add to span for global coordinates
    pub expected: Vec<String>,
    pub found: Option<String>,
}
```

The `span_offset` field enables accurate error positioning in multi-document streams.
Use `error.global_span()` to get the span relative to the original input.

### Recovery Strategies

1. **Invalid Tokens**: Lexer produces `Token::Invalid` for unrecognized input
2. **Invalid Nodes**: Parser produces `Value::Invalid` for unparsable structures
3. **Continue Parsing**: After an error, parser skips to next valid token and continues
4. **Partial Collections**: Collections with errors still include valid elements

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

- **333 tests** covering all YAML 1.2 features
- **331 passing (99.4%)** - 2 tests fail due to tab-in-indentation edge cases
- Tests are in `tests/test_suite.rs`
- Error analysis test (`analyze_error_kinds`) tracks error distribution across 440+ error test cases

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

Each module has unit tests for specific functionality:

- Stream lexer tests: Document splitting, directive extraction
- Context lexer tests: Tokenization, flow depth tracking, INDENT/DEDENT
- Parser tests: AST building, error recovery, anchor resolution

### Test Organization

Following user preference:

- **Data-driven tests**: Loop over predefined test sets
- **Separate datasets**: Positive and negative test cases in different datasets
- **Not many individual test functions**: Use parameterized tests

---

## Known Limitations

See `TECHNICAL_DEBT.md` for comprehensive documentation. Key limitations:

### 1. Performance

- Not optimized for speed (focus has been on correctness)
- Three-layer architecture has some overhead
- Block scalars still require allocation for processing (chomping, folding)

### 2. Memory Usage

- Main `parse()` API returns owned data for convenience
- Use `parse_single_document()` for zero-copy parsing when needed
- Could use arena allocation for even better performance

### 3. Unicode Handling

- Basic Unicode support, but not fully tested
- Could improve handling of Unicode edge cases

### 4. Streaming

- Currently parses entire input at once
- Could support streaming for large files

---

## Future Improvements

### Recently Completed (2026-02-27)

**Dependency Removal:**

1. ✅ **Chumsky Removal** - Completely removed the `chumsky` parser combinator library
   - Implemented custom `Span` struct (was `chumsky::span::SimpleSpan`)
   - `Span::new(range)` creates from `Range<usize>`
   - `Span::at(pos)` for zero-width spans, `Span::union()` for merging
   - **Zero external dependencies** - entire crate is now self-contained

**Error Improvements:**

1. ✅ **Specific Error Kinds** - Replaced 87% of generic `UnexpectedToken` errors
   - **Before:** 535 `UnexpectedToken` occurrences across 120 test cases
   - **After:** 69 `UnexpectedToken` occurrences across 26 test cases
   - New error kinds with actionable suggestions:
     - `TrailingContent` (346 occurrences) - "Remove extra content after value"
     - `ContentOnSameLine` (62) - "Move content to new indented line"
     - `MissingSeparator` (32) - "Add comma between items"
     - `MultilineImplicitKey` (17) - "Use explicit key syntax"
     - `UnexpectedColon` (9) - "Use quoted string for colons in values"
     - `UnmatchedBracket` - "Check for extra closing brackets"

**Lexer Improvements:**

1. ✅ **Token Module Extraction** - Token types moved to dedicated `token.rs`
2. ✅ **Zero-Allocation Character Iteration** - Direct string slicing (no `Vec<char>`)
3. ✅ **SourceMap Utility** - Line/column position tracking for IDE integration
4. ✅ **State Machine Extraction** - `next_token()` refactored from ~230 to ~45 lines
5. ✅ **Unified Quoted String Handling** - Shared helpers reduce duplication
6. ✅ **Zero-Copy Tokenization** - `Token<'input>` uses `Cow<'input, str>`
   - Borrows directly from input for comments, anchor names
   - Allocates only when content is transformed (escapes, trimming)

**Parser Infrastructure Improvements:**

1. ✅ **Property Collection Helper** - `collect_node_properties()` method
   - Reduces code duplication for anchor/tag collection loops
2. ✅ **Method Extraction** - Token-specific helpers in mod.rs
    - `handle_flow_collection_as_value()` - Flow mapping/sequence handling
    - `handle_anchor_in_value()` / `handle_tag_in_value()` - Property handling
    - Reduced `parse_value_with_properties` from ~300 to ~127 lines
3. ✅ **Flow Collection Unification** - Common flow utilities in flow.rs
    - `enter_flow_collection()` / `exit_flow_collection()` - Depth tracking
    - `handle_flow_comma()` / `handle_flow_entry_end()` - Entry handling
4. ✅ **Zero-Copy Scalar Parsing** - Reduced allocations
    - `Node<'input>` and `Value<'input>` use lifetimed `Cow<'input, str>`
    - `into_owned()` methods convert to `'static` lifetime when needed

See `LEXER_IMPROVEMENTS.md` and `PARSER_IMPROVEMENTS.md` for detailed progress tracking.

### Short-Term (Feature Additions)

1. **Schema Validation**
   - Support for YAML schemas
   - Type validation

2. **Pretty Printing**
   - Format YAML output
   - Preserve comments and formatting

### Long-Term (Major Changes)

1. **Streaming API**
   - Support for large files
   - Event-based parsing

2. **YAML 1.3 Support**
   - When YAML 1.3 spec is finalized
   - Backward compatibility with 1.2

---

## References

- **YAML 1.2 Specification**: https://yaml.org/spec/1.2/spec.html
- **YAML Test Suite**: https://github.com/yaml/yaml-test-suite
- **Technical Debt Documentation**: See `TECHNICAL_DEBT.md` in this directory

---

## Conclusion

This YAML parser achieves **99.4% YAML 1.2 compliance** (331/333 tests) through a carefully designed three-layer architecture with **zero external dependencies**:

1. **Stream Lexer**: Handles document boundaries and directives
2. **Context Lexer**: Provides context-aware tokenization with INDENT/DEDENT
3. **Parser**: Builds AST with error recovery

**Key achievements:**

- **Zero dependencies**: Self-contained crate with custom `Span` implementation
- **Zero-copy parsing**: `Cow<'input, str>` throughout for minimal allocations
- **Actionable errors**: 23 error kinds with specific suggestions (87% reduction in generic errors)
- **~7,200 lines**: Readable, hand-written recursive descent parser

The architecture prioritizes **correctness** and **error recovery** over performance, making it suitable for IDE integration and user-facing tools where helpful error messages are crucial.

The codebase is clean of clippy warnings and ready for production use.
