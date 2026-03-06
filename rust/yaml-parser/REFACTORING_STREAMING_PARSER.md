<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Streaming Parser Refactoring Plan

This document describes the incremental refactoring plan to transform the YAML parser
from a batch-oriented architecture to a true streaming architecture.

## Current State (After Phase 4)

The **lexer** is now streaming-capable:

- `DocumentLexer` implements `Iterator<Item = RichToken>`
- Handles directives, document markers, and content in a single pass

The **parser** is still batch-oriented:

- Takes `&[RichToken]` (full token slice)
- Emits all events into `Vec<Event>`
- Uses backward-scanning and bounded lookahead

```text
┌─────────────────────────────────────────────────────────────┐
│  DocumentLexer::new(input).collect::<Vec<_>>()              │  ← Streaming capable
│  Output: Vec<RichToken>                                     │  ← But collected into Vec
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Parser::new(&tokens, input)                                │  ← Takes &[RichToken]
│  - Processes all tokens                                     │
│  - Emits all events into Vec<Event>                         │  ← Batch output
└─────────────────────────────────────────────────────────────┘
```

## Obstacles to Streaming

### 1. Backward Scanning

`current_indent()` scans all previously consumed tokens to find the most recent `LineStart`:

```rust
pub fn current_indent(&self) -> IndentLevel {
    self.tokens.get(..self.pos).iter().rev().find_map(...)
}
```

### 2. Bounded Lookahead

Many places use `self.tokens.get(self.pos + n)` for lookahead validation.

### 3. Backtracking (Only 2 places!)

- `check_trailing_content_at_root()` - saves position, looks ahead, restores
- `parse_value_with_properties()` - peeks at dedented content

### 4. Batch Event Collection

Events are pushed to `self.events: Vec<Event>`.

---

## Transformation Steps

### Step 1: Track Indent State (Low Risk) ✅ COMPLETE

**Goal**: Eliminate backward-scanning in `current_indent()`.

**Changes**:

1. ✅ Added `current_line_indent: IndentLevel` field to `Parser` struct
2. ✅ Updated `advance()` to track `LineStart` tokens when consumed
3. ✅ Replaced `current_indent()` to return the tracked value (simple field access)

### Step 2: Add Peek Buffer (Medium Risk) ✅ COMPLETE

**Goal**: Abstract lookahead access for streaming compatibility.

**Changes**:

1. ✅ Added `peek_nth(n: usize)` method to Parser
2. ✅ Converted lookahead patterns in `block.rs`, `flow.rs`, `mod.rs`, `scalar.rs`
3. 🔜 Full buffer with iterator input (deferred until Step 5)

**Note**: Currently `peek_nth()` is backed by slice indexing. When we switch to
iterator input in Step 5, we'll replace the backing with a `VecDeque` buffer.

### Step 3: Remove Backtracking (Low Risk) ✅ COMPLETE

**Goal**: Convert position save/restore patterns to peek-only.

**Changes**:

1. ✅ `check_trailing_content_at_root()` → peek-only loop using `peek_nth()`
2. ✅ `parse_value_with_properties()` → peek-only lookahead with count, then consume

---

## Future Steps (To Be Designed)

### Step 4: Callback-Based Emission (TBD)

**Idea**: Replace `self.events.push(event)` with callback.

**Open Questions**:

- Should it be a callback `FnMut(Event)` or a channel?
- How to handle errors alongside events?
- Impact on API and backwards compatibility?

### Step 5: Iterator Parser (TBD - Needs Breakdown)

**Goal**: Parser becomes `Iterator<Item = Event>`.

**Challenges**:

- Recursive descent doesn't map naturally to iterators
- Need explicit state machine or coroutine-like approach
- Complex state management for nested structures

**Possible Sub-Steps** (to be refined):

- 5a: Extract parsing state into explicit enum
- 5b: Convert outer loop to iterator pattern
- 5c: Handle nested structure state
- 5d: Full iterator implementation

---

## Target Architecture

```text
┌─────────────────────────────────────────────────────────────┐
│  DocumentLexer::new(input)                                  │
│  Output: Iterator<Item = RichToken>                         │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Parser::new(lexer)                                         │
│  - Consumes tokens lazily via peek buffer                   │
│  - Emits events via callback or as iterator                 │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Consumer (EventParser, direct callback, etc.)              │
│  - Processes events as they arrive                          │
└─────────────────────────────────────────────────────────────┘
```

## Progress Tracking

- [x] Step 1: Track indent state ✅
- [x] Step 2: Add peek buffer (interface ready, full buffer deferred) ✅
- [x] Step 3: Remove backtracking ✅
- [ ] Step 4: Callback-based emission (design pending)
- [ ] Step 5: Iterator parser (breakdown pending)

## Test Compliance

All changes must maintain 100% YAML 1.2 compliance (842/842 tests).

**Current Status**: 842/842 tests passing after Steps 1-3.
