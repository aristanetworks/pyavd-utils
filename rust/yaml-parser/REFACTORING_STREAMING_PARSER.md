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

### Step 1: Track Indent State (Low Risk) ✅

**Goal**: Eliminate backward-scanning in `current_indent()`.

**Changes**:

1. Add `current_line_indent: IndentLevel` field to `Parser` struct
2. Update it in `advance()` when consuming `LineStart` tokens
3. Replace `current_indent()` to return the tracked value

### Step 2: Add Peek Buffer (Medium Risk)

**Goal**: Replace direct slice access with bounded peek buffer.

**Changes**:

1. Replace `&[RichToken]` with iterator + `VecDeque` buffer
2. Add `peek_nth(n: usize)` method that fills buffer as needed
3. Update all `self.tokens.get(self.pos + n)` to use `peek_nth(n)`

### Step 3: Remove Backtracking (Low Risk)

**Goal**: Convert position save/restore patterns to peek-only.

**Changes**:

1. `check_trailing_content_at_root()` → use `peek_nth()` without advancing
2. `parse_value_with_properties()` → count dedents via `peek_nth()` without consuming

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

- [ ] Step 1: Track indent state
- [ ] Step 2: Add peek buffer
- [ ] Step 3: Remove backtracking
- [ ] Step 4: Callback-based emission (design pending)
- [ ] Step 5: Iterator parser (breakdown pending)

## Test Compliance

All changes must maintain 100% YAML 1.2 compliance (842/842 tests).
