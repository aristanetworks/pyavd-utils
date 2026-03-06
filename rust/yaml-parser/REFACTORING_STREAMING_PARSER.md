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

## Phase 2: Iterator Parser via State Machine

The goal is to make the Parser implement `Iterator<Item = Event>` for true event-by-event
streaming. This requires converting the recursive descent parser into an explicit state machine.

### Core Insight

The state machine replaces the **call stack**. Currently:

```text
parse_document()
  → parse_value()
    → parse_block_sequence()
      → parse_value()  // for each entry
```

With a state machine:

```text
state_stack: [Document::Content, BlockSequence::Entry, Value::Pending]
```

When we produce an event, we return it. When `next()` is called again, we look at the
stack and continue from where we left off.

### Step 4: Iterator Shell (No Behavior Change)

**Goal**: Create `StreamingParser` wrapper with `Iterator` interface.

**Changes**:

1. Create `StreamingParser` that wraps existing `Parser`
2. Implement `Iterator<Item = Event>` by draining from internal buffer
3. Buffer is filled by calling existing batch parsing methods
4. Add comparison test: both parsers must produce identical event sequences

```rust
pub struct StreamingParser<'input> {
    parser: Parser<'_, 'input>,
    buffer: VecDeque<Event<'input>>,
}

impl Iterator for StreamingParser<'_> {
    type Item = Event<'_>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(e) = self.buffer.pop_front() {
            return Some(e);
        }
        if !self.parser.is_eof() {
            self.parser.parse_document_events();
            std::mem::swap(&mut self.buffer, &mut self.parser.events);
            return self.buffer.pop_front();
        }
        None
    }
}
```

**Test**: Run both parsers on all 842 test cases, assert identical events.

### Step 5: Document-Level States

**Goal**: Extract document start/end into state-driven emission.

**Changes**:

1. Add `StreamState` enum for document-level states
2. Handle `DocumentStart` and `DocumentEnd` events directly in `next()`
3. Delegate content parsing to existing recursive code

```rust
enum StreamState {
    Ready,
    EmitDocStart { has_marker: bool, span: Span },
    ParseDocContent { min_indent: IndentLevel },  // delegates to old code
    EmitDocEnd { span: Span },
    BetweenDocs,
}
```

**Test**: Still identical output, but doc boundaries are now state-driven.

### Step 6: Root Value Dispatch

**Goal**: State-driven dispatch at the value level.

**Changes**:

1. When in `ParseDocContent`, inspect token and push appropriate state
2. Each value type becomes a state that (initially) delegates to old code

```rust
enum ValueState {
    Dispatch,
    BlockSequence { indent: IndentLevel },  // still delegates
    BlockMapping { indent: IndentLevel },
    FlowCollection,
    Scalar,
    Null,
}
```

### Step 7+: Convert Collections One at a Time

**Goal**: Replace delegation with true state-driven parsing.

Pick one construct at a time (simplest first):

1. **Flow sequences** → `FlowSeqState { EmitStart, BeforeEntry, AfterEntry, EmitEnd }`
2. **Flow mappings** → similar states
3. **Block sequences** → `BlockSeqState { ... }`
4. **Block mappings** → `BlockMapState { ... }`
5. **Scalars** (plain, quoted, block)

Each construct conversion:

- Add states for that construct
- When state needs to parse a nested value, push `ValueState::Dispatch`
- When nested parsing completes, control returns to parent state
- Maintain test compatibility throughout

### Error Recovery in State Machine

Error recovery translates cleanly to the state machine:

1. Errors are still collected in `self.errors: Vec<ParseError>`
2. When an error is detected, we can push recovery states:

```rust
enum StreamState {
    // ... normal states ...

    // Error recovery states
    RecoverToIndent { target_indent: IndentLevel },
    RecoverToFlowEnd { depth: usize },
    RecoverToDocBoundary,
}
```

1. Recovery becomes explicit stack manipulation instead of implicit call stack unwinding
2. This is actually **cleaner** than recursive recovery because recovery points are visible

**Example**: Unterminated flow sequence

```rust
// In next():
FlowSeqState::BeforeEntry => {
    if self.is_eof() {
        self.error(ErrorKind::UnterminatedFlow, span);
        self.state_stack.pop();  // explicit "unwind"
        return Some(Event::SequenceEnd { ... });
    }
    // ... normal parsing
}
```

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
│  StreamingParser::new(lexer)                                │
│  - Implements Iterator<Item = Event>                        │
│  - State machine with explicit stack                        │
│  - Consumes tokens lazily via peek buffer                   │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  Consumer (EventParser, direct iteration, etc.)             │
│  - Processes events one at a time as they arrive            │
└─────────────────────────────────────────────────────────────┘
```

## Progress Tracking

### Phase 1: Preparation (Complete)

- [x] Step 1: Track indent state ✅
- [x] Step 2: Add peek buffer interface ✅
- [x] Step 3: Remove backtracking ✅

### Phase 2: State Machine Transformation

- [ ] Step 4: Iterator shell with comparison test
- [ ] Step 5: Document-level states
- [ ] Step 6: Root value dispatch
- [ ] Step 7+: Convert collections (flow seq, flow map, block seq, block map, scalars)

## Test Compliance

All changes must maintain 100% YAML 1.2 compliance (842/842 tests).

**Current Status**: 842/842 tests passing after Steps 1-3.

**Regression Safety**: A comparison test will verify that `StreamingParser` produces
identical event sequences to the batch `Parser` throughout the transformation.
