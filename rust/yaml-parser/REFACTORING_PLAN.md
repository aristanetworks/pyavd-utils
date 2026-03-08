<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Code Structure Refactoring Plan

## Goal

Break up large files into maintainable modules optimized for both LLM and human comprehension.

**Target file size**: 200-500 lines (ideal), 800 lines (maximum)

**Rationale**:

- LLMs can view ~100 lines at a time with context
- Humans can understand a file in one sitting
- Single responsibility principle
- Easier testing and debugging
- Better code navigation

## Current State

| File                        | Lines | Status         |
|-----------------------------|-------|----------------|
| `src/emitter/emitter.rs`    | 5,511 | ❌ Too large   |
| `src/lexer/document.rs`     | 1,901 | ❌ Too large   |
| `src/parser/mod.rs`         | 688   | ✅ Acceptable  |

## Detailed File Breakdown

### Current vs Proposed

| Current File            | Lines     | →   | Proposed Files       | Lines Each  | Total     |
|-------------------------|-----------|-----|----------------------|-------------|-----------|
| `lexer/document.rs`     | 1,901     | →   | 9 files (see below)  | 100-300     | 1,900     |
| `emitter/emitter.rs`    | 5,511     | →   | 14 files (see below) | 150-500     | 5,500     |
| `parser/mod.rs`         | 688       | →   | 4 files              | 150-200     | 700       |
| **Total**               | **8,100** | →   | **27 files**         | **avg 300** | **8,100** |

## Proposed Structure

### 1. Lexer Module (`src/lexer/`)

**Current**: Single 1,901-line file
**Proposed**: Split into logical concerns

```text
src/lexer/
├── mod.rs                    (~100 lines) - Public API, re-exports
├── lexer.rs                  (~300 lines) - Core Lexer struct, Iterator impl
├── token.rs                  (~200 lines) - Token types (already exists)
├── rich_token.rs             (~50 lines)  - RichToken wrapper (already exists)
├── directives.rs             (~150 lines) - Directive parsing (%YAML, %TAG)
├── scalars/
│   ├── mod.rs                (~50 lines)  - Scalar parsing coordination
│   ├── plain.rs              (~200 lines) - Plain scalar lexing
│   ├── single_quoted.rs      (~150 lines) - Single-quoted string lexing
│   └── double_quoted.rs      (~200 lines) - Double-quoted string lexing
├── flow.rs                   (~150 lines) - Flow indicator handling
├── block.rs                  (~150 lines) - Block scalar headers, markers
├── whitespace.rs             (~150 lines) - Whitespace, newlines, indentation
└── helpers.rs                (~100 lines) - Utility functions (peek, advance, etc.)
```

**Benefits**:

- Each scalar type is self-contained
- Flow vs block concerns separated
- Directive handling isolated
- Easy to find and modify specific token types

### 2. Emitter Module (`src/emitter/`)

**Current**: Single 5,511-line file
**Proposed**: Split by parsing concern

```text
src/emitter/
├── mod.rs                    (~150 lines) - Public API, re-exports
├── emitter.rs                (~400 lines) - Core Emitter struct, Iterator impl, state stack
├── state.rs                  (~200 lines) - ParseState enum and phase enums
├── document.rs               (~300 lines) - Document-level parsing (prepare, finish, directives)
├── value.rs                  (~400 lines) - Value dispatch (parse_value, properties)
├── block/
│   ├── mod.rs                (~50 lines)  - Block parsing coordination
│   ├── sequence.rs           (~400 lines) - Block sequence parsing
│   ├── mapping.rs            (~500 lines) - Block mapping parsing
│   └── scalar.rs             (~400 lines) - Block scalar parsing (literal, folded)
├── flow/
│   ├── mod.rs                (~50 lines)  - Flow parsing coordination
│   ├── sequence.rs           (~350 lines) - Flow sequence parsing
│   └── mapping.rs            (~400 lines) - Flow mapping parsing
├── scalar.rs                 (~300 lines) - Scalar parsing (plain, quoted)
├── syntax_checks.rs          (~400 lines) - YAML syntax error detection (tabs, multiline keys, etc.)
├── indent.rs                 (~150 lines) - Indentation tracking
├── whitespace.rs             (~150 lines) - Whitespace skipping utilities
└── helpers.rs                (~200 lines) - Utility functions (peek, lookahead, etc.)
```

**Key Groupings**:

- **State management**: `state.rs` - All state enums in one place
- **Document lifecycle**: `document.rs` - Start, end, directives
- **Value dispatch**: `value.rs` - Main entry point for parsing values
- **Block structures**: `block/` - All block-context parsing
- **Flow structures**: `flow/` - All flow-context parsing
- **Syntax checking**: `syntax_checks.rs` - Error detection (NOT related to schema validation crate)
- **Infrastructure**: `indent.rs`, `whitespace.rs`, `helpers.rs`

**Benefits**:

- Block vs flow parsing clearly separated
- State definitions in one place
- Each file has clear responsibility
- Easier to test individual components

### 3. Parser Module (`src/parser/`)

**Current**: Single 688-line file
**Proposed**: Split into 4 focused files

```text
src/parser/
├── mod.rs                    (~200 lines) - Main orchestration and public API
├── document.rs               (~150 lines) - Document parsing
├── node.rs                   (~200 lines) - Node construction helpers
└── anchor.rs                 (~150 lines) - Anchor/alias resolution
```

**Benefits**:

- Clearer separation of concerns
- Document parsing logic isolated
- Anchor resolution logic in dedicated file
- Easier to find and modify specific functionality

### 4. Event Module

**Current**: Two-file module with minimal content
**Proposed**: Consolidate into single file

```text
src/event.rs                  (~200 lines) - Event enum and types
```

**Rationale**: Similar to other supporting modules (error.rs, span.rs, value.rs), the event types are simple enough to live in a single file

### 5. Supporting Modules

**Current**: Already well-structured
**Proposed**: Minor changes

```text
src/
├── error.rs                  - Error types
├── span.rs                   - Source location tracking
├── value.rs                  - AST node types
├── event.rs                  - Event types (consolidated from event/)
└── lib.rs                    - Public API
```

**Note**: The emitter contains ~400 lines of error checking functions (like `check_multiline_implicit_key()`, `check_tabs_as_indentation()`, etc.). These will remain in the emitter as they are tightly coupled to the parsing state machine. They are NOT related to the separate `validation` crate which handles schema validation.

## Migration Strategy

### Phase 1: Lexer Refactoring (Low Risk)

1. Create new module structure
2. Move code to new files
3. Update imports
4. Run tests (should pass without changes)
5. Commit

**Estimated effort**: 4-6 hours

### Phase 2: Event Consolidation (Low Risk)

1. Create `src/event.rs`
2. Move content from `src/event/types.rs` and `src/event/mod.rs`
3. Remove `src/event/` directory
4. Update imports
5. Run tests
6. Commit

**Estimated effort**: 1 hour

### Phase 3: Emitter Refactoring (Medium Risk)

1. Create new module structure
2. Extract state definitions to `state.rs`
3. Split block/flow parsing
4. Move helper functions
5. Update imports
6. Run tests
7. Commit

**Estimated effort**: 8-12 hours

### Phase 4: Documentation Update

1. Update `ARCHITECTURE.md` with new structure
2. Add module-level documentation
3. Update examples if needed

**Estimated effort**: 2-3 hours

## Testing Strategy

- Run full test suite after each phase
- No behavior changes - pure refactoring
- All 496 YAML Test Suite tests must pass
- No new clippy warnings

## Benefits Summary

### For LLMs

- Can view entire files in context window
- Clear file names indicate purpose
- Less scrolling/searching needed
- Better code retrieval with codebase-retrieval tool

### For Humans

- Easier to navigate codebase
- Clear separation of concerns
- Faster to find relevant code
- Easier to onboard new developers
- Better IDE performance (smaller files)

### For Maintenance

- Easier to test individual components
- Clearer git diffs
- Reduced merge conflicts
- Better code review experience

## Next Steps

1. ✅ Review and approve this plan
2. Commit previous changes
3. Execute Phase 1 (Lexer) - ~6 hours
4. Execute Phase 2 (Event) - ~1 hour
5. Execute Phase 3 (Emitter) - ~12 hours
6. Execute Phase 4 (Documentation) - ~3 hours
7. Review

**Total estimated effort**: 21-23 hours

## Execution Notes

### Phase 1: Lexer Refactoring - Skipped

**Deviation from Plan**: After analyzing the actual code structure in `src/lexer/document.rs` (1,901 lines), the proposed 9-file split with a `scalars/` subdirectory is not optimal for this codebase. The lexer is highly integrated with shared state and helper methods. A more conservative approach is needed to maintain code cohesion and avoid breaking changes.

**Decision**: Skipping lexer refactoring for now. The lexer will remain as `document.rs`. The focus will shift to Phases 2-4 which offer clearer separation boundaries and better maintainability gains.

### Phase 2: Event Consolidation - Complete ✅

**Status**: Successfully consolidated `src/event/` directory into single `src/event.rs` file (416 lines).

**Changes**:

- Merged `src/event/mod.rs` and `src/event/types.rs` into `src/event.rs`
- Deleted old `src/event/` directory
- All tests pass (97 unit tests + 44 integration tests + YAML test suite)

**Result**: Cleaner module structure, easier to navigate. Event types are now in a single, focused file similar to other supporting modules like `error.rs`, `span.rs`, and `value.rs`.
