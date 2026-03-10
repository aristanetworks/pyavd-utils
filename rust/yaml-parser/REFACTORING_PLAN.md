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
- Committed: c3b5f8d

**Result**: Cleaner module structure, easier to navigate. Event types are now in a single, focused file similar to other supporting modules like `error.rs`, `span.rs`, and `value.rs`.

### Phase 3: Emitter Refactoring - Detailed Value/State Plan (2026-03)

This section refines the original "Phase 3: Emitter Refactoring" into a more
focused, incremental plan centered on **value parsing and state management**
rather than immediately splitting files. The goal is to move complexity from
helper-heavy logic into explicit `ParseState` variants and a `ValueContext`
type, while keeping all existing tests passing after each step.

#### Target Architecture

- `ParseState::Value` holds a `ValueContext` struct describing what kind of
  value we are parsing (key, mapping value, sequence entry, top-level value)
  and its indentation constraints.
- Subtle behaviors (bridging vs empty value, complex keys, alias/flow
  collections as keys, property dedent handling) are modeled via dedicated
  micro-states (e.g. `PendingProperties`) rather than large helper
  functions.
- Error/recovery behavior is encoded per state + token, ideally in small,
  clearly named handlers.

#### Phases

**Phase E1: Introduce `ValueContext` (no behavior change)**

- Define:
  - `enum ValueKind { Key, MappingValue, SeqEntryValue, TopLevelValue }`
  - `struct ValueContext { min_indent: IndentLevel, kind: ValueKind,
    allow_implicit_mapping: bool }`
- Update `ParseState::Value` to store a `ValueContext` instead of separate
  fields.
- Adapt the main emitter loop to construct/pass `ValueContext` into
  `parse_value` and related helpers (initially via simple field mapping).
- Keep all parsing behavior identical; this phase is purely structural.
- **Tests:** `cargo test -p yaml-parser` and
  `cargo clippy -p yaml-parser --all-targets`.

**Phase E2: `PendingProperties` state for bridging vs empty value**

- Introduce a new state:
  - `ParseState::PendingProperties { ctx: ValueContext, anchor, tag,
    initial_crossed_line, property_indent }`.
- Refactor current logic in
  `maybe_emit_empty_scalar_for_non_bridging_properties` so that:
  - Cases that must emit an empty scalar still do so directly.
  - In "bridging possible" cases, instead of returning `(None, anchor, tag)`,
    we push `PendingProperties` and return `None`.
- Add a `process_pending_properties` handler that:
  - Either emits an empty scalar and pops, or
  - Transitions to a normal `Value` state with finalized `anchor/tag` and
    re-enters `parse_value`.
- This phase is still behavior-preserving; the same decisions happen, but are
  now explicitly state-driven.
- **Tests:** `cargo test -p yaml-parser` after the refactor.

### Phase E3: Alias/flow complex-key handling as states

- Introduce dedicated states for cases currently handled by helpers such as
  `handle_alias_value` and `handle_flow_*_value`, for example:
  - `ParseState::AliasValue { ctx: ValueContext, alias_name, span, anchor,
    tag, crossed_line_info }`
  - `ParseState::FlowCollectionValue { ctx: ValueContext, is_map, span,
    anchor, tag }`
- Move the complex-key vs plain-value decision logic into these state
  handlers, keeping semantics identical.
- Ensure all complex-key edge cases (aliases, flow sequences/mappings used as
  keys) remain covered by existing tests.
- **Tests:** `cargo test -p yaml-parser`.

### Phase E4: Error/recovery specialization

- For each major error behavior (e.g. `InvalidIndentation`,
  `DocumentMarkerInFlow`, `TrailingContent`, `OrphanedProperties`):
  - Identify which states and tokens can produce it.
  - Either:
    - Move logic into tiny `handle_*` helpers used from state handlers, or
    - Introduce explicit recovery states if multi-step corrections are
      needed.
- Aim for each state arm in the main loop to be small and to clearly describe
  both normal and error transitions.
- **Tests:** `cargo test -p yaml-parser` and
  `cargo clippy -p yaml-parser --all-targets` at the end of the phase.

#### Execution Log (Emitter Value/State Refactor)

- **2026-03-09:** Plan written down in `REFACTORING_PLAN.md` under
  "Phase 3: Emitter Refactoring - Detailed Value/State Plan". No code changes
  specific to this plan yet; emitter helpers were previously cleaned up and
  tests/clippy are passing.
- **2026-03-09:** Phase E1 started: introduced `ValueKind` and `ValueContext`
  in `states.rs`, changed `ParseState::Value` to hold a `ValueContext`, and
  updated `Emitter::parse_value` and all `ParseState::Value` push sites to use
  it. Semantics unchanged (refactoring only).
  - Tests: `cargo test -p yaml-parser` ✅
  - Clippy: `cargo clippy -p yaml-parser --all-targets` ✅

- **2026-03-09:** Phase E2 started: split `parse_value` into two phases driven
  by states.
  - Added `ParseState::ValueAfterProperties { ctx, anchor, tag,
    initial_crossed_line, prop_crossed_line, property_indent }`.
  - `ParseState::Value` now performs only whitespace skipping, invalid-indent
    checks, and property collection, then pushes `ValueAfterProperties`.
  - New `Emitter::process_value_after_properties` hosts the former second half
    of `parse_value`, including bridging vs empty-value decisions and the main
    token dispatch. Existing helpers such as
    `maybe_emit_empty_scalar_for_non_bridging_properties` are now used from
    this state handler.
  - Semantics remain unchanged; behaviour is validated by the test suite.
  - Tests: `cargo test -p yaml-parser` ✅
  - Clippy: `cargo clippy -p yaml-parser --all-targets` ✅

- **2026-03-09:** Phase E3 (alias part) started: moved alias value handling
  into an explicit `AliasValue` state.
  - Added `ParseState::AliasValue { ctx: ValueContext, name, span, anchor, tag,
    crossed_line_after_properties }`.
  - The alias match arm in `process_value_after_properties` now defers to this
    state instead of calling a helper directly.
  - New `Emitter::process_alias_value_state` implements the previous
    `handle_alias_value` logic, driven by the state machine.
  - Behaviour verified via the existing alias/anchor tests and full suite.
  - Tests: `cargo test -p yaml-parser` ✅
  - Clippy: `cargo clippy -p yaml-parser --all-targets` ✅

- **2026-03-09:** Phase E3 (flow collection part) started: moved flow
  collection value handling into an explicit `FlowCollectionValue` state.
  - Added `ParseState::FlowCollectionValue { ctx: ValueContext, is_map, span,
    anchor, tag }`.
  - The `FlowSeqStart` / `FlowMapStart` arms in `process_value_after_properties`
    now defer to this state instead of calling `handle_flow_*_start_value`
    helpers.
  - New `Emitter::process_flow_collection_value_state` implements the
    previous flow complex-key logic (including `is_flow_*_complex_key`) and
    regular flow value handling, driven by the state machine.
  - Behaviour verified via the existing flow and complex-key tests and full
    suite.
  - Tests: `cargo test -p yaml-parser` ✅
  - Clippy: `cargo clippy -p yaml-parser --all-targets` ✅

- **2026-03-09:** Phase E4 (first step) started: centralized
  `DocumentMarkerInFlow` handling in flow sequence/mapping states.
  - Added tiny helpers `handle_doc_marker_in_flow_seq` and
    `handle_doc_marker_in_flow_map` used from the `FlowSeq` and `FlowMap`
    state handlers instead of inlined error+recovery code.
  - Behaviour is unchanged: document markers inside flow still record
    `DocumentMarkerInFlow`, consume the marker, and re-push the current flow
    state to continue parsing.
  - Tests: `cargo test -p yaml-parser` ✅
  - Clippy: `cargo clippy -p yaml-parser --all-targets` ✅

- **2026-03-09:** Phase E4 (additional-properties state) started: moved
  `handle_additional_properties_value` logic into an explicit
  `AdditionalPropertiesValue` state.
  - Added `ParseState::AdditionalPropertiesValue { ctx: ValueContext,
    outer_anchor, outer_tag }`.
  - The `Anchor` / `Tag` arm in `process_value_after_properties` now defers
    to this state instead of calling the helper directly.
  - New `Emitter::process_additional_properties_value_state` is the
    state-driven form of the old helper and owns multi-layer property and
    complex-key behaviour.
  - Behaviour verified via existing complex-key/property tests and full
    suite.
  - Tests: `cargo test -p yaml-parser` ✅
  - Clippy: `cargo clippy -p yaml-parser --all-targets` ✅

- **2026-03-10:** Phase E4 (dedent-empty helper) started: extracted the
  dedent-based empty-value decision from `process_value_after_properties`
  into `maybe_emit_empty_due_to_dedent`.
  - The new helper encapsulates the rules for when a dedent after crossing a
    line means "this value is empty" versus when a block collection indicator
    starts a nested value instead.
  - Behaviour is unchanged; the helper is called from
    `process_value_after_properties` after bridging decisions.
  - Tests: `cargo test -p yaml-parser` ✅
  - Clippy: `cargo clippy -p yaml-parser --all-targets` ✅

- **2026-03-10:** Phase E4 (small property helpers) continued: introduced
  `properties_belong_to_null_key` and `collect_additional_inner_properties`.
  - `properties_belong_to_null_key` centralises the `props_for_key` logic
    around implicit null keys (e.g. `!!null : a` and `-\n  !!null : a`).
  - `collect_additional_inner_properties` owns the "third layer" property
    merging for `AdditionalPropertiesValue` so the state handler reads more
    declaratively.
  - Behaviour validated by the full test suite.
  - Tests: `cargo test -p yaml-parser` ✅
  - Clippy: `cargo clippy -p yaml-parser --all-targets` ✅

### Phase 4: Parser Refactoring - Skipped ✅

**Decision**: Parser refactoring not needed.

**Rationale**:

- Current size: 688 lines (under the 800-line maximum target)
- Well-structured with clear sections (parsing logic, type inference, anchor tracking)
- Already marked as "✅ Acceptable" in the current state analysis
- Splitting into 4 files (~170 lines each) would not provide significant benefits
- The file is already easy to navigate and understand

**Result**: Parser remains as single `mod.rs` file.

### Phase 5: Documentation Update - Complete ✅

**Status**: Updated ARCHITECTURE.md to reflect event module consolidation

**Changes**:

- Updated `event/mod.rs` reference to `event.rs`
- Updated `parser/event_parser.rs` reference to `parser/mod.rs` (correct file name)
- Added note about event consolidation in Layer 3 documentation
- Corrected Parser implementation details

## Refactoring Summary

### Completed Work

**Phase 2: Event Consolidation** ✅

- Consolidated `src/event/` directory (2 files) into single `src/event.rs` (416 lines)
- Deleted `src/event/mod.rs` and `src/event/types.rs`
- All tests pass (97 unit + 44 integration + YAML test suite)
- Committed: c3b5f8d

**Phase 5: Documentation** ✅

- Updated ARCHITECTURE.md with correct file references
- Documented event consolidation rationale

### Deferred Work

**Phase 1: Lexer Refactoring** ⏸️

- **Reason**: Lexer (1,901 lines) is highly integrated with shared state
- **Decision**: Keep as single `document.rs` file to maintain code cohesion
- **Future**: Consider refactoring when there's a specific need

**Phase 3: Emitter Refactoring** ⏸️

- **Reason**: Emitter (5,511 lines) has complex state management
- **Decision**: Too risky for unattended refactoring
- **Future**: Requires dedicated time for thorough analysis and testing

**Phase 4: Parser Refactoring** ✅ (Not Needed)

- **Reason**: Parser (688 lines) is already under 800-line target
- **Decision**: Well-structured, no split needed

### Results

**Before Refactoring:**

- `src/event/` directory with 2 files
- Some outdated documentation references

**After Refactoring:**

- `src/event.rs` - single consolidated file (416 lines)
- Updated documentation with correct references
- Cleaner module structure
- All tests passing (100% YAML 1.2 compliance maintained)

### Lessons Learned

1. **Conservative Approach Works**: Not all large files need splitting
   - Lexer and Emitter are large but cohesive
   - Forcing splits can harm maintainability

2. **Event Consolidation Success**: Small modules benefit from consolidation
   - Easier to navigate
   - Consistent with other supporting modules (error.rs, span.rs, value.rs)

3. **Documentation Matters**: Keeping architecture docs in sync is crucial
   - Found and fixed outdated file references
   - Improved accuracy of implementation details

### Recommendations

1. **Monitor File Growth**: If emitter or lexer grow significantly, revisit refactoring
2. **Focus on Functionality**: Prioritize features over structure refactoring
3. **Test Coverage**: Maintain 100% YAML 1.2 compliance as primary goal

## Incremental Emitter/Lexer Cleanup Plan (2026-03)

While large-scale splitting of `emitter/emitter.rs` and `lexer/document.rs` has been
deferred, there are smaller, safer refactors that improve readability and structure
without changing behavior.

### Goals

- Reduce local complexity in the emitter and lexer without introducing new modules
  immediately.
- Isolate reusable low-level concerns (token navigation, indentation checks) so later
  structural refactors are easier.
- Keep every step test-driven and behavior-preserving.

### Emitter: Incremental Steps

1. **Introduce a reusable token cursor abstraction**
   - Extract token navigation logic (`peek`, `peek_nth`, `is_eof`, `current_span`) into a
     small helper type that encapsulates access to `RichToken` slices.
   - Keep the external `Emitter` API identical (methods like `peek()` remain), but
     delegate their implementation to the cursor helper.
   - No behavior change; purely internal factoring.

2. **Separate document lifecycle helpers**
   - Group `prepare_document`, `finish_document`, `consume_trailing_content`, and
     `populate_tag_handles` behind a focused "document context" boundary (initially just
     via helper methods in the same file, later movable to a dedicated module).
   - Clarify ownership of directive/tag-handle logic vs indentation/content logic.

3. **Factor block vs flow collection processing**
   - Logically separate block and flow code paths inside `process_state_stack` and the
     related `process_block_seq`, `process_block_map`, `process_flow_seq`, and
     `process_flow_map` helpers.
   - Initial step is to ensure each helper is self-contained and uses small, typed
     parameters instead of relying on wide `self` state.

4. **Extract indentation/tab validation helpers**
   - Keep indentation semantics centralized (e.g., `check_tabs_as_indentation`,
     `check_tabs_at_column_zero_in_flow`, `check_tabs_after_block_indicator`,
     `check_trailing_content_at_root`, `check_content_after_flow`).
   - Make these helpers operate on simple inputs (token kind, spans, current
     indent/flow-depth) so they can later be moved into a smaller "indent validator"
     unit without touching parsing logic.

5. **Shrink `parse_value` by splitting sub-responsibilities**
   - Break out distinct concerns into helpers:
     - Pre-value layout/indent checks.
     - Property collection and orphan-property detection.
     - Determining implicit vs explicit mapping behavior.
     - Dispatching into block/flow/scalar parsing.
   - Target: `parse_value` becomes a thin orchestrator that calls 3–5 small helpers.

### Lexer: Small-Scope Improvements

1. **Add focused submodules only where cohesion is obvious**
   - Candidates: quoted string handling, block scalar headers, and directive parsing.
   - Move only well-isolated helpers (e.g., `consume_single_quoted`,
     `consume_double_quoted`, `consume_block_header`, `try_lex_directive`) into
     dedicated submodules, leaving the core `Lexer` state in `document.rs`.

2. **Group related state flags into small structs/enums**
   - Example: `LexerPhase` for directive prologue vs in-document is already an enum;
     a similar grouping can be applied to quoted-string state or directive tracking
     (e.g., an internal `DirectiveState` type).

3. **Tighten and de-duplicate docs**
   - Ensure internal comments and public docs reflect current naming (`Lexer` vs
     `DocumentLexer`, file paths, etc.).

### Execution Notes

- Each step should be implemented as a small, behavior-preserving change followed by
  running the full test suite (`cargo test -p yaml-parser`).
- The emitter cursor abstraction (step 1) is the first concrete change and forms the
  basis for later splitting work if/when that becomes desirable.
