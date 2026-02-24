<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Parser Infrastructure Improvements

This document tracks potential improvements to the YAML parser infrastructure.

## Current State

The parser is fully functional with 100% YAML 1.2 test suite compliance (333/333 tests).
It implements error recovery, span tracking, and the Node/Value AST model with proper
anchor/tag handling.

## Identified Improvement Areas

### Phase 1: Code Organization and Complexity Reduction

#### 1.1 Extract Property Collection Helper

**Status:** [x] Complete
**Complexity:** Low
**Impact:** Reduces code duplication

**Problem:** The anchor/tag collection pattern appeared in multiple places:

- `parse_value_with_properties()` in `mod.rs`
- `parse_block_mapping_with_props()` in `block.rs`
- Similar inline patterns in flow parsing

**Solution:** Created `collect_node_properties()` method in `mod.rs` that:

- Loops over Anchor/Tag/Whitespace tokens
- Accumulates into `NodeProperties`
- Handles duplicate anchor/tag errors with Named variants
- Returns the collected properties

**Implementation:** `Parser::collect_node_properties()` in `mod.rs` (lines 272-312)

#### 1.2 Reduce `parse_value_with_properties` Complexity

**Status:** [x] Complete
**Complexity:** Medium
**Impact:** Improves maintainability

**Problem:** The method was ~300 lines with a large match statement handling 15+ token types.

**Solution:** Extracted token-specific helpers:

- `handle_flow_collection_as_value()` - Shared flow mapping/sequence logic with key detection
- `handle_anchor_in_value()` - Anchor property accumulation with duplicate detection
- `handle_tag_in_value()` - Tag property accumulation with handle validation

**Result:** Reduced `parse_value_with_properties` from ~300 lines to ~127 lines.
Still has `#[allow(clippy::too_many_lines)]` since clippy threshold is 100, but
match arms are now minimal - further extraction would reduce clarity.

#### 1.3 Unify Flow Collection Entry Parsing

**Status:** [x] Complete
**Complexity:** Medium
**Impact:** Reduces duplication in `flow.rs`

**Problem:** `parse_flow_mapping()` and `parse_flow_sequence()` had similar structure:

- Flow depth tracking
- Comma handling
- Error recovery with `skip_to_flow_delimiter()`
- Loop progress guards

**Solution:** Extracted common flow parsing utilities:

- `enter_flow_collection()` - Flow depth and column tracking setup
- `exit_flow_collection()` - Flow depth and column tracking cleanup
- `handle_flow_comma()` - Consecutive comma detection and handling
- `handle_flow_entry_end()` - Comma/end token handling after entries

**Note:** Did not create a `FlowContext` struct as the helpers proved sufficient.
The `parse_flow_mapping` function is now under clippy's 100 line threshold.

### Phase 2: Performance and Memory

#### 2.1 Zero-Copy Scalar Parsing

**Status:** [x] Complete
**Complexity:** Medium-High
**Impact:** Reduces allocations for string values

**Problem:** Parser created owned `String` values even when the input could be borrowed.
The lexer already used `Cow<'input, str>` for tokens.

**Solution:**

- Changed `Value::String(String)` to `Value::String(Cow<'input, str>)`
- Changed `Value::Alias(String)` to `Value::Alias(Cow<'input, str>)`
- Changed `Node::anchor` and `Node::tag` from `Option<String>` to `Option<Cow<'input, str>>`
- Added `<'input>` lifetime parameter to `Node` and `Value`
- Added `into_owned()` methods to convert to `'static` lifetime
- The main `parse()` function returns owned data (`Node<'static>`) for convenience
- Lower-level `parse_single_document()` returns borrowed data for zero-copy when needed

**API Design:**

- `parse(input: &str) -> (Vec<Node<'static>>, Vec<ParseError>)` - returns owned, easy to use
- `parse_single_document(&tokens, &input, &directives) -> (Option<Node<'input>>, ...)` - zero-copy

**Trade-offs:**

- API complexity increased (lifetime parameter propagates through parser internals)
- Users get owned data from `parse()` by default (no action needed)
- Advanced users can use lower-level API for zero-copy benefits

#### 2.2 Reduce Intermediate Allocations in Block Scalars

**Status:** [ ] Not Started (may be partially addressed by 2.1)
**Complexity:** Medium
**Impact:** Improves block scalar performance

**Problem:** `collect_block_scalar_content()` builds a `Vec<String>` of lines,
then joins them. For large block scalars, this creates intermediate allocations.

**Solution:**

- Estimate final size based on byte range
- Build result string directly with pre-allocated capacity
- Track newlines without intermediate line vector

**Note:** With zero-copy parsing (2.1), many simple scalars are already zero-copy.
Block scalars still require processing (chomping, folding) so they create owned strings.

### Phase 3: Error Handling Improvements

#### 3.1 Consistent Use of Contextual Error Variants

**Status:** [x] Complete
**Complexity:** Low
**Impact:** Better error messages

**Problem:** Some errors used basic variants when richer context was available.

**Solution:** Audited error creation sites and updated to use Named variants:

- `DuplicateAnchor` → `DuplicateAnchorNamed { first, second }`
- `DuplicateTag` → `DuplicateTagNamed { first, second }`
- `UndefinedAlias` → `UndefinedAliasNamed(name)`

**Implementation:** Updated all error creation sites in `mod.rs`, `block.rs`, and `flow.rs`
to use the contextual Named variants with specific anchor/tag/alias names.

#### 3.2 Expected Token Sets for Better Diagnostics

**Status:** [x] Complete
**Complexity:** Low
**Impact:** Clearer error messages

**Problem:** `ParseError::expected` field was often empty.

**Solution:** Added `error_expected()` method and updated key decision points:

- Flow collection entry end: expects `,` or closing delimiter
- Flow mapping after key: expects `:`, `,`, or `}`
- Consecutive commas in flow: expects `value`, `key`, or closing delimiter
- `parse_value_with_properties` catch-all: expects `scalar`, `sequence`, `mapping`, etc.

**Note:** The existing infrastructure (`ParseError::expected` field, `with_expected()` builder)
was already in place. The implementation focused on populating these fields at key error sites.

## Implementation Priority

**Completed:**

1. ~~**1.1 Property Collection Helper**~~ - ✓ Done
2. ~~**3.1 Contextual Error Variants**~~ - ✓ Done
3. ~~**1.2 Method Extraction**~~ - ✓ Done
4. ~~**1.3 Flow Unification**~~ - ✓ Done
5. ~~**3.2 Expected Token Sets**~~ - ✓ Done
6. ~~**2.1 Zero-Copy Scalar Parsing**~~ - ✓ Done

**Remaining (recommended order):**

1. **2.2 Block Scalar Allocations** - Only if profiling shows need

## Dropped Items

*None yet - all items are under consideration.*

## Notes

- All changes must maintain 100% YAML 1.2 test suite compliance
- For bigger changes, discuss approaches before implementing
- Keep ARCHITECTURE.md updated with significant changes
