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

**Status:** [ ] Not Started
**Complexity:** Low
**Impact:** Reduces code duplication

**Problem:** The anchor/tag collection pattern appears in multiple places:

- `parse_value_with_properties()` in `mod.rs` (lines 784-894)
- `parse_block_mapping_with_props()` in `block.rs` (lines 341-366)
- Similar inline patterns in flow parsing

**Solution:** Create a `collect_node_properties()` method that:

- Loops over Anchor/Tag/Whitespace tokens
- Accumulates into `NodeProperties`
- Handles duplicate anchor/tag errors
- Returns the collected properties

#### 1.2 Reduce `parse_value_with_properties` Complexity

**Status:** [ ] Not Started
**Complexity:** Medium
**Impact:** Improves maintainability

**Problem:** The method is ~260 lines with a large match statement handling 15+ token types.
Currently has `#[allow(clippy::too_many_lines)]`.

**Solution:** Extract token-specific handlers:

- `handle_flow_mapping()` - Flow `{` parsing with key detection
- `handle_flow_sequence()` - Flow `[` parsing with key detection
- `handle_block_sequence()` - Block `-` parsing
- `handle_anchor_property()` - `&name` accumulation
- `handle_tag_property()` - `!tag` accumulation
- `handle_line_start()` - Newline/indentation handling

#### 1.3 Unify Flow Collection Entry Parsing

**Status:** [ ] Not Started
**Complexity:** Medium
**Impact:** Reduces duplication in `flow.rs`

**Problem:** `parse_flow_mapping()` and `parse_flow_sequence()` have similar structure:

- Flow depth tracking
- Comma handling
- Error recovery with `skip_to_flow_delimiter()`
- Loop progress guards

**Solution:** Extract common flow parsing utilities:

- `FlowContext` struct to track depth and delimiters
- `parse_flow_entry()` helper for key-value or single value
- Shared comma/delimiter handling

### Phase 2: Performance and Memory

#### 2.1 Zero-Copy Scalar Parsing

**Status:** [ ] Not Started
**Complexity:** Medium-High
**Impact:** Reduces allocations for string values

**Problem:** Parser creates owned `String` values even when the input could be borrowed.
The lexer already uses `Cow<'input, str>` for tokens.

**Solution:**

- Change `Value::String(String)` to `Value::String(Cow<'input, str>)`
- This requires adding a lifetime parameter to `Value` and `Node`
- Benefits most for simple unquoted scalars and keys

**Trade-offs:**

- API complexity increases (lifetime parameter propagates)
- May not be worth it if most use cases convert to owned anyway
- Consider as optional optimization, not required

#### 2.2 Reduce Intermediate Allocations in Block Scalars

**Status:** [ ] Not Started
**Complexity:** Medium
**Impact:** Improves block scalar performance

**Problem:** `collect_block_scalar_content()` builds a `Vec<String>` of lines,
then joins them. For large block scalars, this creates intermediate allocations.

**Solution:**

- Estimate final size based on byte range
- Build result string directly with pre-allocated capacity
- Track newlines without intermediate line vector

### Phase 3: Error Handling Improvements

#### 3.1 Consistent Use of Contextual Error Variants

**Status:** [ ] Not Started
**Complexity:** Low
**Impact:** Better error messages

**Problem:** Some errors use basic variants (e.g., `ErrorKind::UnexpectedToken`)
when richer context is available.

**Solution:**

- Audit error creation sites
- Use `Named` variants where context is available
- Example: `DuplicateKey` → `DuplicateKeyNamed(key_string)`

#### 3.2 Expected Token Sets for Better Diagnostics

**Status:** [ ] Not Started
**Complexity:** Low
**Impact:** Clearer error messages

**Problem:** `ParseError::expected` field is often empty.

**Solution:**

- At key decision points, populate expected tokens
- Example: After `:` in mapping, expected values are scalars, collections, etc.

## Implementation Priority

**Recommended order:**

1. **1.1 Property Collection Helper** - Quick win, reduces duplication
2. **3.1 Contextual Error Variants** - Quick win, improves user experience
3. **1.2 Method Extraction** - Larger effort, improves maintainability
4. **1.3 Flow Unification** - Medium effort, reduces duplication
5. **2.1/2.2 Performance** - Only if profiling shows need

## Dropped Items

*None yet - all items are under consideration.*

## Notes

- All changes must maintain 100% YAML 1.2 test suite compliance
- For bigger changes, discuss approaches before implementing
- Keep ARCHITECTURE.md updated with significant changes
