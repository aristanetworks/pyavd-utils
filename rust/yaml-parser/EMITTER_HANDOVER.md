<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Emitter Development Handover

## Session Update (2026-03-08, Latest)

### Current Status
- **YAML Test Suite**: 842/842 ✅
- **Structural Equivalence**: 97.5% (392/402 tests)
- **Error Equivalence**: 92.0% (370/402 tests)
- **Full Equivalence**: 79.4% (319/402 tests)

### Span Parity Analysis

**Important Finding**: Many span differences reveal that the **emitter has more accurate spans** than the batch parser. The drop from 83.6% to 79.4% after fixing empty literal scalar spans is due to the emitter being more correct.

#### Cases Where Emitter is More Correct

1. **Empty Literal Scalar Spans** (2G84-00):
   - Batch: `4..7` (includes trailing newline)
   - Emitter: `4..6` (just the header `|0`)
   - **Verdict**: Emitter correct - newline is YAML syntax, not scalar content

2. **Invalid Spans in Batch Parser** (2G84-02, 2G84-03):
   - Batch: `Span { start: 4, end: 0 }` (end < start, clearly wrong)
   - Emitter: `4..7` (correct)
   - **Verdict**: Emitter correct - batch has a bug

3. **MappingEnd Spans** (236B):
   - Batch: `18..18` (after error content "invalid")
   - Emitter: `10..10` (after last valid content "bar")
   - **Verdict**: Emitter correct - should point to end of valid mapping content

4. **DocumentEnd Spans** (27NA):
   - Both produce `18..18` (end of content, not EOF)
   - **Verdict**: Both correct ✅

#### Remaining Span Differences

The remaining ~20% of tests with span differences fall into these categories:
- Block scalar spans (different handling of trailing content)
- Flow collection start spans (different range calculations)
- Empty scalar positions (different placement logic)

**Recommendation**: Accept emitter spans as the new standard where they are more correct. The batch parser has several span calculation bugs that should not be replicated.

### Recent Fixes

#### Fixed: DMG6 (InvalidIndentation Count Parity)
The batch parser reports `InvalidIndentation` twice for nested blocks encountering an orphan indent:
1. From the inner mapping's `advance_to_same_indent` when `n < target_indent`
2. From the outer mapping's `advance_to_same_indent` when `n > target_indent` but is an "over-indented orphan"

The emitter's `skip_ws_and_newlines_for_mapping` was only catching case #2. Fixed by adding orphan detection in the early-exit path when `n < mapping_indent`:

```rust
// In skip_ws_and_newlines_for_mapping, when n < mapping_indent:
if !self.is_valid_indent(n) && self.has_content_at_orphan_level_from(1) {
    self.error(ErrorKind::InvalidIndentation, span);
}
```

This ensures both the inner and outer mappings report the error, matching the batch parser's behavior.

#### Fixed: EW3V, HU3P (Orphan Indentation in Plain Scalar Continuations)
The plain scalar parser was consuming `LineStart` tokens for potential continuations without checking if the indent was valid. When the parser decided NOT to continue (e.g., seeing a mapping key), the token was "stolen" from the mapping's indentation checker.

Fixed by moving the orphan indentation check to AFTER `try_consume_plain_continuation` returns false:

```rust
// In parse_plain_scalar, after try_consume_plain_continuation returns false:
if !self.is_valid_indent(indent) && self.has_content_at_orphan_level_from(1) {
    self.error(ErrorKind::InvalidIndentation, line_span);
}
```

This ensures the error is only reported when the line is both an orphan AND rejected as a continuation.

#### Fixed: G9HC (InvalidIndentation + OrphanedProperties for Anchors at Invalid Indent)
When `parse_value` crosses a line to a dedented position (e.g., column 0 when min_indent > 0) and the first token is an anchor/tag, the batch parser reports both `InvalidIndentation` and `OrphanedProperties`.

Fixed by adding a check in `parse_value` immediately after `skip_ws_and_newlines_returns_crossed_line()`:

```rust
if initial_crossed_line && self.current_indent < min_indent {
    if let Some((tok, span)) = self.peek() {
        if matches!(tok, Token::Anchor(_) | Token::Tag(_)) {
            self.error(ErrorKind::InvalidIndentation, span);
            self.error(ErrorKind::OrphanedProperties, span);
        } else if matches!(tok, Token::Plain(_) | Token::StringStart(_)) {
            self.error(ErrorKind::InvalidIndentation, span);
        }
    }
}
```

#### Fixed: LHL4 (ContentOnSameLine for Tag Followed by Flow Collection)
When a tag is immediately followed by a flow collection with no space (e.g., `!invalid{}`), the batch parser reports `ContentOnSameLine` for the flow collection start token.

Fixed by adding a check in `collect_properties_with_min_indent` after consuming a tag:

```rust
// In PropAction::Tag handler, after advance():
let tag_looks_legitimate = !tag_str.contains('"') && !tag_str.contains('`');
let tag_end = span.end_usize();
if tag_looks_legitimate {
    if let Some((next_tok, next_span)) = self.peek() {
        let is_content = matches!(
            next_tok,
            Token::Plain(_) | Token::StringStart(_) | Token::FlowSeqStart | Token::FlowMapStart | Token::BlockSeqIndicator
        );
        if is_content && next_span.start_usize() == tag_end {
            self.error(ErrorKind::ContentOnSameLine, next_span);
        }
    }
}
```

#### Fixed: H7J7 (Orphaned Properties Detection)
Added indentation-aware property detection in `parse_value`. When `collect_properties_with_min_indent` crosses a line boundary but stops due to invalid indent, we now check if the next content is a property token at invalid indent and report `OrphanedProperties`.

#### Fixed: GT5M (TrailingContent at Document Level)
Changed `check_trailing_after_document` to report `TrailingContent` (not `OrphanedProperties`) for all trailing tokens at document level, matching batch parser behavior.

#### Added: error_unless_span_has_error helper
Added `error_unless_span_has_error()` at line ~372 to prevent duplicate error reporting for the same span. Used in `consume_trailing_content` to avoid re-reporting spans already flagged by earlier detection.

### Remaining Error Parity Issues (32 tests)

| Category | Count | Examples | Notes |
|----------|-------|----------|-------|
| `TrailingContent` count mismatches | ~15 | GT5M, BD7L, JY7Z, BS4K, LHL4, P2EQ, 62EZ | Batch often reports same span twice |
| `MissingSeparator` vs `UnexpectedEof` | 2 | CML9, DK4H | Flow context error type differences |
| `InvalidIndentation` count mismatches | 2 | 2CMS, 4EJS | Different error recovery behavior |
| Extra errors | 1 | N782 | Emitter reports more DocumentMarkerInFlow |

### Key Error Differences Explained

1. **Duplicate Errors (GT5M pattern)**: The batch parser sometimes reports the same error twice from different call sites. The emitter's `error_unless_span_has_error` prevents this, causing count mismatches.

2. **ContentOnSameLine**: The batch detects more same-line content errors. The emitter needs enhanced detection in certain contexts.

### Structural Equivalence Issues (10 tests)

Span differences that cause structural test failures:
- DocumentEnd span off-by-one (229Q, 27NA, 2AUY, 2EBW, etc.)
- MappingEnd span differences (236B)
- Literal scalar span differences (2G84-*)
- MappingStart span differences (26DV)

### Files Modified This Session
- `rust/yaml-parser/src/parser/emitter.rs` - Orphaned properties detection, error deduplication, scalar continuation orphan check, tag followed by flow collection check
- `rust/yaml-parser/src/tests.rs` - Debug tests (`debug_ew3v`, `debug_g9hc`, `debug_hu3p`, `debug_lhl4`, `debug_p2eq`, `debug_62ez`)

### Key Test Commands
```bash
# Run full equivalence test
cargo test emitter_equivalence -- --nocapture 2>&1 | grep -E "Structural Equivalence|Error Equivalence"

# Debug specific test
cargo test --lib debug_ew3v -- --nocapture

# See all error failures
cargo test emitter_equivalence -- --nocapture 2>&1 | grep -A 100 "Error Failures"
```

### Recommended Next Steps

1. **Investigate `TrailingContent` duplicates** (GT5M, BD7L, JY7Z, BS4K, LHL4, P2EQ, 62EZ, etc.)
   - The batch parser reports the same `TrailingContent` error multiple times from different call sites
   - The emitter's `error_unless_span_has_error` prevents duplicates
   - This accounts for ~15 of the 32 remaining failures
   - Decision needed: Should we match the batch parser's duplicate reporting, or is the emitter's behavior correct?

2. **Fix flow context error types** (CML9, DK4H)
   - Batch reports `MissingSeparator`, emitter reports `UnexpectedEof + TrailingContent`
   - Different error recovery behavior in flow collections

3. **Fix structural span issues** (10 tests)
   - DocumentEnd spans often off-by-one
   - MappingEnd/MappingStart span calculation differences

---



## Current Status

Pass rate: **100% (842/842)** - All positive (402) and negative (440) tests pass.

The streaming `Emitter` module is now fully compliant with the YAML Test Suite. Error detection has been ported from the legacy batch parser.

### Ported Error Detection (from legacy batch parser)

- `UndefinedAlias` - Alias used before anchor is defined
- `PropertiesOnAlias` - Anchor/tag applied to alias (invalid)
- `DuplicateAnchor` - Multiple anchors on same node
- `DuplicateTag` - Multiple tags on same node
- `MultilineImplicitKey` - Implicit key spans multiple lines (invalid in block context)

## Key Files

- `rust/yaml-parser/src/parser/emitter.rs` - The streaming emitter implementation
- `rust/yaml-parser/src/parser/mod.rs` - Legacy batch parser (reference)
- `rust/yaml-parser/src/parser/flow.rs` - Batch parser's flow collection handling (reference)
- `rust/yaml-parser/tests/test_suite.rs` - Contains `debug_26dv` test for iterative debugging

## Test Commands

```bash
# Run full equivalence test
cargo test emitter_equivalence -- --nocapture

# Run debug test for specific case
cargo test debug_26dv -- --nocapture
```

## Architecture

The emitter uses an explicit state stack (`ParseState` enum) instead of recursive descent. Key states:

- `Value` - Parse any value at a minimum indent
- `BlockSeq`/`BlockMap` - Block collections with phases
- `FlowSeq`/`FlowMap` - Flow collections with phases
- `EmitScalar`/`EmitAlias`/`EmitSeqStart` - Deferred event emission

## Recently Completed: Implicit Flow Mappings (87E4) ✓

### The Problem (FIXED)

Input: `'implicit block key' : [ 'implicit flow key' : value ]`

Inside flow sequences, a value followed by `:` creates an **implicit flow mapping**:

```yaml
[ key: value ]  # This is a sequence containing ONE mapping entry
```

### Solution Implemented

Added `is_implicit_flow_mapping_entry()` helper function that looks ahead to detect:

- Properties (anchor/tag)
- Scalar tokens (Plain with multiline continuations, quoted strings)
- Flow collections `[...]` and `{...}`
- Checks if followed by `Colon`

Added new phases to `FlowSeqPhase`:

- `ImplicitMapValue` - After parsing key, consume `:` and parse value
- `ImplicitMapEnd` - After parsing value, emit `MappingEnd`

Modified `FlowSeqPhase::BeforeEntry` to check for implicit mapping and emit `MappingStart` first.

## Remaining Failures (22 total - ALL ERROR CASES)

All remaining structural failures are error test cases with different error recovery behavior.
Examples: `236B`, `7MNF`, `9MAG`, `G7JE`, `GDY7`, etc.

## Key Concepts

### Bridging

Properties on one line can apply to a block collection starting on a subsequent line:

```yaml
&anchor
- item  # anchor applies to the sequence, not null
```

### Complex Keys

Non-scalar mapping keys like `[ a, b ]: value`. The emitter has `is_flow_seq_complex_key()` for this.

### Line Boundary Detection

`collect_properties()` returns `crossed_line_boundary` to distinguish:

- `&anchor value` (anchor on scalar)
- `&anchor\nvalue` (anchor on collection containing value)

## Recent Fixes

- **26DV, 4JVG**: Property ownership for block mappings
  - Fixed `parse_value` to pass `prop_crossed_line` (not `initial_crossed_line || prop_crossed_line`) to `parse_scalar_or_mapping`
  - Key insight: What matters is whether there's a line boundary AFTER the properties, not whether we crossed a line from the parent context
  - `&anchor\n  key:` → prop_crossed_line=true → anchor on MAPPING
  - `\n  &anchor key:` → prop_crossed_line=false → anchor on KEY

- **87E4**: Implicit flow mappings (`[ key: value ]`)
  - Added `is_implicit_flow_mapping_entry()` lookahead helper
  - Added `FlowSeqPhase::ImplicitMapValue` and `ImplicitMapEnd` phases
  - Handles plain scalars (including multiline), quoted strings, flow collections, aliases

- **6BFJ**: Multiple anchors with complex keys (`&mapping\n&key [...]`)
  - Added `EmitSeqStart` state for deferred sequence emission
  - Added `is_flow_seq_complex_key()` lookahead helper

- **WZ62**: Tagged keys in flow mappings (`{ !!str : bar }`)
  - Added flow context check at start of `Token::Colon` arm in `parse_value`
  - In flow context (`flow_depth > 0`), a tag followed by colon is an empty tagged key
  - Should NOT start a block mapping inside flow context

- **S3PD, W42U**: Empty value detection and block sequence siblings
  - Added dedent check in `parse_value` for proper empty scalar emission
  - Added exceptions for block collection indicators (`-`, `?`, `:`) at same indent
  - Refined to treat sequence siblings at same indent as siblings, not nested values

- **V9D5**: Compact block mappings
  - Fixed by properly handling indentation in block mapping detection
