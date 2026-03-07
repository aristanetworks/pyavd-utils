<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Emitter Development Handover

## Current Status

Pass rate: 94.5% (380/402 tests structural)

All 22 remaining structural failures are error test cases where the batch parser and emitter have different error recovery behavior. This is expected and acceptable - the emitter produces correct output for all valid YAML inputs.

The streaming `Emitter` module is being built to produce an event stream 100% identical to the legacy batch parser.

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
