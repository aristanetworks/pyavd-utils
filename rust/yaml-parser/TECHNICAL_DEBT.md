<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# YAML Parser Technical Debt

This file tracks issues that were root-caused but not fully fixed, to inform future refactoring efforts.

---

## Issue #1: Anchor Names Can Contain Colons (Test 2SXE)

**Date Identified:** 2026-02-18
**Status:** PARTIALLY RESOLVED

**Failing Test:** 2SXE - "Anchors With Colon in Name"

**Input:**

```yaml
&a: key: &a value
foo:
  *a:
```

**Root Cause Analysis:**

1. **Lexer Issue:** `consume_anchor_name()` only accepts alphanumeric, `-`, `_`. According to YAML 1.2 spec, anchor names (`ns-anchor-char+`) can contain any non-whitespace character except flow indicators (`[]{},`). This includes colons.

2. **Parser Architecture Issue (RESOLVED):** We refactored the parser to separate node properties from values. The `Node` struct now has:
   - `anchor: Option<String>` - node's anchor property
   - `tag: Option<String>` - node's tag property
   - `value: Value` - the actual value
   - `span: Span` - source span

**What Was Fixed:**

- Parser architecture now properly treats anchors and tags as node properties
- `NodeProperties` helper struct accumulates properties before parsing value
- Properties are applied using `apply_properties_and_register()`

**Remaining Issue:**

- The lexer still uses conservative anchor name parsing (alphanumeric, `-`, `_`)
- This means `&a:` is still parsed as `Anchor("a")` + `Colon`
- To fully fix, the lexer needs to accept colons in anchor names, which requires careful handling

**Related Tests:**

- 2SXE: Anchors With Colon in Name (lexer issue still blocks this test)

---

## Issue #2: Invalid Nested Mappings with Plain Scalars (Test ZCZ6)

**Date Identified:** 2026-02-18

**Failing Test:** ZCZ6 - "Invalid mapping in plain single line value"

**Input:**

```yaml
a: b: c: d
```

**Expected:** Error - multiple nested mappings on same line are invalid

**Current Behavior:** Parses as `{a: {b: {c: d}}}` without error

**Root Cause Analysis:**

We implemented validation for QUOTED scalars in nested positions (`a: 'b': c`), but the same rule applies to PLAIN scalars. According to YAML spec and PyYAML behavior:

- `a: b: c` is invalid (nested mapping on same line)
- `a: 'b': c` is invalid (same reason)
- `a:\n  b: c` is valid (nested mapping on new line with proper indentation)

**Why Not Fully Fixed:**
The current validation in `parse_scalar_or_mapping()` only checks for quoted scalars in nested positions. Extending to plain scalars requires careful consideration:

- Need to track if we're parsing a value position (after a colon on same line)
- Plain scalars are more ambiguous than quoted scalars

**Recommended Fix:**
Extend the `is_nested_value_position` check in `parse_scalar_or_mapping()` to also apply when the key is a plain scalar, not just quoted scalar.

**Related Tests:**

- ZCZ6: Invalid mapping in plain single line value
- Potentially HU3P, 2CMS, G7JE (other multiline/nested plain scalar issues)

---

## Issue #3: Scalar Value with Two Anchors Across Lines (Test 4JVG)

**Date Identified:** 2026-02-18
**Status:** RESOLVED

**Failing Test:** 4JVG - "Scalar value with two anchors"

**Input:**

```yaml
top1: &node1
  &k1 key1: val1
```

**Expected:** This is VALID - `&node1` applies to the nested mapping `{key1: val1}`, and `&k1` applies to the key `key1`.

**Solution Implemented:**

1. Added `crossed_line_boundary` flag to `NodeProperties` struct
2. When parsing across line boundaries:
   - `&a &b value` on same line → DuplicateAnchor error (invalid)
   - `&a\n&b key: value` across lines → Try to parse as nested block mapping
   - If mapping succeeds: outer anchor applies to mapping, inner to key
   - If not a mapping: report duplicate anchor error
3. Created `parse_block_mapping_with_props()` helper to parse block mappings where the first key already has properties
4. Updated anchor, tag, and alias handling to use line boundary awareness

**Test Status:** Test 4JVG now passes along with related tests 7BMT, U3XV, 26DV, 9KAX.

---

## Issue #4: Invalid Plain Scalar Start vs Multiline Plain Scalar Continuations

**Date Identified:** 2026-02-18
**Status:** PARTIALLY FIXED (workaround implemented)

**Tests:**

- U99R - "Invalid comma in tag" - NOW PASSING (with workaround)
- FBC9 - "Allowed characters in plain scalars" - NOW PASSING (with workaround)

**Fix Applied (2026-02-18):**
Added validation in parser that tags followed immediately by content (no whitespace) are errors.
To avoid false positives from lexer bugs (where `!"#$%...` is incorrectly tokenized as Tag+Plain),
we skip this validation if the tag contains characters not valid in YAML tags (e.g., `"`, backtick).
This workaround fixes both tests but doesn't address the underlying lexer architecture issue.

**Original Failing Behavior:**

**U99R Input:**

```yaml
- !!str, xxx
```

**Expected for U99R:** Error - `,` cannot start a plain scalar according to YAML 1.2 spec [126] ns-plain-first(c).

**FBC9 Input:**

```yaml
safe: a!"#$%&'()*+,-./09:;<=>?@AZ[\]^_`az{|}~
     !"#$%&'()*+,-./09:;<=>?@AZ[\]^_`az{|}~
```

**Expected for FBC9:** Valid YAML - line 2 is a continuation of line 1's multiline plain scalar value.

**Root Cause Analysis:**

1. **U99R Issue:** After the tag `!!str`, the plain scalar `, xxx` starts with `,` which is a c-flow-indicator. According to YAML 1.2 spec rule [126], plain scalars cannot start with c-indicator characters (except `-`, `?`, `:` with special rules). The lexer allows `, xxx` as a plain scalar, which is incorrect.

2. **FBC9 Issue:** Line 2 starts with `!` which the lexer interprets as a tag. But since line 2 is more indented than the key (indent=5 vs key indent=0), it's actually a **continuation** of the multiline plain scalar from line 1, so `!` should NOT be treated as a tag.

3. **Attempted Parser Fix:** Added validation that if a tag is followed by a plain scalar starting with a c-indicator on the same line, emit an error. This catches U99R correctly.

4. **Why It Breaks FBC9:** On line 2, the lexer tokenizes:
   - `Tag("\"#$%&'()*+")` at span 51..62 (incorrectly)
   - `Plain(",-./09...")` at span 62..89

   The parser sees a tag followed by a plain scalar starting with `,` on the "same line" (no `LineStart` between them) and emits an error. But this is wrong because the entire line 2 should be part of a multiline plain scalar.

**Fundamental Issue:**

The lexer doesn't understand **multiline plain scalar continuations**. It tokenizes each line independently without considering whether the current line is a continuation of a plain scalar from the previous line.

To properly fix this:

1. The lexer needs to track the indentation context (current mapping key's indentation)
2. When a line is more indented than the key, recognize it as a continuation
3. In continuation context, don't tokenize `!`, `&`, `*`, etc. as special characters

**Why Not Fixed:**

This requires significant architectural changes to the lexer. The lexer currently has no concept of "continuation lines" - it would need to:

1. Track the column of the current mapping key
2. Pass this context between lines
3. Disable special character tokenization when in continuation context

This is a non-trivial change that affects the entire lexer architecture.

**Workaround Options:**

1. Accept that multiline plain scalars with special starting characters are not fully supported
2. Implement full indentation-aware lexing (significant effort)
3. Move plain scalar tokenization to the parser level (lexer only emits raw lines)

**Related Tests:**

- U99R: Invalid comma in tag
- FBC9: Allowed characters in plain scalars
- Other multiline plain scalar tests: 2CMS, 8XDJ, BF9H, BS4K, G7JE, GDY7, HU3P

---

## Issue #5: Invalid Content After Document Root Value (KS4U)

**Date Identified:** 2026-02-18
**Status:** NOT FIXED (blocked by Issue #6)

**Failing Test:** KS4U - "Invalid item after end of flow sequence"

**Input:**

```yaml
---
[
sequence item
]
invalid item
```

**Expected:** Error - after the flow sequence `]`, `invalid item` is extra content that shouldn't be there.

**Current Behavior:** Parses `invalid item` as a second document, no error.

**Root Cause Analysis:**

1. The stream_lexer correctly extracts the content for the explicit document
2. The parser's `parse_stream` function treats the content as potentially having multiple documents
3. After parsing the flow sequence, it sees `invalid item` and parses it as another document

**Attempted Fix:**

Added a check in `parse_layered` that if `raw_doc.explicit_start` is true and `docs.len() > 1`, emit an error for the extra documents.

**Why the Fix Was Reverted:**

The fix caused massive regressions (298 → 276 passing tests) because:

1. The stream_lexer discards content after `---` on the same line (Issue #6)
2. For `--- !!str\nd\ne`, the `!!str` tag is lost, and `d` and `e` are parsed as separate scalars
3. With my fix, the second scalar `e` was incorrectly flagged as an error

**Dependencies:**

This issue cannot be properly fixed until Issue #6 (content after `---` being discarded) is resolved.

---

## Issue #6: Content After Document Start Marker Is Discarded

**Date Identified:** 2026-02-18
**Status:** NOT FIXED

**Example Input:**

```yaml
--- !!str
d
e
```

**Expected:** One document with tag `!!str` and value `d\ne` (multiline scalar)

**Current Behavior:** The `!!str` tag is discarded by the stream_lexer. The content becomes just `d\ne\n`, parsed as two separate scalars.

**Root Cause:**

In `stream_lexer.rs`, lines 118-122:

```rust
// Skip the marker and rest of line - content after --- on the same line is included
// (e.g., `--- value` or `--- >` for block scalars)
// We skip to end of line and then past the newline so content starts on next line
pos = skip_to_eol(input, marker_end);
content_start = pos;
```

The comment says content after `---` "is included" but the code actually skips it entirely!

**Impact:**

- Tags, anchors, and values on the same line as `---` are lost
- Multiline block scalars with headers on the `---` line don't parse correctly
- This affects many tests including 35KP, 6JQW, B3HG, T26H, T5N4, etc.

**Fix Required:**

The stream_lexer needs to include content after `---` (but not the `---` itself) in the document content. The content should start after the `---` (including the space).

---

## Issue #7: Invalid Content After Root-Level Sequence (TD5N, 6S55, BD7L)

**Date Identified:** 2026-02-18

**Failing Tests:**

- TD5N: Invalid scalar after sequence (`- item1\n- item2\ninvalid`)
- 6S55: Invalid scalar at the end of sequence (`key:\n - bar\n - baz\n invalid`)
- BD7L: Invalid mapping after sequence (`- item1\n- item2\ninvalid: x`)

**Input (TD5N):**

```yaml
- item1
- item2
invalid
```

**Root Cause Analysis:**

After parsing a block sequence, if there's content at the same indentation level that isn't a sequence indicator (`-`), the parser should error. Instead, the parser treats it as a new implicit document.

The issue is in `parse_block_sequence()` which silently returns after the while loop exits (when the next token isn't `BlockSeqIndicator`), and then `parse_stream()` happily parses the remaining content as a new document.

**Why It's Hard to Fix:**

The naive fix (check for content at `seq_indent` after the while loop) breaks valid cases like AZ63:

```yaml
one:
- 2
- 3
four: 5
```

Here, `four: 5` is at indent 0 (same as sequence items), but it's valid because the sequence is a VALUE of `one:`, and `four:` is a sibling key in the root mapping.

The difference:

- **TD5N**: Sequence IS the document root → content after it at indent 0 is invalid
- **AZ63**: Sequence is VALUE of a mapping → content at parent mapping's indent is valid

**Potential Fix:**

The validation needs to know whether the sequence is the document root or nested inside a mapping. This requires:

1. Passing a "context" parameter to `parse_block_sequence()` indicating if it's the root
2. Or, doing the validation in `parse_stream()` after `parse_value(0)` returns

For approach 2, we'd need to check: if the root value is a sequence/scalar (not a mapping), and there's trailing content at indent 0, that's an error.

**Attempted Fix (2026-02-18) - REVERTED:**

Created `parse_single_document()` function that:

1. Parses exactly ONE value at indent 0
2. After parsing, checks for remaining content at column > 0 and reports errors

Result: Test 6S55 passed (correctly reported error for `invalid` at column 1), but caused regression from 306 to 282 passing tests.

**Reason for regression:**

Multiline plain scalar continuations (Issue #4) are not properly consumed by the parser. For test 36F6:

```yaml
---
plain: a
 b

 c
```

The parser:

1. Parses `plain: a` as a mapping
2. Only captures `a` as the value (single Plain token)
3. Leaves `b` and `c` as "remaining content"
4. The `parse_single_document` check correctly flags content at column 1 as errors
5. But `b` and `c` SHOULD have been consumed as multiline plain scalar continuations

The `parse_single_document` check is CORRECT - the problem is that `parse_scalar()` doesn't consume multiline plain scalar continuations. Fixing Issue #4 (multiline plain scalars) must be done BEFORE the `parse_single_document` approach can work.

**Dependency chain:**

1. Fix Issue #4 (multiline plain scalar parsing)
2. Then enable `parse_single_document` in `parse_layered()`
3. This will fix Issue #7 (6S55, TD5N, BD7L)

---

## Future Considerations

### Parser Architecture Improvements

1. **Separate Node Properties from Values (COMPLETED):** The `Node` struct now properly separates anchor/tag properties from the value.

2. **Indentation Tracking:** Many failing tests relate to indentation validation. A proper indentation-aware parser would track:
   - Current block indentation level
   - Whether we're in block or flow context
   - Allowed indentation for continuation

3. **Two-Phase Parsing:** Consider separating:
   - Phase 1: Structure recognition (indentation, block/flow, node boundaries)
   - Phase 2: Content parsing (scalars, anchors, tags)

### Current Test Status (330/333 = 99.1%)

**Latest update (2026-02-19):** Fixed tag shorthand scope validation (QLJ7) and related document
boundary issues in the stream lexer.

**Key changes made:**

1. **Tag Handle Validation (NEW):** Parser now validates that named tag handles (like `!prefix!`)
   are declared in the current document's prolog via `%TAG` directives.
   - Added `tag_handles: HashMap<String, String>` to Parser struct
   - Added `validate_tag_handle()` method to check tags against declared handles
   - Modified `parse_single_document()` to accept directives from stream lexer

2. **Stream Lexer Document Boundary Fix:** Fixed incorrect directive assignment when `...`
   is followed by directives for the next document.
   - Directives after `...` now correctly go to the following document, not the current one
   - Fixed 5TYM regression (both documents had their own `%TAG` declarations)

3. **Modal Lexer Architecture:** Single lexer with Stream and Document modes
   - Stream mode: handles directives (`%YAML`, `%TAG`), recognizes `---`/`...` as document markers
   - Document mode: full content tokenization with proper context tracking
   - Tracks flow depth, quote state, indentation levels
   - Correctly handles `...` inside quoted strings (not a document marker)
   - Validates document markers in flow context (error)

2. **Parser fixes for mapping key properties:**
   - `parse_block_mapping()` in `block.rs`: Now collects anchor/tag properties before parsing keys
   - `parse_remaining_mapping_entries()` in `scalar.rs`: Same fix for continuation entries
   - Both now properly error on `PropertiesOnAlias` (e.g., `&anchor *alias`)

3. **Fixed tests:**
   - SU74: Anchor and alias as mapping key (`&b *alias : value`) - now errors correctly
   - H7J7: Node anchor not indented - now errors correctly
   - RXY3, 5TRB: Document markers inside quoted strings
   - 3HFZ: Invalid content after document end marker
   - N782: Invalid document markers in flow style
   - H7TQ: Extra words on %YAML directive
   - SF5V: Duplicate YAML directive

**Error recovery tests added (2026-02-19):** 15 comprehensive tests in `lib.rs` module `error_recovery`:

- Unterminated flow sequences and mappings
- Invalid escape sequences
- Unterminated quoted strings
- Flow collections with consecutive commas (error recovery)
- Duplicate anchors, undefined aliases
- Tabs in indentation
- Multiple errors in single document
- Nested flow error recovery
- Error span accuracy

**Remaining failures (3 tests) - ALL are "Expected error but parsing succeeded":**

- **Flow/quoted scalar indentation (2 tests):** 9C9N, QB6E
  - 9C9N: Flow sequence `[a, b, c]` with `b,` and `c]` continuation lines at column 0
  - QB6E: Quoted string `"a\nb\nc"` with continuation lines at column 0

- **Anchor indentation (1 test):** G9HC
  - `&anchor` at column 0 not properly indented under `seq:`

**Recently fixed:**

- **QLJ7 (Tag shorthand scope):** Tag handles now validated per-document
- **9HCY, 9MMA, B63P (Directive validation):** Now pass - likely fixed by stream lexer improvements

---

## Issue #8: Content at Column 0 After Block Structure Is Parsed as New Document (GT5M, H7J7, G9HC, CXX2)

**Status:** Root-caused, not fixed (would require significant refactoring)

**Tests affected:** GT5M, H7J7, G9HC, CXX2

**Problem description:**
When content at column 0 follows a block structure (sequence or mapping), the parser treats it as a new implicit document. But in some cases this is invalid:

**Example 1 - GT5M:**

```yaml
- item1
&node
- item2
```

**Current behavior:** Parses as two documents: `[item1]` and `&node [item2]`
**Expected:** Error - `&node` at column 0 after a sequence item is not valid (not a `-`)

**Example 2 - H7J7:**

```yaml
key: &x
!!map
  a: b
```

**Current behavior:** Parses as two documents: `{key: null}` and `!!map {a: b}`
**Expected:** Error - `!!map` at column 0 is not properly indented for the value position

**Root cause:**
In `parse_stream()`, when `parse_value(0)` completes for the first structure (e.g., the sequence or mapping),
the parser returns to `parse_stream`, finds more content at column 0, and parses it as a new implicit document.

The fundamental issue is distinguishing between:

1. **Valid implicit new document:** Content at column 0 that forms a valid standalone structure
2. **Invalid continuation:** Content at column 0 that violates structure rules

For sequences: `- item1\n- item2` is ONE sequence, but `- item1\n&node\n- item2` should error because
`&node` interrupts the sequence structure without being a valid sequence item.

**Attempted solutions:** None yet

**Why not fixed:**
This would require significant changes to how `parse_stream` interacts with block structure parsers:

1. Block sequence/mapping parsers would need to report "I stopped because of invalid content" vs "I stopped because structure ended"
2. `parse_stream` would need to distinguish between "valid document end" and "invalid content interrupting structure"
3. Could potentially be solved by having block parsers error on invalid content at their indentation level instead of stopping

**Related to:** Issue #7 (invalid content after sequences)

---

## Issue #9: Orphan Block Indicators at Invalid Indentation (4HVU, ZVH3, DMG6, etc.)

**Date Identified:** 2026-02-18
**Status:** PARTIALLY RESOLVED (INDENT/DEDENT implemented, most tests now pass)

**Original Failing Tests:** 4HVU, ZVH3, DMG6, EW3V, N4JP, U44R (indentation validation failures)
**Now Passing:** 4HVU, ZVH3, DMG6, EW3V, N4JP, U44R - all these tests now pass after INDENT/DEDENT implementation

**Example - 4HVU:**

```yaml
key:
   - ok       # indent 3
   - also ok  # indent 3
  - wrong     # indent 2 (INVALID)
```

**Expected:** Error - `- wrong` at indent 2 is invalid (less than sibling items at indent 3)

**Current Behavior:** Parser correctly ends sequence at `LineStart(2)`, but `- wrong` is left unparsed and causes no error.

**Root Cause Analysis:**

The fundamental issue is **orphan block indicators** - block structure markers (`-`, `?`, `:`) that appear at indentation levels that don't fit any valid structure:

1. When `parse_block_sequence()` sees `LineStart(2) < seq_indent(3)`, it correctly returns (sequence ends)
2. Control returns to `parse_scalar_or_mapping()` which was parsing the value of `key:`
3. The mapping parser expects content at indent 0 (the map's indent), not indent 2
4. The orphan `- wrong` at indent 2 is neither:
   - At the sequence level (indent 3)
   - At the mapping level (indent 0)
5. Currently, this orphan is silently ignored or parsed as a new implicit document

**User's Architectural Question:**
"Why is the lexer not outputting end of block sequence?"

**Answer:** YAML block collections don't have explicit end markers (unlike flow collections with `]` and `}`). Block collections end implicitly based on indentation changes. The lexer only emits `LineStart(n)` tokens; the parser determines collection boundaries.

**Potential Solutions:**

1. **Lexer emits BlockStart/BlockEnd tokens (RECOMMENDED):** The lexer tracks an indentation stack and emits:
   - `BlockStart(indent)` when indentation increases
   - `BlockEnd` when indentation decreases (one per level popped)

   This mirrors Python's INDENT/DEDENT tokens and aligns with how flow collections already work
   (`FlowSeqStart`/`FlowSeqEnd`, `FlowMapStart`/`FlowMapEnd`).

2. **Parser validates orphan content:** After returning from a nested structure, the parser checks if remaining content at intermediate indentation levels is valid. This requires passing "expected indentation" context through the call stack.

3. **Error on unexpected block indicators:** In `parse_value()`, when we see `BlockSeqIndicator` (`-`) at an indentation that doesn't match any active structure, emit an error.

**IMPLEMENTED (2026-02-19): Option 1 (INDENT/DEDENT tokens)**

Following user's suggestion, implemented Python-style INDENT/DEDENT tokens (named `Indent(n)` and `Dedent`
instead of `BlockStart`/`BlockEnd` to avoid assuming block/flow context at lexer level).

Implementation details:

1. Added `Indent(usize)` and `Dedent` to `Token` enum in `lexer.rs`
2. In `context_lexer.rs`:
   - Added `indent_stack: Vec<usize>` field initialized to `[0]`
   - After emitting `LineStart(indent)`, compare `indent` to `stack.top()`:
     - If `indent > stack.top()`: push `indent`, emit `Indent(indent)`
     - If `indent < stack.top()`: pop and emit `Dedent` for each level
   - Only emit INDENT/DEDENT when `flow_depth == 0` (block context)
3. Updated `parser.rs`:
   - `skip_ws()` now skips `Indent` tokens (like whitespace) but NOT `Dedent`
   - Block structure parsers use `Dedent` as a signal to stop parsing
   - `parse_single_document()` skips remaining `Dedent` tokens after document parsing

**Results:**

- Test suite improved from 307/333 to 318/333 (95.5%)
- Tests 4HVU, ZVH3, DMG6, EW3V, N4JP, U44R now pass
- Zero parse errors on valid input

**Remaining indentation issues (see test categories above):**

- Flow/quoted scalar continuation indentation (9C9N, QB6E)

---

### Tests to Revisit

**Still failing (3 tests - all "Expected error but parsing succeeded"):**

- 9C9N, QB6E (flow/quoted scalar indentation validation)
- G9HC (anchor indentation)

**Previously failing, now fixed:**

- QLJ7 (tag shorthand scope - fixed by per-document tag handle validation)
- 9HCY, 9MMA, B63P (directive validation - fixed by stream lexer document boundary improvements)
- SU74 (anchor on alias - fixed by collecting key properties in mapping parsers)
- H7J7 (node anchor not indented - fixed along with SU74)
- RXY3, 5TRB (document markers inside quoted strings - fixed by modal lexer)
- 3HFZ (invalid content after document end - fixed by seen_doc_end_on_line flag)
- N782 (document markers in flow style - fixed by modal lexer)
- H7TQ, SF5V (YAML directive validation - fixed by directive parsing improvements)
- CXX2 (mapping on --- line - fixed earlier)
- KS4U (content after flow sequence - fixed earlier)
- BD7L, TD5N, GT5M (invalid content after block sequence - fixed earlier)
- BS4K (comment terminates plain scalar - fixed earlier)
- 5LLU, S98Z, W9L4 (block scalar indentation validation)
- 4HVU, ZVH3, DMG6, EW3V, N4JP, U44R (orphan block indicators - fixed by INDENT/DEDENT)
- 6S55, 9KBC (fixed by earlier changes)
- V9D5 (explicit mapping keys - fixed by first_entry flag)
- HS5T (multiline plain scalar continuations - fixed by min_indent.max(1))

**Still on the list for potential fixes:**

- 2SXE (anchor names with colons - lexer fix needed)
- ZCZ6 (nested mappings with plain scalars - validation rule needed)

---

## Issue #10: Modal Lexer Architecture (NEW - 2026-02-19)

**Status:** IMPLEMENTED

**Background:**

The original layered architecture (stream_lexer → context_lexer → parser) had fundamental information gaps:
- Stream lexer split documents but didn't understand quoted strings
- Couldn't distinguish `'...'` (string content) from `...` (document end marker)
- Context lexer saw content but didn't know about document boundaries

**Solution Implemented:**

Created `modal_lexer.rs` - a single lexer with internal mode switching:

1. **Stream Mode:** Between documents
   - Handles `%YAML`, `%TAG` directives
   - Recognizes `---`/`...` as document markers at column 0
   - Tracks `seen_yaml_directive` for duplicate detection

2. **Document Mode:** Inside documents
   - Full content tokenization
   - Tracks `flow_depth` (nested `{`/`[`)
   - Quote state (inside single/double quotes)
   - Indentation levels
   - Only recognizes `---`/`...` as markers when NOT in quotes/flow AND at column 0
   - Tracks `seen_doc_end_on_line` for validating content after `...`

**Key Files:**
- `src/modal_lexer.rs` - New modal lexer implementation (~1140 lines)
- `src/lib.rs` - Updated to use `parse_layered()` with modal lexer
- Old `stream_lexer.rs` and `context_lexer.rs` still exist but are unused (can be removed)

**Tests Fixed by Modal Lexer:**
- RXY3: `...` inside single-quoted string
- 5TRB: `---` inside double-quoted string
- 3HFZ: Invalid content after `...` on same line
- N782: Document markers inside flow context

---

## Issue #11: Directive Validation Gaps

**Status:** FIXED

**Fixed:**
- H7TQ: Extra words on %YAML directive (`%YAML 1.2 foo`)
- SF5V: Duplicate YAML directive
- 9HCY: Directive after document content without `...` footer
- 9MMA: Directive by itself with no document following
- B63P: Directive followed by document end marker

**Implementation Notes:**
The stream_lexer now properly handles document boundaries:
- When `...` is encountered, the current document is finalized immediately
- Directives after `...` are collected for the next document
- When `---` is encountered with only directives (no content), those directives belong
  to the document being started, not a separate empty document

This architecture naturally handles the directive validation cases because orphan directives
(without following content) result in proper error detection.

---

## Milestone: 100% YAML 1.2 Test Suite Compliance

**Date Achieved:** 2026-02-19

**Final Status:** 333/333 tests passing (100%)

### Key Improvements Made

1. **String Tokenization Refactor**: Changed from consuming entire quoted strings as single tokens to emitting separate `StringStart(QuoteStyle)`, `StringContent(String)`, `LineStart(n)`, and `StringEnd(QuoteStyle)` tokens. This allows the parser to validate continuation indentation inside strings.

2. **Flow Context Column Tracking**: Added `flow_context_columns: Vec<usize>` to the parser to track the starting column of each nested flow context. This enables validation that continuation lines in flow collections don't appear at column 0 when the flow collection started at a column > 0.

3. **Tab-Indented Flow Collections**: Fixed validation to check the actual column of content tokens after whitespace/tabs, not just the `LineStart(n)` value which doesn't account for tabs.

4. **Anchor Indentation Validation**: Added check that anchors appearing as values must be at indentation >= `min_indent`. This catches cases like `seq:\n&anchor\n- a` where the anchor at column 0 is invalid for a value that requires indentation >= 1.

5. **Orphaned Properties Detection**: When parsing remaining mapping entries, if properties (anchor/tag) are collected but no scalar key follows, an error is now reported for the orphaned properties.

### Tests Fixed by These Changes

- **6CA3**: Tab indented top flow - Tab-indented flow sequences now correctly validate
- **9C9N**: Flow sequence with column 0 continuations - Error correctly raised
- **QB6E**: Quoted string with column 0 continuations - Error correctly raised
- **G9HC**: Invalid anchor in zero indented sequence - Error correctly raised
- **NAT4, XV9V**: Empty lines in quoted strings - Correctly allowed regardless of indentation

### Architecture Notes

The parser now uses a consistent pattern for tracking context:
- `flow_depth: usize` - Depth of nested flow collections
- `flow_context_columns: Vec<usize>` - Starting column of each flow context
- `in_quoted_string: bool` (in lexer) - Suppresses INDENT/DEDENT inside strings
- `min_indent: usize` parameter - Threaded through parsing to validate indentation
