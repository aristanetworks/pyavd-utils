
<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

Here’s a first pass at the **indentation-state-design** doc, at the “plan / architecture” level you asked for (no per-call-site details, no code changes).

You can drop this into something like `rust/yaml-parser/INDENTATION_STATE_DESIGN.md` or similar if you like.

---

## Indentation State Design (Plan)

### 1. Motivation

The current emitter relies on a mix of:

- **Lexer-level indentation tokens**: `LineStart(n)`, `Indent(n)`, `Dedent`, etc.
- **Global column computations** via `column_of_position(span.start_usize())`, which scan the input string to find the previous newline.
- **Ad-hoc indentation checks** scattered across:
  - Block sequences (`process_block_seq`)
  - Block mappings (`process_block_map` and `skip_ws_and_newlines_for_mapping`)
  - Value parsing (`Value` / `ValueAfterProperties`)
  - Error checks for `InvalidIndentation`, `InvalidIndentationContext`, `InvalidDirective`, tabs in block scalars, etc.

This leads to:

- **Complexity**: indentation semantics are partly encoded in the lexer, partly in emitter helper functions, and partly at individual call sites using column calculations.
- **Performance risk**: repeated `column_of_position` calls on large inputs induce O(n²)-ish behaviour, particularly visible on `large_sequence`.
- **Asymmetry**: block mappings use a more stateful indentation model (with `skip_ws_and_newlines_for_mapping`, `current_indent`, and `indent_stack`), while block sequences rely more heavily on per-token column computations.

The goal of this design is to:

1. Move indentation handling **into the state machine** (parse states + `ValueContext`), rather than re-deriving it step by step from the raw input string.
2. Make block sequences and mappings share a **consistent indentation model**.
3. Reduce or eliminate **semantic** uses of `column_of_position` in hot paths.
4. Revisit whether we still need the lexer’s `Indent/Dedent` machinery once the emitter owns indentation semantics.

---

### 2. Existing Building Blocks

Relevant current abstractions:

- **Lexer**:
  - Emits `LineStart(n: IndentLevel)` for each new logical line.
  - Emits `Indent(n)` / `Dedent` tokens to reflect changes in indentation stack.
  - Does *not* track explicit “column” beyond those tokens; column is reconstructed in the emitter via `column_of_position`.

- **Emitter struct**:
  - Tracks:
    - `current_indent: IndentLevel` (updated on `LineStart(n)`).
    - `indent_stack: Vec<IndentLevel>` for active block structures.
    - `crossed_line_boundary`, `last_line_start_span`, etc.
  - Uses `column_of_position` in several places for decisions like:
    - “is this `-` at or below the sequence indent?”
    - “is this `-` inside the content area rather than at block level?”
    - “is `%` at column 0?” (directive vs content)
    - Various tab / indentation error checks.

- **State stack (`ParseState`) and `ValueContext`**:

````rust path=rust/yaml-parser/src/emitter/states.rs mode=EXCERPT
pub(super) struct ValueContext {
    pub min_indent: IndentLevel,
    pub kind: ValueKind,
    pub allow_implicit_mapping: bool,
}

pub(super) enum ParseState<'input> {
    BlockSeq { indent: IndentLevel, phase: BlockSeqPhase, start_span: Span, ... },
    BlockMap { indent: IndentLevel, phase: BlockMapPhase, start_span: Span, ... },
    Value    { ctx: ValueContext, ... },
    ValueAfterProperties { ctx: ValueContext, ... },
    // ...
}
````

Key point: the state machine *already* carries an indentation parameter (`indent`, `min_indent`), but not all indentation decisions are expressed in terms of that state.

---

### 3. Design Direction

#### 3.1. Principle: Structured indentation in state, not ad-hoc column checks

We want to make the **state stack** the single source of truth for:

- What is the **base indentation** of the current construct?
- How do we interpret upcoming `LineStart(n)`:
  - as a dedent (end of this construct),
  - as same-level continuation,
  - or as deeper indentation (nested value / block scalar content)?

Concretely:

- For any active *block* construct (sequence, mapping, block scalar):
  - The relevant parse state holds:
    - A **base indent** (`block_indent`) for entries / key positions.
    - Optionally, a **content indent** (`content_indent`) for block scalars.
- `ValueContext.min_indent` is the **canonical minimum allowed indent** for the value at that position.
- When a `LineStart(n)` is consumed:
  - Comparisons are always done against these stored state values (`block_indent`, `min_indent`, `content_indent`), not against a freshly computed absolute column.

This implies:

- Semantic decisions such as “does this `-` start a new sequence entry?” or “have we dedented below this mapping?” are expressed as:
  - `n < block_indent` → structure ends.
  - `n == block_indent` → new entry at this level.
  - `n > block_indent` → nested structure or block scalar.

#### 3.2. Block sequences

Current behaviour (simplified):

- `BlockSeq { indent }` is pushed when we start a sequence.
- In `BeforeEntry`:
  - We call `skip_ws_and_newlines()`.
  - We peek:
    - If we see `BlockSeqIndicator` at column computed via `column_of_position`:
      - If `entry_indent < indent` → `SequenceEnd`.
      - Else → continue; value’s `min_indent = entry_indent + 1`.
    - If we see doc markers / EOF / dedent-like patterns → `SequenceEnd`.
    - Else → trailing-content checks using further `column_of_position` calls.

Target design:

- `BlockSeq.indent` is the **canonical block indent** for sequence entries.
- We avoid recomputing global columns for structural decisions:

  - After `skip_ws_and_newlines_returns_crossed_line()`:
    - If we *crossed a line* and `current_indent < BlockSeq.indent`:
      - End the sequence (`SequenceEnd`), exactly like mapping’s `BeforeKey` uses `current_indent < indent`.
    - If `current_indent == BlockSeq.indent` and we see `BlockSeqIndicator`:
      - New entry at this level.
    - If `current_indent > BlockSeq.indent`:
      - We’re inside content nested under an entry: this should be handled by the *value parser* (`Value` / `ValueAfterProperties`) using `ValueContext.min_indent`.

- For **same-line nested sequences** such as:

````yaml mode=EXCERPT
- - nested
````

- The *outer* sequence’s entry value has `ValueContext.min_indent = BlockSeq.indent + 1`.
- While parsing that value:
  - We treat a `BlockSeqIndicator` as a candidate nested sequence **if**, in the `Value` logic, it appears at (or above) `min_indent` relative to the current line’s indentation.
  - This decision happens in the *value* state, not via a free-floating `column_of_position` check inside `BlockSeq::BeforeEntry`.

The key shift:

> Block sequence semantics become a combination of `BlockSeq.indent`, `ValueContext.min_indent`, and `LineStart(n)` / `current_indent`, rather than global column arithmetic.

#### 3.3. Block mappings

Block mappings already follow a more stateful approach:

- `BlockMap { indent }` uses:
  - `skip_ws_and_newlines_for_mapping(indent)` which understands blank lines and comment-only lines.
  - `current_indent < indent` to detect dedent/end.
  - `indent_stack` and `is_valid_indent` for orphan-indentation detection.

Plan:

- **Align block sequences to this model**, and
- Revisit block mapping logic to:
  - Express any remaining column-based semantics (e.g. “BlockSeqIndicator inside content area”) as **relative to `ValueContext.min_indent` and `current_indent`**.
  - Reduce or remove reliance on `column_of_position` except where the exact column is needed purely for error spans.

This is also an opportunity to sanity-check whether block mappings always behave correctly around:

- implicit keys,
- complex keys spanning multiple lines,
- sequences nested under mappings with tricky indentation.

#### 3.4. Value-level base indent

`ValueContext.min_indent` is our natural hook for a **base indentation per value**:

- For any `Value` or `ValueAfterProperties` state:
  - `ctx.min_indent` indicates the minimum indent at which the value’s own content may appear.
  - For nested collections (block seq/map, block scalar content), this becomes their “anchor” for interpreting subsequent `LineStart(n)` tokens.

We want:

- All “is this allowed here?” indentation checks for scalars, nested sequences, and nested mappings to be described in terms of:
  - `ctx.min_indent`
  - `current_indent` from `LineStart(n)`
  - The parent block’s `indent`/`content_indent` if applicable.

This makes indentation a *local* property of each state, instead of something inferred from absolute columns.

---

### 4. Role of `column_of_position` after the refactor

We don’t necessarily need to delete `column_of_position`, but we want to:

- **Remove it from semantic control flow** in hot paths, especially:
  - `process_block_seq`
  - `Value` / `ValueAfterProperties` decision logic
  - “BlockSeqIndicator inside content vs at entry position” checks.

Use cases where it may still make sense to keep it:

- **Diagnostics / suggestions** only, e.g.:
  - “`%` at column 0 is an invalid directive here.”
  - Detailed `InvalidIndentationContext` messages that want the exact column number.
- Non-hot paths where correctness and clarity trump every microsecond.

If, after refactoring, all semantic decisions are expressible with:

- `ValueContext.min_indent`
- `BlockSeq.indent` / `BlockMap.indent`
- `current_indent` and `indent_stack`
- The phase enums (`BlockSeqPhase`, `BlockMapPhase`, etc.)

then `column_of_position` becomes an implementation detail for error reporting and can even be simplified or localized.

---

### 5. Impact on the lexer (`Indent`/`Dedent`)

Once the emitter owns indentation semantics more completely, we should reassess:

- Do we still gain enough from the lexer emitting `Indent`/`Dedent` tokens, or can we:
  - Rely solely on `LineStart(n)` + `ValueContext.min_indent` + emitter-managed `indent_stack`, or
  - Simplify the lexer’s indentation model so it emits fewer structural tokens?

Questions to answer in that follow‑up:

1. Which parts of the emitter currently *require* `Indent`/`Dedent`?
   - Some mapping logic uses `Dedent` explicitly (e.g. consuming one dedent when leaving a mapping).
2. After the redesign, can the emitter:
   - Maintain `indent_stack` purely from `LineStart(n)` and its own decisions, without help from lexer `Indent`/`Dedent`?
   - Or at least use a simpler subset of what the lexer emits?

If we can remove or simplify `Indent`/`Dedent` handling:

- The lexer becomes conceptually simpler.
- The interface between lexer and emitter becomes:
  - “line starts at indent `n`”
  - “stream of content tokens”
  with indentation semantics wholly encapsulated in the emitter state machine.

---

### 6. Plan of Record (high-level steps)

This is intentionally **not** per-call-site, but a sequence of conceptual refactors:

1. **Inventory & classification**
   Catalogue all uses of `column_of_position` and indentation-sensitive logic in the emitter. For each call site, classify:
   - Semantic (affects which events are emitted / structure).
   - Diagnostic-only.

2. **Define the indentation model per state**
   For each relevant `ParseState`:
   - `BlockSeq`: what is `indent` intended to mean? (entry base indent).
   - `BlockMap`: confirm and document its meaning of `indent`.
   - `Value` / `ValueAfterProperties`: precise semantics of `ctx.min_indent` in block vs flow context.
   - Block scalar states (if any): how they relate to content vs block base indent.

   Write this down clearly (e.g. in this doc or `ARCHITECTURE.md`).

3. **Refactor semantics to be state‑driven**
   For block sequences and mappings:
   - Express decisions like “new entry vs end vs nested content” purely in terms of:
     - `indent` from the relevant state,
     - `ctx.min_indent`,
     - `current_indent` from `LineStart(n)` / `skip_ws_and_newlines_*`,
     - `indent_stack` and `crossed_line_boundary`.
   - Remove semantic dependencies on `column_of_position`.

4. **Unify sequence and mapping indentation logic where sensible**
   - Extract common logic for “cross line, compare with block indent, handle blank lines/comments”.
   - Ensure block sequences get the same quality of indentation handling as mappings (and vice versa).

5. **Revisit `Indent`/`Dedent` usage**
   With the new state-based indentation model:
   - Identify remaining uses of lexer `Indent`/`Dedent`.
   - Decide whether to:
     - Keep them but simplify how they are consumed, or
     - Move to an emitter-only indentation stack driven by `LineStart(n)` and state transitions.

6. **Keep `column_of_position` for diagnostics only**
   - For error spans/suggestions that genuinely need the exact column, keep (or simplify) `column_of_position`.
   - Ensure it is no longer on the hot path for common data (e.g., large sequences).
