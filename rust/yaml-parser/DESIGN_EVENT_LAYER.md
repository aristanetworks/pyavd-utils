<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Design: Event Emitter Layer

**Status:** ✅ COMPLETE - 100% YAML 1.2 Compliance Achieved
**Author:** AI Assistant
**Date:** 2026-03-04
**Updated:** 2026-03-06
**Tracking:** This document tracks the transformation of the parser to emit events.

---

## 🎉 MILESTONE: 100% YAML Test Suite Compliance (2026-03-06)

The Event Layer has achieved **100% compliance** with the YAML 1.2 Test Suite:

| Test Category | Passed | Failed | Total | Pass Rate |
|---------------|--------|--------|-------|-----------|
| **Event Layer (positive tests)** | 402 | 0 | 402 | **100%** |
| **Error test cases (negative tests)** | 440 | 0 | 440 | **100%** |
| **Total** | **842** | **0** | **842** | **100%** |

### Key Fixes for Final Compliance

1. **Speculative Lookahead for Block Mappings (9KAX)**
   - `parse_block_mapping_with_props` now saves parser state (position, events, errors)
   - Speculatively parses to confirm key+colon pattern exists
   - Only emits `MappingStart` after confirmation
   - Restores state cleanly if not a mapping

2. **Trailing Whitespace in Block Scalars (MJS9)**
   - `consume_block_line_with_detection` recovers trailing whitespace
   - Scans raw input between token span end and newline position
   - Preserves spaces/tabs that lexer consumed but excluded from Plain token spans

3. **Flow Collections as Mapping Keys (LX3P)**
   - `flow_collection_is_mapping_key` lookahead in `parse_value_with_properties`
   - Emits `MappingStart` before flow collection events when key pattern detected

4. **Tab Handling After Directives (DK95-07)**
   - Fixed in `src/lexer/stream.rs` to skip whitespace-only lines correctly

---

## TRANSFORMATION APPROACH (2026-03-05 Update)

Instead of building a separate EventEmitter that duplicates parsing logic, we are
**transforming the existing Parser to emit events**. This preserves the proven
parsing logic while adding event emission.

### Key Insight

The existing Parser already solves all the hard problems:

- Block scalar collection with proper chomping
- Multiline plain scalar continuation
- Indentation tracking
- Flow/block context switching
- Property (anchor/tag) handling

The EventEmitter was failing because it re-implemented these from scratch.

### Transformation Strategy

1. **Add `events: Vec<Event<'input>>` to Parser struct**
2. **Add `emit(&mut self, event)` helper method**
3. **At structure boundaries, emit events:**
   - `parse_block_sequence`: emit `SequenceStart` on entry, `SequenceEnd` on exit
   - `parse_block_mapping`: emit `MappingStart` on entry, `MappingEnd` on exit
   - `parse_scalar`: emit `Scalar` with collected value
   - etc.
4. **Keep the Node building for backwards compatibility**
5. **Add `parse_to_events()` API that returns events instead of nodes**

### Function → Event Mapping

| Parser Function | Emits on Entry | Emits on Exit | Notes |
|----------------|----------------|---------------|-------|
| `parse_stream` | `StreamStart` | `StreamEnd` | Top level |
| Document processing | `DocumentStart` | `DocumentEnd` | Per document |
| `parse_block_sequence` | `SequenceStart` | `SequenceEnd` | Block style |
| `parse_block_mapping` | `MappingStart` | `MappingEnd` | Block style |
| `parse_flow_sequence` | `SequenceStart` | `SequenceEnd` | Flow style |
| `parse_flow_mapping` | `MappingStart` | `MappingEnd` | Flow style |
| `parse_scalar` | `Scalar` | - | With style |
| `parse_literal_block_scalar` | `Scalar` | - | Literal style |
| `parse_folded_block_scalar` | `Scalar` | - | Folded style |
| Alias handling | `Alias` | - | |

### Implementation Order

1. Add events field and emit() to Parser
2. Wire up stream/document events
3. Wire up sequence events (simpler than mapping)
4. Wire up mapping events
5. Wire up scalar events
6. Test against YAML Test Suite

---

## Problem Statement

The current parser is complex because it mixes multiple concerns:

1. **Structure detection** - Is this a mapping? A sequence? Where does it end?
2. **Indentation tracking** - Block structure depends on indent levels
3. **Flow/block mode switching** - Different parsing rules in each mode
4. **Type inference** - Converting scalars to Bool/Int/Float/String
5. **AST building** - Creating Node tree with spans
6. **Error recovery** - Continuing after parse errors

This manifests in functions like `parse_scalar_or_mapping` (~150 lines) which must:

- Parse a scalar
- Look ahead for `:` to detect mapping key
- Handle multiline implicit key errors
- Build nodes with correct spans
- Apply type inference

The YAML Test Suite also tests at the "serialization tree" level (structure + original text),
not the "representation graph" level (typed AST) that we produce. This mismatch requires
span-based reconstruction in the test harness.

---

## Proposed Solution

Add an **Event Emitter** layer between the Document Lexer and Parser:

```text
Current:    Tokens → Parser → AST (typed values)

Proposed:   Tokens → Event Emitter → Parser → AST
                          ↓
                   Serialization Tree Events
                   (structure + original text)
```

---

## Architecture Layers

### Layer 1: Stream Lexer (existing)

- Splits input into documents
- Extracts directives (`%YAML`, `%TAG`)
- Output: `Vec<RawDocument>`

### Layer 2: Document Lexer (existing)

- Tokenizes document content
- Handles INDENT/DEDENT
- Output: `Vec<RichToken>`

### Layer 3: Event Emitter (NEW)

- Consumes tokens, emits structural events
- Tracks indentation and flow depth
- Handles error recovery
- Output: `Iterator<Item = Event>` + `Vec<ParseError>`

### Layer 4: Parser (simplified)

- Consumes events, builds AST
- Applies type inference to scalars
- Resolves anchors/aliases
- Output: `Vec<Node>`

---

## Event Enum Design

```rust
/// Serialization tree events following YAML 1.2 spec.
/// These represent structure + presentation, not typed values.
#[derive(Debug, Clone, PartialEq)]
pub enum Event<'input> {
    StreamStart,
    StreamEnd,

    DocumentStart { explicit: bool },
    DocumentEnd { explicit: bool },

    MappingStart {
        flow: bool,
        anchor: Option<&'input str>,
        tag: Option<&'input str>,
        span: Span,
    },
    MappingEnd { span: Span },

    SequenceStart {
        flow: bool,
        anchor: Option<&'input str>,
        tag: Option<&'input str>,
        span: Span,
    },
    SequenceEnd { span: Span },

    Scalar {
        style: ScalarStyle,
        value: Cow<'input, str>,  // Original text, not type-resolved
        anchor: Option<&'input str>,
        tag: Option<&'input str>,
        span: Span,
    },

    Alias {
        name: &'input str,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarStyle {
    Plain,
    SingleQuoted,
    DoubleQuoted,
    Literal,   // |
    Folded,    // >
}
```

---

## Error Recovery Strategy

Error recovery happens in the **Event Emitter**, not the Parser.

### Rationale

- Recovery is about structure: "skip to next line at this indent"
- Event emitter already tracks indent levels and flow depth
- Parser receives clean event stream, stays simple

### Approach

1. When error detected, record it in shared error list
2. Skip tokens until recovery point (next valid indent, closing bracket, etc.)
3. Emit synthetic events if needed (e.g., `MappingEnd` for unclosed mapping)
4. Continue emitting events

### Example

```yaml
key1: value1
key2: [unclosed
key3: value3
```

Event emitter produces:

```text
MappingStart
Scalar("key1") Scalar("value1")
Scalar("key2") SequenceStart
  [ERROR: unclosed flow sequence]
  [RECOVERY: skip to next line, emit synthetic SequenceEnd]
SequenceEnd
Scalar("key3") Scalar("value3")
MappingEnd
```

Parser builds: `{ key1: value1, key2: [], key3: value3 }`

---

## Simplified Parser

With events, the parser becomes straightforward recursive descent:

```rust
impl<'input> Parser<'input> {
    pub fn parse(&mut self) -> Vec<Node<'input>> {
        let mut documents = Vec::new();

        while let Some(event) = self.next_event() {
            match event {
                Event::DocumentStart { .. } => {
                    documents.push(self.parse_document());
                }
                Event::StreamEnd => break,
                _ => { /* skip */ }
            }
        }

        documents
    }

    fn parse_node(&mut self) -> Option<Node<'input>> {
        match self.next_event()? {
            Event::MappingStart { anchor, tag, span, .. } => {
                Some(self.parse_mapping(anchor, tag, span))
            }
            Event::SequenceStart { anchor, tag, span, .. } => {
                Some(self.parse_sequence(anchor, tag, span))
            }
            Event::Scalar { value, anchor, tag, span, .. } => {
                let typed_value = Self::infer_type(&value);
                Some(Node::new_with_props(typed_value, span, anchor, tag))
            }
            Event::Alias { name, span } => {
                Some(Node::new(Value::Alias(name.into()), span))
            }
            _ => None,
        }
    }

    fn parse_mapping(&mut self, anchor, tag, span) -> Node<'input> {
        let mut pairs = Vec::new();

        loop {
            match self.peek_event() {
                Some(Event::MappingEnd { .. }) => {
                    self.advance();
                    break;
                }
                Some(_) => {
                    let key = self.parse_node().unwrap_or_else(Node::invalid);
                    let value = self.parse_node().unwrap_or_else(Node::invalid);
                    pairs.push((key, value));
                }
                None => break,
            }
        }

        Node::new_with_props(Value::Mapping(pairs), span, anchor, tag)
    }
}
```

---

## Benefits Summary

| Benefit                    | Description                                          |
| -------------------------- | ---------------------------------------------------- |
| **Simpler parser**         | ~50 lines of straightforward recursive descent       |
| **Centralized recovery**   | All error recovery in event emitter, not scattered   |
| **Test suite alignment**   | Events match test format directly                    |
| **Streaming support**      | Process large files without full AST                 |
| **Round-tripping**         | Preserve scalar styles for emit                      |
| **Cleaner separation**     | Each layer does one thing                            |

---

## Migration Path

### Phase 1: Design & Prototype ✅ COMPLETE

- [x] Write this design doc
- [x] Prototype `Event` enum
- [x] Prototype event emitter for simple cases (block mapping, sequence, scalars)
- [x] Prototype simplified parser consuming events
- [x] Validate with subset of test suite

### Phase 2: Feature Parity ✅ COMPLETE (2026-03-06)

- [x] Handle all block structures
- [x] Handle flow collections
- [x] Handle all scalar types (quoted, block)
- [x] Handle anchors, aliases, tags
- [x] Handle directives
- [x] Implement error recovery
- [x] **Pass full test suite (402/402 positive, 440/440 negative)**

### Phase 3: Cleanup (In Progress)

- [ ] Remove old parser code (legacy AST path has 18 known failures)
- [x] Update public API - `emit_events()` function available
- [ ] Update documentation
- [ ] Performance benchmarking

### Phase 4: New Capabilities (optional)

- [ ] Streaming API for large files
- [ ] YAML emitter using events
- [ ] Round-trip preservation

---

## Risks & Mitigations

| Risk                          | Mitigation                                             |
| ----------------------------- | ------------------------------------------------------ |
| Performance regression        | Benchmark at Phase 2 end, optimize if needed           |
| Event emitter becomes complex | It handles fewer concerns than current parser          |
| Breaking API changes          | Keep `parse()` API stable, events are additive         |
| Incomplete migration          | Phase 2 must pass all tests before Phase 3             |

---

## Open Questions

1. **Should events be an iterator or collected Vec?**
   - Iterator: More flexible, streaming-friendly
   - Vec: Simpler, allows random access
   - Recommendation: Start with Vec, add iterator API later

2. **Where does tag expansion happen?**
   - Option A: Event emitter expands tags using directives
   - Option B: Parser expands tags
   - Recommendation: Event emitter, since it has directive context

3. **How to handle implicit vs explicit document markers?**
   - Current: We don't track this well
   - Proposal: Event emitter tracks and emits correct `explicit` flag

4. **Should we expose events in public API?**
   - Yes: Enables streaming, tools, round-tripping
   - Recommendation: Yes, as `parse_events()` function

---

## Appendix: Current vs Proposed Complexity

### Current `parse_scalar_or_mapping` (simplified)

```rust
fn parse_scalar_or_mapping(&mut self) -> Option<Node> {
    let scalar = self.parse_scalar()?;

    // Skip whitespace, track comments
    self.skip_whitespace();

    // Check for mapping key
    if let Some(Token::Colon) = self.peek() {
        // Validate not multiline implicit key
        self.check_multiline_implicit_key(scalar.span);
        self.advance(); // consume colon

        // Parse value
        let value = self.parse_value()?;

        // Check for more entries at same indent
        let pairs = self.collect_mapping_entries(scalar, value)?;

        return Some(Node::mapping(pairs));
    }

    // Just a scalar
    Some(scalar)
}
```

### Proposed: Event emitter handles structure, parser just builds

```rust
// Event emitter (handles the complexity)
fn emit_block_content(&mut self) -> Event {
    let scalar = self.consume_scalar();

    if self.peek_is_colon() {
        self.emit(Event::MappingStart { .. });
        self.emit(Event::Scalar { value: scalar, .. });
        // ... continue emitting mapping content
    } else {
        self.emit(Event::Scalar { value: scalar, .. });
    }
}

// Parser (trivial)
fn parse_node(&mut self) -> Node {
    match self.next_event() {
        Event::MappingStart { .. } => self.parse_mapping(),
        Event::Scalar { value, .. } => Node::new(infer_type(value)),
        // ...
    }
}
```

---

## References

- [YAML 1.2 Spec: Processing Model](https://yaml.org/spec/1.2.2/#31-processes)
- [libyaml event API](https://pyyaml.org/wiki/LibYAML)
- [Current parser architecture](./ARCHITECTURE.md)
