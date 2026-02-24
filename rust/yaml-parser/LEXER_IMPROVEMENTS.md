<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Lexer Improvement Action Plan

**Created:** 2026-02-24
**Status:** In Progress

## Overview

This document tracks improvements to the YAML lexer for better IDE/language server support and performance.

## Use Cases

1. **Validation/Loading** - Parse YAML files for data extraction
2. **Language Server** - IDE integration with real-time feedback

---

## Implementation Status

### Quick Wins (Start Here)

- [x] **1. Add `SourceMap` struct** - Line/column position tracking ✅ 2026-02-24
- [x] **2. Move `Token` to `token.rs`** - Consolidate token types ✅ 2026-02-24
- [x] **3. Remove `Vec<char>` allocation** - Use char iterator optimization ✅ 2026-02-24

### Phase 1: Language Server Foundation

- [ ] **1.1 Token Trivia Preservation** - Associate comments/whitespace with tokens
- [ ] **1.2 Incremental Lexing Support** - Re-lex only affected regions
- [x] **1.3 Line/Column Position Tracking** - SourceMap utility ✅ 2026-02-24 (Quick Win #1)

### Phase 2: Performance Optimizations

- [ ] **2.1 Zero-Copy Tokenization** - Use `Cow<'input, str>` for token content
- [ ] **2.2 Lazy Token Iteration** - Return iterator instead of Vec
- [x] **2.3 Character Iterator Optimization** ✅ 2026-02-24 (Quick Win #3)

### Phase 3: Error Recovery Improvements

- [ ] **3.1 Rich Error Context** - Better error messages with suggestions

### Phase 4: Architecture Refinements

- [x] **4.1 Consolidate Token Types** ✅ 2026-02-24 (Quick Win #2)
- [x] **4.2 State Machine Extraction** - Break up monolithic `next_token()` ✅ 2026-02-24
- [x] **4.3 Unify Quoted String Handling** - Reduce duplication ✅ 2026-02-24

---

## Change Log

### 2026-02-24: Quick Wins and Phase 4 Complete

- ✅ Quick Win #1: Added SourceMap for line/column position tracking
- ✅ Quick Win #2: Extracted token types to dedicated `token.rs` module
- ✅ Quick Win #3: Removed Vec<char> allocation, using string slicing instead
- ✅ Phase 4.2: Extracted state machine from `next_token()` (230→45 lines)
  - Added 10 helper methods for different token types
  - Improved maintainability and testability
- ✅ Phase 4.3: Unified quoted string handling
  - Extracted `handle_quoted_newline()` and `finalize_quoted_string()` helpers
  - Reduced code duplication by ~35 lines

### 2026-02-24: Initial Plan Created

- Analyzed current architecture
- Identified improvement areas
- Created prioritized action plan

---

## Technical Notes

### Current Architecture

- Stream Lexer (`stream_lexer.rs`, 477 lines): Document boundaries/directives
- Context Lexer (`context_lexer.rs`, 1210 lines): Context-aware tokenization
- Legacy Lexer (`lexer.rs`, 858 lines): Chumsky-based, provides Token types

### Key Constraints

- Must maintain 100% YAML 1.2 test suite compliance (333/333 tests)
- Error recovery must continue working
- Span tracking must be preserved
