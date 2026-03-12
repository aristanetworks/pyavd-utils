<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# ValidatableValue Trait Design

## Goal

Create a trait abstraction that allows the validation code to work with both:

- `serde_json::Value` (current implementation)
- `yaml_parser::Node` (new YAML parser with span tracking)

This enables:

1. Gradual migration from serde_json to yaml_parser
2. Better error reporting with source spans from yaml_parser
3. Reuse of validation logic across both representations

## Current Usage Analysis

The validation code uses these operations on `serde_json::Value`:

| Operation | Example | Purpose |
| --------- | ------- | ------- |
| `value.as_object()` | `value.as_object()` | Get as mapping |
| `value.as_array()` | `value.as_array()` | Get as sequence |
| `value.as_str()` | `value.as_str()` | Get string value |
| `value.as_i64()` | `value.as_i64()` | Get integer value |
| `value.as_bool()` | `value.as_bool()` | Get boolean value |
| `value.is_null()` | `value.is_null()` | Check for null |
| `value.get(key)` | `item.get(primary_key)` | Get child by key |
| Map iteration | `input.iter()` | Iterate key-value pairs |
| Array iteration | `items.iter().enumerate()` | Iterate with index |
| `input.contains_key()` | `input.contains_key(key)` | Check key exists |

## Trait Design

### Core Trait: `ValidatableValue`

```rust
pub trait ValidatableValue: Sized {
    // Type checking
    fn is_null(&self) -> bool;

    // Value extraction (returns None if wrong type)
    fn as_str(&self) -> Option<&str>;
    fn as_i64(&self) -> Option<i64>;
    fn as_f64(&self) -> Option<f64>;
    fn as_bool(&self) -> Option<bool>;

    // Structural access
    fn as_mapping(&self) -> Option<impl ValidatableMapping<Value = Self>>;
    fn as_sequence(&self) -> Option<impl ValidatableSequence<Value = Self>>;

    // Quick child access (for lookups like `item.get(primary_key)`)
    fn get(&self, key: &str) -> Option<&Self>;
}
```

### Helper Trait: `ValidatableMapping`

```rust
pub trait ValidatableMapping {
    type Value: ValidatableValue;

    fn get(&self, key: &str) -> Option<&Self::Value>;
    fn contains_key(&self, key: &str) -> bool;
    fn iter(&self) -> impl Iterator<Item = (&str, &Self::Value)>;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool { self.len() == 0 }
}
```

### Helper Trait: `ValidatableSequence`

```rust
pub trait ValidatableSequence {
    type Value: ValidatableValue;

    fn iter(&self) -> impl Iterator<Item = &Self::Value>;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool { self.len() == 0 }
}
```

## Implementation Plan

### Phase 1: Define Traits

1. Create `rust/validation/src/validatable.rs` with trait definitions
2. Export from `mod.rs`

### Phase 2: Implement for serde_json::Value

1. Implement `ValidatableValue` for `serde_json::Value`
2. Implement `ValidatableMapping` for `serde_json::Map<String, Value>`
3. Implement `ValidatableSequence` for `Vec<Value>`
4. Write unit tests

### Phase 3: Implement for yaml_parser::Node

1. Add `yaml-parser` as dependency to validation crate
2. Implement `ValidatableValue` for `yaml_parser::Node`
3. Implement `ValidatableMapping` for node mappings
4. Implement `ValidatableSequence` for node sequences
5. Write unit tests

### Phase 4: Refactor Validation Code

1. Update `Validation` trait to use `ValidatableValue` instead of `Value`
2. Update each validation module (dict, list, str, int, boolean, any)
3. Ensure all tests pass

### Phase 5: Integration

1. Add span-aware error reporting (optional enhancement)
2. Update `validate_yaml` to use yaml_parser + new validation path

## Key Challenges

### 1. Lifetimes

`yaml_parser::Node<'input>` has a lifetime parameter. The trait needs to handle this:

```rust
impl<'input> ValidatableValue for yaml_parser::Node<'input> { ... }
```

### 2. Return Types

`as_mapping()` and `as_sequence()` return different concrete types for each implementation.
Using `impl Trait` in return position (RPITIT) requires Rust 1.75+.

### 3. String Ownership

- `serde_json::Value` uses `String`
- `yaml_parser::Node` uses `Cow<'input, str>`
Both can return `&str`, so this should work.

### 4. Mapping Key Types

- `serde_json::Map` uses `String` keys
- `yaml_parser` mappings use `Node` keys (which could be non-strings)
For validation, we only care about string keys, so return `&str`.

## File Structure

```text
rust/validation/src/
├── validatable.rs          # NEW: Trait definitions + impls
├── validation/
│   ├── mod.rs              # Update Validation trait
│   ├── dict.rs             # Refactor to use ValidatableValue
│   ├── list.rs             # Refactor to use ValidatableValue
│   └── ...
└── ...
```

## Testing Strategy

1. Unit tests for each trait implementation
2. Existing validation tests should continue to pass
3. New tests validating yaml_parser::Node values
4. Integration tests comparing results from both paths

## Cleanup Tasks

The following tasks should be done once the migration is complete:

- [ ] Remove `validate_value` method from `Validation` trait (currently a default impl
      that delegates to `validate_any` for backward compatibility)
- [ ] Update all test call sites from `validate_value` to `validate_any`
- [ ] Remove the `serde_json::Value` import from `validation/mod.rs` once `validate_value` is removed
