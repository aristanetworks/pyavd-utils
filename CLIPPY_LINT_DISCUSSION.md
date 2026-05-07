<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Clippy Lint Discussion

This document covers the Clippy rules in [Cargo.toml](Cargo.toml) that are currently marked `TODO: discuss`.

The main point to keep in mind is that these are all `clippy::restriction` lints. Restriction lints are useful when a project wants a very specific house style, but they are not automatically more idiomatic than ordinary Rust. For each lint below, "suggested level" means the level that is most likely to produce idiomatic code for this repository, not the only defensible setting.

## Summary

| Lint | Current level | Suggested level | Why |
| --- | --- | --- | --- |
| `ref_patterns` | `allow` | `deny` | Modern match ergonomics and `as_ref`/`as_mut` usually make `ref` bindings noisier than the alternatives. |
| `semicolon_inside_block` | `allow` | `deny` | This enforces the common Rust style of putting the semicolon inside a unit-returning block. |
| `semicolon_outside_block` | `allow` | `allow` | Denying this would push toward the less common mirror style. |
| `separated_literal_suffix` | `allow` | `deny` | Unsuffixed separation such as `1u8` is the more common Rust style. |
| `unseparated_literal_suffix` | `allow` | `allow` | Denying this would force `1_u8`, which is readable but less common. |
| `string_add` | `allow` | `allow` | `String + &str` is valid idiomatic Rust in simple consume-and-append cases; alternatives are context-dependent. |
| `unwrap_used` | `allow` | `deny` | Library code should normally propagate or model failure instead of panicking; tests are already covered by `clippy.toml`. |

## `ref_patterns`

This lint objects to `ref` and `ref mut` bindings inside patterns.

Example that the lint is meant to discourage:

```rust
if let Some(ref mut items) = coerced_items {
    items.push(value);
}

match &node.value {
    Value::String(ref text) => println!("{text}"),
    _ => {}
}
```

More idiomatic alternatives usually make the borrowing point clearer:

```rust
if let Some(items) = coerced_items.as_mut() {
    items.push(value);
}

match &node.value {
    Value::String(text) => println!("{text}"),
    _ => {}
}
```

Suggested level: `deny`.

Rust's match ergonomics usually remove the need for `ref` in patterns, and `Option::as_ref` or `Option::as_mut` is often clearer when the goal is to borrow the inside of an `Option`. There are still complex destructuring cases where `ref` can be concise, but with `allow_attributes_without_reason = "deny"` those can be handled by a local `#[expect(clippy::ref_patterns, reason = "...")]`.

## `semicolon_inside_block`

This lint objects to a semicolon placed after a unit-returning block when it can be placed inside the block.

Example that the lint is meant to discourage:

```rust
unsafe {
    ffi_call()
};
```

Preferred style:

```rust
unsafe {
    ffi_call();
}
```

Suggested level: `deny`.

For standalone blocks, the common Rust style is to terminate the inner statement and leave the closing brace unadorned. This also matches how `if`, `match`, and `unsafe` blocks are normally written when they are used for side effects.

## `semicolon_outside_block`

This is the mirror preference of `semicolon_inside_block`: it prefers the semicolon after the block expression instead of inside it, where the lint applies.

Style this pushes toward:

```rust
unsafe {
    ffi_call()
};
```

Style it pushes away from:

```rust
unsafe {
    ffi_call();
}
```

Suggested level: `allow`.

The outside-block form is legal, but it reads more like "evaluate this block expression and discard its value" than "run this statement". That distinction is useful when a block produces a meaningful value, but for unit-returning side-effect blocks the inside semicolon is easier to scan and closer to normal Rust formatting.

## `separated_literal_suffix`

This lint objects to a numeric suffix separated from the literal by an underscore.

Example that the lint is meant to discourage:

```rust
let byte = 1_u8;
let mask = 1_u128 << 63;
```

Preferred style if this lint is denied:

```rust
let byte = 1u8;
let mask = 1u128 << 63;
```

Suggested level: `deny`.

The unseparated form is the more common Rust spelling for simple type suffixes. The separated form can be readable when the literal already has digit grouping, but suffixes are type annotations rather than part of the numeric grouping itself, so a consistent unseparated style is easier to recognize.

## `unseparated_literal_suffix`

This lint objects to a numeric suffix directly attached to the literal.

Example that the lint is meant to discourage:

```rust
let byte = 1u8;
let mask = 1u128 << 63;
```

Preferred style if this lint is denied:

```rust
let byte = 1_u8;
let mask = 1_u128 << 63;
```

Suggested level: `allow`.

This setting is the counterpart to `separated_literal_suffix`; the two should not both be denied. For this repository, allowing unseparated suffixes and denying separated suffixes gives the more conventional result.

## `string_add`

This lint objects to using the `+` operator for string concatenation.

Example that the lint is meant to discourage:

```rust
let path = base + "/" + name;
```

Often clearer alternatives:

```rust
let path = format!("{base}/{name}");
```

```rust
let mut path = base;
path.push('/');
path.push_str(name);
```

Suggested level: `allow`.

`String + &str` consumes the left-hand `String`, which can be surprising in larger expressions, and `format!` or `push_str` is often clearer when assembling more than one part. Still, the operator is an ordinary Rust API and is idiomatic for simple "consume this owned string and append one borrowed string" cases. A blanket denial is more of a house-style rule than an idiom rule.

## `unwrap_used`

This lint objects to `unwrap` and `unwrap_err`.

Example that the lint is meant to discourage in library code:

```rust
let schema = store.get("avd_design").unwrap();
let parsed = serde_json::from_str(input).unwrap();
```

More idiomatic production code usually propagates or models the failure:

```rust
let schema = store
    .get("avd_design")
    .ok_or_else(|| SchemaError::missing("avd_design"))?;

let parsed = serde_json::from_str(input)?;
```

Suggested level: `deny`.

For library code, panicking on an ordinary fallible operation is rarely the right contract. Denying this lint pushes code toward `Result`, `Option`, explicit error variants, or a deliberately documented panic point. This repository's `clippy.toml` already sets `allow-unwrap-in-tests = true`, so denying `unwrap_used` at the workspace level should not require changing normal test assertions just to appease Clippy.

For genuinely infallible construction, prefer a narrow local expectation with a reason:

```rust
#[expect(clippy::unwrap_used, reason = "the regex is a fixed literal tested at startup")]
static REF_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[a-z][a-z_]*#").unwrap());
```
