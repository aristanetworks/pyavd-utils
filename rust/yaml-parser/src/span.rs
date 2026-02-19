// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Span types for tracking source locations.

use chumsky::span::SimpleSpan;

/// A span representing a range in the source code.
///
/// This is an alias for chumsky's `SimpleSpan`, which tracks byte offsets.
/// The span is a half-open range `[start, end)`.
pub type Span = SimpleSpan<usize>;

/// A value with an associated source span.
///
/// This is the fundamental type for representing parsed values with their
/// source locations. Every node in the AST carries its span.
pub type Spanned<T> = (T, Span);

/// Extension trait for creating spanned values.
pub trait WithSpan: Sized {
    /// Attach a span to this value.
    fn with_span(self, span: Span) -> Spanned<Self> {
        (self, span)
    }
}

impl<T> WithSpan for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::span::Span as _;

    #[test]
    fn test_spanned_value() {
        let value = "hello".to_owned();
        let spanned = value.with_span(Span::new((), 0..5));
        assert_eq!(spanned.0, "hello");
        assert_eq!(spanned.1.start, 0);
        assert_eq!(spanned.1.end, 5);
    }
}
