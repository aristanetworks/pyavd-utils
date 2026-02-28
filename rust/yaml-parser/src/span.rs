// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Span types for tracking source locations.

use std::ops::Range;

/// A span representing a range in the data.
///
/// This is a simple span type that tracks byte offsets as a half-open range `[start, end)`.
/// The span is used throughout the parser to track source locations for error reporting.
///
/// Uses `u32` for compact storage (8 bytes instead of 16), supporting files up to 4GB.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    /// The start byte offset (inclusive).
    pub start: u32,
    /// The end byte offset (exclusive).
    pub end: u32,
}

impl Span {
    /// Create a new span from a range.
    ///
    /// # Panics
    /// Panics in debug mode if the range exceeds `u32::MAX`.
    #[must_use]
    #[inline]
    pub const fn new(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end: range.end as u32,
        }
    }

    /// Create a zero-width span at a single position.
    #[must_use]
    #[inline]
    pub const fn at(pos: usize) -> Self {
        Self {
            start: pos as u32,
            end: pos as u32,
        }
    }

    /// Return the length of the span in bytes.
    #[must_use]
    #[inline]
    pub const fn len(&self) -> usize {
        (self.end.saturating_sub(self.start)) as usize
    }

    /// Check if the span is empty (zero-width).
    #[must_use]
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.start >= self.end
    }

    /// Create a span that encompasses both this span and another.
    #[must_use]
    #[inline]
    pub fn union(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Convert to a `Range<usize>`.
    #[must_use]
    #[inline]
    pub const fn to_range(self) -> Range<usize> {
        self.start as usize..self.end as usize
    }
}

impl From<Range<usize>> for Span {
    #[inline]
    fn from(range: Range<usize>) -> Self {
        Self::new(range)
    }
}

impl From<Span> for Range<usize> {
    #[inline]
    fn from(span: Span) -> Self {
        span.to_range()
    }
}

/// A value with an associated source span.
///
/// This is the fundamental type for representing parsed values with their
/// source locations. Every node in the AST carries its span.
pub type Spanned<T> = (T, Span);

/// A line/column position in source code.
///
/// Line and column numbers are 1-based (first line is line 1, first column is column 1).
/// Column is counted in Unicode code points, not bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    /// 1-based line number.
    pub line: usize,
    /// 1-based column number (in Unicode code points).
    pub column: usize,
}

impl Position {
    /// Create a new position.
    #[must_use]
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

/// Source map for converting byte offsets to line/column positions.
///
/// This is useful for IDE integration where error messages and diagnostics
/// need to be displayed with human-readable line/column numbers.
///
/// # Example
///
/// ```
/// use yaml_parser::SourceMap;
///
/// let source = "key: value\nnested:\n  - item";
/// let map = SourceMap::new(source);
///
/// // "value" starts at byte 5
/// let pos = map.position(5);
/// assert_eq!(pos.line, 1);
/// assert_eq!(pos.column, 6);
///
/// // "nested" starts at byte 11 (after newline)
/// let pos = map.position(11);
/// assert_eq!(pos.line, 2);
/// assert_eq!(pos.column, 1);
/// ```
pub struct SourceMap {
    /// Byte offset of each line start (0-indexed by line number - 1).
    /// `line_starts[0]` is always 0 (start of first line).
    line_starts: Vec<usize>,
}

impl SourceMap {
    /// Create a new source map from input text.
    #[must_use]
    pub fn new(input: &str) -> Self {
        let mut line_starts = vec![0];

        for (byte_pos, ch) in input.char_indices() {
            if ch == '\n' {
                // The next line starts after this newline
                line_starts.push(byte_pos + 1);
            }
        }

        Self { line_starts }
    }

    /// Convert a byte offset to a line/column position.
    ///
    /// Returns a 1-based line and column number.
    /// If the offset is beyond the end of input, returns the position of the last character + 1.
    #[must_use]
    pub fn position(&self, byte_offset: usize) -> Position {
        // Binary search to find the line containing this offset
        let line_idx = self
            .line_starts
            .partition_point(|&start| start <= byte_offset)
            .saturating_sub(1);

        // Note: line_idx is always valid because line_starts always has at least
        // one element (initialized with [0]), and partition_point returns at most
        // len, so saturating_sub(1) gives at most len-1.
        let line_start = self.line_starts.get(line_idx).copied().unwrap_or(0);

        // Column is 1-based, so add 1 to the position within the line
        let column = byte_offset.saturating_sub(line_start) + 1;

        Position {
            line: line_idx + 1, // 1-based line number
            column,
        }
    }

    /// Get the byte range for a given line number (1-based).
    ///
    /// Returns `None` if the line number is out of bounds.
    /// The returned range is `[start, end)` where `end` is either:
    /// - The start of the next line (including the newline character)
    /// - The end of the input for the last line
    #[must_use]
    pub fn line_range(&self, line: usize) -> Option<std::ops::Range<usize>> {
        if line == 0 || line > self.line_starts.len() {
            return None;
        }

        let start = self.line_starts.get(line - 1).copied()?;
        let end = self.line_starts.get(line).copied().unwrap_or(usize::MAX);

        Some(start..end)
    }

    /// Get the total number of lines.
    #[must_use]
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_map_single_line() {
        let map = SourceMap::new("hello");
        assert_eq!(map.line_count(), 1);
        assert_eq!(map.position(0), Position::new(1, 1));
        assert_eq!(map.position(4), Position::new(1, 5));
    }

    #[test]
    fn test_source_map_multiple_lines() {
        let map = SourceMap::new("line1\nline2\nline3");
        assert_eq!(map.line_count(), 3);

        // First line
        assert_eq!(map.position(0), Position::new(1, 1));
        assert_eq!(map.position(4), Position::new(1, 5));
        assert_eq!(map.position(5), Position::new(1, 6)); // newline char

        // Second line starts at byte 6
        assert_eq!(map.position(6), Position::new(2, 1));
        assert_eq!(map.position(10), Position::new(2, 5));

        // Third line starts at byte 12
        assert_eq!(map.position(12), Position::new(3, 1));
    }

    #[test]
    fn test_source_map_line_range() {
        let map = SourceMap::new("ab\ncd\nef");

        assert_eq!(map.line_range(1), Some(0..3)); // "ab\n"
        assert_eq!(map.line_range(2), Some(3..6)); // "cd\n"
        assert_eq!(map.line_range(3), Some(6..usize::MAX)); // "ef" (last line)
        assert_eq!(map.line_range(0), None);
        assert_eq!(map.line_range(4), None);
    }

    #[test]
    fn test_source_map_empty() {
        let map = SourceMap::new("");
        assert_eq!(map.line_count(), 1);
        assert_eq!(map.position(0), Position::new(1, 1));
    }

    #[test]
    fn test_source_map_yaml_example() {
        let yaml = "key: value\nnested:\n  - item";
        let map = SourceMap::new(yaml);

        assert_eq!(map.line_count(), 3);

        // "value" starts at byte 5
        assert_eq!(map.position(5), Position::new(1, 6));

        // "nested" starts at byte 11
        assert_eq!(map.position(11), Position::new(2, 1));

        // "- item" starts at byte 21
        assert_eq!(map.position(21), Position::new(3, 3));
    }
}
