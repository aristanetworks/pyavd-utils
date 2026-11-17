<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# pyavd-utils-gen

Build-time schema compilation and artifact generation used by AVD development tooling.

This package is released together with `pyavd-utils`. It is not required by pyAVD at runtime.

## Checking AVD regex patterns

`pyavd_utils_gen.metaschema.is_valid_avd_regex_pattern(pattern)` returns whether a
pattern compiles using the same compiler and settings as AVD runtime validation.
Malformed or unsupported patterns return `False`.

The compiler anchors patterns to the complete value with `^(?:...)$`. It supports
Unicode-aware Perl shorthand classes (`\d`, `\s`, `\w` and their complements),
word boundaries, lookarounds, and variable-length lookbehinds. Broad Unicode
properties and scripts such as `\p{L}` and `\p{Greek}` are not supported.

```python
from pyavd_utils_gen.metaschema import is_valid_avd_regex_pattern

assert is_valid_avd_regex_pattern(r"Ethernet\d+(?:/\d+)?")
assert not is_valid_avd_regex_pattern(r"\p{Greek}+")
```

This function only checks compilation; it does not match input data or guarantee
that matching cannot fail at runtime, for example on the backtracking limit.
It can be used as a custom JSON Schema `regex` format checker.
