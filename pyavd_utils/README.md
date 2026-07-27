<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# `pyavd_utils` package layout

This package is the Python-facing wrapper around the Rust extension built by
`rust/python-bindings`.

The intended public import surface is the small set of Python wrapper modules:

```python
from pyavd_utils.passwords import cbc_encrypt
from pyavd_utils.schema_store import init_store_from_file
from pyavd_utils.validation import validate_json
```

The compiled binding module and its nested modules are implementation details.
They may change without compatibility guarantees.

## Runtime structure

The package is built around one compiled extension module:

```text
pyavd_utils/_bindings.cpython-*.so
```

That single shared object contains all Rust-backed Python bindings. This keeps
shared Rust-side process state in one place.

The extension exposes internal PyO3 submodules as attributes:

```python
from pyavd_utils._bindings import _passwords
from pyavd_utils._bindings import _schema_store
from pyavd_utils._bindings import _validation
```

The Python wrapper modules import from those internal modules and re-export the
supported package-level API.

## Stub structure

The `_bindings/` directory is stub-only:

```text
pyavd_utils/_bindings/
  __init__.pyi
  _passwords.pyi
  _schema_store.pyi
  _validation.pyi
```

There is deliberately no `pyavd_utils/_bindings/__init__.py`. At runtime,
`pyavd_utils._bindings` is the compiled extension module, not a Python package.
Adding a real `__init__.py` would conflict with the extension module name.

The stub-only directory exists so type checkers can understand the nested PyO3
module attributes exposed by the extension. This mirrors the approach used by
projects such as `cryptography`, where a compiled module has a matching `.pyi`
package tree for static analysis.

Because `_bindings` is not a runtime package, this import form is not supported:

```python
from pyavd_utils._bindings._passwords import cbc_encrypt
```

Use attribute-module imports internally instead:

```python
from pyavd_utils._bindings import _passwords
```

External callers should use the public wrapper modules.

## Documentation sources

There are two places where documentation can appear:

- Rust doc comments on `#[pyo3::pymodule]`, `#[pyfunction]`, and `#[pyclass]`
  items can become runtime `__doc__` text used by `help()` and interactive
  inspection.
- `.pyi` docstrings are used by Python type checkers and many LSP/editor hover
  experiences.

For now these docs are maintained manually. That means runtime docs and editor
docs can drift if both are edited independently. Keep this in mind when changing
signatures, behavior, or error messages.

Long term, generating stubs or docs from one source of truth would be better.
Until then, keep the `.pyi` files aligned with the Rust bindings and public
Python wrappers.
