<!--
  ~ Copyright (c) 2025 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# `pyavd-utils` 🦀

[![CI Status](https://img.shields.io/github/actions/workflow/status/ClausHolbechArista/pyavd-utils/pull-request-management.yml?branch=main)](https://github.com/ClausHolbechArista/pyavd-utils/actions?query=workflow%3Aci)
[![SonarCloud Coverage](https://sonarcloud.io/api/project_badges/measure?project=ClausHolbechArista_pyavd-utils&metric=coverage)](https://sonarcloud.io/dashboard?id=ClausHolbechArista_pyavd-utils)
[![PyPI Downloads](https://img.shields.io/pypi/dm/pyavd-utils?label=PyPI%20Downloads)](https://pypi.org/project/pyavd-utils/)
[![Supported Python Versions](https://img.shields.io/pypi/pyversions/pyavd-utils)](https://pypi.org/project/pyavd-utils/)
[![License](https://img.shields.io/github/license/ClausHolbechArista/pyavd-utils)](https://github.com/ClausHolbechArista/pyavd-utils/blob/main/LICENSE)

***

## Purpose

`pyavd-utils` provides functionalities in **Rust** for the Python package, **PyAVD**.

**PyAVD** is designed to expose the core logic of the Arista Validated Designs (**AVD**) Ansible collection (`arista.avd`) as a standalone, dependency-lite Python library. This allows developers to utilize AVD's complex data processing capabilities—like **input validation**, **AVD facts generation**, and **structured configuration generation**—within custom Python applications without requiring a full Ansible installation or runtime.

This repository, `pyavd-utils`, provides functionalities to PyAVD, specifically handling the heavy lifting of data structure validation and manipulation with the speed and safety guarantees of **Rust**.

***

## ⚠️ Internal Warning

This package is an **internal dependency** designed solely for the use of the `pyavd` Python library. It exposes low-level functionality and is not intended for direct use by end-users.

> [!WARNING]
> **Should not be used directly** and **may not follow semantic versioning**.
>
> Changes in minor and patch versions may include breaking API changes without prior notice.

***

## Key Features

This Rust library provides the fundamental, performance-critical components for:

* **Schema Validation:** Efficiently validating input variables against the defined AVD schemas, such as those used by `eos_designs` and `eos_cli_config_gen`.
* **Type Coercion:** Handling automatic and reliable type conversion for variables during the validation process.
* **Data Transformation:** Executing the complex logic required to generate intermediate data structures like **AVD Facts** and the device-specific **Structured Configuration**.

By offloading these intensive tasks to compiled Rust code, `pyavd-utils` ensures that PyAVD maintains high performance, making the core AVD logic fast and robust when integrated into custom automation tools.

***

## Installation

`pyavd-utils` is installed automatically as a dependency when installing the main `pyavd` library:

```bash
pip install pyavd
```

The package is built as a set of Python bindings for the Rust code using `setuptools-rs`, ensuring compatibility and ease of installation across various platforms.

## Contribution and Development

We welcome contributions to the `pyavd-utils` project. As this is a performance-focused component, any contributions should include thorough testing and benchmarking to ensure stability and verify performance improvements.

Please refer to the main [AVD GitHub repository](https://github.com/ClausHolbechArista/avd) for general contribution guidelines.

## License

`pyavd-utils` is licensed under Apache2. See the [LICENSE](https://www.google.com/search?q=https://github.com/ClausHolbechArista/pyavd-utils/blob/main/LICENSE) file for details.
