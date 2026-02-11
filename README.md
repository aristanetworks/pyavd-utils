<!--
  ~ Copyright (c) 2025-2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# `pyavd-utils` 🦀

[![CI Status](https://img.shields.io/github/actions/workflow/status/aristanetworks/pyavd-utils/pull-request-management.yml?branch=main)](https://github.com/aristanetworks/pyavd-utils/actions?query=workflow%3Aci)
[![SonarCloud Coverage](https://sonarcloud.io/api/project_badges/measure?project=aristanetworks_pyavd-utils&metric=coverage)](https://sonarcloud.io/dashboard?id=aristanetworks_pyavd-utils)
[![PyPI Downloads](https://img.shields.io/pypi/dm/pyavd-utils?label=PyPI%20Downloads)](https://pypi.org/project/pyavd-utils/)
[![Supported Python Versions](https://img.shields.io/pypi/pyversions/pyavd-utils)](https://pypi.org/project/pyavd-utils/)
[![License](https://img.shields.io/github/license/aristanetworks/pyavd-utils)](https://github.com/aristanetworks/pyavd-utils/blob/main/LICENSE)

---

## ⚠️ Danger

This package is an **internal dependency** designed solely for the use of the `pyavd` Python library. It exposes low-level functionality and is not intended for direct use by end-users.

> [!CAUTION]
> **Should not be used directly** and **may not follow semantic versioning**.
>
> Changes in minor and patch versions may include breaking API changes without prior notice.
---

## Purpose

`pyavd-utils` provides functionalities in **Rust** for the Python package, **PyAVD**.

**PyAVD** is designed to expose the core logic of the **AVD** Ansible collection (`arista.avd`) as a standalone, dependency-lite Python library. This allows developers to utilize AVD's complex data processing capabilities—like **input validation**, **AVD facts generation**, and **structured configuration generation**—within custom Python applications without requiring a full Ansible installation or runtime.

This repository, `pyavd-utils`, provides functionalities to PyAVD, specifically handling the heavy lifting of data structure validation and manipulation with the speed and safety guarantees of **Rust**.

---

## Contribution and Development

We welcome contributions to the `pyavd-utils` project. As this is a performance-focused component, any contributions should include thorough testing and benchmarking to ensure stability and verify performance improvements.

Please refer to the main [AVD GitHub repository](https://github.com/aristanetworks/avd) for general contribution guidelines.

## License

`pyavd-utils` is licensed under Apache2. See the [LICENSE](https://github.com/aristanetworks/pyavd-utils/blob/main/LICENSE) file for details.
