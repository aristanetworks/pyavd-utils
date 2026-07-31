<!--
  ~ Copyright (c) 2025-2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

```mermaid
---
title: Rust crate layout
---
graph LR
validation["crate validation"]
avdschema["crate avdschema"]
validation --->|depends on| avdschema
passwords["crate passwords"]
python_bindings["crate python-bindings"]
python_bindings --->|depends on| avdschema
python_bindings --->|depends on| validation
python_bindings --->|depends on| passwords
```
