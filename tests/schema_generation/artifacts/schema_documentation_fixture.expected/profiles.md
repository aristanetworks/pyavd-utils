<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->
=== "Table"

    | Variable | Type | Required | Default | Value Restrictions | Description |
    | -------- | ---- | -------- | ------- | ------------------ | ----------- |
    | [<samp>profiles</samp>](## "profiles") | List, items: Dictionary |  | See (+) on YAML tab | Min Length: 1<br>Max Length: 5 | Reusable profiles.<br>Second line. |
    | [<samp>&nbsp;&nbsp;-&nbsp;name</samp>](## "profiles.[].name") | String | Required, Unique |  |  | Profile name. |
    | [<samp>&nbsp;&nbsp;&nbsp;&nbsp;priority</samp>](## "profiles.[].priority") | Integer |  | `10` | Min: 1<br>Max: 100<br>Valid Values:<br>- <code>10</code><br>- <code>20</code> |  |
    | [<samp>&nbsp;&nbsp;&nbsp;&nbsp;mode</samp>](## "profiles.[].mode") <span style="color:red">deprecated</span> | String |  |  | Min Length: 2<br>Max Length: 20<br>Format: ipv4<br>Value is converted to lower case.<br>Valid Values:<br>- <code><value(s) of custom_modes></code><br>- <code>active</code><br>- <code>passive</code><br>Pattern: `^[a-z]+$` | <span style="color:red">This key is deprecated. Support will be removed in AVD version 7.0.0. Use <samp>state</samp> instead. See [here](https://example.invalid/mode) for details.</span> |
    | [<samp>&nbsp;&nbsp;&nbsp;&nbsp;removed</samp>](## "profiles.[].removed") <span style="color:red">removed</span> | String |  |  |  | <span style="color:red">This key was removed. Support was removed in AVD. Use <samp>state</samp> instead.</span> |
    | [<samp>&nbsp;&nbsp;&nbsp;&nbsp;opaque</samp>](## "profiles.[].opaque") | Dictionary |  | `{'enabled': True}` |  |  |

=== "YAML"

    ```yaml
    # Reusable profiles.
    # Second line.
    profiles: # 1-5 items # (1)!

        # Profile name.
      - name: <str; required; unique>
        priority: <int; 1-100; 10 | 20; default=10>
        # This key is deprecated.
        # Support will be removed in AVD version 7.0.0.
        # Use `state` instead.
        # See [here](https://example.invalid/mode) for details.
        mode: <str; length 2-20; "<value(s) of custom_modes>" | "active" | "passive">
        opaque: <dict> # default={'enabled': True}
    ```

    1. Default Value

        ```yaml
        profiles:
        - name: default
          settings:
            long_value: flexible exact-match 16384 l2-shared 98304 l3-shared 131072
        ```
