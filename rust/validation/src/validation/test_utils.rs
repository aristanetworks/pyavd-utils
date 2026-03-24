// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::Store;
use serde::Deserialize as _;
use serde_json::json;

pub(crate) fn get_test_store() -> Store {
    Store::deserialize(json!({
        "eos_config": {
            "type": "dict",
            "keys": {
                "key1": {
                    "type": "str",
                    "$ref": "eos_config#/keys/key2",
                },
                "key2": {
                    "type": "str",
                    "description": "this is from key2",
                }
            }
        },
        "avd_design": {
            "type": "dict",
            "allow_other_keys": true,
            "keys": {
                "key3": {
                    "type": "str",
                    "$ref": "eos_config#/keys/key2",
                }
            }
        },
        "cv_deploy": {
            "type": "dict",
            "keys": {
                "key4": {
                    "type": "str",
                    "description": "this is from key4",
                },
                "key5": {
                    "type": "str",
                    "$ref": "cv_deploy#/keys/key4",
                }
            }
        }
    }))
    .unwrap()
}
