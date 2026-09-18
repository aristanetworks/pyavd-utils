// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::sync::LazyLock;

use fancy_regex::Regex;

use super::walker::Walker as _;
use crate::StoreSource;
use crate::any::SourceSchema;
use crate::resolve::errors::SchemaResolverError;

/// Regex matching $ref syntax according the AVD metaschema.
static REF_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("^([a-z][a-z_]*)#((/[a-z$][\\.a-z0-9_]*)*)$").unwrap());

/// Resolve the given ref by first finding the relevant schema in in the store
/// and afterwards walk that schema according to the path.
/// Returns the schema pointed to by the ref, or an error for invalid ref.
pub(crate) fn resolve_ref<'a>(
    ref_: &str,
    store: &'a StoreSource,
) -> Result<&'a SourceSchema, SchemaResolverError> {
    let syntax_err = || SchemaResolverError::RefSyntax {
        schema_ref: ref_.to_owned(),
    };
    // unwrap_or_default() cannot fail: the regex is compiled above and uses no lookarounds.
    let captures = REF_REGEX
        .captures(ref_)
        .unwrap_or_default()
        .ok_or_else(syntax_err)?;
    let schema_name = captures.get(1).ok_or_else(syntax_err)?.as_str();
    let schema_path = captures.get(2).ok_or_else(syntax_err)?.as_str();

    let path_iter = schema_path.split('/').skip(1).peekable();
    let schema = store.get(schema_name)?;
    Ok(schema.walk(path_iter)?)
}

#[cfg(test)]
mod tests {
    use super::resolve_ref;
    use crate::Load as _;
    use crate::StoreSource;
    use crate::resolve::errors::SchemaResolverError;
    use crate::resolve::walker::SchemaWalkError;
    use crate::str::SourceStr;
    use crate::utils::test_utils::get_test_store;

    #[test]
    // Testing with eos_cli_config_gen ref
    fn resolve_ref_ok() {
        let test_store = get_test_store();
        let result = resolve_ref("eos_cli_config_gen#/keys/key2", &test_store);
        assert!(result.is_ok());
        let result_schema = result.unwrap();
        let str_schema_result: Result<&SourceStr, _> = result_schema.try_into();
        assert!(str_schema_result.is_ok());
        let str_schema = str_schema_result.unwrap();
        assert!(str_schema.base.description.is_some());
        assert_eq!(
            str_schema.base.description.as_ref().unwrap(),
            "this is from key2"
        );
    }

    #[test]
    // Testing with eos_config ref
    fn resolve_ref_ok_2() {
        let test_store = get_test_store();
        let result = resolve_ref("eos_config#/keys/key2", &test_store);
        assert!(result.is_ok());
        let result_schema = result.unwrap();
        let str_schema_result: Result<&SourceStr, _> = result_schema.try_into();
        assert!(str_schema_result.is_ok());
        let str_schema = str_schema_result.unwrap();
        assert!(str_schema.base.description.is_some());
        assert_eq!(
            str_schema.base.description.as_ref().unwrap(),
            "this is from key2"
        );
    }

    #[test]
    // Testing with cv_deploy ref
    fn resolve_ref_ok_3() {
        let test_store = get_test_store();
        let result = resolve_ref("cv_deploy#/keys/key4", &test_store);
        assert!(result.is_ok());
        let result_schema = result.unwrap();
        let str_schema_result: Result<&SourceStr, _> = result_schema.try_into();
        assert!(str_schema_result.is_ok());
        let str_schema = str_schema_result.unwrap();
        assert!(str_schema.base.description.is_some());
        assert_eq!(
            str_schema.base.description.as_ref().unwrap(),
            "this is from key4"
        );
    }

    #[test]
    fn resolve_ref_err_1() {
        let test_store = get_test_store();
        let ref_ = "#/keys/key2";
        let result = resolve_ref(ref_, &test_store);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            SchemaResolverError::RefSyntax { .. }
        ));
    }

    #[test]
    fn resolve_ref_err_2() {
        let test_store = get_test_store();
        let ref_ = "eos_cli_config_gen";
        let result = resolve_ref(ref_, &test_store);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            SchemaResolverError::RefSyntax { .. }
        ));
    }

    #[test]
    fn resolve_ref_err_3() {
        let test_store = get_test_store();
        let ref_ = "wrong_schema#/keys/key2";
        let result = resolve_ref(ref_, &test_store);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            SchemaResolverError::SchemaStore(
                crate::source_store::SchemaStoreError::InvalidSchemaName(_),
            )
        ));
    }

    #[test]
    fn resolve_ref_walks_all_schema_containers() {
        let store = StoreSource::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "keys": {
                        "list": {"type": "list", "items": {"type": "bool"}}
                    },
                    "dynamic_keys": {"dynamic": {"type": "int"}},
                    "$defs": {"definition": {"type": "str"}}
                }
            }"#,
        )
        .unwrap();

        assert!(matches!(
            resolve_ref("test#/keys/list/items", &store),
            Ok(crate::any::SourceSchema::Bool(_))
        ));
        assert!(matches!(
            resolve_ref("test#/dynamic_keys/dynamic", &store),
            Ok(crate::any::SourceSchema::Int(_))
        ));
        assert!(matches!(
            resolve_ref("test#/$defs/definition", &store),
            Ok(crate::any::SourceSchema::Str(_))
        ));
    }

    #[test]
    fn resolve_ref_reports_structured_walk_errors() {
        let store = StoreSource::from_json(
            r#"{
                "scalar": {"type": "str"},
                "test": {
                    "type": "dict",
                    "keys": {
                        "list": {"type": "list"}
                    },
                    "dynamic_keys": {"dynamic": {"type": "int"}},
                    "$defs": {"definition": {"type": "str"}}
                }
            }"#,
        )
        .unwrap();

        assert!(matches!(
            resolve_ref("test#/keys/missing", &store),
            Err(SchemaResolverError::SchemaWalk(
                SchemaWalkError::PathNotFound { element }
            )) if element == "missing"
        ));
        for mapping in ["keys", "dynamic_keys", "$defs"] {
            assert!(matches!(
                resolve_ref(&format!("test#/{mapping}"), &store),
                Err(SchemaResolverError::SchemaWalk(
                    SchemaWalkError::PointingToMapping { mapping: found }
                )) if found == mapping
            ));
        }
        assert!(matches!(
            resolve_ref("test#/invalid/path", &store),
            Err(SchemaResolverError::SchemaWalk(
                SchemaWalkError::InvalidPathElement { element }
            )) if element == "invalid"
        ));
        assert!(matches!(
            resolve_ref("scalar#/keys/value", &store),
            Err(SchemaResolverError::SchemaWalk(
                SchemaWalkError::NotDictOrList
            ))
        ));
        assert!(matches!(
            resolve_ref("test#/keys/list/items", &store),
            Err(SchemaResolverError::SchemaWalk(
                SchemaWalkError::PathNotFound { element }
            )) if element == "items"
        ));
    }
}
