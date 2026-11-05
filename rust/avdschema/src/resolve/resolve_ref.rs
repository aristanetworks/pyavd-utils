// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Resolve parsed schema references against authored declarations.

use super::walker::Walker as _;
use crate::SchemaPath;
use crate::StoreSource;
use crate::any::SourceLayer;
use crate::resolve::errors::SchemaResolverError;

/// Follow typed edges to a declaration without expanding its own reference layers.
/// Syntax has already been checked when the source model was deserialized.
pub(crate) fn resolve_ref<'a>(
    reference: &SchemaPath,
    store: &'a StoreSource,
) -> Result<SourceLayer<'a>, SchemaResolverError> {
    let schema = store.get(reference.schema_name())?;
    Ok(schema.walk(reference.elements())?)
}

#[cfg(test)]
mod tests {
    use super::resolve_ref;
    use crate::Load as _;
    use crate::StoreSource;
    use crate::any::SourceLayer;
    use crate::resolve::errors::SchemaResolverError;
    use crate::resolve::walker::SchemaWalkError;
    use crate::str::SourceStr;
    use crate::utils::test_utils::get_test_store;

    #[test]
    // Testing with eos_cli_config_gen ref
    fn resolve_ref_ok() {
        let test_store = get_test_store();
        let result = resolve_ref(
            &"eos_cli_config_gen#/keys/key2".parse().unwrap(),
            &test_store,
        );
        assert!(result.is_ok());
        let SourceLayer::Schema(result_schema) = result.unwrap() else {
            panic!("key reference should resolve to a recursive schema")
        };
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
        let result = resolve_ref(&"eos_config#/keys/key2".parse().unwrap(), &test_store);
        assert!(result.is_ok());
        let SourceLayer::Schema(result_schema) = result.unwrap() else {
            panic!("key reference should resolve to a recursive schema")
        };
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
        let result = resolve_ref(&"cv_deploy#/keys/key4".parse().unwrap(), &test_store);
        assert!(result.is_ok());
        let SourceLayer::Schema(result_schema) = result.unwrap() else {
            panic!("key reference should resolve to a recursive schema")
        };
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
    fn resolve_ref_err_3() {
        let test_store = get_test_store();
        let ref_ = "wrong_schema#/keys/key2".parse().unwrap();
        let result = resolve_ref(&ref_, &test_store);
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
                    "dynamic_keys": {"node_type_keys.key": {"type": "int"}},
                    "$defs": {"definition": {"type": "str"}}
                }
            }"#,
        )
        .unwrap();

        let list_item = resolve_ref(&"test#/keys/list/items".parse().unwrap(), &store).unwrap();
        assert!(matches!(
            list_item,
            SourceLayer::Schema(crate::any::SourceSchema::Bool(_))
        ));

        let dynamic_key = resolve_ref(
            &"test#/dynamic_keys/node_type_keys.key".parse().unwrap(),
            &store,
        )
        .unwrap();
        assert!(matches!(
            dynamic_key,
            SourceLayer::Schema(crate::any::SourceSchema::Int(_))
        ));

        let definition = resolve_ref(&"test#/$defs/definition".parse().unwrap(), &store).unwrap();
        assert!(matches!(
            definition,
            SourceLayer::Schema(crate::any::SourceSchema::Str(_))
        ));
    }

    #[test]
    fn resolve_ref_reports_structured_walk_errors() {
        let store = StoreSource::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "keys": {
                        "scalar": {"type": "str"},
                        "list": {"type": "list"}
                    },
                    "dynamic_keys": {"node_type_keys.key": {"type": "int"}},
                    "$defs": {"definition": {"type": "str"}}
                }
            }"#,
        )
        .unwrap();

        assert!(matches!(
            resolve_ref(&"test#/keys/missing".parse().unwrap(), &store),
            Err(SchemaResolverError::SchemaWalk(
                SchemaWalkError::PathNotFound { element }
            )) if element == "missing"
        ));
        assert!(matches!(
            resolve_ref(&"test#/items".parse().unwrap(), &store),
            Err(SchemaResolverError::SchemaWalk(
                SchemaWalkError::InvalidPathElement { element }
            )) if element == "items"
        ));
        assert!(matches!(
            resolve_ref(&"test#/keys/scalar/keys/value".parse().unwrap(), &store),
            Err(SchemaResolverError::SchemaWalk(
                SchemaWalkError::NotDictOrList
            ))
        ));
        assert!(matches!(
            resolve_ref(&"test#/keys/list/items".parse().unwrap(), &store),
            Err(SchemaResolverError::SchemaWalk(
                SchemaWalkError::PathNotFound { element }
            )) if element == "items"
        ));
    }
}
