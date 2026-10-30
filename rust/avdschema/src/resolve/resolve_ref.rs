// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Parse authored references at the resolution boundary and follow typed schema edges.

use super::walker::Walker as _;
use crate::SchemaPath;
use crate::SchemaPathParseError;
use crate::SchemaWalkError;
use crate::StoreSource;
use crate::any::SourceLayer;
use crate::resolve::errors::SchemaResolverError;

/// Decode a source reference while retaining the resolver's structured error categories.
/// Invalid authored syntax remains a compiler diagnostic rather than a Serde load failure.
pub(crate) fn parse_reference(reference: &str) -> Result<SchemaPath, SchemaResolverError> {
    reference.parse().map_err(|error| match error {
        SchemaPathParseError::InvalidSyntax {
            reference: invalid_reference,
        } => SchemaResolverError::RefSyntax {
            schema_ref: invalid_reference,
        },
        SchemaPathParseError::InvalidElement { element } => {
            SchemaWalkError::InvalidPathElement { element }.into()
        }
        SchemaPathParseError::IncompleteMapping { mapping } => {
            SchemaWalkError::PointingToMapping { mapping }.into()
        }
        SchemaPathParseError::UnsupportedDynamicSource(error) => SchemaWalkError::PathNotFound {
            element: error.path,
        }
        .into(),
    })
}

/// A parsed reference paired with its authored declaration target.
#[derive(Debug)]
pub(crate) struct ResolvedReference<'a> {
    /// Parsed address retained for generation and other downstream consumers.
    #[cfg(any(test, feature = "generation"))]
    pub path: SchemaPath,
    /// Declaration before reference-layer expansion.
    pub target: SourceLayer<'a>,
}

/// Parse and resolve an authored reference once, retaining its typed address.
pub(crate) fn resolve_ref<'a>(
    reference: &str,
    store: &'a StoreSource,
) -> Result<ResolvedReference<'a>, SchemaResolverError> {
    let path = parse_reference(reference)?;
    let schema = store.get(path.schema_name())?;
    let target = schema.walk(path.elements())?;
    Ok(ResolvedReference {
        #[cfg(any(test, feature = "generation"))]
        path,
        target,
    })
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
        let result = resolve_ref("eos_cli_config_gen#/keys/key2", &test_store);
        assert!(result.is_ok());
        let SourceLayer::Schema(result_schema) = result.unwrap().target else {
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
        let result = resolve_ref("eos_config#/keys/key2", &test_store);
        assert!(result.is_ok());
        let SourceLayer::Schema(result_schema) = result.unwrap().target else {
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
        let result = resolve_ref("cv_deploy#/keys/key4", &test_store);
        assert!(result.is_ok());
        let SourceLayer::Schema(result_schema) = result.unwrap().target else {
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
                    "dynamic_keys": {"node_type_keys.key": {"type": "int"}},
                    "$defs": {"definition": {"type": "str"}}
                }
            }"#,
        )
        .unwrap();

        let list_item = resolve_ref("test#/keys/list/items", &store).unwrap();
        assert_eq!(list_item.path.to_reference(), "test#/keys/list/items");
        assert!(matches!(
            list_item.target,
            SourceLayer::Schema(crate::any::SourceSchema::Bool(_))
        ));

        let dynamic_key = resolve_ref("test#/dynamic_keys/node_type_keys.key", &store).unwrap();
        assert_eq!(
            dynamic_key.path.to_reference(),
            "test#/dynamic_keys/node_type_keys.key"
        );
        assert!(matches!(
            dynamic_key.target,
            SourceLayer::Schema(crate::any::SourceSchema::Int(_))
        ));

        let definition = resolve_ref("test#/$defs/definition", &store).unwrap();
        assert_eq!(definition.path.to_reference(), "test#/$defs/definition");
        assert!(matches!(
            definition.target,
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
            resolve_ref("test#/keys/scalar/keys/value", &store),
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
