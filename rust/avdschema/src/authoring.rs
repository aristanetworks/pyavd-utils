// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Build-time checks that deliberately do not restrict historical deserialization.

use ordermap::OrderMap;

use crate::SchemaDiagnostic;
use crate::SchemaDiagnostics;
use crate::any::SourceSchema;
use crate::base::Base;
use crate::base::DataValue;

pub(crate) fn validate(
    roots: &[(&str, &crate::dict::SourceRootDict)],
) -> Result<(), SchemaDiagnostics> {
    let mut validator = AuthoringValidator::default();
    for (name, root) in roots {
        let path = vec![(*name).to_owned()];
        validator.base(&root.base, &path);
        validator.children(root.keys.as_ref(), &path, "keys", NameKind::Static);
        validator.children(
            root.dynamic_keys.as_ref(),
            &path,
            "dynamic_keys",
            NameKind::Path,
        );
        validator.children(
            root.schema_defs.as_ref(),
            &path,
            "$defs",
            NameKind::Definition,
        );
    }
    if validator.diagnostics.is_empty() {
        Ok(())
    } else {
        Err(SchemaDiagnostics::new(validator.diagnostics))
    }
}

#[derive(Default)]
struct AuthoringValidator {
    diagnostics: Vec<SchemaDiagnostic>,
}

impl AuthoringValidator {
    fn base<T: DataValue>(&mut self, base: &Base<T>, path: &[String]) {
        if let Some(name) = &base.display_name
            && (name.is_empty() || name.contains('\n'))
        {
            self.invalid(
                &child_path(path, "display_name"),
                name,
                "a nonempty display name without a newline",
            );
        }
        if let Some(description) = &base.description
            && description.is_empty()
        {
            self.invalid(
                &child_path(path, "description"),
                description,
                "a nonempty description",
            );
        }
    }

    fn schema(&mut self, schema: &SourceSchema, path: &[String]) {
        match schema {
            SourceSchema::Bool(schema) => self.base(&schema.base, path),
            SourceSchema::Int(schema) => self.base(&schema.base, path),
            SourceSchema::Str(schema) => {
                self.base(&schema.base, path);
                if let Some(pattern) = &schema.pattern
                    && let Err(error) = pattern.get_compiled_pattern()
                {
                    self.diagnostics.push(SchemaDiagnostic::InvalidPattern {
                        schema_path: child_path(path, "pattern"),
                        pattern: pattern.pattern.clone(),
                        error: error.to_string(),
                    });
                }
            }
            SourceSchema::List(schema) => {
                self.base(&schema.base, path);
                if let Some(key) = &schema.primary_key {
                    self.name(key, &child_path(path, "primary_key"), NameKind::Definition);
                }
                if let Some(keys) = &schema.unique_keys {
                    for (index, key) in keys.iter().enumerate() {
                        self.name(
                            key,
                            &child_path(&child_path(path, "unique_keys"), &index.to_string()),
                            NameKind::Path,
                        );
                    }
                }
                if let Some(items) = &schema.items {
                    self.schema(items, &child_path(path, "items"));
                }
            }
            SourceSchema::Dict(schema) => {
                self.base(&schema.base, path);
                self.children(schema.keys.as_ref(), path, "keys", NameKind::Static);
            }
        }
    }

    fn children(
        &mut self,
        children: Option<&OrderMap<String, SourceSchema>>,
        path: &[String],
        keyword: &str,
        kind: NameKind,
    ) {
        if let Some(children) = children {
            let path = child_path(path, keyword);
            for (name, schema) in children {
                let child = child_path(&path, name);
                self.name(name, &child, kind);
                self.schema(schema, &child);
            }
        }
    }

    fn name(&mut self, name: &str, path: &[String], kind: NameKind) {
        let valid = (matches!(kind, NameKind::Static)
            && matches!(name, "Vxlan1" | "MIB_family_name"))
            || (name.starts_with(|character: char| character.is_ascii_lowercase())
                && name.bytes().all(|character| {
                    character.is_ascii_lowercase()
                        || character.is_ascii_digit()
                        || character == b'_'
                        || (matches!(kind, NameKind::Path) && character == b'.')
                }));
        if !valid {
            self.invalid(
                path,
                name,
                match kind {
                    NameKind::Static => "a snake_case key, Vxlan1, or MIB_family_name",
                    NameKind::Definition => "a snake_case key",
                    NameKind::Path => "a snake_case key or dot-notation path",
                },
            );
        }
    }

    fn invalid(&mut self, path: &[String], value: &str, expected: &'static str) {
        self.diagnostics
            .push(SchemaDiagnostic::InvalidAuthoringValue {
                schema_path: path.to_vec(),
                value: value.to_owned(),
                expected,
            });
    }
}

#[derive(Clone, Copy)]
enum NameKind {
    Static,
    Definition,
    Path,
}

fn child_path(path: &[String], key: &str) -> Vec<String> {
    let mut child = path.to_vec();
    child.push(key.to_owned());
    child
}

#[cfg(test)]
mod tests {
    use crate::Load as _;
    use crate::SchemaDiagnostic;
    use crate::Store;
    use crate::StoreSource;

    #[test]
    fn checks_overridden_patterns_and_unused_definitions() -> Result<(), String> {
        let source = StoreSource::from_json(
            r#"{
            "test": {"type":"dict",
                "$defs": {
                    "unused": {"type":"str", "pattern":"(["},
                    "base": {"type":"str", "pattern":"(["}
                },
                "keys": {
                    "value": {"type":"str", "$ref":"test#/$defs/base", "pattern":"valid"}
                }
            }
        }"#,
        )
        .map_err(|error| error.to_string())?;
        Store::compile(&source).map_err(|error| error.to_string())?;
        let Err(diagnostics) = source.validate_authoring() else {
            return Err("unused definitions should be checked".into());
        };
        let paths = diagnostics
            .iter()
            .map(|diagnostic| match diagnostic {
                SchemaDiagnostic::InvalidPattern { schema_path, .. } => schema_path.join("/"),
                other => format!("unexpected diagnostic: {other}"),
            })
            .collect::<Vec<_>>();
        if paths != ["test/$defs/unused/pattern", "test/$defs/base/pattern"] {
            return Err(format!("unexpected diagnostic paths: {}", paths.join(", ")));
        }
        Ok(())
    }

    #[test]
    fn reports_multiple_authoring_errors_without_restricting_loading() -> Result<(), String> {
        let source = StoreSource::from_json(
            r#"{
            "test": {"type":"dict", "keys": {
                "Invalid_key": {"type":"str", "display_name":"two\nlines"},
                "value": {"type":"int", "description":""}
            }}
        }"#,
        )
        .map_err(|error| error.to_string())?;
        Store::compile(&source).map_err(|error| error.to_string())?;
        let Err(diagnostics) = source.validate_authoring() else {
            return Err("invalid authoring values should be rejected".into());
        };
        let message = diagnostics.to_string();
        if diagnostics.iter().count() != 3
            || !message.contains("test/keys/Invalid_key/display_name")
            || !message.contains("test/keys/value/description")
        {
            return Err(format!("unexpected authoring diagnostics: {message}"));
        }
        Ok(())
    }

    #[test]
    fn accepts_valid_regex_dialect_and_legacy_optional_nulls() -> Result<(), String> {
        let source = StoreSource::from_json(
            r#"{
            "test": {"type":"dict", "description":null, "keys": {
                "Vxlan1": {"type":"str", "pattern":"(?<=a+)b|\\w+"},
                "MIB_family_name": {"type":"str"},
                "entries": {"type":"list", "primary_key":"name", "unique_keys":["parent.name"]}
            }}
        }"#,
        )
        .map_err(|error| error.to_string())?;
        source
            .validate_authoring()
            .map_err(|error| error.to_string())?;
        Store::compile(&source).map_err(|error| error.to_string())?;
        Ok(())
    }
}
