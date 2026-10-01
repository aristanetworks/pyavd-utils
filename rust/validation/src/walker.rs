// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Archived schema validation walker.
//!
//! This strategy validates every child and optionally reconstructs the complete
//! coerced value. Schema modules validate
//! individual nodes and edges; recursive walking, path handling, diagnostics,
//! and output ownership stay here.

use avdschema::DictView;
use avdschema::ListView;
use avdschema::SchemaView;
use avdschema::Store;

use crate::context::Context;
use crate::context::ValidationState;
use crate::feedback::IgnoredEosConfigKey;
use crate::feedback::Violation;
use crate::validatable::ValidatableMappingPair as _;
use crate::validatable::ValidatableSequence as _;
use crate::validatable::ValidatableValue;
use crate::validation::NodeValidation;
use crate::validation::boolean;
use crate::validation::dict;
use crate::validation::int;
use crate::validation::list;
use crate::validation::str;

// This must be kept up to date when adding role keys in eos_config schema.
// TODO: Eventually this will go away as we stop warning.
const EOS_CLI_CONFIG_GEN_ROLE_KEYS: [&str; 8] = [
    "avd_structured_config_file_format",
    "custom_templates",
    "eos_cli_config_gen_configuration",
    "eos_cli_config_gen_documentation",
    "eos_cli_config_gen_keep_tmp_files",
    "eos_cli_config_gen_tmp_dir",
    "eos_cli_config_gen_validate_inputs_batch_size",
    "read_structured_config_from_file",
];

pub(crate) struct Validator<'context, 'store> {
    context: &'context mut Context,
    schema_store: &'store Store,
}

impl<'context, 'store> Validator<'context, 'store> {
    pub(crate) fn new(schema_store: &'store Store, context: &'context mut Context) -> Self {
        Self {
            context,
            schema_store,
        }
    }

    /// Validate the complete value tree rooted at `schema`.
    pub(crate) fn validate<V: ValidatableValue>(
        &mut self,
        schema: SchemaView<'store>,
        value: &V,
    ) -> Option<V::Coerced> {
        self.visit(schema, value, &mut ValidationState::default())
    }

    #[cfg(test)]
    pub(crate) fn validate_with_state<V: ValidatableValue>(
        &mut self,
        schema: SchemaView<'store>,
        value: &V,
        state: &mut ValidationState,
    ) -> Option<V::Coerced> {
        self.visit(schema, value, state)
    }

    fn visit<V: ValidatableValue>(
        &mut self,
        schema: SchemaView<'store>,
        value: &V,
        state: &mut ValidationState,
    ) -> Option<V::Coerced> {
        let previous_relaxed_validation = state.relaxed_validation;
        let coerced = match schema {
            SchemaView::Bool(_) => self.visit_bool(value, state),
            SchemaView::Int(schema) => self.visit_int(schema, value, state),
            SchemaView::Str(schema) => self.visit_str(schema, value, state),
            SchemaView::List(schema) => self.visit_list(schema, value, state),
            SchemaView::Dict(schema) => {
                if schema.begin_relaxed_validation() {
                    state.relaxed_validation = true;
                }
                self.visit_dict(schema, value, state)
            }
        };
        state.relaxed_validation = previous_relaxed_validation;
        coerced
    }

    fn visit_bool<V: ValidatableValue>(
        &mut self,
        value: &V,
        state: &mut ValidationState,
    ) -> Option<V::Coerced> {
        match boolean::validate_node(value, self.context, state) {
            NodeValidation::Valid(value_) => self
                .context
                .configuration
                .return_coerced_data
                .then(|| value.coerce_bool(value_)),
            NodeValidation::Null => self
                .context
                .configuration
                .return_coerced_data
                .then(|| value.coerce_null()),
            NodeValidation::Invalid => None,
        }
    }

    fn visit_int<V: ValidatableValue>(
        &mut self,
        schema: avdschema::IntView<'_>,
        value: &V,
        state: &mut ValidationState,
    ) -> Option<V::Coerced> {
        match int::validate_node(schema, value, self.context, state) {
            NodeValidation::Valid(value_) => self
                .context
                .configuration
                .return_coerced_data
                .then(|| value.coerce_int(value_)),
            NodeValidation::Null => self
                .context
                .configuration
                .return_coerced_data
                .then(|| value.coerce_null()),
            NodeValidation::Invalid => None,
        }
    }

    fn visit_str<V: ValidatableValue>(
        &mut self,
        schema: avdschema::StrView<'_>,
        value: &V,
        state: &mut ValidationState,
    ) -> Option<V::Coerced> {
        match str::validate_node(schema, value, self.context, state) {
            NodeValidation::Valid(value_) => self
                .context
                .configuration
                .return_coerced_data
                .then(|| value.coerce_str(value_.into_owned())),
            NodeValidation::Null => self
                .context
                .configuration
                .return_coerced_data
                .then(|| value.coerce_null()),
            NodeValidation::Invalid => None,
        }
    }

    fn visit_list<V: ValidatableValue>(
        &mut self,
        schema: ListView<'store>,
        value: &V,
        state: &mut ValidationState,
    ) -> Option<V::Coerced> {
        let sequence = match list::validate_node(schema, value, self.context, state) {
            NodeValidation::Valid(sequence) => sequence,
            NodeValidation::Null => {
                return self
                    .context
                    .configuration
                    .return_coerced_data
                    .then(|| value.coerce_null());
            }
            NodeValidation::Invalid => return None,
        };
        let mut coerced = self
            .context
            .configuration
            .return_coerced_data
            .then(|| Vec::with_capacity(sequence.len()));

        for (index, item) in sequence.iter().enumerate() {
            state.path.push(index.to_string());
            list::validate_item_node(schema, item, self.context, state);
            let coerced_item = if let Some(item_schema) = schema.items() {
                self.visit(item_schema, item, state)
            } else {
                None
            };
            if let Some(ref mut items) = coerced {
                items.push(coerced_item.unwrap_or_else(|| item.clone_to_coerced()));
            }
            state.path.pop();
        }
        coerced.map(|items| value.coerce_sequence(items))
    }

    fn visit_dict<V: ValidatableValue>(
        &mut self,
        schema: DictView<'store>,
        value: &V,
        state: &mut ValidationState,
    ) -> Option<V::Coerced> {
        let mapping = match dict::validate_node(value, self.context, state) {
            NodeValidation::Valid(mapping) => mapping,
            NodeValidation::Null => {
                return self
                    .context
                    .configuration
                    .return_coerced_data
                    .then(|| value.coerce_null());
            }
            NodeValidation::Invalid => return None,
        };
        let coerced_items = self.visit_mapping(schema, &mapping, state);
        dict::finish_node_validation(schema, value, &mapping, self.context, state);
        coerced_items.map(|items| value.coerce_mapping(items))
    }

    fn visit_mapping<'data, M: crate::validatable::ValidatableMapping<'data>>(
        &mut self,
        schema: DictView<'store>,
        input: &M,
        state: &mut ValidationState,
    ) -> Option<Vec<<M::Value as ValidatableValue>::CoercedMappingItem>> {
        let mut coerced_items = self
            .context
            .configuration
            .return_coerced_data
            .then(Vec::new);

        if !schema.has_schema_keys() {
            if let Some(ref mut items) = coerced_items {
                for pair in input.iter() {
                    items.push(pair.coerced_item(pair.value().clone_to_coerced()));
                }
            }
            return coerced_items;
        }

        let eos_config_schema = if state.path.is_empty()
            && self.context.configuration.warn_eos_config_keys
            && let Some(SchemaView::Dict(eos_config_schema)) = self.schema_store.get("eos_config")
        {
            Some(eos_config_schema)
        } else {
            None
        };
        let resolved_dynamic_keys = avdschema::resolve_dynamic_keys(
            schema,
            input.as_schema_data_mapping(),
            self.context.configuration.dynamic_key_overrides.as_deref(),
        );

        for pair in input.iter() {
            let display_key = pair.display_key();
            let schema_key = pair.schema_key();
            let path_key = schema_key.as_deref().unwrap_or(&display_key);
            let input_value = pair.value();
            let key_span = pair.key_span();
            state.path.push(path_key.to_owned());

            let Some(input_schema_key) = schema_key.as_deref() else {
                if !schema.allow_other_keys() {
                    self.context
                        .add_error_with_span(state, key_span, Violation::UnexpectedKey());
                }
                if let Some(ref mut items) = coerced_items {
                    items.push(pair.coerced_item(input_value.clone_to_coerced()));
                }
                state.path.pop();
                continue;
            };

            let key_schema = if let Some(key_schema) = schema.key(input_schema_key) {
                Some(key_schema)
            } else if let Some(key_schema) = resolved_dynamic_keys.get(input_schema_key).copied() {
                Some(key_schema)
            } else if input_schema_key.starts_with('_') {
                None
            } else {
                if !schema.allow_other_keys() {
                    self.context.add_error_with_span(
                        state,
                        key_span.clone(),
                        Violation::UnexpectedKey(),
                    );
                } else if let Some(eos_config_schema) = eos_config_schema
                    && eos_config_schema.key(input_schema_key).is_some()
                    && !EOS_CLI_CONFIG_GEN_ROLE_KEYS.contains(&input_schema_key)
                {
                    self.context.add_warning_with_span(
                        state,
                        key_span.clone(),
                        IgnoredEosConfigKey {},
                    );
                }
                None
            };
            let coerced_value = key_schema.and_then(|key_schema| {
                if dict::check_deprecation(key_schema, key_span, input, self.context, state) {
                    None
                } else {
                    self.visit(key_schema, input_value, state)
                }
            });

            if let Some(ref mut items) = coerced_items {
                items.push(
                    pair.coerced_item(
                        coerced_value.unwrap_or_else(|| input_value.clone_to_coerced()),
                    ),
                );
            }
            state.path.pop();
        }
        coerced_items
    }
}
