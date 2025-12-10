// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

/// Macro to avoid repeating the match for AnySchema just to call the same method on each variant.
///
/// ```rust
/// use avdschema::{any::{AnySchema, Shortcuts}, delegate_anyschema_method};
/// trait Foo {
///   fn method_no_args(&self) -> bool;
/// }
/// impl Foo for AnySchema {
///     fn method_no_args(&self) -> bool {
///         delegate_anyschema_method!(self, is_required,)
///     }
/// }
/// ```
///
/// will generate
///
/// ```rust
/// use avdschema::{any::{AnySchema, Shortcuts}};
/// trait Foo {
///   fn method_no_args(&self) -> bool;
/// }
/// impl Foo for AnySchema {
///     fn method_no_args(&self) -> bool {
///         match self {
///             Self::Bool(schema) => schema.is_required(),
///             Self::Int(schema) => schema.is_required(),
///             Self::Str(schema) => schema.is_required(),
///             Self::List(schema) => schema.is_required(),
///             Self::Dict(schema) => schema.is_required(),
///         }
///     }
/// }
/// ```
///
/// It is also possible to give extra arguments to the called method like `delegate_anyschema_method!(self, validate_value, value, ctx)`
#[macro_export]
macro_rules! delegate_anyschema_method {
    ($self:ident, $method:ident, $($arg:expr),*) => {
        match $self {
            Self::Bool(schema) => schema.$method($($arg,)*),
            Self::Int(schema) => schema.$method($($arg,)*),
            Self::Str(schema) => schema.$method($($arg,)*),
            Self::List(schema) => schema.$method($($arg,)*),
            Self::Dict(schema) => schema.$method($($arg,)*),
        }
    };
}
