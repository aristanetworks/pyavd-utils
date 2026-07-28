// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::create_exception;
use pyo3::exceptions::PyException;

create_exception!(
    pyavd_utils.validation,
    ValidationError,
    PyException,
    "Base exception for pyavd_utils.validation."
);
create_exception!(
    pyavd_utils.validation,
    ValidationStoreNotInitializedError,
    ValidationError,
    "Schema store was not initialized."
);
create_exception!(
    pyavd_utils.validation,
    ValidationStoreAlreadyInitializedError,
    ValidationError,
    "Schema store was already initialized."
);
create_exception!(
    pyavd_utils.validation,
    ValidationStoreLoadJsonError,
    ValidationError,
    "Schema store JSON load error."
);
create_exception!(
    pyavd_utils.validation,
    ValidationStoreLoadYamlError,
    ValidationError,
    "Schema store YAML load error."
);
create_exception!(
    pyavd_utils.validation,
    ValidationStoreLoadIoError,
    ValidationError,
    "Schema store I/O load error."
);
create_exception!(
    pyavd_utils.validation,
    ValidationStoreInvalidExtensionError,
    ValidationError,
    "Schema store input file has an invalid extension."
);
create_exception!(
    pyavd_utils.validation,
    ValidationStoreNoFilesFoundError,
    ValidationError,
    "Schema store input directory has no matching files."
);
create_exception!(
    pyavd_utils.validation,
    ValidationSchemaTypeError,
    ValidationError,
    "Schema reference resolved to an invalid schema type."
);
create_exception!(
    pyavd_utils.validation,
    ValidationRefSyntaxError,
    ValidationError,
    "Schema reference has invalid syntax."
);
create_exception!(
    pyavd_utils.validation,
    ValidationSchemaPathError,
    ValidationError,
    "Schema reference path was not found."
);
create_exception!(
    pyavd_utils.validation,
    ValidationInvalidSchemaNameError,
    ValidationError,
    "Schema name was not found in the schema store."
);
create_exception!(
    pyavd_utils.validation,
    ValidationSchemaWalkError,
    ValidationError,
    "Schema reference walk failed."
);
create_exception!(
    pyavd_utils.validation,
    ValidationInvalidJsonDataError,
    ValidationError,
    "Input data is not valid JSON."
);
create_exception!(
    pyavd_utils.validation,
    ValidationInvalidAdhocSchemaJsonError,
    ValidationError,
    "Ad hoc schema is not valid JSON."
);
create_exception!(
    pyavd_utils.validation,
    ValidationInvalidCoercedDataJsonError,
    ValidationError,
    "Coerced validation output could not be serialized as JSON."
);
create_exception!(
    pyavd_utils.validation,
    ValidationInternalError,
    ValidationError,
    "Internal validation error."
);
