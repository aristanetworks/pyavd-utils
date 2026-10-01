// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::create_exception;
use pyo3::exceptions::PyException;

create_exception!(
    pyavd_utils.validation,
    ValidationError,
    PyException,
    "Base exception for pyavd_utils validation helpers."
);
create_exception!(
    pyavd_utils.validation,
    ValidationInvalidSchemaNameError,
    ValidationError,
    "Schema name was not found in the schema store."
);
create_exception!(
    pyavd_utils.validation,
    ValidationSchemaPathError,
    ValidationError,
    "Schema path resolution failed."
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
    "Ad hoc schema is not valid."
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
    ValidationStoreLoadError,
    ValidationError,
    "Schema store could not be loaded."
);
create_exception!(
    pyavd_utils.validation,
    ValidationStoreLoadIoError,
    ValidationStoreLoadError,
    "Schema store I/O load error."
);
