# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.

from __future__ import annotations

from typing import TYPE_CHECKING, ClassVar, Literal, TypeAlias

from pyavd._eos_cli_config_gen.schema import EosCliConfigGen
from pyavd._schema.models.avd_indexed_list import AvdIndexedList
from pyavd._schema.models.avd_list import AvdList
from pyavd._schema.models.avd_model import AvdModel

if TYPE_CHECKING:
    from pyavd._utils import Undefined, UndefinedType


class SchemaGenerationFixture(AvdModel):
    """Subclass of AvdModel."""

    class Accounting(AvdModel):
        """Subclass of AvdModel."""

        Type: TypeAlias = Literal["none", "start-stop", "stop-only"]

        class MethodsItem(AvdModel):
            """Subclass of AvdModel."""

            Method: TypeAlias = Literal["logging", "group"]
            _fields: ClassVar[dict] = {"method": {"type": str}, "group": {"type": str}}
            method: Method
            group: str | None
            """
            Specify the server group to be used.
            This option is applicable only when the `method` key is
            explicitly set to `group`.
            """

            if TYPE_CHECKING:

                def __init__(self, *, method: Method | UndefinedType = Undefined, group: str | UndefinedType | None = Undefined) -> None:
                    """
                    MethodsItem.

                    Subclass of AvdModel.

                    Args:
                        method: method
                        group:
                           Specify the server group to be used.
                           This option is applicable only when the `method` key is
                           explicitly set to `group`.

                    """

        class Methods(AvdList[MethodsItem]):
            """Subclass of AvdList with `MethodsItem` items."""

        Methods._item_type = MethodsItem

        _fields: ClassVar[dict] = {"type": {"type": str}, "methods": {"type": Methods}}
        type: Type
        methods: Methods
        """Subclass of AvdList with `MethodsItem` items."""

        if TYPE_CHECKING:

            def __init__(self, *, type: Type | UndefinedType = Undefined, methods: Methods | UndefinedType = Undefined) -> None:
                """
                Accounting.

                Subclass of AvdModel.

                Args:
                    type: type
                    methods: Subclass of AvdList with `MethodsItem` items.

                """

    class Authentication(AvdModel):
        """Subclass of AvdModel."""

        _fields: ClassVar[dict] = {"login": {"type": EosCliConfigGen.AaaAuthentication.Login}}
        login: EosCliConfigGen.AaaAuthentication.Login

        if TYPE_CHECKING:

            def __init__(self, *, login: EosCliConfigGen.AaaAuthentication.Login | UndefinedType = Undefined) -> None:
                """
                Authentication.

                Subclass of AvdModel.

                Args:
                    login: login

                """

    class InterfaceProfilesItem(AvdModel):
        """Subclass of AvdModel."""

        class Commands(AvdList[str]):
            """Subclass of AvdList with `str` items."""

        Commands._item_type = str

        _fields: ClassVar[dict] = {"name": {"type": str}, "commands": {"type": Commands}, "shutdown": {"type": bool, "default": False}}
        name: str
        """Interface-Profile Name."""
        commands: Commands
        """Subclass of AvdList with `str` items."""
        shutdown: bool
        """Default value: `False`"""

        if TYPE_CHECKING:

            def __init__(
                self, *, name: str | UndefinedType = Undefined, commands: Commands | UndefinedType = Undefined, shutdown: bool | UndefinedType = Undefined
            ) -> None:
                """
                InterfaceProfilesItem.

                Subclass of AvdModel.

                Args:
                    name: Interface-Profile Name.
                    commands: Subclass of AvdList with `str` items.
                    shutdown: shutdown

                """

    class InterfaceProfiles(AvdIndexedList[str, InterfaceProfilesItem]):
        """Subclass of AvdIndexedList with `InterfaceProfilesItem` items. Primary key is `name` (`str`)."""

        _primary_key: ClassVar[str] = "name"

    InterfaceProfiles._item_type = InterfaceProfilesItem

    _fields: ClassVar[dict] = {
        "accounting": {"type": Accounting},
        "authentication": {"type": Authentication},
        "interface_profiles": {"type": InterfaceProfiles},
    }
    _allow_other_keys: ClassVar[bool] = True
    accounting: Accounting
    """Subclass of AvdModel."""
    authentication: Authentication
    """Subclass of AvdModel."""
    interface_profiles: InterfaceProfiles
    """Subclass of AvdIndexedList with `InterfaceProfilesItem` items. Primary key is `name` (`str`)."""

    if TYPE_CHECKING:

        def __init__(
            self,
            *,
            accounting: Accounting | UndefinedType = Undefined,
            authentication: Authentication | UndefinedType = Undefined,
            interface_profiles: InterfaceProfiles | UndefinedType = Undefined,
        ) -> None:
            """
            SchemaGenerationFixture.

            Subclass of AvdModel.

            Args:
                accounting: Subclass of AvdModel.
                authentication: Subclass of AvdModel.
                interface_profiles: Subclass of AvdIndexedList with `InterfaceProfilesItem` items. Primary key is `name` (`str`).

            """
