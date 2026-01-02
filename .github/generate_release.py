#!/usr/bin/env python
# Copyright (c) 2023-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""
generate_release.py.

This script is used to generate the release.yml file as per
https://docs.github.com/en/repositories/releasing-projects-on-github/automatically-generated-release-notes
"""

from pathlib import Path
from typing import Any

import yaml

SCOPES = [
    "requirements",
    "validation",
    "passwords",
    "rust",
]

# CI and Test are excluded from Release Notes
CATEGORIES = {
    "Feat": "Features",
    "Fix": "Bug Fixes",
    "Cut": "Cut",
    "Doc": "Documentation",
    # Excluding "CI": "CI",
    "Bump": "Bump",
    # Excluding "Test": "Test",
    "Revert": "Revert",
    "Refactor": "Refactoring",
}


class SafeDumper(yaml.SafeDumper):
    """
    Make yamllint happy.

    https://github.com/yaml/pyyaml/issues/234#issuecomment-765894586
    """

    # pylint: disable=R0901,W0613,W1113

    def increase_indent(self, flow: bool = False, *_args: Any, **_kwargs: Any) -> None:
        return super().increase_indent(flow=flow, indentless=False)


if __name__ == "__main__":
    exclude_list: list[str] = []
    categories_list: list[dict[str, str | list[str]]] = []

    # First add exclude labels
    for scope in SCOPES:
        exclude_list.append(f"rn: Test({scope})")
        exclude_list.append(f"rn: CI({scope})")
    exclude_list.extend(["rn: Test", "rn: CI"])

    # Then add the categories

    breaking_label_categories = ["Feat", "Fix", "Cut", "Revert", "Refactor", "Bump"]
    breaking_labels = [f"rn: {cc_type}({scope})!" for cc_type in breaking_label_categories for scope in SCOPES]
    breaking_labels.extend(f"rn: {cc_type}!" for cc_type in breaking_label_categories)

    categories_list.append(
        {
            "title": "Breaking Changes",
            "labels": breaking_labels,
        },
    )

    # Add fixes for scopes
    categories_list.extend(
        {
            "title": f"Fixed issues in {scope}",
            "labels": [f"rn: Fix({scope})"],
        }
        for scope in SCOPES
    )

    # Add other fixes
    categories_list.append(
        {
            "title": "Other Fixed issues",
            "labels": ["rn: Fix"],
        },
    )

    # Add features for scopes
    categories_list.extend(
        {
            "title": f"New features and enhancements in {scope}",
            "labels": [f"rn: Fix({scope})"],
        }
        for scope in SCOPES
    )

    # Add other features
    categories_list.append(
        {
            "title": "Other new features and enhancements",
            "labels": ["rn: Feat"],
        },
    )

    doc_labels = [f"rn: Doc({scope})" for scope in SCOPES]
    doc_labels.append("rn: Doc")

    categories_list.append(
        {
            "title": "Documentation",
            "labels": doc_labels,
        },
    )

    # Add the catch all
    categories_list.append(
        {
            "title": "Other Changes",
            "labels": ["*"],
        },
    )
    with Path("release.yml").open("w", encoding="utf-8") as release_file:
        yaml.dump(
            {
                "changelog": {
                    "exclude": {"labels": exclude_list},
                    "categories": categories_list,
                },
            },
            release_file,
            Dumper=SafeDumper,
            sort_keys=False,
        )
