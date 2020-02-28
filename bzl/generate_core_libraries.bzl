# Copyright 2020 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Provides a macro to generate haskell_core_library rules for a GHC build."""

load(
    "//bzl:def.bzl",
    "haskell_core_library",
    "haskell_library",
)
load(
    "//bzl/private:default_packages.bzl",
    "default_package_names",
    "nondefault_core_package_names",
)
load("@bazel_skylib//rules:build_test.bzl", "build_test")

def generate_core_library_rules():
    """Creates haskell_core_library rules for the GHC build."""
    for name in nondefault_core_package_names + default_package_names:
        haskell_core_library(
            name = name,
            package = name,
        )

    default_packages_rule = "default_packages"
    haskell_library(
        name = default_packages_rule,
        visibility = ["//visibility:private"],
        deps = default_package_names,
    )

    build_test(
        name = "default_packages_build_test",
        targets = [default_packages_rule],
    )
