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

"""Attributes and functions for getting build rule toolchains."""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("//bzl:config.bzl", "HaskellCompilerInfo")
load(
    "//bzl/private:cc.bzl",
    "cc_toolchain_attrs",
    "get_configured_cc_toolchain",
)

def get_toolchains(ctx):
    """Returns the GHC and C++ toolchains for a Haskell rule.

    Returns:
      A struct containing two fields:
      - haskell: A HaskellCompilerInfo provider
      - cc: A struct as returned by get_configured_cc_toolchain.
    """
    return struct(
        haskell = ctx.attr._ghc_config[HaskellCompilerInfo],
        cc = get_configured_cc_toolchain(ctx),
    )

toolchains_attrs = dicts.add({
    "_ghc_config": attr.label(
        default = Label("//third_party/haskell/ghc:config"),
    ),
}, cc_toolchain_attrs)

toolchains_bootstrap_attrs = dicts.add({
    "_ghc_config": attr.label(
        default = Label("//third_party/haskell/ghc:config"),
    ),
}, cc_toolchain_attrs)
