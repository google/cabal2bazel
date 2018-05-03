# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# converted from extension //haskell/build_defs
# Common build definitions for Haskell code.

# Convert a Cabal package name into the corresponding versionless
# package in //third_party/haskell.  This does handle package names
# with hyphens.  Note that it does not support version constraints,
# nor does it import the package from Cabal for you.  See
# https://opensource.google.com/docs/thirdparty/haskell/ if you need
# to depend on a package that doesn't exist in //third_party.
def hackage(name):
  munged_name = name.replace("-", "_")
  return "//third_party/haskell/" + munged_name + (
      ":" + name if munged_name != name else "")


def haskell_build_defs_build_defs__side_effects():
  # Paths to the current version of GHC's binaries/libraries.
  # Note that usually you can just import GHC.Paths from the ghc-paths package
  # rather than using these BUILD variables directly.
  native.vardef("GHC_BINARY", "third_party/haskell/ghc/v8_0_2_$(TARGET_CPU)/bin/ghc")

  native.vardef(
      "GHC_PKG_BINARY",
      "third_party/haskell/ghc/v8_0_2_$(TARGET_CPU)/bin/ghc-pkg",)

  native.vardef("GHC_LIBDIR", "third_party/haskell/ghc/v8_0_2_$(TARGET_CPU)/lib/ghc-8.0.2")

  native.vardef("GHC_PATHS_DOCDIR", "third_party/haskell/ghc/v8_0_2_$(TARGET_CPU)/g3doc/libraries")

