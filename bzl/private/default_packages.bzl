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

""" A list of packages that every Haskell rule implicitly depends on.

TODO(b/120225063): Reduce this list (or remove it entirely).
"""
default_package_names = [
    "array",
    "base",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "filepath",
    "ghc-prim",
    "integer-gmp",
    "mtl",  # TODO(judahjacobson): Remove this
    "pretty",
    "process",
    "template-haskell",
    "terminfo",
    "text",  # TODO(judahjacobson): Remove this
    "time",
    "transformers",
    "unix",
]

# Packages that are:
# - Part of the "core" GHC package DB
# - Not otherwise present in //third_party/haskell, for example because
#   it's a dependency of the "ghc" package
# - Not otherwise considered "default" packages.  To use them, a rule
#   needs to depend on a corresponding haskell_core_library rule.
nondefault_core_package_names = [
    "ghc",
    "ghc-boot",
    "ghc-boot-th",
    "ghci",
    "hpc",
]

# All "core" packages that we get from the GHC installation,
# rather than adding separately under third_party/haskell.
core_package_names = default_package_names + nondefault_core_package_names
