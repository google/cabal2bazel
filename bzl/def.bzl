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

"""This file reexports all of the Haskell build rules.

For more information, see go/haskell-rules.
"""

load(
    "//bzl/private:binary.bzl",
    _haskell_binary = "haskell_binary",
    _haskell_bootstrap_binary = "haskell_bootstrap_binary",
    _haskell_test = "haskell_test",
)
load(
    "//bzl/private:library.bzl",
    _haskell_core_library = "haskell_core_library",
    _haskell_library = "haskell_library",
    _haskell_proto_aspect = "haskell_proto_aspect",
    _haskell_proto_library = "haskell_proto_library",
    _haskell_static_foreign_library = "haskell_static_foreign_library",
)

haskell_binary = _haskell_binary
haskell_bootstrap_binary = _haskell_bootstrap_binary
haskell_core_library = _haskell_core_library
haskell_library = _haskell_library
haskell_proto_aspect = _haskell_proto_aspect
haskell_proto_library = _haskell_proto_library
haskell_test = _haskell_test
haskell_static_foreign_library = _haskell_static_foreign_library
