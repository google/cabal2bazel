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

"""Declares nonstandard locations of third-party haskell libraries."""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load(
    "//bzl/private:default_packages.bzl",
    "default_package_names",
    "nondefault_core_package_names",
)

common_package_overrides = dicts.add(
    {
        name: "//third_party/haskell/ghc:" + name
        for name in nondefault_core_package_names + default_package_names
    },
    {
        "clash-ghc": "//third_party/haskell/clash/clash-ghc:clash_ghc",
        "clash-lib": "//third_party/haskell/clash/clash-lib:clash_lib",
        "clash-prelude": "//third_party/haskell/clash/clash-prelude:clash_prelude",
        "haskell-indexer-backend-core": "//third_party/haskell/haskell_indexer:haskell_indexer_backend_core",
        "haskell-indexer-backend-ghc": "//third_party/haskell/haskell_indexer:haskell_indexer_backend_ghc",
        "haskell-indexer-frontend-kythe": "//third_party/haskell/haskell_indexer:haskell_indexer_frontend_kythe",
        "haskell-indexer-pathutil": "//third_party/haskell/haskell_indexer:haskell_indexer_pathutil",
        "haskell-indexer-pipeline-ghckythe": "//third_party/haskell/haskell_indexer:haskell_indexer_pipeline_ghckythe",
        "haskell-indexer-translate": "//third_party/haskell/haskell_indexer:haskell_indexer_translate",
        "kythe-schema": "//third_party/haskell/haskell_indexer:kythe_schema",
        "proto-lens-runtime": "//third_party/haskell/proto_lens:proto_lens_runtime",
        "text-offset": "//third_party/haskell/haskell_indexer:text_offset",
    },
)
