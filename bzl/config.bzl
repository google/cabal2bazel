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

"""Skylark rules for configuring the Haskell compiler.

This file contains a BUILD rule for configuring the executables and support
files for a particular version of the Haskell compiler (i.e., GHC).

Generally these rules should only be used inside of
//third_party/haskell/ghc.  The standard Haskell build rules (haskell_library,
haskell_binary, etc.) will automatically import them by default.

Example usage:

ghc_config(
  name = "config",
  # Note: this corresponds to GHC's "major" version number, e.g.,
  # 7.8 rather than 7.8.4.  It's used to define the __GLASGOW_HASKELL__ macro
  # for utilities like hsc2hs.
  version = 708,
  compiler = "bin/ghc",
  compiler_bundle = ":ghc-support-files"
  ghc_pkg = "bin/ghc-pkg",
  ghc_pkg_bundle = ":ghc-pkg-support-files",
  hsc2hs = "bin/hsc2hs",
  hsc2hs_template = "lib/template-hsc.h",
  default_packages = "default-packages.txt",
  default_options = ["-Wall", "-Werror"],
)

# Re-export config from another package:
ghc_config(
  name = "config-reexport",
  config = "//path/to/specific/ghc/version:config",
)

"""
def _impl(ctx):
  if ctx.attr.config:
    return struct(haskell_config=ctx.attr.config.haskell_config)

  return struct(
      haskell_config=struct(
          version=ctx.attr.version,
          binutils_path=ctx.attr.binutils_path,
          compiler=ctx.executable.compiler,
          compiler_bundle=ctx.files.compiler_bundle,
          library_bundle=ctx.files.library_bundle,
          ghc_pkg=ctx.executable.ghc_pkg,
          ghc_pkg_bundle=ctx.files.ghc_pkg_bundle,
          hsc2hs=ctx.executable.hsc2hs,
          hsc2hs_template=ctx.file.hsc2hs_template,
          default_packages=ctx.file.default_packages,
          default_options=ctx.attr.default_options,
      )
  )

ghc_config = rule(
    implementation=_impl,
    attrs={
        "config": attr.label(providers=["haskell_config"]),
        "version": attr.int(),
        "compiler": attr.label(executable=True, cfg="host"),
        "compiler_bundle": attr.label_list(allow_files=True),
        "library_bundle": attr.label_list(allow_files=True),
        # Ideally, we would use a semantic label for binutils_path. However,
        # Bazel understands packages, not directores; and we want a directory.
        "binutils_path": attr.string(),
        "ghc_pkg": attr.label(executable=True, cfg="host"),
        "ghc_pkg_bundle": attr.label_list(allow_files=True),
        "hsc2hs": attr.label(executable=True, cfg="host"),
        "hsc2hs_template": attr.label(allow_files=True, single_file=True),
        "default_packages": attr.label(allow_files=True, single_file=True),
        "default_options": attr.string_list(),
    },
)
