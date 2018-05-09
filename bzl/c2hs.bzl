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

load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")

def impl(ctx):
  cpp_fragment = ctx.fragments.cpp

  depfiles = depset()
  for x in ctx.attr.deps:
    depfiles += x.cc.transitive_headers
  cpp_toolchain = find_cpp_toolchain(ctx)

  ctx.action(
      inputs = [ctx.file.src] + list(depfiles),
      outputs = [ctx.outputs.out],
      executable = ctx.executable._c2hs,
      mnemonic = "HaskellC2Hs",
      progress_message = "C2hs generating Haskell file for " + str(ctx.label),
      arguments = [
          # Instead of cpp, use the C compiler in preprocessor mode.
          "--cpp", str(cpp_toolchain.compiler_executable),
          "-C-E",
          # Set paths used to find headers.
          "-C-I.",
          # Input/output paths.
          "-o", ctx.outputs.out.path,
          ctx.file.src.path,
      ] + ["-C" + f for f in cpp_fragment.unfiltered_compiler_options([])],
  )

"""c2hs: A rule for generating haskell FFI bindings using c2hs.

Example:
  load("//bzl/c2hs", "c2hs")

  c2hs(
      name = "FooFFI",
      src = "FooFFI.chs",
      deps = [
          ":foo_ffi.h"
      ],
  )

  haskell_library(
      name = "Foo",
      srcs = ["Foo.hs", "FooFFI.hs"],
      ...
  )

Args:
  src: *.chs file.
  deps: List of cc_library rules providing headers needed by the chs file.
"""
c2hs = rule(
    implementation=impl,
    attrs= {
        "src": attr.label(mandatory=True, allow_files=True, single_file=True),
        "deps": attr.label_list(providers=['cc']),
        "_c2hs": attr.label(
            default=Label("//bzl/c2hs"),
            cfg="host",
            executable=True),
        "_cc_toolchain": attr.label(
            default=Label("@bazel_tools//tools/cpp:current_cc_toolchain")
        ),
    },
    output_to_genfiles = True,  # Needed to work with haskell rules.
    outputs = {"out": "%{name}.hs"},
    fragments = ["cpp"],
)
