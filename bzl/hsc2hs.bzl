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

"""Build rules for hsc2hs, which generates Haskell (*.hs) from *.hsc files."""
def impl(ctx):
  cpp = ctx.fragments.cpp
  config = ctx.attr._ghc_config.haskell_config

  compiler_flags = depset(cpp.compiler_options([]) + cpp.unfiltered_compiler_options([]))
  compiler_flags += ["-D" + "__GLASGOW_HASKELL__=" + str(config.version)]
  depfiles = depset(config.compiler_bundle + [config.hsc2hs_template])
  for dep in ctx.attr.deps:
    depfiles = depfiles | dep.cc.transitive_headers
    compiler_flags = compiler_flags | dep.cc.compile_flags
  linker_flags = (cpp.mostly_static_link_options([], True)
                  + cpp.link_options)

  ctx.action(
      inputs = [ctx.file.src] + list(depfiles),
      outputs = [ctx.outputs.out],
      executable = config.hsc2hs,
      arguments = (
          [
              "-o", ctx.outputs.out.path,
              "-c", str(cpp.compiler_executable),
              "-l", str(cpp.compiler_executable),
              "-t", config.hsc2hs_template.path,
          ]
          + ["--cflag=" + f
             for flag in compiler_flags for f in ctx.tokenize(flag)]
          + ["--lflag=" + f for f in linker_flags]
          + ["-I", "."]
          + [ctx.file.src.path]
      ),
      mnemonic = "HaskellHsc2hs",
      progress_message = "Hsc2hs generating Haskell file " + str(ctx.label),
  )

"""A BUILD rule for hsc2hs files.

This rule takes a *.hsc file (used for Haskell FFI) and generates a *.hs file
which can be used in the "srcs" of a haskell_{library,binary,test}.

Example usage:

    hsc2hs(
        name = "Foo-hsc",
        src = "Foo.hsc",
    )

    haskell_library(
        name = "Foo",
        srcs = [
            ":Foo-hsc",
            "Bar.hs",  # Ordinary source file
        ],
    )
"""
hsc2hs = rule(
    implementation=impl,
    attrs={
        "src": attr.label(mandatory=True, allow_files=True, single_file=True),
        "deps": attr.label_list(),
        "_ghc_config": attr.label(
            default=Label("//third_party/haskell/ghc:config")),
    },
    output_to_genfiles = True,  # Needed for native rules
    outputs = {
        "out": "%{src}.hs",  # Don't ask why the extension is dropped from 'src'
    },
    fragments = ["cpp"],
)
