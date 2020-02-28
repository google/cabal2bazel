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

"""c2hs: A rule for generating haskell FFI bindings using c2hs.
"""

load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "C_COMPILE_ACTION_NAME",
)
load("//tools/build_defs/haskell:config.bzl", "HaskellCompilerInfo", "ghc_preprocessor_defines")

UNSUPPORTED_FEATURES = [
    "thin_lto",
    "module_maps",
    "use_header_modules",
    "fdo_instrument",
    "fdo_optimize",
]

def _impl(ctx):
    depfiles = depset(
        ctx.files._crosstool,
        transitive = [dep[CcInfo].compilation_context.headers for dep in ctx.attr.deps],
    )
    cpp_toolchain = find_cpp_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cpp_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features + UNSUPPORTED_FEATURES,
    )
    variables = cc_common.create_compile_variables(
        feature_configuration = feature_configuration,
        cc_toolchain = cpp_toolchain,
        preprocessor_defines = depset(ghc_preprocessor_defines(ctx.attr._ghc_config[HaskellCompilerInfo])),
    )
    compiler_options = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
        variables = variables,
    )

    ctx.actions.run(
        inputs = depset([ctx.file.src] + depfiles.to_list()),
        outputs = [ctx.outputs.out],
        executable = ctx.executable._c2hs,
        mnemonic = "HaskellC2Hs",
        progress_message = "C2hs generating Haskell file for " + str(ctx.label),
        arguments = [
            # Instead of cpp, use the C compiler in preprocessor mode.
            "--cpp",
            str(cpp_toolchain.compiler_executable),
            "-C-E",
            # Set paths used to find headers.
            "-C-I.",
            # Input/output paths.
            "-o",
            ctx.outputs.out.path,
            ctx.file.src.path,
        ] + ["-C" + f for f in compiler_options],
    )

c2hs = rule(
    implementation = _impl,
    doc = """
c2hs: A rule for generating haskell FFI bindings using c2hs.

Example:
  load("//third_party/haskell/c2hs:c2hs.bzl", "c2hs")

  c2hs(
      name = "FooFFI",
      src = "FooFFI.chs",
      out = "Foo.hs",
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
  out: The name of the generated output file.
  deps: List of cc_library rules providing headers needed by the chs file.
""",
    attrs = {
        "src": attr.label(mandatory = True, allow_single_file = True),
        "deps": attr.label_list(providers = ["cc"]),
        "out": attr.output(mandatory = True),
        "_c2hs": attr.label(
            default = Label("//third_party/haskell/c2hs:c2hs_bin"),
            cfg = "host",
            executable = True,
        ),
        "_crosstool": attr.label_list(
            cfg = "host",
            default = [
                Label("//tools/cpp:crosstool"),
                Label("//third_party/crosstool:v18"),
            ],
        ),
        "_cc_toolchain": attr.label(
            default = Label("//tools/cpp:current_cc_toolchain"),
        ),
        "_ghc_config": attr.label(
            default = Label("//third_party/haskell/ghc:config"),
        ),
    },
    fragments = ["cpp"],
)
