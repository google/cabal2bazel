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

"""Build rules for hsc2hs, which generates Haskell (*.hs) from *.hsc files."""

load(
    "//bzl/private:cc.bzl",
    "cc_toolchain_attrs",
    "compile_command",
    "get_compile_flags",
    "get_configured_cc_toolchain",
    "get_static_runtime_libs",
    "link_executable_command",
)
load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("//bzl:config.bzl", "HaskellCompilerInfo", "ghc_preprocessor_defines")

def _impl(ctx):
    config = ctx.attr._ghc_config[HaskellCompilerInfo]

    cc_toolchain = get_configured_cc_toolchain(ctx)

    compile = compile_command(
        cc_toolchain,
        preprocessor_defines = depset(ghc_preprocessor_defines(config)),
        user_compile_flags = ["-w", "-Wno-error"],
    )
    link = link_executable_command(cc_toolchain)

    depfiles = depset(
        config.compiler_bundle + [config.hsc2hs_template],
        transitive = [dep[CcInfo].compilation_context.headers for dep in ctx.attr.deps],
    )

    compiler_flags = depset(
        compile.arguments +
        [flag for dep in ctx.attr.deps for flag in get_compile_flags(dep[CcInfo])],
    )

    static_runtime_libs = get_static_runtime_libs(ctx)
    linker_flags = link.arguments + [f.path for f in static_runtime_libs]

    ctx.actions.run(
        inputs = depset(
            [ctx.file.src] + static_runtime_libs,
            transitive = [
                depfiles,
                cc_toolchain.toolchain.all_files,
            ],
        ),
        outputs = [ctx.outputs.out],
        executable = config.hsc2hs,
        arguments = (
            [
                "-o",
                ctx.outputs.out.path,
                "-c",
                compile.executable,
                "-l",
                link.executable,
                "-t",
                config.hsc2hs_template.path,
            ] +
            [
                "--cflag=" + f
                for flag in compiler_flags.to_list()
                for f in ctx.tokenize(flag)
            ] +
            ["--lflag=" + f for f in linker_flags] +
            ["-I", "."] +
            [ctx.file.src.path]
        ),
        mnemonic = "HaskellHsc2hs",
        progress_message = "Hsc2hs generating Haskell file " + str(ctx.label),
    )

hsc2hs = rule(
    implementation = _impl,
    attrs = dicts.add(cc_toolchain_attrs, {
        "src": attr.label(
            mandatory = True,
            allow_single_file = True,
            doc = "A source *.hsc file.",
        ),
        "deps": attr.label_list(
            providers = [CcInfo],
            doc = """
A list of cc_library targets that the source .hsc file depends on.
""",
        ),
        "out": attr.output(
            mandatory = True,
            doc = "The name of the generated output file (ending in '.hs').",
        ),
        "_ghc_config": attr.label(
            default = Label("//third_party/haskell/ghc:config"),
        ),
    }),
    fragments = ["cpp"],
    doc = """
A BUILD rule for hsc2hs files.

This rule takes a *.hsc file (used for Haskell FFI) and generates a *.hs file
which can be used in the "srcs" of a haskell_{library,binary,test}.

Example usage:

    hsc2hs(
        name = "Foo-hsc",
        src = "Foo.hsc",
        out = "Foo.hs",
    )

    haskell_library(
        name = "Foo",
        srcs = [
            ":Foo-hsc",
            "Bar.hs",  # Ordinary source file
        ],
    )
""",
)
