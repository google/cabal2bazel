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

"""Example file for creating a C++ binary from the Starlark API."""

load(
    "//bzl/private:cc.bzl",
    "cc_toolchain_attrs",
    "get_configured_cc_toolchain",
    "get_object_linking_args",
    "get_static_runtime_libs",
    "link_executable_command",
)
load("@bazel_skylib//lib:dicts.bzl", "dicts")

def _impl_binary(ctx):
    output = ctx.actions.declare_file(ctx.label.name)
    libs = [
        lib
        for dep in ctx.attr.deps
        for lib in dep[CcInfo].linking_context.libraries_to_link.to_list()
    ]

    cc_toolchain = get_configured_cc_toolchain(ctx)
    static_runtime_libs = get_static_runtime_libs(ctx)
    object_args = [get_object_linking_args(lib) for lib in libs]
    link = link_executable_command(
        cc_toolchain,
        is_linking_dynamic_library = False,
        is_static_linking_mode = True,
        output_file = output.path,
        user_link_flags =
            [arg for oarg in object_args for arg in oarg.arguments] +
            [f.path for f in static_runtime_libs],
    )

    ctx.actions.run(
        executable = link.executable,
        arguments = link.arguments,
        inputs = depset(
            [input for oarg in object_args for input in oarg.inputs] +
            static_runtime_libs,
            transitive = [cc_toolchain.toolchain.all_files],
        ),
        outputs = [output],
        mnemonic = "HaskellTestCcLink",
    )

    return [DefaultInfo(executable = output)]

cc_api_binary = rule(
    implementation = _impl_binary,
    executable = True,
    fragments = ["cpp"],
    attrs = dicts.add({
        "deps": attr.label_list(providers = [CcInfo]),
    }, cc_toolchain_attrs),
)
