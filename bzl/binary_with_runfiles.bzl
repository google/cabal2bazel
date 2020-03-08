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

"""A build rule to collect the runfiles for a binary."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("//bzl/private:action.bzl", "multi_action")

def _impl_binary_with_runfiles(ctx):
    input_runfiles = ctx.attr.src[DefaultInfo].default_runfiles.files

    input_bin = ctx.executable.src
    output_bin = ctx.actions.declare_file(ctx.label.name)
    output_runfiles_dir = ctx.actions.declare_directory(ctx.label.name + ".runfiles")

    new_runfile_paths = [
        (f.path, paths.join(output_runfiles_dir.path, "{WORKSPACE}", f.short_path))  # TODO
        for f in input_runfiles.to_list()
    ]

    ctx.actions.run(
        outputs = [output_bin],
        inputs = [input_bin],
        executable = "/bin/cp",
        # Use "-p" to preserve whether runfiles are executable.
        arguments = ["-p", input_bin.path, output_bin.path],
        progress_message = "Copying binary for " + str(ctx.label),
    )

    # Generate the runfiles in one action, rather than spamming the build
    # graph with a separate action for each runfile.
    multi_action(
        ctx = ctx,
        outputs = [output_runfiles_dir],
        inputs = depset(input_runfiles),
        progress_message = "Creating runfiles for " + str(ctx.label),
        actions =
            [
                struct(
                    cmd = "/bin/mkdir",
                    args = ["-p", paths.dirname(f)],
                )
                for _, f in new_runfile_paths
            ] +
            [
                struct(
                    cmd = "/bin/cp",
                    args = [f, g],
                )
                for f, g in new_runfile_paths
            ],
    )

    all_outputs = [output_bin, output_runfiles_dir]
    return [DefaultInfo(
        files = depset(all_outputs),
        # Pass along the files in runfiles in their same relative locations.
        # For example: if the binary //foo:bar has a runfile //a/b, in a test
        # they will appear as
        # .../test.runfiles/{WORKSPACE}/foo/bar
        # .../test.runfiles/{WORKSPACE}/foo/bar.runfiles/{WORKSPACE}/a/b
        runfiles = ctx.runfiles(files = all_outputs),
    )]

binary_with_runfiles = rule(
    implementation = _impl_binary_with_runfiles,
    attrs = {
        "src": attr.label(
            executable = True,
            cfg = "target",
            doc = """
The label of a rule that produces a binary executable (for example,
cc_binary or haskell_binary).
""",
        ),
    },
)

def _impl_bootstrap_runfiles(ctx):
    return [DefaultInfo(
        files = ctx.attr.src[DefaultInfo].default_runfiles.files,
    )]

binary_runfiles = rule(
    attrs = {
        "src": attr.label(
            allow_single_file = True,
            cfg = "target",
            executable = True,
            mandatory = True,
            providers = [[DefaultInfo]],
        ),
    },
    implementation = _impl_bootstrap_runfiles,
    doc = """
A BUILD rule for extracting the runfiles of the given binary target.

One use case is to provide shared libraries for running the bootstrap GHC
binary to compile other Haskell targets.

Example usage:

    binary_runfiles(
        name = "mybinary_runfiles",
        src = ":mybinary",
    )
""",
)
