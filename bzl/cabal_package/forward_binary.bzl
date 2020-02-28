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

"""A build rule to forward a binary from another rule without affecting shared linking.

Consider the alternative of using an sh_binary to forward a rule:

sh_binary(
    name = "link",
    srcs = ["//path/to:rule"],
)

That will create a symlink named "link" that points to the original rule.  Unfortunately,
if the original rule is a shared binary, that will break any rpaths using $EXEC_ORIGIN
(e.g., "$EXEC_ORIGIN/../../_solib_k8").  Since $EXEC_ORIGIN doesn't follow symlinks,
it will try to resolve them relative to "link" instead of "rule".

Instead, using

forward_binary(
    name = "forward",
    src = "//path/to:rule",
)

will create an executable shell script named "forward" that invokes the original rule.
In that case, $EXEC_ORIGIN will be the location of the original rule, as expected.
"""

def _impl(ctx):
    output = ctx.actions.declare_file(ctx.label.name)

    # Point to the top-level directory (bazel-bin or runfiles).
    prefix = "$(dirname \"$BASH_SOURCE\")/" + "../" * (len(output.short_path.split("/")) - 1)
    ctx.actions.write(
        output,
        "\n".join(
            [
                "#!/bin/bash",
                "set -euo pipefail",
                "exec {} \"$@\"".format(prefix + ctx.executable.src.short_path),
            ],
        ),
        is_executable = True,
    )
    return [DefaultInfo(
        executable = output,
        files = ctx.attr.src[DefaultInfo].files,
        runfiles = ctx.runfiles(files = [output, ctx.executable.src]).merge(
            ctx.attr.src[DefaultInfo].data_runfiles,
        ),
    )]

forward_binary = rule(
    implementation = _impl,
    executable = True,
    attrs = {
        "src": attr.label(executable = True, cfg = "target"),
    },
)
