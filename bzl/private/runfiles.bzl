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

"""Functions for collecting runfiles of a rule."""

def collect_runfiles(
        ctx,
        files = [],
        data = [],
        deps = []):
    """Collects runfiles for the current rule.

    Args:
      ctx: The current rule context.
      files: A list of Files to explicitly include in the output runfiles.
        Usually concerns the outputs of this rule.
      data: A list of targets to treat as "data".  Their files and runfiles
        will both be collected into the output runfiles.
      deps: A list of targets to treat as "deps".  Their runfiles
        (but not files) will be collected into the output runfiles.

    Returns:
      A ctx.runfiles that aggregates all of the inputs.
    """
    return ctx.runfiles(
        files = files,
        transitive_files = depset(
            transitive =
                [t[DefaultInfo].files for t in data] +
                [t[DefaultInfo].default_runfiles.files for t in data + deps],
        ),
    )

runfile_attrs = {
    "data": attr.label_list(
        allow_files = True,
        doc = "Files needed by this rule at runtime.",
    ),
}
