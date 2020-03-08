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

"""TODO(judahjacobson): Write module docstring."""

load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")
load("@bazel_skylib//rules:build_test.bzl", "build_test")

def generated_stardoc(name = "", input = "", deps = [], **kwargs):
    """Generates a Stardoc file and an associated build test.

    This rule generates a Markdown rule documentation file, using
    Stardoc.

    The generated file name will be {name}.md.

    Args:
      name: The basename of the output Markdown file (without an extension).
      input: The .bzl file to process.
      deps: A list of bzl_library targets imported by this file.
      **kwargs: Other parameters to the stardoc() macro.
    """
    doc_rule_name = name + "-stardoc"
    doc_name = name + "-doc.md"
    genrule_name = name + "-genrule"
    stardoc(
        name = doc_rule_name,
        input = input,
        deps = deps,
        out = doc_name,
        **kwargs
    )

    # Verify that the rule builds.
    build_test(
        name = name + "_test",
        targets = [genrule_name],
    )
