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

"""Genrule for the Happy parser generator."""

def genhappy(src, out, **kwargs):
    """Runs the Happy parser generator to create a Haskell source file.

    For more information, see https://haskell.org/happy.

    Example:

        load("//third_party/haskell/happy:build_defs.bzl", "genhappy")

        genhappy(
            src = "MyParser.y",
            out = "MyParser.hs"
        )

        haskell_binary(
            name = "MyParser",
            srcs = [ "MyParser.hs" ]
            ...
        )

    Args:
      src: The input Happy file (.y)
      out: The output Haskell source file (.hs).
      **kwargs: Other parameters for the underlying genrule.
    """

    native.genrule(
        name = out + ".hs_happy",
        srcs = [src],
        outs = [out],
        tools = ["//third_party/haskell/happy:happy_bin"],
        cmd = "$(location //third_party/haskell/happy:happy_bin) -g -a -c -o $(OUTS) $(SRCS)",
        **kwargs
    )
