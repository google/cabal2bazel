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

"""Genrule for the Alex scanner generator."""

def genalex(src, out, **kwargs):
    """Runs the Alex lexer generator to create a Haskell source file.

    For more information, see https://haskell.org/alex.

    Example:

        load("//third_party/haskell/alex:build_defs.bzl", "genalex")

        genalex(
            src = "MyScanner.x",
            out = "MyScanner.hs"
        )

        haskell_binary(
            name = "MyScanner",
            srcs = [ "MyScanner.y.hs" ]
            ...
        )

    Args:
      src: The input Alex file (.x)
      out: The output Haskell source file (.hs).
      **kwargs: Other parameters for the underlying genrule.
    """
    native.genrule(
        name = out + ".hs_alex",
        srcs = [src],
        outs = [out],
        tools = ["//third_party/haskell/alex:alex_bin"],
        cmd = "//third_party/haskell/alex:alex_bin -g -o $(OUTS) $(SRCS)",
        **kwargs
    )
                                                                                                    

