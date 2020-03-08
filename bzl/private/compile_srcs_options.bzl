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

"""Options around compiling Haskell source files."""

load("@bazel_skylib//lib:paths.bzl", "paths")

# File extension types of Haskell source files.
_HASKELL_SOURCE_BOOT = [
    ".hs-boot",
    ".lhs-boot",
]

_HASKELL_SOURCE_NONBOOT = [
    ".hs",
    ".lhs",
]

# Options for compiling source files.
# These are present in haskell_library/binary/test but not haskell_proto_library/aspect.
_compile_srcs_options_attrs = {
    "srcs": attr.label_list(
        allow_files = _HASKELL_SOURCE_NONBOOT + _HASKELL_SOURCE_BOOT,
        doc = """
A list of Haskell source files.  The allowed extensions are: <code>.hs</code>,
<code>.lhs</code>, <code>.hs-boot</code>, and <code>.lhs-boot</code>.
""",
    ),
    "ghcopts": attr.string_list(
        doc = """
Arguments to be added to the GHC compilation command.
This is subject to "Make variable" substitution.
<br><br>
Note: for <code>+RTS ... -RTS</code> options, use the
<code>ghc_rtsopts</code> attribute instead.
""",
    ),
    "ghc_rtsopts": attr.string_list(
        doc = """
Runtime flags to pass to the GHC compilation command.
They will be surrounded with the <code>+RTS</code> and
<code>-RTS</code> flags, as described
<a href="http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#setting-rts-options-on-the-command-line">here</a>.  For example:
<code>ghc_rtsopts = ["-A128m"]</code>
<br><br>
These flags are only used by GHC at compile-time, and do not affect the
binaries or tests produced by this rule or other rules that depend on it.
""",
    ),
    "preprocessor": attr.label(
        executable = True,
        cfg = "host",
        doc = """
A binary target.  Specifies an executable to be run as a GHC
preprocessor.  The executable path of the binary target is provided
to GHC via the <code>-pgmF</code> option.
""",
    ),
    "extra_src_files": attr.label_list(
        allow_files = True,
        doc = """
Additional source files that should be available when running the Haskell
compiler.  For example: files that are read by TemplateHaskell, or nonstandard
files that are included by CPP.
<br><br>
Note: Haskell source files should reference the <code>extra_src_files</code>
by their paths relative to the workspace directory.
""",
    ),
    "executes_deps_in_compile": attr.bool(
        doc = """
Whether this target uses compile-time metaprogramming that executes code
from its dependencies.  Should be true when using either the <code>TemplateHaskell</code>
or <code>QuasiQuotes</code> extensions or any ANN pragmas.
""",
    ),
    "allow_deps_as_plugins": attr.bool(
        doc = """
Internal attribute; should only be used for third-party code with
cabal_haskell_package.  Allows the labels specified in <code>deps</code>
to be used as compiler plugins.
<br><br>
""",
    ),
}

def _executes_deps_in_compile(ctx):
    # Default to False because haskell_proto_library doesn't need this
    # functionality.
    return getattr(ctx.attr, "executes_deps_in_compile", False)

def _compile_srcs_options_get(ctx):
    """Extracts the relevant attributes for compiling Haskell source files.

    Does not include the source files themselves, which are handled separately.

    Args:
      ctx: The current rule context.

    Returns:
      A struct consisting of the following fields:
        - srcs: The source files (.hs or .lhs) that should be compiled.
        - inputs: A depset of Files that should be present at compile time.
          Includes .srcs, hs-boot files, extra_src_files, etc.
        - arguments: Command-line flags to pass to GHC.  Does not include the
          source files.
        - rts_arguments: Other flags that are meant for the RTS when running GHC.
        - needs_dynamic_libraries: Whether this rule will need access to the
            deps' shared libraries.
    """
    arguments = []
    inputs = ctx.files.srcs + ctx.files.extra_src_files

    preprocessor = ctx.executable.preprocessor
    if preprocessor:
        arguments += ["-F", "-pgmF", preprocessor.path]
        inputs += [preprocessor]

    # Custom GHC flags for this build rule.  Put them last so they can override
    # previous options.
    arguments += [
        ctx.expand_location(
            opt,
            ctx.attr.extra_src_files,
        ).format(**ctx.var)
        for opt in ctx.attr.ghcopts
    ]
    if "+RTS" in arguments or "-RTS" in arguments:
        fail("The 'ghcopts' attribute cannot have RTS options; found " +
             str(arguments) + ". Put them in 'rts_ghcopts' instead, without the " +
             "surrounding +RTS/-RTS.")

    if not ctx.attr.allow_deps_as_plugins:
        arguments += ["-hide-all-plugin-packages"]

    return struct(
        srcs = [
            f
            for f in ctx.files.srcs
            if paths.split_extension(f.path)[1] in _HASKELL_SOURCE_NONBOOT
        ],
        inputs = depset(inputs),
        arguments = arguments,
        rts_arguments = ctx.attr.ghc_rtsopts,
        needs_dynamic_libraries = _executes_deps_in_compile(ctx),
    )

def _compile_srcs_options_simple(srcs = []):
    return struct(
        srcs = srcs,
        inputs = depset(srcs),
        arguments = [],
        rts_arguments = [],
        needs_dynamic_libraries = False,
    )

def _compile_srcs_options_add_args(options, args):
    """Adds arguments to an existing compile_srcs_options struct."""
    return struct(
        srcs = options.srcs,
        inputs = options.inputs,
        arguments = options.arguments + args,
        rts_arguments = options.rts_arguments,
        needs_dynamic_libraries = options.needs_dynamic_libraries,
    )

compile_srcs_options = struct(
    attrs = _compile_srcs_options_attrs,
    get = _compile_srcs_options_get,
    simple = _compile_srcs_options_simple,
    add_args = _compile_srcs_options_add_args,
)
