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

"""Skylark rules for building Haskell binaries.

This file contains BUILD rules to compile Haskell libraries and binaries.
See go/haskell-rule-docs for documentation of these rules and their attributes.

To use these rules, add a load() statement at the top of your BUILD file; for
example:
load("//bzl:def.bzl", "haskell_library", "haskell_binary")

To enable profiling for a build, set the "prof" define variable to true, e.g.

  bazel build --define prof=true //path/to:target
"""

# This extension often writes InstalledPackageInfo files (*.spec) as an
# intermediate step of registering GHC packages. Documentation is available
# here:
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#installed-pkg-info

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load(
    "//bzl/private:cc.bzl",
    "cpp_options",
)
load(
    "//bzl/private:module_names.bzl",
    "check_module_names",
    "check_module_names_attrs",
)
load("//bzl/private:compile_srcs_options.bzl", "compile_srcs_options")
load(
    "//bzl/private:compile.bzl",
    "compile_sources",
)
load(
    "//bzl/private:info.bzl",
    "compile_info_output_groups",
)
load("//bzl/private:link.bzl", "link_binary")
load(
    "//bzl/private:proto_file.bzl",
    "proto_attrs",
)
load(
    "//bzl/private:providers.bzl",
    "collect_deps",
    "default_packages_attrs",
    "dependency_attrs",
    "get_dependencies",
)
load(
    "//bzl/private:runfiles.bzl",
    "collect_runfiles",
    "runfile_attrs",
)
load(
    "//bzl/private:toolchains.bzl",
    "get_toolchains",
    "toolchains_attrs",
    "toolchains_bootstrap_attrs",
)
load("//bzl:settings.bzl", "settings")

def _unsupported_config(ctx):
    """Checks whether we're building with an unsupported configuration.

    See b/128308184 for context.

    Args:
      ctx: The current rule context

    Returns:
      An empty string if the current build is supported;
      otherwise, a string with the name of the unsupported configuration.
    """

    if ctx.var.get("msan_config") == "true":
        return "--config=msan"
    return ""

def _get_main_is_module(main_is):
    """Parses the module name from a "main_is" attribute.

    The attribute may be either Foo.Bar or Foo.Bar.baz.

    Args:
      main_is: A string, the main_is attribute

    Returns:
      A string, a valid Haskell module name.
    """
    components = main_is.split(".")
    if components and components[-1] and components[-1][0:1].islower():
        return ".".join(components[:-1])
    else:
        return main_is

def _compile_binary(ctx, toolchains, metadata, out, srcs_options, srcs, deps):
    """Run the Haskell compiler to produce a binary file.

    Args:
      ctx: The current rule context.
      toolchains: A struct as returned by get_toolchains().
      metadata: The metadata for this binary.
      out: The output binary File.
      srcs_options: A struct as returned from compile_srcs_options.get().
      srcs: A list of source Files.
      deps: A struct() of dependencies for compiling this code, of the form
        returned by collect_deps().

    Returns:
      A pair of:
        - struct as returned from compile_sources.
        - List of shared libraries that should be added to the runfiles.
    """
    srcs = ctx.files.srcs

    lib = compile_sources(
        ctx = ctx,
        toolchains = toolchains,
        metadata = metadata,
        srcs_options = srcs_options,
        deps = deps,
    )

    is_tsan = ctx.var.get("tsan_config") == "true"

    if hasattr(ctx.executable, "_check_module_names") and not ctx.attr.skip_module_name_check:
        module_names_ok = check_module_names(
            ctx,
            toolchains.haskell,
            srcs_options,
            main_is = _get_main_is_module(ctx.attr.main_is) if ctx.attr.main_is else "Main",
        )
    else:
        module_names_ok = None

    shared_lib_runfiles = link_binary(
        ctx,
        toolchains,
        out,
        # TSAN always links dynamically.
        dynamic = not ctx.attr.linkstatic or is_tsan,
        objects_dir = lib.prof.object if lib.prof else lib.object,
        deps = deps,
        profiling = True if lib.prof else False,
        module_names_ok = module_names_ok,
    )

    return (lib, shared_lib_runfiles)

"""Attributes for compiling libraries and linking shared libraries."""

def _impl_binary(ctx):
    """Implementation of the haskell_binary rule.

    A binary outputs the following files:
      - {name}: The built binary.
      - {name}.spec: A human-readable version of the GHC package used for
          linking this binary, for debugging.

    Args:
      ctx: The current rule context.

    Returns:
      A list of providers.  In particular, the files consist of:
        - {name}, the built binary
        - {name}.spec, a human-readable version of the GHC package that we use
            to link this binary.  (Returned for debugging.)
    """

    out = ctx.actions.declare_file(ctx.label.name)

    # To limit the number of "noasan", etc. tags
    # blacklisting Haskell rules from various TAPs,
    # have binaries create a trivially failing script instead.
    unsupported = _unsupported_config(ctx)
    if unsupported:
        ctx.actions.write(
            output = out,
            is_executable = True,
            content = "\n".join([
                "#!/bin/sh",
                "echo \"Error: Haskell targets are not supported with {}.\" >&2".format(
                    unsupported,
                ),
                "exit 1",
            ]),
        )
        return [DefaultInfo(executable = out)]

    toolchains = get_toolchains(ctx)
    deps = collect_deps(get_dependencies(ctx))

    srcs_options = compile_srcs_options.add_args(
        compile_srcs_options.get(ctx),
        cpp_options(deps) +
        (["-main-is", ctx.attr.main_is] if ctx.attr.main_is else []),
    )

    metadata = struct(name = "main", key = "main", version = "")
    lib, shared_lib_runfiles = _compile_binary(
        ctx = ctx,
        toolchains = toolchains,
        metadata = metadata,
        out = out,
        srcs_options = srcs_options,
        srcs = ctx.files.srcs,
        deps = deps,
    )

    runfiles = collect_runfiles(
        ctx = ctx,
        files = shared_lib_runfiles,
        deps = ctx.attr.deps,
        data = ctx.attr.data,
    )
    return [
        DefaultInfo(
            executable = out,
            runfiles = runfiles,
        ),
        OutputGroupInfo(**dicts.add(
            dict(haskell_check = depset([lib.checked_hi])),
            compile_info_output_groups(
                ctx = ctx,
                metadata = metadata,
                deps = deps,
                srcs_options = srcs_options,
                hidden_modules = [],
                runfiles = runfiles.files,
                wrapper_lib = lib.wrapper_lib,
            ),
        )),
    ]

def _binary_attrs(stamp_default, is_bootstrap = False):
    """Attributes allowed in haskell_binary and haskell_test rules."""
    return dicts.add(
        toolchains_attrs,
        proto_attrs,
        settings.attributes,
        compile_srcs_options.attrs,
        runfile_attrs,
        dependency_attrs,
        # CheckModuleNames can't be used as an attribute of haskell_bootstrap_binary
        # because it itself is an instance of that rule.
        {} if is_bootstrap else check_module_names_attrs,
        {
            "main_is": attr.string(
                doc = """
The module that contains the <code>main</code> function.  If not set,
defaults to <code>"Main"</code>.
""",
            ),
            "threaded": attr.bool(
                default = True,
                doc = """
True by default; can be used to disable threaded RTS when linking
a binary.  Do not disable lightly.
""",
            ),
            "linkopts": attr.string_list(
                doc = """
Arguments to be added to the GHC linking command.
""",
            ),
            "linkstatic": attr.bool(
                default = True,
                doc = """
Link the binary in static mode.  If enabled, links <code>.a</code>'s for
the Haskell and C/C++ dependencies whenever possible.
If disabled, uses the dynamic GHC runtime, and use shared libraries
(<code>.so</code>) to link the Haskell and C/C++ dependencies.

The dynamic runtime may be better for programs using the GHC API,
since it uses the system dynamic linker to load <code>.so</code>s
for TemplateHaskell, rather than the older codepath that loads
<code>.a</code>'s.  GHC doesn't have a way to use the system
dynamic linker without also dynamically linking Haskell dependencies.

Note: profiling builds (<code>--define prof=true</code>) are always
built in static mode.
""",
            ),
            "skip_module_name_check": attr.bool(
                doc = """
Disable the check that module names correspond to the longest capitalized suffix
of the source filepath.  Should only be enabeld for third-party code.
""",
            ),
        },
    )

haskell_binary = rule(
    attrs = dicts.add(_binary_attrs(stamp_default = True), default_packages_attrs),
    executable = True,
    fragments = ["cpp"],
    implementation = _impl_binary,
    doc = """
A BUILD rule for compiling an executable binary from Haskell source files.

Example usage:

    haskell_binary(
        name = "mybinary",
        srcs = ["Main.hs"],
    )
""",
)

def _bootstrap_binary_attrs(stamp_default = False):
    return dicts.add(
        _binary_attrs(stamp_default, is_bootstrap = True),
        toolchains_bootstrap_attrs,
    )

haskell_bootstrap_binary = rule(
    attrs = _bootstrap_binary_attrs(stamp_default = False),
    executable = True,
    fragments = ["cpp"],
    implementation = _impl_binary,
    doc = """
A BUILD rule for compiling a bootstrap GHC executable from Haskell source files.
This is used only for building
//third_party/haskell/ghc/vendor_src/v8_6_4:ghc_bin.

Example usage:

    haskell_bootstrap_binary(
        name = "mybinary",
        srcs = ["Main.hs"],
    )
""",
)

haskell_test = rule(
    attrs = dicts.add(_binary_attrs(stamp_default = False), default_packages_attrs),
    fragments = ["cpp"],
    test = True,
    implementation = _impl_binary,
    doc = """
A BUILD rule for compiling a test from Haskell source files.

Example usage:

    haskell_test(
        name = "mylibrary_test",
        srcs = ["Main.hs"],
    )
""",
)
