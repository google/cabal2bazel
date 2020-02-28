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

"""Functions for creating static and dynamic shared libraries."""

load("@bazel_skylib//lib:collections.bzl", "collections")
load("//bzl/private:action.bzl", "multi_action")
load(
    "//bzl/private:cc.bzl",
    "get_ar_executable",
    "get_dynamic_runtime_libs",
    "link_executable_command",
    "shared_library_name",
)
load("//bzl/private:ghc_options.bzl", "ghc_options")
load("//bzl/private:package.bzl", "package")

def create_static_lib_archive(ctx, cc_toolchain, archive_name, object_dir, obj_extension):
    """Links the given objects together into a single archive (.a) file.

    Args:
      ctx: The current rule context
      cc_toolchain: A struct as returned by get_configured_cc_toolchain.
      archive_name: The name of the output archive, minus the "lib"
        prefix and ".a" suffix
      object_dir: A directory containing object files.
      obj_extension: The extension of object files to link (e.g. "o")

    Returns:
      A File of a library archive.
    """

    # Combine all the .o files into a single archive.
    lib_archive = ctx.actions.declare_file("{}/lib{}.a".format(
        package.libdir(ctx),
        archive_name,
    ))

    multi_action(
        ctx = ctx,
        outputs = [lib_archive],
        inputs = depset(
            [object_dir],
            transitive = [cc_toolchain.toolchain.all_files],
        ),
        progress_message = "Creating archive for %s" % str(ctx.label),
        mnemonic = "HaskellArchive",
        actions = [
            struct(cmd = "find %s -name '*.%s' | xargs %s cqsD %s" %
                         (
                             object_dir.path,
                             obj_extension,
                             get_ar_executable(cc_toolchain),
                             lib_archive.path,
                         )),
        ],
    )
    return lib_archive

def _shared_linker_args(output_lib, libs, runtime_libs = []):
    """Returns linker flags to create a DSO that links against some shared libraries.

    Includes both the command to actually link against the libraries, and also path
    information about how to find them (at both link-time and run-time).

    Args:
      output_lib: The lib being created.
      libs: A list of Files, expected to be *.so files.
      runtime_libs: A list of Files: runtime libraries that the libs link against,
        which we also need to help the linker find.

    Returns:
      A list of strings; flags to the linker.
    """

    # Point the linker at the directories containing the dependencies.
    # Also include the runtime libraries, which are implicit dependencies of every cc_library.
    dynamic_lib_dirs = collections.uniq(
        [p.dirname for p in (libs + runtime_libs)],
    )
    origin_rpath_prefix = "$ORIGIN/" + "../" * len(output_lib.dirname.split("/"))
    return (
        # Link against the given libs.
        ["-l" + f for f in [shared_library_name(p) for p in libs] if f] +
        # Where to find them at link time.
        ["-L" + p for p in dynamic_lib_dirs] +
        # Where to find them at runtime.  We embed two rpaths:
        # - Relative to the root; this works when $PWD is the root,
        #   for example when Bazel is running a build action.
        # - Relative to this output library, using $ORIGIN.  This works when we're in a
        #   different directory than root, but *only* when run locally, not on Forge
        #   due to the symlink tree it creates.  We'd like to use $EXEC_ORIGIN there too,
        #   but that only works for executables, not for DSOs.
        ["-Wl,-rpath," + d for d in dynamic_lib_dirs] +
        ["-Wl,-rpath," + origin_rpath_prefix + d for d in dynamic_lib_dirs]
    )

def create_shared_haskell_lib(ctx, toolchains, archive_name, lib, collected_deps):
    """Links the Haskell-built *.dyn_o objects together into a shared library.

    Args:
      ctx: The current rule context
      toolchains: A struct as returned by def.bzl:_get_toolchains.
      archive_name: The name of the output archive, minus the "lib"
        prefix and ".a" suffix.
      lib: The compiled library.
      collected_deps: A struct of dependencies of this target.

    Returns:
      A File for the resulting "*.so" library.
    """
    config = toolchains.haskell
    ld = link_executable_command(
        toolchains.cc,
        output_file = None,
        is_linking_dynamic_library = True,
    )

    # TODO: do we need to also include the version number when it's nontrivial?
    shared_lib = ctx.actions.declare_file("{}/lib{}-ghc{}.so".format(
        package.libdir(ctx),
        archive_name,
        config.version,
    ))
    package_link = package.link_options(collected_deps)
    options = (
        ghc_options.library_root(config) +
        config.common_options +
        package_link.arguments +
        ["-optl" + p for p in ld.arguments] +
        ["-shared", "-dynamic", "-o", shared_lib.path] +
        ["-optl" + a for a in _shared_linker_args(
            shared_lib,
            collected_deps.immediate_cc_dylibs,
            get_dynamic_runtime_libs(ctx),
        )]
    )
    ctx.actions.run_shell(
        command = "{} $@ $(find {} -name \"*.dyn_o\")".format(
            config.compiler.path,
            lib.object.path,
        ),
        arguments = options,
        inputs = depset(
            config.compiler_bundle + [config.compiler, lib.object] +
            collected_deps.immediate_cc_dylibs,
            transitive = [
                package_link.inputs,
                collected_deps.transitive.haskell.shared_libs,
                collected_deps.transitive.haskell.cc_wrapper_libs,
                toolchains.cc.toolchain.all_files,
            ],
        ),
        outputs = [shared_lib],
        mnemonic = "HaskellLinkSharedLibrary",
        progress_message = "Linking Haskell shared library for " + str(ctx.label),
    )

    return shared_lib

def create_wrapper_shared_cc_lib(ctx, cc_toolchain, name, libs):
    """Links the given shared libraries into another "wrapper" library.

    Args:
      ctx: The current rule context.
      cc_toolchain: A struct as returned by get_configured_cc_toolchain.
      name: The name of the output file.
      libs: A list of "*.so" files.

    Returns:
      A File, which is a shared library that links against all of the given libs.
    """

    output_lib = ctx.actions.declare_file(name)

    # Point the linker at the directories containing the dependencies.
    # Also include the runtime libraries, which are implicit dependencies of every cc_library.
    ld = link_executable_command(
        cc_toolchain,
        output_file = output_lib.path,
        is_linking_dynamic_library = True,
        user_link_flags = _shared_linker_args(
            output_lib,
            libs,
            get_dynamic_runtime_libs(ctx),
        ),
    )

    ctx.actions.run(
        inputs = depset(libs, transitive = [cc_toolchain.toolchain.all_files]),
        outputs = [output_lib],
        executable = ld.executable,
        mnemonic = "HaskellLinkSharedCcDeps",
        progress_message = "Linking shared CC deps for Haskell rule " + str(ctx.label),
        arguments = ld.arguments,
    )
    return output_lib
