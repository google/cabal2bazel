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

load("@bazel_skylib//lib:paths.bzl", "paths")
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

def create_shared_haskell_lib(ctx, toolchains, archive_name, lib, collected_deps, bundle_cc_deps):
    """Links the Haskell-built *.dyn_o objects together into a shared library.

    Args:
      ctx: The current rule context
      toolchains: A struct as returned by def.bzl:get_toolchains.
      archive_name: The name of the output archive, minus the "lib"
        prefix and ".a" suffix.
      lib: The compiled library.
      collected_deps: A struct of dependencies of this target.

    Returns:
      A pair of File objects. First one is the resulting "*.so" Haskell library,
      while the second is a LibraryToLink representing a cc dynamic library that
      links against all cc dependencies of the Haskell target.
    """
    config = toolchains.haskell
    ld = link_executable_command(
        toolchains.cc,
        output_file = None,
        is_linking_dynamic_library = True,
    )

    # GHC expects the compiler version suffix in the .so file name.
    # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#building-a-package-from-haskell-source
    shared_lib = ctx.actions.declare_file("{}/lib{}-ghc{}.so".format(
        package.libdir(ctx),
        archive_name,
        config.version,
    ))

    if bundle_cc_deps:
        # Bundle all CC deps into a self-contained dynamic shared library. It
        # will remain dynamic even when the library will get linked into a
        # mostly-statically linked executable, and will then be copied into
        # the .runfiles directory along with the Haskell shared library.
        #
        # Note that this is potentially inefficient when a target uses fully
        # shared linking and depends on multiple Haskell libraries that
        # have shared CC dependencies. In that situation each Haskell library
        # will bundle its own copy of the CC dependency.
        #
        # We could achieve the same by doing the parsing of LibraryToLink
        # dependencies here and trying to link as many of them as static
        # libs, but this would unnecessarily replicate the CC toolchain logic
        # that we use here.
        cc_deps_lib_path = "{}/lib{}-cc-deps.so".format(
            package.libdir(ctx),
            archive_name,
        )
        cc_deps_lib = dynamic_cc_deps_archive(
            ctx,
            toolchains.cc,
            cc_deps_lib_path,
            collected_deps.immediate_cc_libs,
        )
        libs_to_link = [cc_deps_lib.dynamic_library]
    else:
        libs_to_link = collected_deps.immediate_cc_dylibs
        cc_deps_lib = None

    package_link = package.link_options(collected_deps)
    options = (
        ghc_options.library_root(config) +
        config.common_options +
        package_link.arguments +
        ["-optl" + p for p in ld.arguments] +
        ["-shared", "-dynamic", "-o", shared_lib.path] +
        ["-optl" + a for a in _shared_linker_args(
            shared_lib,
            libs_to_link,
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
            libs_to_link,
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

    return shared_lib, cc_deps_lib

def dynamic_cc_deps_archive(ctx, cc_toolchain, lib_path, libs):
    """(Mostly) Statically link the specified cc libraries into a shared library.

    This weird trick hardens the Haskell shared libraries, making it possible to
    use them in targets that use mostly static linking. Without it, blaze is happy
    to link the cc dependencies directly into the output and consider them resolved,
    without realizing that the Haskell .so files will need a shared version of them.
    This helper bundles all cc libraries in a shared object, and hides the possibility
    of creating its static version from blaze, guaranteeing that the cc deps are always
    available to be dynamically loaded.

    Args:
      ctx: The current rule context.
      cc_toolchain: A struct as returned by get_configured_cc_toolchain.
      lib_path: The desired path of the output file.
      libs: A list of LibraryToLink objects representing the cc dependencies.

    Returns:
      A LibraryToLink, wrapping a shared library that links against all of the libs.
    """

    # Point the linker at the directories containing the dependencies.
    # Also include the runtime libraries, which are implicit dependencies of every cc_library.
    linking_ctx = cc_common.create_linking_context(
        libraries_to_link = libs,
    )
    lib_name = paths.basename(lib_path)

    # Note that even though the output of .link can be used as a static
    # library, we consciously fail to report that such a static library
    # can be treated as an output of this rule, to ensure that this
    # .so will be added to the runfiles.
    linking_outputs = cc_common.link(
        actions = ctx.actions,
        feature_configuration = cc_toolchain.feature_configuration,
        cc_toolchain = cc_toolchain.toolchain,
        output_type = "dynamic_library",
        link_deps_statically = True,
        linking_contexts = [linking_ctx],
        name = lib_name,
    )

    # We have to copy this out, because cc_common.link returns a file in _solib
    # which triggers an assertion in create_library_to_link.
    linked_lib = linking_outputs.library_to_link.dynamic_library
    output_lib = ctx.actions.declare_file(lib_path)
    ctx.actions.run_shell(
        inputs = [linked_lib],
        outputs = [output_lib],
        command = "cp -f \"$1\" \"$2\"",
        arguments = [linked_lib.path, output_lib.path],
        mnemonic = "HaskellCopyCcDeps",
        progress_message = "Copying CC dependency bundle for Haskell rule " + str(ctx.label),
        use_default_shell_env = True,
    )
    return cc_common.create_library_to_link(
        actions = ctx.actions,
        feature_configuration = cc_toolchain.feature_configuration,
        cc_toolchain = cc_toolchain.toolchain,
        dynamic_library = output_lib,
    )

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
