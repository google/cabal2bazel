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

"""Functions for linking Haskell binaries."""

load("@bazel_skylib//lib:collections.bzl", "collections")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "//bzl/private:cc.bzl",
    "get_dynamic_libraries",
    "get_dynamic_runtime_libs",
    "get_object_linking_args",
    "get_static_runtime_libs",
    "link_executable_command",
    "shared_library_name",
)
load("//bzl/private:ghc_options.bzl", "ghc_options")
load("//bzl/private:package.bzl", "package")

def link_binary(ctx, toolchains, out, dynamic, objects_dir, deps, module_names_ok, profiling):
    """Run the Haskell compiler to link object files into a binary.

    Args:
      ctx: The current rule context.
      toolchains: A struct as returned by def.bzl:)get_toolchains.
      out: The output binary File.
      dynamic: Whether to link against static or shared Haskell libraries,
        and whether to use GHC's static or dynamic runtimes.
      objects_dir: A directory containing object files.
      deps: A struct() of dependencies as returned by collect_deps().
      module_names_ok: An optional File signalling that the module names were
        checked.  The link step depends on this file, to make sure the build
        fails if the module names are wrong.
      profiling: A Bool; whether this target is being built with profiling.
        Note: profiled builds will always be built statically.

    Returns:
      A list of shared libraries that should be used as runfiles (if any).
      Empty if linkstatic=True.
    """

    # Profiled builds are always built statically, since GHC doesn't
    # provide a profiled+dynamic RTS.
    if profiling:
        dynamic = False
    linking_args = _get_linking_args(
        ctx,
        toolchains.cc,
        deps,
        dynamic,
    )

    link_options = toolchains.haskell.common_options + ctx.attr.linkopts

    if ctx.attr.threaded:
        link_options += ["-threaded"]

    link_options += ["-rtsopts"]

    package_link = package.link_options(deps)

    # Specifying -package-id" for each immediate dependency tells GHC to link it.
    # GHC will also automatically include any transitive dependencies.
    link_options = (
        ghc_options.library_root(toolchains.haskell) +
        link_options +
        package_link.arguments
    )

    # Collect all the shared libraries needed by this binary.
    # We'll include them in the runfiles, and also set the binary's
    # rpath to link to them.
    shared_libraries = []
    if dynamic:
        # Set dynload=deploy so GHC doesn't try to embed RPATH automatically;
        # we'll do that manually below, with relative paths to $EXEC_ORIGIN.
        link_options += ["-dynamic", "-dynload=deploy"]
        ext = "dyn_o"
        shared_libraries = (
            get_dynamic_libraries(ctx, deps.transitive.cc) +
            # Group all Haskell shared libraries in a single directory,
            # to make the rpath simpler (and faster to search).
            _collect_files(
                ctx,
                ctx.label.name + ".solib",
                # These files all have unique basenames:
                # The basename contain the core package name+version:
                toolchains.haskell.shared_libraries +
                deps.transitive.haskell.cc_wrapper_libs.to_list() +
                # The basename contains the package metadata.key:
                [p for p in deps.transitive.haskell.shared_libs.to_list()],
            )
        )
    elif profiling:
        # From above, profiling implies static binaries.
        link_options += ["-prof"]
        ext = "p_o"
    else:
        ext = "o"

    # Collect all of the inputs to the link action.
    inputs = depset(
        [objects_dir] +
        ([module_names_ok] if module_names_ok else []) +
        toolchains.haskell.compiler_bundle +
        (toolchains.haskell.prof_bundle if profiling else []),
        transitive = [
            linking_args.files,
            package_link.inputs,
            deps.transitive.haskell.shared_libs if dynamic else deps.transitive.haskell.static_libs,
            toolchains.cc.toolchain.all_files,
        ],
    )

    # Fail if there are any errors, including from $(..) expressions, pipes,
    # or undefined variables.
    command = "set -eu -o pipefail; {} $@ {}".format(
        toolchains.haskell.compiler.path,
        "$(find {} -name \"*.{}\")".format(objects_dir.path, ext),
    )

    linker_args = ctx.actions.args()

    # A large number of dependencies can cause an "Argument list too long"
    # error. Pass linker arguments via a file using the '@file' syntax.
    linker_args.use_param_file("-optl@%s", use_always = True)
    linker_args.add_all(linking_args.flags)
    for p in _rpaths_for_shared_libraries(out, shared_libraries):
        linker_args.add("-Wl,-rpath," + p)
    other_args = ctx.actions.args()
    other_args.add_all(link_options)
    other_args.add("-o", out.path)
    ctx.actions.run_shell(
        command = command,
        arguments = [linker_args, other_args],
        inputs = inputs,
        progress_message = "Linking Haskell binary %s" % str(ctx.label),
        mnemonic = "HaskellLinkBinary",
        outputs = [out],
    )

    return shared_libraries

def _rpaths_for_shared_libraries(out, shared_libraries):
    """Returns the rpath entries to look up the given list of shared libraries."""

    # Computes the relative paths from this binary's directory to the directories that hold
    # each shared library.
    shared_lib_dirs = collections.uniq([paths.dirname(p.short_path) for p in shared_libraries])

    # A directory inside of which the shared_lib_dirs may be found.
    # This path is relative to the directory containing the output binary.
    # Note: the logic is a subset of the algorithm to find the runfiles directory
    # in {WORKSPACE}/haskell/Google/Runtime/BuildHelper.hs.
    root_paths = [
        # Relative to this file in the same tree.  Useful when run as the data of another
        # binary/test, or when run by "bazel test":
        #   Executable:      bar.runfiles/{WORKSPACE}/path/to/foo
        #   Library to find: bar.runfiles/{WORKSPACE}/another/path/for/libfile.so
        # Points to:
        #   bar.runfiles/{WORKSPACE}/path/to/../..,
        # or, equivalently,
        #   bar.runfiles/{WORKSPACE}
        "../" * (len(out.short_path.split("/")) - 1),

        # In a ".runfiles" directory next to the binary:
        #   Executable:      path/to/foo
        #   Library to find: path/to/foo.runfiles/{WORKSPACE}/another/path/for/libfile.so
        # Points to:
        #   path/to/foo.runfiles/{WORKSPACE}
        out.basename + ".runfiles/{WORKSPACE}/",  # TODO: fill in workspace name
    ]

    return [
        paths.join("\\$EXEC_ORIGIN", root_path, p)
        for root_path in root_paths
        for p in shared_lib_dirs
    ]

def _get_linking_args(ctx, cc_toolchain, deps, dynamic):
    """Returns the flags to pass to the linker when linking a binary.

    TODO(judahjacobson): Consider returning Args from this function.

    Args:
      ctx: The current rule context.
      cc_toolchain: A struct as returned by get_configured_cc_toolchain.
      deps: A struct() of dependencies for compiling this code, of the form
        returned by collect_deps().
      dynamic: Whether this binary is being built against shared libraries.

    Returns:
      A struct consisting of the following fields:
        files: Support files for this package.
        arguments: The arguments to pass to the linker.
    """

    # Link against the cc_library dependencies.
    if dynamic:
        user_link_flags = deps.transitive.cc.link_flags.to_list()

        # TODO(judahjacobson): We may not need to explicitly link against every
        # C++ library, since the Haskell libraries we depend on already link against
        # their C++ dependencies.  Consider instead only:
        # 1) Libraries that this haskell_binary/test rule depends directly on, and
        # 2) Any libraries that are alwayslink.
        lib_files = [
            lib.dynamic_library
            for lib in deps.transitive.cc.libs.to_list()
            if lib.dynamic_library
        ] + get_dynamic_runtime_libs(ctx)
        user_link_flags += ["-L" + dd for dd in collections.uniq([p.dirname for p in lib_files])]
        user_link_flags += ["-l" + ff for ff in [shared_library_name(p) for p in lib_files] if ff]

        # Link directly against the objects for libraries that don't have dynamic
        # versions.
        object_args = [
            get_object_linking_args(lib)
            for lib in deps.transitive.cc.libs.to_list()
            if not lib.dynamic_library
        ]
        user_link_flags += [arg for oarg in object_args for arg in oarg.arguments]
        lib_files += [input for oarg in object_args for input in oarg.inputs]
    else:
        # Link statically against all transitive C library dependencies.
        static_runtime_libs = get_static_runtime_libs(ctx)
        object_args = [get_object_linking_args(lib) for lib in deps.transitive.cc.libs.to_list()]
        user_link_flags = (
            [arg for oarg in object_args for arg in oarg.arguments] +
            # Link against libstdc++/libc++ and libgcc, for cc_libraries that use them.
            [f.path for f in static_runtime_libs] +
            deps.transitive.cc.link_flags.to_list()
        )
        lib_files = [input for oarg in object_args for input in oarg.inputs] + static_runtime_libs

    command = link_executable_command(
        cc_toolchain,
        is_linking_dynamic_library = False,
        is_static_linking_mode = True,
        user_link_flags = user_link_flags,
    )

    return struct(
        files = depset(lib_files, transitive = [deps.transitive.cc.additional_link_inputs]),
        flags = command.arguments,
    )

def _collect_files(ctx, dir_name, files):
    """Copies all of the input files into a single directory.

    Assumes that each file has a unique basename.

    Args:
      ctx: The current rule context.
      dir_name: The name of a directory in the current package.
      files: A list of Files.

    Returns:
      A list of Files, all within the given dir_name.
    """
    outputs = {ctx.actions.declare_file(dir_name + "/" + f.basename): f for f in files}
    dir_path = paths.join(ctx.bin_dir.path, ctx.label.package, dir_name)
    ctx.actions.run(
        outputs = outputs.keys(),
        inputs = files,
        progress_message = "Collecting files into {} for {}".format(dir_name, ctx.label),
        executable = "/bin/cp",
        arguments = [f.path for f in files] + [dir_path],
    )

    return outputs.keys()
