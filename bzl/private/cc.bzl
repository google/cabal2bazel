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

"""Functions for backwards-compatible handling of flags for C++ dependencies."""

load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "CPP_LINK_DYNAMIC_LIBRARY_ACTION_NAME",
    "CPP_LINK_EXECUTABLE_ACTION_NAME",
    "CPP_LINK_STATIC_LIBRARY_ACTION_NAME",
    "C_COMPILE_ACTION_NAME",
)

def get_compile_flags(ccinfo):
    """Builds compilation flags. This replaces the old API dep.cc.compile_flags

    This is not the command line that C++ rules will use. For that the toolchain API should be
    used (feature configuration and variables).

    Args:
      ccinfo: A CcInfo instance.

    Returns:
      A list of strings
    """
    options = []
    compilation_context = ccinfo.compilation_context
    for define in compilation_context.defines.to_list():
        options.append("-D{}".format(define))

    for system_include in compilation_context.system_includes.to_list():
        if len(system_include) == 0:
            system_include = "."
        options.append("-isystem{}".format(system_include))

    for include in compilation_context.includes.to_list():
        if len(include) == 0:
            include = "."
        options.append("-I{}".format(include))

    for quote_include in compilation_context.quote_includes.to_list():
        if len(quote_include) == 0:
            quote_include = "."
        options.append("-iquote{}".format(quote_include))

    return options

_UNSUPPORTED_FEATURES = [
    # TODO
]

def get_configured_cc_toolchain(ctx, unsupported_features = []):
    """Gets the toolchain for compiling C/C++.

    Args:
      ctx: The current rule context

    Returns:
      A struct containing two fields:
        toolchain: A CcToolchainProvider instancel
        features: A FeatureConfiguration instance.
    """
    cc_toolchain = find_cpp_toolchain(ctx)
    return struct(
        toolchain = cc_toolchain,
        feature_configuration = cc_common.configure_features(
            ctx = ctx,
            cc_toolchain = cc_toolchain,
            requested_features = ctx.features + ["static_linking_mode"],
            unsupported_features = ctx.disabled_features + _UNSUPPORTED_FEATURES +
                                   unsupported_features,
        ),
    )

def compile_command(
        toolchain,
        user_compile_flags = [],
        preprocessor_defines = depset(),
        source_file = None,
        output_file = None):
    """Get the command-line executable/arguments for compiling C source files."""
    variables = cc_common.create_compile_variables(
        feature_configuration = toolchain.feature_configuration,
        cc_toolchain = toolchain.toolchain,
        user_compile_flags = user_compile_flags,
        preprocessor_defines = preprocessor_defines,
        source_file = source_file,
        output_file = output_file,
    )

    return struct(
        executable = cc_common.get_tool_for_action(
            feature_configuration = toolchain.feature_configuration,
            action_name = C_COMPILE_ACTION_NAME,
        ),
        arguments = cc_common.get_memory_inefficient_command_line(
            feature_configuration = toolchain.feature_configuration,
            action_name = C_COMPILE_ACTION_NAME,
            variables = variables,
        ),
    )

def link_executable_command(
        toolchain,
        is_linking_dynamic_library = False,
        is_static_linking_mode = False,
        user_link_flags = [],
        output_file = None):
    """Get the command-line executable/arguments for linking object files."""
    variables = cc_common.create_link_variables(
        cc_toolchain = toolchain.toolchain,
        feature_configuration = toolchain.feature_configuration,
        is_linking_dynamic_library = is_linking_dynamic_library,
        is_static_linking_mode = is_static_linking_mode,
        user_link_flags = user_link_flags,
        output_file = output_file,
    )

    action_name = CPP_LINK_DYNAMIC_LIBRARY_ACTION_NAME if is_linking_dynamic_library else CPP_LINK_EXECUTABLE_ACTION_NAME
    return struct(
        executable = cc_common.get_tool_for_action(
            feature_configuration = toolchain.feature_configuration,
            action_name = action_name,
        ),
        arguments = cc_common.get_memory_inefficient_command_line(
            feature_configuration = toolchain.feature_configuration,
            action_name = action_name,
            variables = variables,
        ),
    )

def get_ar_executable(toolchain):
    return cc_common.get_tool_for_action(
        feature_configuration = toolchain.feature_configuration,
        action_name = CPP_LINK_STATIC_LIBRARY_ACTION_NAME,
    )

cc_toolchain_attrs = {
    # Do not add references, temporary attribute for find_cpp_toolchain.
    # See go/skylark-api-for-cc-toolchain for more details.
    "_cc_toolchain": attr.label(
        default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
    ),
}

def get_explicit_dynamic_libraries(ctx, cc_deps):
    """The explicit shared C library dependencies for this target.

    These libraries must be linked explicitly with "-l".

    Args:
      ctx: The current rule context.
      cc_deps: A struct of all C++ dependencies for this target,
        as stored in HaskellInfo.transitive.cc.

    Returns:
      A list of Files.
    """
    return [
        lib.dynamic_library
        for lib in cc_deps.libs.to_list()
        # "Forwarding" C libraries without any srcs of their own
        # don't specify a dynamic_library.  We can just ignore them
        # since they don't add any new symbols.
        if lib.dynamic_library
    ]

def get_dynamic_libraries(ctx, cc_deps):
    """Returns a list of Files of shared C library dependencies.

    Includes "toolchain" dependencies that are linked implicitly
    (e.g. libstdc++).

    Args:
      ctx: The current rule context.
      cc_deps: A struct of all C++ dependencies for this target,
        as stored in HaskellInfo.transitive.cc.

    Returns:
      A list of Files.
    """
    return (get_explicit_dynamic_libraries(ctx, cc_deps) +
            get_dynamic_runtime_libs(ctx))

def get_static_runtime_libs(ctx):
    cc_toolchain = get_configured_cc_toolchain(ctx)
    return cc_toolchain.toolchain.static_runtime_lib(
        feature_configuration = cc_toolchain.feature_configuration,
    ).to_list()

def get_dynamic_runtime_libs(ctx):
    cc_toolchain = get_configured_cc_toolchain(ctx)
    return cc_toolchain.toolchain.dynamic_runtime_lib(
        feature_configuration = cc_toolchain.feature_configuration,
    ).to_list()

def shared_library_name(f):
    """Returns the name of a shared library, suitable as an argument to "-l".

    Args:
      f: File object for the shared library.

    Returns:
      The name of the shared library suitable as an argument to "-l". May
      return None if this library should be ignored.
    """
    if f.basename[:3] != "lib":
        fail("Unrecognized basename for file " + str(f))

    ext = f.extension
    if ext == "so":
        return f.basename[3:-3]

    # Some shared libraries are named libgcc_s.so.1. Ignore them.
    name = f.basename[:-(len(ext) + 1)]
    if name[-3:] == ".so":
        return None

    fail("Unrecognized basename for file " + str(f))

def get_object_linking_args(lib):
    """Collects the objects for a given library.

    Args:
      lib: A LibraryToLink object.

    Returns:
      A struct containing two fields:
      - inputs: A list of Files (*.o).
      - arguments: A list of command-line arguments to pass to the link action.
    """

    # Prefer the PIC library to help link the shared library: b/28739425
    objects = lib.pic_objects or lib.objects
    if not objects:
        # If there's a static library (e.g. a cc_library wrapping
        # a static library checked into the build), use that.
        # Note: This also uses a static library for libraries
        # without sources (e.g.: header-only).  But it's ok because
        # that library has trivial contents.
        static_lib = lib.pic_static_library or lib.static_library
        if static_lib:
            return struct(
                inputs = [static_lib],
                arguments = [static_lib.path],
            )
        else:
            return struct(inputs = [], arguments = [])
    if lib.alwayslink:
        start_flag = "-Wl,-whole-archive"
        end_flag = "-Wl,-no-whole-archive"
    else:
        start_flag = "-Wl,--start-lib"
        end_flag = "-Wl,--end-lib"
    return struct(
        inputs = objects,
        arguments = [start_flag] + [o.path for o in objects] + [end_flag],
    )

def cpp_options(deps):
    """Command-line flags from the immediate C++ deps."""
    return ["-optP" + flag for flag in deps.cpp_flags]
