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

"""Emits information for tools like hrepl and ghcide to consume."""

load(
    "//bzl/private:cc.bzl",
    "get_explicit_dynamic_libraries",
)
load(
    "//bzl/private:proto_file.bzl",
    "write_proto_file",
)

def library_info_output_groups(ctx, package_id, transitive_haskell):
    """Output groups for using a target as a library.

    Args:
      ctx: The current rule context.
      package_id: The key for this library.
      transitive_haskell: The HaskellInfo.transitive.haskell provider output
        from this target, covering both it and its (recursive) dependencies.

    Returns:
      A dict mapping keys to depsets, for use in OutputGroupInfo.
    """
    library_info = write_proto_file(
        ctx,
        output_name = ctx.label.name + ".HaskellLibrary",
        proto_type = "haskell.LibraryInfo",
        content = struct(
            package_id = package_id,
            transitive_package_dbs =
                [cache.dirname for cache in transitive_haskell.caches.to_list()],
            transitive_package_ids = transitive_haskell.package_ids.to_list(),
            transitive_haskell_shared_libs =
                [lib.path for lib in transitive_haskell.shared_libs.to_list()],
        ),
    )

    return {
        "haskell_library_info": depset([library_info]),
        "haskell_transitive_deps": depset(
            transitive = [
                transitive_haskell.shared_libs,
                transitive_haskell.caches,
                transitive_haskell.hi_dirs,
            ],
        ),
    }

def compile_info_output_groups(
        ctx,
        metadata,
        deps = struct(),
        srcs_options = struct(),
        hidden_modules = [],
        runfiles = depset(),
        wrapper_lib = None):
    """Output groups for compiling a target.

    Args:
      ctx: The current rule context.
      metadata: Identifiers for this target.
      deps: A struct as returned by collect_deps.
      srcs_options: A struct as returned from compile_srcs_options.get().
      hidden_modules: A list of strings.
      runfiles: A depset of Files that may be used at run-time.
      wrapper_lib: An option File; a shared library wrapping the C++ dependencies
        of this target.

    Returns:
      A dict mapping keys to depsets, for use in OutputGroupInfo.
    """
    all_wrapper_libs = depset(
        [wrapper_lib] if wrapper_lib else [],
        transitive = [deps.transitive.haskell.cc_wrapper_libs],
    )
    info_file = write_proto_file(
        ctx,
        output_name = ctx.label.name + ".HaskellCompile",
        proto_type = "haskell.CompileInfo",
        content = struct(
            source_files = [f.path for f in srcs_options.srcs],
            hidden_modules = hidden_modules,
            package_name = metadata.name,
            options = srcs_options.arguments,
            runfiles = [
                struct(
                    full_path = f.path,
                    short_path = f.short_path,
                )
                for f in runfiles.to_list()
            ],
            transitive_cc_wrapper_libs = [lib.path for lib in all_wrapper_libs.to_list()],
            transitive_cc_shared_libs =
                [lib.path for lib in get_explicit_dynamic_libraries(ctx, deps.transitive.cc)],
        ),
    )
    return {
        "haskell_compile_info_pb": depset([info_file]),
        "haskell_source_files": srcs_options.inputs,
        "haskell_cdeps_shared_lib": all_wrapper_libs,
        "haskell_runfiles": runfiles,
    }
