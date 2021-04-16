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

"""Definitions of providers for Haskell rules, and how to construct/join them."""

load("@bazel_skylib//lib:collections.bzl", "collections")
load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("//bzl/private/builddata:builddata.bzl", "builddata")
load("//bzl/private:cc.bzl", "get_compile_flags")
load(
    "//bzl/private:default_packages.bzl",
    "default_package_names",
)

"""Helper functions for managing providers between Haskell Skylark rules.

Every haskell_library rule has a "haskell" provider which is a struct with
several Haskell-specific fields.  "Transitive" data (which is the union of all
recursive dependencies) is stored in the "haskell.transitive" subfield.
"""
HaskellInfo = provider(
    fields = [
        # GHC package name/ID/version for this rule.
        "metadata",
        # A dict, mapping module names to a struct containing two fields
        # corresponding to where the module was originally defined (which may be
        # this target itself):
        # - original_module
        # - original_package_key
        "exposed_modules",
        # Transitive providers for this and all (recursive)
        # dependencies.  A struct, whose fields are:
        #
        #   haskell: A struct for the Haskell code.
        #     package_ids: A depset of strings (package keys)
        #     hi_dirs: A depset of Files, directories containing .hi files
        #     caches: A depset of Files, each of which is a GHC package DB cache file
        #     static_libs: A depset of static archive Files ("libHS*.a")
        #     shared_libs: A depset of shared archive Files ("libHS*.so")
        #     cc_wrapper_libs: A depset of shared archive Files that "wrap" (i.e.,
        #       depend on) the C++ libraries (cc.libs).  Used in hrepl.
        #   haskell_checked: A struct for type-checking Haskell code without codegen.
        #     hi_dirs: A depset of Files, directories containing .hi files
        #     caches: A depset of Files, each of which is a GHC package DB cache file
        #   cc: A struct for the cc_library dependencies:
        #     libs: A depset of LibraryToLink objects.
        #     link_flags: The set of flags needed to link the cc_library dependencies.
        #       TODO(judahjacobson): We treat link_flags as a set in order to dedup them
        #       between transitive providers, but this might break linkopts that depend on the
        #       order within a single cc_library rule.  It's unclear whether this breaks
        #       anything in practice.
        #     additional_link_inputs: A depset of additional File inputs needed at
        #       link-time, such as linker scripts.
        "transitive",
        # A protocol buffer string, human-readably encoded.  See
        # haskell_library_compile_info for more details.
        "compile_info",
        # An empty file signifying that this target's module names correspond to their filepaths.
        # May be None if this target has no checked-in srcs.
        "module_names_ok",
    ],
)

def new_haskell_checked_transitive(
        transitive_deps = struct(),
        hi_dirs = [],
        cache = None):
    return struct(
        hi_dirs = depset(
            hi_dirs,
            transitive = [transitive_deps.hi_dirs],
        ) if hi_dirs else transitive_deps.hi_dirs,
        caches = depset(
            [cache],
            transitive = [transitive_deps.caches],
        ) if cache else transitive_deps.caches,
    )

def new_haskell_transitive(
        transitive_deps = struct(),
        package_id = None,
        hi_dirs = [],
        cache = None,
        static_libs = [],
        shared_libs = [],
        wrapper_lib = None):
    """Make a struct suitable for the "HaskellInfo.transitive.haskell" provider.

    Args:
      transitive_deps: A struct of transitive fields (as specified above), from
        the dependencies of this rule.
      package_id: A string; the key for this library.
      hi_dirs: (Optional): Directories containing this library's .hi files.
      cache: A GHC package database cache file for this library.
      static_libs: A list of static archives for this library.
      shared_libs: A list of shared archives for this library.
      wrapper_lib: (Optional) A shared archive wrapping the C++ deps of this library.

    Returns:
      A struct of transitive fields.
    """
    return struct(
        package_ids = depset([package_id], transitive = [transitive_deps.package_ids]),
        hi_dirs = depset(
            hi_dirs,
            transitive = [transitive_deps.hi_dirs],
        ) if hi_dirs else transitive_deps.hi_dirs,
        caches = depset(
            [cache],
            transitive = [transitive_deps.caches],
        ) if cache else transitive_deps.caches,
        static_libs = depset(static_libs, transitive = [transitive_deps.static_libs]),
        shared_libs = depset(shared_libs, transitive = [transitive_deps.shared_libs]),
        cc_wrapper_libs = depset(
            [wrapper_lib],
            transitive = [transitive_deps.cc_wrapper_libs],
        ) if wrapper_lib else transitive_deps.cc_wrapper_libs,
    )

def empty_transitive(package_id):
    """Returns an empty transitive provider."""
    return struct(
        haskell = struct(
            package_ids = depset([package_id]),
            hi_dirs = depset(),
            caches = depset(),
            static_libs = depset(),
            shared_libs = depset(),
            cc_wrapper_libs = depset(),
        ),
        haskell_checked = struct(
            hi_dirs = depset(),
            caches = depset(),
        ),
        cc = struct(
            libs = depset(),
            link_flags = depset(),
            additional_link_inputs = depset(),
        ),
    )

def add_cc_lib(transitive_deps = struct(), cc_dep = struct()):
    """Make a new "HaskellInfo.transitive.cc" struct by adding a cc_library target.

    Args:
      transitive_deps: A struct of transitive fields, as in HaskellInfo.transitive.cc
      cc_dep: A cc_library dependency.

    Returns:
      A new transitive fields struct which includes the given cc_library.
    """
    ccinfo = cc_dep[CcInfo]
    return struct(
        libs = depset(ccinfo.linking_context.libraries_to_link, transitive = [transitive_deps.libs]),
        link_flags = depset(ccinfo.linking_context.user_link_flags, transitive = [transitive_deps.link_flags]),
        additional_link_inputs = depset(transitive = [
            ccinfo.linking_context.additional_inputs,
            transitive_deps.additional_link_inputs,
        ]),
    )

def collect_deps(dependencies):
    """Group and combine the given Haskell and C/C++ dependencies.

    Args:
      dependencies: A struct as returned from get_dependencies.

    Returns:
      A struct consisting of the following fields:
        exposed_package_ids: A list of distinct GHC package IDs for haskell_library
          rules that may be used to compile this target.
        exposed_modules: A list of structs, as in Haskell.exposed_modules.
        plugin_package_ids: A list of distinct GHC package IDs for haskell_library
          rules that may be used as compiler plugins for this target.
        plugin_shared_libs: Shared libraries for each plugin and its
          transitive dependencies.
        transitive: The union of the transitive providers for all Haskell
          and C++ dependencies, as the same format in HaskellInfo.transitive.
        plugins: A struct containing the fields:
          - package_ids: A list of distinct GHC package IDs for haskell_library
            rules that may be used as compiler plugins for this target.
          - shared_libs: Shared libraries for each plugin and its trasitive dependencies.
          - caches: A depset of files, package DBs for the plugins and their
            dependencies.
          - hi_dirs: A depset of directories containing .hi files for the
            plugins and their dependencies.
        cc_headers: The set of C/C++ headers from the immediate cc_library
          dependencies (as well as their own transitive dependencies).  They may
          be needed by CPP.
        cpp_flags: Flags for running CPP in order to use the cc_headers when
          building the current rule.
        immediate_cc_libs: A list of LibraryToLink objects of cc_library
          rules that this target depends directly on.
        immediate_cc_dylibs: A list of Files of ".so" files for the cc_library
          rules that this target depends directly on.
        immediate_module_names_ok: A list of (empty) Files signifying that the
          immediate dependencies have module names consistent with filepaths.
    """

    # The "deps" attribute enforces that labels have either HaskellInfo or CcInfo.
    haskell_deps = [
        dep[HaskellInfo]
        for dep in (dependencies.deps + dependencies.default_deps)
        if HaskellInfo in dep
    ]
    haskell_explicit_deps = [
        dep[HaskellInfo]
        for dep in dependencies.deps
        if HaskellInfo in dep
    ]
    haskell_plugins = [p[HaskellInfo] for p in dependencies.plugins]
    all_haskell = haskell_deps + haskell_plugins
    cc_deps = [
        dep[CcInfo]
        for dep in dependencies.deps
        # Exclude Haskell deps, which also provide CcInfo.
        if CcInfo in dep and HaskellInfo not in dep
    ]

    immediate_cc_libs = [lib for dep in cc_deps for lib in dep.linking_context.libraries_to_link]

    # Note: it's also hard to tell from here whether a library is "nontrivial";
    # for example, even if there are no "srcs" (e.g. only headers, or a forwarding rule),
    # Bazel will still create lib.static_library.  As a proxy for "nontrivial",
    # look for `lib.dynamic_library != None`.
    immediate_cc_dylibs = [
        lib.dynamic_library
        for lib in immediate_cc_libs
        if lib.dynamic_library
    ]
    if immediate_cc_dylibs:
        immediate_cc_libs += dependencies.faked_builddata_libs
        immediate_cc_dylibs += [
            lib.dynamic_library
            for lib in dependencies.faked_builddata_libs
            if lib.dynamic_library
        ]

    # Note: plugins, like deps, need to be linked into the final binary.  As a
    # result, we combine both into the transitive struct.
    return struct(
        exposed_package_ids = [dep.metadata.key for dep in haskell_deps],
        # Don't reexport modules from default packages unless they're explicitly
        # specified as deps.
        exposed_modules = [dep.exposed_modules for dep in haskell_explicit_deps],
        transitive = struct(
            haskell = struct(
                package_ids = depset(
                    transitive = [dep.transitive.haskell.package_ids for dep in all_haskell],
                ),
                hi_dirs = depset(
                    transitive = [dep.transitive.haskell.hi_dirs for dep in all_haskell],
                ),
                caches = depset(
                    transitive = [dep.transitive.haskell.caches for dep in all_haskell],
                ),
                static_libs = depset(
                    transitive = [dep.transitive.haskell.static_libs for dep in all_haskell],
                ),
                shared_libs = depset(
                    transitive = [dep.transitive.haskell.shared_libs for dep in all_haskell],
                ),
                cc_wrapper_libs = depset(
                    transitive = [dep.transitive.haskell.cc_wrapper_libs for dep in all_haskell],
                ),
            ),
            haskell_checked = struct(
                hi_dirs = depset(
                    transitive = [dep.transitive.haskell_checked.hi_dirs for dep in all_haskell],
                ),
                caches = depset(
                    transitive = [dep.transitive.haskell_checked.caches for dep in all_haskell],
                ),
            ),
            cc = struct(
                libs = depset(
                    immediate_cc_libs,
                    transitive = [dep.transitive.cc.libs for dep in all_haskell],
                ),
                link_flags = depset(
                    [flag for dep in cc_deps for flag in dep.linking_context.user_link_flags],
                ),
                additional_link_inputs = depset(
                    transitive = [dep.linking_context.additional_inputs for dep in cc_deps] +
                                 [dep.transitive.cc.additional_link_inputs for dep in all_haskell],
                ),
            ),
        ),
        plugins = struct(
            package_ids = collections.uniq([p.metadata.key for p in haskell_plugins]),
            shared_libs = depset(
                transitive = [dep.transitive.haskell.shared_libs for dep in haskell_plugins],
            ),
            caches = depset(
                transitive = [dep.transitive.haskell.caches for dep in haskell_plugins],
            ),
            hi_dirs = depset(
                transitive = [dep.transitive.haskell.hi_dirs for dep in haskell_plugins],
            ),
        ),
        cc_headers = depset(transitive = [dep.compilation_context.headers for dep in cc_deps]),
        cpp_flags = [flag for dep in cc_deps for flag in get_compile_flags(dep)],
        immediate_cc_libs = collections.uniq(immediate_cc_libs),
        only_transitive_cc_libs = depset(transitive = [dep.transitive.cc.libs for dep in all_haskell]),
        immediate_cc_dylibs = collections.uniq(immediate_cc_dylibs),
        immediate_module_names_ok =
            [dep.module_names_ok for dep in all_haskell if dep.module_names_ok],
    )

def fix_aspect_providers(lib_providers):
    """Change a list of providers to be returnable from an aspect.

    Args:
      lib_providers: A list of providers, e.g., as returned from forward_deps().

    Returns:
      A list of providers which is valid to return from an aspect.
    """

    # The DefaultInfo provider can't be returned from aspects.
    return [p for p in lib_providers if not hasattr(p, "files")]

dependency_attrs = dicts.add(builddata.compile_attrs, {
    "deps": attr.label_list(
        providers = [[HaskellInfo], [CcInfo]],
        doc = """
Other libraries that this target depends on.  The list
may contain <code>haskell_library</code>, <code>cc_library</code> or
<code>haskell_proto_library</code> targets.  The transitive
<code>deps</code> will all be linked into the final binary target.
<br><br>
Any modules imported by the <code>srcs</code> of a target must be defined in
one of the direct <code>deps</code> of the target itself.
(This behavior is roughly equivalent to the layering_check for C++ rules.)
""",
    ),
    "plugins": attr.label_list(
        providers = [HaskellInfo],
        doc = """
Other <code>haskell_library</code> rules that this target can use
as compiler plugins.  That is, it specifies the flag
<code>-fplugin=MODULE</code> via either the <code>ghcopts</code>
attribute or an <code>OPTIONS_GHC</code> language pragma.
<br><br>
The <code>plugins</code> and their own
transitive <code>deps</code> will be linked into the final binary target,
similar to the <code>deps</code> attribute.  The rules behave this way
because plugins may splice definitions from themselves into the compilation
result.
""",
    ),
})

default_packages_attrs = {
    "default_packages": attr.label_list(
        providers = [HaskellInfo],
        default = [
            Label("//third_party/haskell/ghc:" + p)
            for p in default_package_names
        ],
        doc = """
Libraries that our build rules depend on by default.

To disable this feature (usually: only for third_party code),
set `default_packages = []`.

TODO(b/120225063): Reduce or remove this list.
""",
    ),
}

def get_dependencies(ctx):
    """Collects dependencies declared for the given rule.

    Includes both explicit dependencies (e.g., "deps") and
    implicit dependencies (e.g., default packages).

    Args:
      ctx: The current rule context.

    Returns:
      A struct with the fields:
     - deps: A list of targets declared as "deps" of this rule.
     - plugins: A list of targets declared as "plugins" of this rule.
     - default_deps: A list of targets which are implicit "default"
       dependencies of all Haskell rules.
     - faked_builddata_libs: A list of LibraryToLink providers, used to help link
       against builddata libraries with using the "builddata_globals_faked"
       cc_library.
    """
    return struct(
        deps = ctx.attr.deps,
        plugins = getattr(ctx.attr, "plugins", []),
        default_deps = getattr(ctx.attr, "default_packages", []),
        faked_builddata_libs = builddata.get_faked_globals_libs(ctx),
    )

def simple_dependencies(deps):
    """Constructs a dependencies struct from a list of targets."""
    return struct(
        deps = deps,
        plugins = [],
        default_deps = [],
        faked_global_libs = [],
    )
