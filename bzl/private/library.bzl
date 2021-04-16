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

"""Skylark rules for building Haskell code.

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
load("@bazel_skylib//rules:common_settings.bzl", "BuildSettingInfo")
load(
    "//bzl/private:archive.bzl",
    "create_shared_haskell_lib",
    "create_static_lib_archive",
)
load(
    "//bzl/private:cc.bzl",
    "cpp_options",
)
load(
    "//bzl/private:module_names.bzl",
    "check_module_names",
    "check_module_names_attrs",
    "collect_exposed_modules",
    "immediate_exports",
    "render_reexport",
)
load("//bzl/private:compile_srcs_options.bzl", "compile_srcs_options")
load(
    "//bzl/private:compile.bzl",
    "compile_sources",
)
load(
    "//bzl/private:info.bzl",
    "compile_info_output_groups",
    "library_info_output_groups",
)
load("//bzl/private:package.bzl", "package")
load(
    "//bzl/private:proto_file.bzl",
    "generate_proto_files",
    "proto_attrs",
)
load(
    "//bzl/private:providers.bzl",
    "HaskellInfo",
    "collect_deps",
    "default_packages_attrs",
    "dependency_attrs",
    "empty_transitive",
    "fix_aspect_providers",
    "get_dependencies",
    "new_haskell_checked_transitive",
    "new_haskell_transitive",
    "simple_dependencies",
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

_compilation_attrs = dicts.add(
    toolchains_attrs,
    proto_attrs,
    settings.attributes,
)

def _write_package_spec(
        ctx,
        name,
        metadata,
        hi_dirs,
        archive_name,
        collected_deps,
        hidden_modules,
        exported_modules):
    """Creates a .spec file for registering a particular library.

    Args:
      ctx: The current rule context
      name: The basename of the output file.
      metadata: The identifiers for the current rule.
      hi_dirs: A list of directories containing .hi files for this target.
      archive_name: A string; the basename of the archive, minus the extension and initial "lib".
      collected_deps: A struct of rules this library depends on
      hidden_modules: A list of strings.
      exported_modules: A list of strings.

    Returns:
      A File.
    """

    package_spec = ctx.actions.declare_file(name)

    # Write the GHC package spec for this library.
    ctx.actions.write(
        package_spec,
        "\n".join(
            [
                "name: " + metadata.name,
                "id: " + metadata.key,
                "version: " + metadata.version,
                "key: " + metadata.key,
            ] + ["import-dirs: " + ",".join(
                ["${pkgroot}/" + d.basename for d in hi_dirs],
            )] +
            ([] if not archive_name else [
                "library-dirs: ${pkgroot}/" + package.libdir(ctx),
                "hs-libraries: " + archive_name,
            ]) +
            [
                # Make it possible to load any dependency into hrepl.
                # We'll rely on -hide-all-deps and -package-id to control
                # compile-time visibility.
                # (:set -package isn't a good option for users because it makes hrepl
                # unload the current context.)
                "exposed: True",
                "depends: " + ", ".join(collected_deps.exposed_package_ids +
                                        collected_deps.plugins.package_ids),
                "hidden-modules: " + ",".join(hidden_modules),
                "exposed-modules: " + ",".join(
                    [render_reexport(m, r) for m, r in exported_modules.items()],
                ),
            ],
        ),
    )
    return package_spec

def _create_ccinfo(
        ctx,
        toolchains,
        collected_deps,
        cc_deps_lib,
        shared_lib_archives,
        foreign_exports,
        stub_headers):
    """Creates a CcInfo provider for cc_libraries that depend on this one.

    Args:
      ctx: The current rule context.
      toolchains: A struct as returned by get_toolchains().
      cc_deps_lib: A LibraryToLink returned by create_shared_haskell_lib().
      collected_deps: A struct of dependencies of this rule.
      shared_lib_archives: A list of Files (.so)
      foreign_exports: A list of strings; module names with a "foreign export"
      stub_headers: A File, directory containing *_stub.h files generated by GHC.

    Returns:
      A CcInfo provider.
    """

    # TODO(b/144593939): Prune the toolchain shared libraries, if possible.
    all_shared_libs = depset(
        direct = shared_lib_archives,
        transitive = [
            collected_deps.transitive.haskell.shared_libs,
        ],
    )
    libs_to_link = all_shared_libs.to_list() + toolchains.haskell.shared_libraries

    # TODO(b/144594540):  This is inefficient if multiple libraries depend on
    # each other and all have foreign_exports.
    libraries_to_link = [
        cc_common.create_library_to_link(
            actions = ctx.actions,
            feature_configuration = toolchains.cc.feature_configuration,
            cc_toolchain = toolchains.cc.toolchain,
            dynamic_library = slib,
        )
        for slib in libs_to_link
    ]
    if cc_deps_lib != None:
        libraries_to_link.append(cc_deps_lib)
        libraries_to_link += collected_deps.only_transitive_cc_libs.to_list()
    else:
        libraries_to_link += collected_deps.transitive.cc.libs.to_list()
    linking_context = cc_common.create_linking_context(
        libraries_to_link = libraries_to_link,
    )

    (compilation_context, compilation_outputs) = cc_common.compile(
        name = "{}.ccinfo".format(ctx.label.name),
        actions = ctx.actions,
        feature_configuration = toolchains.cc.feature_configuration,
        cc_toolchain = toolchains.cc.toolchain,
        public_hdrs = stub_headers,
        grep_includes = ctx.file._grep_includes,
    )

    return CcInfo(
        compilation_context = compilation_context,
        linking_context = linking_context,
    )

def _haskell_library_impl(
        ctx,
        dependencies = struct(),
        hidden_modules = [],
        reexported_modules = [],
        srcs_options = struct(),
        bundle_cc_deps = False):
    """Common code for building a Haskell library.

    Args:
      ctx: The current rule context.
      dependencies: A struct as returned by get_dependencies().
      hidden_modules: (Optional) A list of modules that should not be exposed
        by this library.
      reexported_modules: (Optional) A list of modules reexports of the form
        "Foo as Bar".
      srcs_options: A struct of attributes that affect the compilation of Haskell
        source files.

    Returns:
      See the documentation of _impl_library().
    """
    toolchains = get_toolchains(ctx)
    metadata = package.metadata(ctx, srcs = srcs_options.srcs, deps = dependencies.deps)
    collected_deps = collect_deps(dependencies)
    srcs_options = compile_srcs_options.add_args(
        srcs_options,
        cpp_options(collected_deps),
    )

    if not srcs_options.srcs:
        lib = None
    else:
        lib = compile_sources(
            ctx = ctx,
            toolchains = toolchains,
            metadata = metadata,
            deps = collected_deps,
            srcs_options = srcs_options,
            hidden_modules = hidden_modules,
        )

    exposed_modules = collect_exposed_modules(
        ctx,
        toolchains.haskell,
        metadata,
        collected_deps.exposed_modules,
        hidden_modules,
        reexported_modules,
        srcs = srcs_options.srcs,
    )

    exposed_modules_list = ctx.actions.declare_file(ctx.label.name + ".exposed-modules")
    ctx.actions.write(exposed_modules_list, "\n".join(
        [render_reexport(m, r) for m, r in exposed_modules.items()],
    ))

    if lib:
        # The name of the library archive (e.g. "HSfoo"), unique within the build.
        # GHC uses "-l" to link the Haskell libs, which would otherwise be
        # two haskell_library targets with the same name in different directories.
        archive_name = "HS" + metadata.key if lib else None

        all_hi_dirs = [lib.hi]
        if lib.prof:
            all_hi_dirs.append(lib.prof.hi)
    else:
        archive_name = None
        all_hi_dirs = []

    package_cache = package.register(
        ctx,
        toolchains.haskell,
        ctx.label.name + ".db",
        _write_package_spec(
            ctx,
            ctx.label.name + ".spec",
            metadata,
            all_hi_dirs,
            archive_name,
            collected_deps,
            hidden_modules,
            exposed_modules,
        ),
    )

    checked_package_cache = package.register(
        ctx,
        toolchains.haskell,
        ctx.label.name + ".checked.db",
        _write_package_spec(
            ctx,
            ctx.label.name + ".checked.spec",
            metadata,
            [lib.checked_hi] if lib else [],
            None,
            collected_deps,
            hidden_modules,
            exposed_modules,
        ),
    )

    # If profiling is enabled, we must create both profiling and non-profiling
    # archives. See comment on _profiling_enabled for reasoning.
    static_lib_archives = []
    shared_lib_archives = []
    hi_dirs = []
    if lib:
        hi_dirs += [lib.hi]
        static_lib_archives += [
            create_static_lib_archive(ctx, toolchains.cc, archive_name, lib.object, "o"),
        ]
        shared_lib, cc_deps_lib = create_shared_haskell_lib(
            ctx,
            toolchains,
            archive_name,
            lib,
            collected_deps,
            bundle_cc_deps = bundle_cc_deps,
        )
        shared_lib_archives += [shared_lib]
        if lib.prof:
            hi_dirs += [lib.prof.hi]
            static_lib_archives += [create_static_lib_archive(ctx, toolchains.cc, archive_name + "_p", lib.prof.object, "p_o")]

    # Don't check module names of:
    # - rules without srcs
    # - third-party cabal_haskell_package targets; they can cause issues more frequently
    #   due to CPP, and module names are already enforced upstream by Cabal.
    if srcs_options.srcs and not metadata.version:
        module_names_ok = check_module_names(
            ctx,
            toolchains.haskell,
            srcs_options,
        )
    else:
        module_names_ok = None

    if hasattr(ctx.files, "data"):
        runfiles = collect_runfiles(
            ctx = ctx,
            files = [lib.mix] if lib and lib.mix else [],
            data = ctx.attr.data,
            deps = dependencies.deps,
        )
    else:
        # haskell_proto_{library/attribute}
        runfiles = ctx.runfiles()

    if lib and bundle_cc_deps:
        # Insert the cc_deps_lib into output's cc dependencies
        cc_output = struct(
            libs = depset(
                direct = [cc_deps_lib],
                transitive = [collected_deps.only_transitive_cc_libs],
            ),
            link_flags = collected_deps.transitive.cc.link_flags,
            additional_link_inputs = collected_deps.transitive.cc.additional_link_inputs,
        )
    else:
        cc_output = collected_deps.transitive.cc

    transitive_output = struct(
        haskell = new_haskell_transitive(
            package_id = metadata.key,
            transitive_deps = collected_deps.transitive.haskell,
            cache = package_cache,
            hi_dirs = hi_dirs,
            static_libs = static_lib_archives,
            shared_libs = shared_lib_archives,
            wrapper_lib = lib.wrapper_lib if lib else None,
        ),
        haskell_checked = new_haskell_checked_transitive(
            transitive_deps = collected_deps.transitive.haskell_checked,
            cache = checked_package_cache,
            hi_dirs = [lib.checked_hi] if lib else [],
        ),
        cc = cc_output,
    )

    default_info = DefaultInfo(
        # If the library has no srcs, put the exposed_modules_list in its output group
        # so that its dependencies still get built.
        files = depset(([lib.hi, lib.object, exposed_modules_list] if lib else [exposed_modules_list]) +
                       ([module_names_ok] if module_names_ok else [])),
        runfiles = runfiles,
    )
    output_group_info = OutputGroupInfo(
        **dicts.add(
            {
                "haskell_check": depset([lib.checked_hi if lib else exposed_modules_list]),
                "haskell_exposed_modules": depset([exposed_modules_list]),
                # Forward the immediate (non-transitive) to make the UX nicer for
                # building haskell_proto_library via the --aspects flag.  (It's a
                # subset of haskell_transitive_deps, but the transitive data would
                # clutter up the console.)
                "haskell_library_files": depset([package_cache] + hi_dirs + shared_lib_archives),
            },
            library_info_output_groups(ctx, metadata.key, transitive_output.haskell),
            compile_info_output_groups(
                ctx = ctx,
                metadata = metadata,
                deps = collected_deps,
                srcs_options = srcs_options,
                hidden_modules = hidden_modules,
                runfiles = runfiles.files,
                wrapper_lib = lib.wrapper_lib if lib else None,
            ),
        )
    )
    haskell_info = HaskellInfo(
        metadata = metadata,
        module_names_ok = module_names_ok,
        exposed_modules = exposed_modules,
        transitive = transitive_output,
    )
    cc_info = [
        _create_ccinfo(
            ctx,
            toolchains,
            collected_deps,
            cc_deps_lib,
            shared_lib_archives,
            ctx.attr.foreign_exports,
            lib.stub_headers,
        ),
    ] if lib and hasattr(ctx.attr, "foreign_exports") and ctx.attr.foreign_exports else []
    instrumented_files_info = [
        coverage_common.instrumented_files_info(
            ctx,
            extensions = ["hs"],
            source_attributes = ["srcs"],
            dependency_attributes = ["deps"],
        ),
    ] if ctx.configuration.coverage_enabled else []

    return (
        [
            default_info,
            output_group_info,
            haskell_info,
        ] +
        cc_info +
        instrumented_files_info
    )

def _impl_library(ctx):
    """Implementation of the haskell_library rule.

    Every library outputs the following files:
      - {name}.db/package.cache: A GHC package database containing (only) this
        library
      - {name}.spec: A human-readable version of package.cache, for debugging
      - {name}.modules: A comma-separated list of Haskell module names contained
        by this library
      - {name}.interfaces.tar: An archive containing the GHC interface files for
        the modules in this library
      - {name}.objects.tar: An archive containing the object files for the
        modules in this library
      - libHS{mangled name}.a: Library containing all the modules' object code

    If this library has no source files, then it just re-exposes all the
    immediate dependencies.

    Args:
      ctx: The current rule context.

    Returns:
      A struct consisting of the default provider and legacy data.

    """

    return _haskell_library_impl(
        ctx,
        dependencies = get_dependencies(ctx),
        hidden_modules = ctx.attr.hidden_modules,
        reexported_modules = ctx.attr.reexported_modules,
        srcs_options = compile_srcs_options.get(ctx),
        bundle_cc_deps = ctx.attr._bundle_cc_deps[BuildSettingInfo].value,
    )

def _with_bundled_cc_deps_impl(settings, attr):
    _ignore = (settings, attr)
    return {"//tools/build_defs/haskell/private:bundle_cc_deps": True}

with_bundled_cc_deps = transition(
    implementation = _with_bundled_cc_deps_impl,
    inputs = [],
    outputs = ["//tools/build_defs/haskell/private:bundle_cc_deps"],
)

def _haskell_static_foreign_library_impl(ctx):
    return [ctx.attr.library[0][CcInfo]]

haskell_static_foreign_library = rule(
    implementation = _haskell_static_foreign_library_impl,
    attrs = {
        "library": attr.label(cfg = with_bundled_cc_deps),
        "_allowlist_function_transition": attr.label(
            default = "//tools/allowlists/function_transition_allowlist",
        ),
    },
)

# Attributes that should be used by libraries, but *not* by binaries.
haskell_library = rule(
    attrs = dicts.add(
        _compilation_attrs,
        check_module_names_attrs,
        compile_srcs_options.attrs,
        dependency_attrs,
        default_packages_attrs,
        runfile_attrs,
        {
            "hidden_modules": attr.string_list(
                doc = """
A list of module names that should be considered "internal" to this library
and not exposed to targets that depend on it.

This attribute is only relevant to third-party packages and should not be set
manually; it is used internally in cabal_haskell_package.
""",
            ),
            "reexported_modules": attr.string_list(
                doc = """
A list of module name reexports.  Each one is of the form "Foo as Bar",
where "Foo" and "Bar" are valid Haskell module names.  This particular example
means that "Foo" is a module exposed by an immediate dependency, which this
rule is exposing as "Bar".  It is fine for the module to be reexposed
with the same name as in the dependency.

Reexported modules must be from packages listed explicitly in "deps".
To reexport a module from an implicit, default package like "base",
add it explicitly to the "deps".
""",
            ),
            "foreign_exports": attr.string_list(
                default = [],
                doc = """
Libraries that contain foreign exports should set this to expose the
generated .h stubs to C++ code.  It should include every Haskell module
with a <code>foreign export</code>.
<br><br>
Ex: <code>["MyModule", "Some.Other.Module"]</code>
""",
            ),
            "cabal_name": attr.string(
                doc = """
The name of this library as specified by Cabal.  Maybe empty.
<br><br>
This attribute is only relevant to third-party packages and should not be set
manually.  It is used internally in cabal_haskell_package to generate
<code>MIN_VERSION_*</code> macros.
""",
            ),
            "cabal_version": attr.string(
                doc = """
The version number of this library.  May be empty.
<br><br>
This attribute is only relevant to third-party packages and should not be set
manually.  It is used internally in cabal_haskell_package to generate
<code>MIN_VERSION_*</code> macros.
""",
            ),
            "_bundle_cc_deps": attr.label(default = "//tools/build_defs/haskell/private:bundle_cc_deps"),
        },
    ),
    fragments = ["cpp"],
    implementation = _impl_library,
    doc = """
This rule compiles a single Haskell library containing one or more source
files.

Example usage:

    haskell_library(
        name = "mylibrary"
        srcs = ["Foo.hs", "Bar.hs"]
    )

The Haskell package name for the library is derived from the target path
relative to the root (with various substitutions made in order to turn the
target path into a valid Haskell package name). This fact should be of no
interest unless the package is used for building things outside the
BUILD framework.
""",
)

def _impl_proto_aspect(target, ctx):
    """Implementation of the haskell_proto_aspect aspect for proto-lens.

    Args:
      target: The current target.
      ctx: The current rule context.

    Returns:
      A compiled haskell_library target (see _impl_library for more details).
    """

    # If this proto_library rule has no "srcs", treat it as just
    # forwarding its immediate dependencies.
    if not target[ProtoInfo].direct_sources:
        res = fix_aspect_providers(_haskell_library_impl(
            ctx,
            dependencies = simple_dependencies(ctx.rule.attr.deps),
            srcs_options = compile_srcs_options.simple(),
        ))
        return res

    # TODO(judahjacobson): For haskell_deps=strict, this currently forwards
    # everything in _proto_support.  Should we be more strict and only
    # forward the haskell_proto_library deps?
    dependencies = simple_dependencies(ctx.rule.attr.deps + ctx.attr._proto_support)

    lib = _haskell_library_impl(
        ctx = ctx,
        dependencies = dependencies,
        srcs_options = compile_srcs_options.simple(
            srcs = generate_proto_files(ctx, target[ProtoInfo]),
        ),
    )

    return fix_aspect_providers(lib)

haskell_proto_aspect = aspect(
    attr_aspects = ["deps"],
    attrs = dicts.add({
        "_protoc_gen_haskell": attr.label(
            executable = True,
            cfg = "host",
            default = Label("//third_party/haskell/proto_lens:proto_lens_protoc_bin"),
        ),
        # Explicit list of dependencies which will be used and exported by all
        # haskell_proto_library_rules.
        # TODO(b/25294611): Be more strict about when they are exported.
        "_proto_support": attr.label_list(
            default = [
                Label("//third_party/haskell/proto_lens:proto_lens_runtime"),
            ],
        ),
    }, _compilation_attrs, check_module_names_attrs),
    fragments = ["cpp"],
    implementation = _impl_proto_aspect,
)

def _impl_proto_library(ctx):
    """Implementation of the haskell_proto_library rule.

    Args:
      ctx: The current rule context.

    Returns:
      A compiled haskell_library target (see _impl_library for more details).
    """
    return _haskell_library_impl(
        ctx,
        dependencies = simple_dependencies(ctx.attr.deps),
        srcs_options = compile_srcs_options.simple(),
    )

haskell_proto_library = rule(
    attrs = dicts.add(_compilation_attrs, {
        "deps": attr.label_list(
            providers = [ProtoInfo],
            aspects = [haskell_proto_aspect],
            doc = """
A list of <code>proto_library</code> targets for which to generate bindings.
Note that transitive <code>proto_library</code> dependencies will get dependencies
automatically generated for them.
""",
        ),
    }),
    fragments = ["cpp"],
    implementation = _impl_proto_library,
    doc = """
A BUILD rule for compiling Haskell protocol buffer bindings.

Example usage:

    proto_library(
        name = "foo_proto",
        srcs = ["foo.proto"],
    )

    haskell_proto_library(
        name = "foo_haskell_pb",
        deps = [":foo_proto"],
    )

To test the build of <code>proto_library</code> rule without adding a
corresponding <code>haskell_proto library</code> rule, run:

    bazel build PATH/TO:foo_proto \
        --aspects=tools/build_defs/haskell:def.bzl%haskell_proto_aspect \
        --output_groups=haskell_library_files
""",
)

def _impl_haskell_core_library(ctx):
    config = get_toolchains(ctx).haskell
    package_name = ctx.attr.package
    if package_name not in config.global_packages:
        fail("Unknown core package name: " + package_name)
    package = config.global_packages[package_name]

    exposed_modules_file = ctx.actions.declare_file(ctx.label.name + ".modules")
    ctx.actions.write(exposed_modules_file, "\n".join(package.exposed_modules))
    output_transitive = empty_transitive(package.key)

    return [
        HaskellInfo(
            metadata = struct(
                name = package_name,
                key = package.key,
                # TODO(judahjacobson): Currently, the version is not used,
                # so we don't bother tracking it.  Note that this means wrapping the package
                # in a "forwarding" src-less rule won't work.
                version = "",
            ),
            module_names_ok = None,  # No srcs
            exposed_modules = immediate_exports(package.key, package.exposed_modules),
            transitive = output_transitive,
        ),
        OutputGroupInfo(**dicts.add(
            {
                "haskell_exposed_modules": [exposed_modules_file],
            },
            library_info_output_groups(ctx, package.key, output_transitive.haskell),
        )),
    ]

haskell_core_library = rule(
    implementation = _impl_haskell_core_library,
    # This rule doesn't strictly need the C++ toolchain, but adding this
    # fragment simplifies toolchains.bzl.
    # Also, all the other rules that depend on this *do* use that toolchain, so
    # we wouldn't gain anything by removing it.
    fragments = ["cpp"],
    attrs = dicts.add(toolchains_bootstrap_attrs, {
        "package": attr.string(
            mandatory = True,
            doc = "The GHC package name of this library.",
        ),
    }, proto_attrs),
    doc = """
A target that exposes a GHC core package; i.e., a library that is part of
the GHC distribution.

Example usage:

    haskell_core_library(
        name = "ghc",
        package = "ghc",
    )

Currently, this is not necessary for most core packages, which are exposed
by default to every Haskell target.  For the full list, see
default_packages.bzl.
""",
)
