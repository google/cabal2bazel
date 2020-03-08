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

"""Skylark rules for configuring the Haskell compiler.

This file contains a BUILD rule for configuring the executables and support
files for a particular version of the Haskell compiler (i.e., GHC).

Generally these rules should only be used inside of
//third_party/haskell/ghc.  The standard Haskell build rules (haskell_library,
haskell_binary, etc.) will automatically import them by default.

Example usage:

ghc_config(
  name = "config",
  version = "8.4.3",
  compiler = "bin/ghc",
  compiler_bundle = ":ghc-support-files"
  ghc_pkg = "bin/ghc-pkg",
  ghc_pkg_bundle = ":ghc-pkg-support-files",
  hsc2hs = "bin/hsc2hs",
  hsc2hs_template = "lib/template-hsc.h",
  global_packages = {"base-4.11.1.0": ["Prelude", ...], ...},
  default_options = ["-Wall", "-Werror"],
)

# Re-export config from another package:
ghc_config(
  name = "config-reexport",
  config = "//path/to/specific/ghc/version:config",
)

"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load(
    "//bzl/private:cc.bzl",
    "cc_toolchain_attrs",
    "get_configured_cc_toolchain",
)
load("//bzl/private:default_packages.bzl", "default_package_names")
load("//bzl/private:module_names.bzl", "immediate_exports")
load("//bzl/private:ghc_options.bzl", "ghc_options")
load("//bzl/private:proto_file.bzl", "proto_attrs", "write_proto_file")

HaskellCompilerInfo = provider(fields = [
    "version",
    "compiler",
    "compiler_bundle",
    "prof_bundle",  # Additional files needed at compile-time for profiled builds
    "library_bundle",
    "library_root",
    "ghc_pkg",
    "ghc_pkg_bundle",
    "hsc2hs",
    "hsc2hs_template",
    "unlit",
    "global_packages",
    "docdir",
    # Options that are common to every invocation of GHC.
    # Deduped here for tools like hrepl that may run on
    # multiple targets.
    "common_options",
    "shared_libraries",
    # A dict from module names to the key/id of the default package that defines them.
    # It has the same format as HaskellInfo.exposed_modules.
    "default_exposed_modules",
])

def _impl(ctx):
    if ctx.attr.config:
        defaults = ctx.attr.config[HaskellCompilerInfo]

        # The `compiler` filed can be overridden, and `compiler_bundle_addons`
        # can be used to add to `compiler_bundle`.
        if ctx.executable.compiler or ctx.files.compiler_bundle_addons:
            compiler = ctx.executable.compiler or defaults.compiler
            compiler_bundle = (
                defaults.compiler_bundle + ctx.files.compiler_bundle_addons
            )

            config = HaskellCompilerInfo(
                version = defaults.version,
                compiler = compiler,
                compiler_bundle = compiler_bundle,
                prof_bundle = defaults.prof_bundle,
                library_bundle = defaults.library_bundle,
                library_root = defaults.library_root,
                ghc_pkg = defaults.ghc_pkg,
                ghc_pkg_bundle = defaults.ghc_pkg_bundle,
                hsc2hs = defaults.hsc2hs,
                hsc2hs_template = defaults.hsc2hs_template,
                unlit = defaults.unlit,
                global_packages = defaults.global_packages,
                docdir = defaults.docdir,
                shared_libraries = defaults.shared_libraries,
                common_options = defaults.common_options,
                default_exposed_modules = defaults.default_exposed_modules,
            )
        else:
            config = defaults
    else:
        common_options = ghc_options.common(
            cc_toolchain = get_configured_cc_toolchain(ctx),
            compilation_mode = ctx.var["COMPILATION_MODE"],
            global_package_ids = ctx.attr.global_packages.keys(),
            default_options = ctx.attr.default_options,
        )

        global_packages = _collect_packages(ctx.attr.global_packages)
        config = HaskellCompilerInfo(
            version = ctx.attr.version,
            compiler = ctx.executable.compiler,
            compiler_bundle = ctx.files.compiler_bundle,
            prof_bundle = ctx.files.prof_bundle,
            library_bundle = ctx.files.library_bundle,
            library_root = ctx.attr.library_root,
            ghc_pkg = ctx.executable.ghc_pkg,
            ghc_pkg_bundle = ctx.files.ghc_pkg_bundle,
            hsc2hs = ctx.executable.hsc2hs,
            hsc2hs_template = ctx.file.hsc2hs_template,
            unlit = ctx.executable.unlit,
            global_packages = global_packages,
            docdir = ctx.attr.docdir,
            shared_libraries = ctx.files.shared_libraries,
            common_options = common_options,
            default_exposed_modules = dicts.add(*[
                immediate_exports(global_packages[p].key, global_packages[p].exposed_modules)
                for p in default_package_names
            ]),
        )

    proto_file = write_proto_file(
        ctx = ctx,
        output_name = ctx.attr.name,
        proto_type = "haskell.GhcConfig",
        content = struct(
            ghc = config.compiler.path,
            library_root = config.library_root,
            common_options = config.common_options +
                             [
                                 "-package-id={}".format(config.global_packages[p].key)
                                 for p in default_package_names
                             ],
        ),
    )

    return [
        config,
        DefaultInfo(files = depset([proto_file])),
    ]

ghc_config = rule(
    implementation = _impl,
    provides = [HaskellCompilerInfo],
    fragments = ["cpp"],
    attrs = dicts.add({
        "config": attr.label(providers = [HaskellCompilerInfo]),
        "version": attr.string(),
        "compiler": attr.label(executable = True, cfg = "target", allow_single_file = True),
        "compiler_bundle": attr.label_list(allow_files = True),
        "compiler_bundle_addons": attr.label_list(allow_files = True),
        "prof_bundle": attr.label_list(allow_files = True),
        "library_bundle": attr.label_list(allow_files = True),
        "library_root": attr.string(),
        "ghc_pkg": attr.label(executable = True, cfg = "host", allow_single_file = True),
        "ghc_pkg_bundle": attr.label_list(allow_files = True),
        "hsc2hs": attr.label(executable = True, cfg = "host", allow_single_file = True),
        "hsc2hs_template": attr.label(allow_single_file = True),
        "unlit": attr.label(executable = True, cfg = "host", allow_single_file = True),
        "global_packages": attr.string_list_dict(),
        "default_options": attr.string_list(),
        "docdir": attr.string(),
        "shared_libraries": attr.label_list(allow_files = True),
    }, proto_attrs, cc_toolchain_attrs),
)

def ghc_preprocessor_defines(info):
    """Returns the value of the __GLASGOW_HASKELL__macro.

    Used by utilities like hsc2hs which need to run CPP.
    Note: the __GLASGOW_HASKELL__ macro corresponds to GHC's "major" version number.
    That is, "708" for version "7.8.4" and "710" for "7.10.3".

    Args:
      info: A HaskellCompileInfo provider.

    Returns:
      A list of preprocessor definition strings.
    """

    components = info.version.split(".")
    if len(components) < 2:
        fail("Incorrect format for GHC version number: " + info.version)
    v0 = components[0]
    v1 = components[1]
    if len(v1) == 1:
        v1 = "0" + v1
    return ["__GLASGOW_HASKELL__=%s%s" % (v0, v1)]

def _collect_packages(packages):
    """Add more information to the dict of package IDs.

    The input is a dict from string (package IDs) to list of string (exposed
    modules), and is suitable as a build rule attribute.
    The output is a struct with more information parsed out.  (structs are not valid
    as attributes.)

    Args:
      packages: A dict of strings (package ids, i.e., names + versions) to lists of strings
        (exposed modules).

    Returns:
      A map from package names to a struct containing the fields:
        - key: The full package ID
        - exposed_modules: A list of strings.
    """
    packages_map = {
        # Parse the name from the package ID, whose format is "{name}-{version}".
        package_id.rpartition("-")[0]: struct(
            key = package_id,
            exposed_modules = modules,
        )
        for package_id, modules in packages.items()
    }

    return packages_map

def _haskell_cc_toolchain_files_impl(ctx):
    return DefaultInfo(files = get_configured_cc_toolchain(ctx).toolchain.all_files)

haskell_cc_toolchain_files = rule(
    implementation = _haskell_cc_toolchain_files_impl,
    attrs = cc_toolchain_attrs,
    doc = """
A rule that forwards CcToolchainInfo.all_files.
Used in particular for MPMs that include the Haskell compiler and thus
also need to include the C++ compilation toolchain.
""",
    fragments = ["cpp"],
)
