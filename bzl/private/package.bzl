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

"""Utility functions to construct package metadata.
"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("//bzl/private:action.bzl", "multi_action")
load("//bzl/private:providers.bzl", "HaskellInfo")

def _package_libdir(ctx):
    """Returns the subdirectory in which to store Haskell library archives.

    Putting them in separate directories helps us isolate them.  In particular,
    if GHC uses "-L" to point the linker at them, we don't want it to accidentally
    pick up other C++ libraries that happen to be in the same directory.
    This has happened in the past with "local" builds; see in particular b/145687795.
    """
    return ctx.label.name + ".libs"

def _package_key(ctx):
    """Get a globally-unique GHC package name for a rule.

    Args:
      ctx: The context of a rule.
      name_override: If want to get the package key of another target in the same
                     package.

    Returns:
      A GHC package identifier, based on the Bazel package and the (overriden)
      name of this rule, which is unique within the build.  It escapes characters
      that are allowed by Bazel but not by GHC.  It also disambiguates between
      different proto API versions.
    """
    res = ctx.label.package + "/" + ctx.label.name

    # "-"s are technicaly allowed, but we escape them since digits can't appear
    # by themselves without a letter.  (Otherwise, ghc might try to treat them
    # like a version number, e.g., "abc-12").
    # (Put this first so it doesn't interact with the other replacements that add
    # "-"s.
    res = res.replace("-", "D-D")
    res = res.replace("/", "S-S")
    res = res.replace("_", "U-U")

    return res

def _package_metadata(ctx, deps = [], srcs = []):
    """Retrieve the package metadata for the given rule.

    Args:
      ctx: The current rule context.
      deps: A list of targets that this rule depends on.
      srcs: The source files for this rule.

    Returns:
      A struct containing the following fields:
        - key: The GHC package identifier; unique for each rule, and suitable
            as the basename of a file (e.g.: library archive or package spec).
        - name: The GHC package name.  Usually the same as the key,
            but may be simpler for third-party packages that use it
            to define Cabal macros.
        - version: The GHC package version.  May be the empty string
            if the rule didn't specify a version.  As with "name",
            this is only used in third-party packages.
    """
    key = _package_key(ctx)

    # If it is a third-party package with a "cabal_version" attribute
    # set, then we use that as the version and the rule basename as the
    # GHC package name.  (This enables Cabal VERSION macros.)
    version = getattr(ctx.attr, "cabal_version", None)
    if version:
        cabal_name = ctx.attr.cabal_name
        if not cabal_name:
            fail("If cabal_version is nonempty, cabal_name must also be nonempty: " + str(ctx.label))
        return struct(
            name = cabal_name,
            version = version,
            key = key,
        )
        # If it is a "forwarding" rule with no "srcs" and exactly one dep,
        # which itself had a version, then use that rules' version and name.
        # This lets third-party "versionless" packages forward dependencies.
        # TODO(judahjacobson): Figure out a less hacky approach.

    elif not srcs:
        haskell_deps = [dep for dep in deps if HaskellInfo in dep]
        if len(haskell_deps) == 1:
            metadata = haskell_deps[0][HaskellInfo].metadata
            if metadata.version:
                return struct(
                    name = metadata.name,
                    version = metadata.version,
                    key = key,
                )

    # Otherwise, use the original Bazel label as the name, and the mangled label
    # as the key/id.
    return struct(
        name = str(ctx.label),
        key = key,
        version = "",
    )

def _generic_package_arguments(package_ids = [], plugin_package_ids = [], caches = []):
    return (
        ["-package-id=" + p for p in package_ids] +
        ["-plugin-package-id=" + p for p in plugin_package_ids] +
        ["-package-db=" + p.dirname for p in caches]
    )

def _package_compile_options(deps):
    """Collects flags for GHC to use the collected deps when compiling.

    Args:
      deps: A struct of collected dependencies.

    Returns:
      A struct containing two fields:
      - arguments: A list of command-line flags
      - inputs: A depset of related input files
      """
    return struct(
        arguments = _generic_package_arguments(
            package_ids = deps.exposed_package_ids,
            plugin_package_ids = deps.plugins.package_ids,
            caches = deps.transitive.haskell.caches.to_list(),
        ),
        inputs = depset(transitive = [
            deps.transitive.haskell.hi_dirs,  # Contains both plugins and regular deps
            deps.transitive.haskell.caches,
        ]),
    )

def _package_link_options(deps):
    """Collects flags for GHC to use the collected deps when linking.

    Args:
      deps: A struct of collected dependencies.

    Returns:
      A struct containing two fields:
      - arguments: A list of command-line flags
      - inputs: A depset of related input files
    """
    return struct(
        arguments = _generic_package_arguments(
            # Note plugins, like deps, are linked into the final target.
            package_ids = deps.exposed_package_ids + deps.plugins.package_ids,
            caches = deps.transitive.haskell.caches.to_list(),
        ),
        inputs = depset(transitive = [
            deps.transitive.haskell.caches,
        ]),
    )

def _package_check_options(deps):
    """Collects flags for GHC to use the collected deps when typechecking without codegen..

    Note: this is not suitable if the target has executes_deps_in_compile. If it does,
    package.compile_options should be used instead.

    Args:
      deps: A struct of collected dependencies.

    Returns:
      A struct containing two fields:
      - arguments: A list of command-line flags
      - inputs: A depset of related input files
    """
    return struct(
        arguments = _generic_package_arguments(
            package_ids = deps.exposed_package_ids,
            plugin_package_ids = deps.plugins.package_ids,
            caches = deps.transitive.haskell_checked.caches.to_list() +
                     # If any plugins are needed, override with the fully compiled packages.
                     # This is technically a little wasteful since means our build rules do a
                     # separate, unnecessary typecheck-only build of the plugins. But there's few
                     # plugins in practice, so it shouldn't be a big deal.
                     deps.plugins.caches.to_list(),
        ),
        inputs = depset(transitive = [
            deps.transitive.haskell_checked.caches,
            deps.transitive.haskell_checked.hi_dirs,
            deps.plugins.caches,
            deps.plugins.hi_dirs,
        ]),
    )

def _package_register(ctx, config, package_db_name, package_spec):
    """Creates and registers a GHC package for the current target.

    Creates a new GHC package database and registers the given package
    (as specified by package_spec) as the only package in that database.

    Args:
      ctx: The current rule context.
      config: A HaskellCompilerInfo provider.
      package_db_name: The name of the output GHC package database.
      package_spec: A File containing the human-readable "spec" for this GHC
        package.

    Returns:
      Returns the binary file containing the package DB.  It will be of the form
      ".../{package_db_name}/package.cache".  To get the actual package DB, call
      "dirname" on the cache file.
    """
    cache = ctx.actions.declare_file(package_db_name + "/package.cache")
    package_db = cache.dirname
    ghc_pkg = config.ghc_pkg
    multi_action(
        ctx = ctx,
        inputs = depset([package_spec] + config.ghc_pkg_bundle),
        outputs = [cache],
        actions = [
            # Before calling ghc-pkg, remove the parent directory of the cache
            # file.  That directory is added automatically by ctx.actions.declare_file(), but
            # "ghc-pkg init" will fail if it already exists.
            struct(cmd = "rm", args = ["-rf", package_db]),
            # Even with -v0 and --force, ghc-pkg complains if the libdir doesn't exist.
            # We *don't* want to depend on the library file itself, since that would prevent
            # us from running this action in parallel with other compilation steps.
            struct(
                cmd = "mkdir",
                args = ["-p", paths.join(
                    ctx.bin_dir.path,
                    ctx.label.package,
                    _package_libdir(ctx),
                )],
            ),
            struct(cmd = ghc_pkg, args = ["-v0", "init", package_db]),
            struct(
                # Append the modules list as a new exposed-modules field.
                # Turn newlines into commas, and remove the trailing comma (if any).
                cmd = ghc_pkg,
                args = [
                    "-v0",
                    "register",
                    # Specify --force to simplify things by preventing ghc-pkg
                    # from checking that support files exist (e.g., *.hi files
                    # of dependencies).
                    "--force",
                    "--global-package-db=" + config.library_root + "/package.conf.d",
                    "--package-db=" + package_db,
                    package_spec.path,
                ],
            ),
        ],
        progress_message = "Registering Haskell package %s" % str(ctx.label),
        mnemonic = "HaskellPackage",
    )

    return cache

package = struct(
    compile_options = _package_compile_options,
    link_options = _package_link_options,
    check_options = _package_check_options,
    metadata = _package_metadata,
    register = _package_register,
    libdir = _package_libdir,
)
