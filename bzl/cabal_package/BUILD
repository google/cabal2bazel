# Support files for the cabal_haskell_package macro.
load("//bzl/cabal_package:core_packages_list.bzl", "core_packages_list")
load("//bzl:def.bzl", "haskell_library")
load("@bazel_skylib//:bzl_library.bzl", "bzl_library")

package(default_visibility = ["//bzl:__subpackages__"])

exports_files([
    "paths.template",
    "test.template",
])

# cabal2build will build this target on-demand to get the list of core packages.
# Having a separate target lets it be up-to-date with the current client,
# rather than the released version of the cabal2build binary.
core_packages_list(
    name = "core_packages",
)

bzl_library(
    name = "cabal_paths",
    srcs = ["cabal_paths.bzl"],
)

bzl_library(
    name = "overrides",
    srcs = ["overrides.bzl"],
)

# A forwarding library that any user of cabal_paths should depend on.
haskell_library(
    name = "PathDeps",
    default_packages = [],
    visibility = [
        "//bzl/tests:__subpackages__",
    ],
    deps = [
        # TODO
    ],
)
