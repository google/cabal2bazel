# Description: GHC binaries to be used by Blaze.
# This is a redirector file so that the Haskell version compilation switch
# can be thrown with minimal BUILD config changes.

licenses(["restricted"])

load(":versions.bzl", "GHC_VERSION_SETTINGS", "ghc_config_settings")
load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("//bzl:config.bzl", "ghc_config", "haskell_cc_toolchain_files")
load("//bzl:generate_core_libraries.bzl", "generate_core_library_rules")

package(default_visibility = ["//visibility:public"])

ghc_config_settings()

# To avoid unwanted dependencies, we declare a visibility that only allows
# //third_party/haskell/ghc to access and forward these targets.
package_group(
    name = "main_ghc",
    packages = ["//third_party/haskell/ghc"],
)

# C/C++ code that needs to include headers from GHC (such as <HsFFI.h>) should
# depend on this target.
alias(
    name = "headers",
    actual = select({
        version: GHC_VERSION_SETTINGS[version].package + ":headers"
        for version in GHC_VERSION_SETTINGS
    }),
)

# The GHC library directory is needed for using the GHC API.
# Applications can use //third_party/haskell/ghc-paths:ghc_paths to
# add those files as a dependency and find the correct location.
filegroup(
    name = "library_bundle",
    srcs = select({
        version: [GHC_VERSION_SETTINGS[version].package + ":library-bundle"]
        for version in GHC_VERSION_SETTINGS
    }),
)

# Support files for ghaddock.
filegroup(
    name = "haddock_support",
    srcs = select({
        version: [GHC_VERSION_SETTINGS[version].package + ":haddock_support"]
        for version in GHC_VERSION_SETTINGS
    }),
)

filegroup(
    name = "everything",
    srcs = select({
        version: [GHC_VERSION_SETTINGS[version].package + ":everything"]
        for version in GHC_VERSION_SETTINGS
    }),
)

# A minimum set of files which the GHC API loads when it initializes.
filegroup(
    name = "settings",
    srcs = select({
        version: [GHC_VERSION_SETTINGS[version].package + ":settings"]
        for version in GHC_VERSION_SETTINGS
    }),
)

ghc_config(
    name = "config",
    config = select({
        version: GHC_VERSION_SETTINGS[version].package + ":config"
        for version in GHC_VERSION_SETTINGS
    }),
)

generate_core_library_rules()

# A target that provides all of the C++ toolchain's files.
haskell_cc_toolchain_files(
    name = "cc_toolchain",
    visibility = ["//devtools/grok/lang/haskell:__subpackages__"],
)

bzl_library(
    name = "versions",
    srcs = ["versions.bzl"],
    deps = [
        "@bazel_skylib//lib:dicts",
    ],
)
