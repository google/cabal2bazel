# Skylark rules for compiling Haskell code.
package(default_visibility = ["//visibility:public"])

exports_files(["paths.template"])

filegroup(
    name = "haskell",
    srcs = [
        "action.bzl",
        "cabal_package.bzl",
        "cabal_paths.bzl",
        "config.bzl",
        "def.bzl",
        "hsc2hs.bzl",
        "metadata.bzl",
        "providers.bzl",
    ],
    visibility = ["//visibility:public"],
)

cc_library(name = "runtime_dependencies")
