# Internal functions for the Haskell build rules.
load("@bazel_skylib//:bzl_library.bzl", "bzl_library")

package(default_visibility = ["//bzl:__subpackages__"])

bzl_library(
    name = "private",
    srcs = [
        "action.bzl",
        "archive.bzl",
        "binary.bzl",
        "cc.bzl",
        "compile.bzl",
        "compile_srcs_options.bzl",
        "default_packages.bzl",
        "ghc_options.bzl",
        "info.bzl",
        "library.bzl",
        "link.bzl",
        "module_names.bzl",
        "package.bzl",
        "proto_file.bzl",
        "providers.bzl",
        "runfiles.bzl",
        "toolchains.bzl",
        "@bazel_tools//tools/build_defs/cc:action_names.bzl",
        "@bazel_tools//tools/cpp:toolchain_utils.bzl",
    ],
    deps = [
        "@bazel_skylib//lib:paths",
        "//bzl:paths",
        "//bzl:settings",
    ],
)
