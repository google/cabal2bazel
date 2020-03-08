# Skylark rules for compiling Haskell code.
load("@bazel_skylib//:bzl_library.bzl", "bzl_library")

package(default_visibility = ["//visibility:public"])

exports_files([
    "cabal_package.bzl",
    "def.bzl",
    "hsc2hs.bzl",
])

filegroup(
    name = "haskell",
    srcs = glob(["*.bzl"]),
    visibility = ["//visibility:public"],
)

bzl_library(
    name = "def",
    srcs = ["def.bzl"],
    visibility = [
        "//bzl:__subpackages__",
    ],
    deps = [
        ":config",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
        "@bazel_skylib//lib:new_sets",
        "//bzl/private",
    ],
)

bzl_library(
    name = "config",
    srcs = [":config.bzl"],
)

bzl_library(
    name = "binary_with_runfiles",
    srcs = [":binary_with_runfiles.bzl"],
)

bzl_library(
    name = "hsc2hs",
    srcs = [":hsc2hs.bzl"],
    deps = [
        ":config",
        "@bazel_skylib//lib:dicts",
        "//bzl/private",
    ],
)

bzl_library(
    name = "paths",
    srcs = ["paths.bzl"],
    deps = [
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "settings",
    srcs = ["settings.bzl"],
    deps = ["@bazel_skylib//rules:common_settings"],
)

bzl_library(
    name = "happy",
    srcs = ["happy.bzl"],
)
bzl_library(
    name = "alex",
    srcs = ["alex.bzl"],
)
bzl_library(
    name = "c2hs",
    srcs = ["c2hs.bzl"],
)
