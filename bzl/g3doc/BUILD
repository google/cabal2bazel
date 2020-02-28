load(":generated_stardoc.bzl", "generated_stardoc")

generated_stardoc(
    name = "rules",
    input = "//bzl:def.bzl",
    deps = [
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
        "@bazel_skylib//lib:new_sets",
        "//bzl:config",
        "//bzl/private",
    ],
)

generated_stardoc(
    name = "hsc2hs",
    input = "//bzl:hsc2hs.bzl",
    deps = [
        "@bazel_skylib//lib:dicts",
        "//bzl:config",
        "//bzl/private",
    ],
)

generated_stardoc(
    name = "cabal_package",
    input = "//bzl:cabal_package.bzl",
    deps = [
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
        "@bazel_skylib//lib:paths",
        "//bzl:alex",
        "//bzl:happy",
        "//third_party/haskell/ghc:versions",
        "//bzl:binary_with_runfiles",
        "//bzl:def",
        "//bzl:hsc2hs",
        "//bzl/cabal_package:cabal_paths",
        "//bzl/cabal_package:overrides",
        "@bazel_skylib//rules:build_test",
    ],
)

generated_stardoc(
    name = "happy",
    input = "//bzl:happy.bzl",
)

generated_stardoc(
    name = "alex",
    input = "//bzl:alex.bzl",
)

generated_stardoc(
    name = "c2hs",
    input = "//bzl:c2hs.bzl",
    deps = [
        "@bazel_skylib//lib:dicts",
        "//bzl:config",
        "//bzl/private",
    ],
)
