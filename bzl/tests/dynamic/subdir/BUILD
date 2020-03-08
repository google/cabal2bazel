# Helper targets for testing the linkstatic attribute.
load("//bzl:def.bzl", "haskell_library")

package(default_visibility = ["//bzl/tests/dynamic:__pkg__"])

haskell_library(
    name = "Library",
    srcs = ["Library.hs"],
)

cc_library(
    name = "clib",
    srcs = ["clib.cc"],
)

haskell_library(
    name = "CLibrary",
    srcs = ["CLibrary.hs"],
    deps = [":clib"],
)
