load("//bzl:def.bzl", "haskell_library", "haskell_test")

haskell_library(
    name = "dependency_lib",
    srcs = ["Dependency.hs"],
    # The Cabal macros should use this name:
    cabal_name = "dependency",
    cabal_version = "1.2.3",
)

haskell_test(
    name = "VersionTest",
    srcs = ["VersionTest.hs"],
    deps = [":dependency_lib"],
)
