# Unit test that we can compile recursive modules with .hs-boot files.

load("//bzl:def.bzl", "haskell_library", "haskell_test")

package(
    features = ["haskell_strict_deps"],
)

haskell_library(
    name = "Recursive",
    srcs = [
        "Boot.hs",
        "Boot.hs-boot",
        "Regular.hs",
    ],
)

haskell_test(
    name = "RecursiveTest",
    srcs = ["RecursiveTest.hs"],
    deps = [":Recursive"],
)

haskell_test(
    name = "RecursiveBinaryTest",
    srcs = [
        "Boot.hs",
        "Boot.hs-boot",
        "RecursiveTest.hs",
        "Regular.hs",
    ],
)
