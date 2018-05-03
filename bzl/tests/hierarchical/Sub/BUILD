# Test that "package_prefix" lets us build modules within the module hierarchy.

load("//bzl:def.bzl", "haskell_library", "haskell_test")

# A rule to generate the Sub.Foo module.
haskell_library(
    name = "Foo",
    srcs = ["Foo.hs"],
)

haskell_test(
    name = "FooTest",
    srcs = ["FooTest.hs"],
    deps = [":Foo"],
)

# A rule that has the same name as something in the parent directory.
haskell_library(
    name = "DupeName",
    srcs = ["DupeName.hs"],
    visibility = [
        "//bzl/tests/hierarchical:__pkg__",
    ],
)
