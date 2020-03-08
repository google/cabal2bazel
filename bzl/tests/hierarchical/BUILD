# Test that we handle all the corner cases of hierarchical modules.
# (See also the "Sub" subdirectory.)

load("//bzl:def.bzl", "haskell_library", "haskell_test")

# A rule where the srcs correspond to the module hierarchy.
haskell_library(
    name = "FooGroup",
    srcs = [
        "Foo.hs",
        "Foo/Bar.hs",
    ],
)

# Re-export a library (since this rule doesn't have any srcs).
haskell_library(
    name = "ReExport",
    deps = [":FooGroup"],
)

# A module that uses the functions in FooGroup.
haskell_library(
    name = "Baz",
    srcs = ["Baz.hs"],
    deps = [":ReExport"],
)

# Another layer of indirection, to make sure that the FooGroup's GHC package DB
# is passed up transitively.
haskell_library(
    name = "Baz2",
    srcs = ["Baz2.hs"],
    deps = [":Baz"],
)

# A binary that depends on everything above.
haskell_test(
    name = "mybinary",
    srcs = ["Main.hs"],
    deps = [":Baz2"],
)

# A rule with the same name as one in the "Sub" directory.
haskell_library(
    name = "DupeName",
    srcs = ["DupeName.hs"],
    deps = [
        "//bzl/tests/hierarchical/Sub:DupeName",
    ],
)

# Check that we can link against two libraries with the same name in different
# directories.
haskell_test(
    name = "DupeNameTest",
    srcs = ["DupeNameTest.hs"],
    deps = [":DupeName"],
)
