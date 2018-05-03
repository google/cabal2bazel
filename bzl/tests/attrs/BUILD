load("//bzl:def.bzl", "haskell_library", "haskell_test")

package(
    features = ["haskell_strict_deps"],
)

# This was checking vardef Make variables which are not supported in bazel.
FOO = "456"

haskell_test(
    name = "GhcOptsTest",
    srcs = ["GhcOptsTest.hs"],
    ghcopts = [
        "-XCPP",
        # Check that we tokenize the ghcopts, which should remove the "'"s.
        "-D'ABC=123'",
        "-DDEF=$(FOO)",
    ],
)

haskell_test(
    name = "MainIs",
    srcs = ["MainIs.hs"],
    main_is = "MainIs.foo",
)

cc_library(
    name = "runfiles_c",
    hdrs = ["runfiles_c.h"],
    data = ["file_c.txt"],
)

haskell_library(
    name = "RunfilesLib",
    srcs = ["RunfilesLib.hs"],
    data = ["file_lib.txt"],
)

# Test the "data" attribute by checking that this binary can read the files
# specified by it and by its dependencies.
haskell_test(
    name = "RunfilesMain",
    srcs = ["RunfilesMain.hs"],
    data = ["file_main.txt"],
    deps = [
        ":RunfilesLib",
        ":runfiles_c",
    ],
)

haskell_test(
    name = "LiterateHaskell",
    srcs = ["LiterateHaskell.lhs"],
)

haskell_test(
    name = "DefaultOptions",
    srcs = ["DefaultOptions.hs"],
)
