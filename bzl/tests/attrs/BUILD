load("//bzl:def.bzl", "haskell_library", "haskell_test")

haskell_test(
    name = "GhcOptsTest",
    srcs = ["GhcOptsTest.hs"],
    ghcopts = [
        "-XCPP",
        "-DABC=123",
        "-DDEF=456",
    ],
)

# Test that we are passing linkopts when linking.
# Set the number of capabilities
haskell_test(
    name = "LinkoptsTest",
    srcs = ["LinkoptsTest.hs"],
    linkopts = ["-with-rtsopts=-N2"],
)

# Test that we are passing RTS options at compile time.
haskell_test(
    name = "GhcRtsoptsTest",
    srcs = ["GhcRtsoptsTest.hs"],
    executes_deps_in_compile = True,
    ghc_rtsopts = ["-N2"],  # Sets the number of capabilities
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
