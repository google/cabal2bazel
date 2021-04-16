# Test that C++ can depend on Haskell.

load("//bzl:def.bzl", "haskell_library", "haskell_static_foreign_library")

haskell_library(
    name = "Lib",
    srcs = [
        "Internal/Inc.hs",
        "Lib.hs",
        "Sub/Lib.hs",
    ],
    features = ["-parse_headers"],
    foreign_exports = [
        "Lib",
        "Sub.Lib",
    ],
    deps = ["//third_party/haskell/text"],
)

cc_test(
    name = "test",
    size = "small",
    srcs = ["test.cc"],
    # TODO(b/144594873): Don't require this.
    features = ["-layering_check"],
    deps = [
        ":Lib",
        "//third_party/haskell/ghc:headers",
    ],
)

haskell_static_foreign_library(
    name = "Lib-static",
    library = ":Lib",
)

# Make sure that static linking works too.
cc_test(
    name = "test-static",
    srcs = ["test.cc"],
    # TODO(b/144594873): Don't require this.
    features = ["-layering_check"],
    linkstatic = True,
    deps = [
        ":Lib-static",
        "//third_party/haskell/ghc:headers",
    ],
)

cc_library(
    name = "clib",
    srcs = ["clib.c"],
)

haskell_library(
    name = "CDep",
    srcs = ["CDep.hs"],
    features = ["-parse_headers"],
    foreign_exports = [
        "CDep",
    ],
    deps = [":clib"],
)

cc_test(
    name = "cdep_test",
    srcs = ["cdep_test.cc"],
    # TODO(b/144594873): Don't require this.
    features = ["-layering_check"],
    deps = [
        ":CDep",
        "//third_party/haskell/ghc:headers",
    ],
)
