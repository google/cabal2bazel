load("//bzl:def.bzl", "haskell_test")
load("//bzl/cabal_package:cabal_paths.bzl", "cabal_paths")

cabal_paths(
    name = "foo_paths",
    out = "Paths_foo.hs",
    package = "foo",
    version = [
        1,
        2,
    ],
)

cabal_paths(
    name = "bar_paths",
    out = "Paths_bar.hs",
    data_dir = "subdir",
    package = "bar",
    version = [
        3,
        4,
    ],
)

haskell_test(
    name = "PathsTest",
    srcs = [
        "PathsTest.hs",
        ":bar_paths",
        ":foo_paths",
    ],
    data = [
        "foo.txt",
        "subdir/bar.txt",
    ],
    default_packages = [],
    # TODO(b/139358194): build_cleaner can't handle this mix of generated and
    # checked-in sources.
    tags = ["nofixdeps"],
    deps = ["//bzl/cabal_package:PathDeps"],
)
