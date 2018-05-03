load("//bzl:def.bzl", "haskell_test")
load("//bzl:cabal_paths.bzl", "cabal_paths")

package(
    features = ["haskell_strict_deps"],
)

cabal_paths(
    name = "foo_paths",
    data = ["foo.txt"],
    package = "foo",
    version = [
        1,
        2,
    ],
)

cabal_paths(
    name = "bar_paths",
    data = ["subdir/bar.txt"],
    data_dir = "subdir",
    package = "bar",
    version = [
        3,
        4,
    ],
)

haskell_test(
    name = "PathsTest",
    srcs = ["PathsTest.hs"],
    deps = [
        ":bar_paths",
        ":foo_paths",
    ],
)
