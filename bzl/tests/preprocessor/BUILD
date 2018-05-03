load("//bzl:def.bzl", "haskell_library", "haskell_test")

package(
    features = ["haskell_strict_deps"],
)

haskell_library(
    name = "PreprocessedLib",
    srcs = ["PreprocessedLib.hs"],
    preprocessor = ":pp",
)

haskell_test(
    name = "PreprocessedTest",
    srcs = ["PreprocessedTest.hs"],
    preprocessor = ":pp",
    deps = [":PreprocessedLib"],
)

sh_binary(
    name = "pp",
    srcs = ["pp.sh"],
)
