load("//bzl:def.bzl", "haskell_library", "haskell_test")

haskell_library(
    name = "PreprocessedLib",
    srcs = ["PreprocessedLib.hs"],
    preprocessor = ":pp",
    # Preprocessors aren't supported in the indexer (b/23957150)
    tags = ["no_grok"],
)

haskell_test(
    name = "PreprocessedTest",
    srcs = ["PreprocessedTest.hs"],
    preprocessor = ":pp",
    # Preprocessors aren't supported in the indexer (b/23957150)
    tags = ["no_grok"],
    deps = [":PreprocessedLib"],
)

sh_binary(
    name = "pp",
    srcs = ["pp.sh"],
)
