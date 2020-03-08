# Test support files for the Haskell build rules.
# In some of the tests, we want to check our handling of
# the "includes" attribute, which is used 
# by the cabal_package macro.
licenses(["unencumbered"])

cc_library(
    name = "hsc2hs_dep",
    hdrs = ["includes/hsc2hs_dep.h"],
    includes = ["includes"],
    visibility = ["//bzl/tests/hsc2hs:__pkg__"],
)

cc_library(
    name = "header_provider",
    hdrs = ["includes/header_provider.h"],
    includes = ["includes"],
    visibility = ["//bzl/tests/ffi:__pkg__"],
)
