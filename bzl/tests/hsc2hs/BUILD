load("//bzl:def.bzl", "haskell_test")
load("//bzl:hsc2hs.bzl", "hsc2hs")

package(
    features = ["haskell_strict_deps"],
)

cc_library(
    name = "c_dep",
    hdrs = ["includes/c_dep.h"],
    includes = ["includes"],
)

hsc2hs(
    name = "Foo",
    src = "Foo.hsc",
    deps = [":c_dep"],
)

haskell_test(
    name = "FooTest",
    srcs = [":Foo"],
)
