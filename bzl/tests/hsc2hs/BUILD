load("//bzl:def.bzl", "haskell_test")
load("//bzl:hsc2hs.bzl", "hsc2hs")
load("@bazel_skylib//rules:build_test.bzl", "build_test")

hsc2hs(
    name = "Foo",
    src = "Foo.hsc",
    out = "Foo.hs",
    deps = ["//test_support:hsc2hs_dep"],
)

haskell_test(
    name = "FooTest",
    srcs = [":Foo"],
)

# Test we can have two hsc2hs() rules on the same input file:
hsc2hs(
    name = "Foo2",
    src = "Foo.hsc",
    out = "Foo2.hsc",
    deps = ["//test_support:hsc2hs_dep"],
)

build_test(
    name = "Foo2BuildTest",
    targets = [":Foo2"],
)
