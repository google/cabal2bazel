# Test that "pass through" rules, like haskell_library without srcs, forward transitive
# data dependencies correctly.

load("//bzl:def.bzl", "haskell_library", "haskell_test")

cc_library(
    name = "cc_with_data",
    data = ["data/cc_data.txt"],
)

haskell_library(
    name = "Provider",
    srcs = ["Provider.hs"],
    data = ["data/foo.txt"],
    #deps = ["//haskell/Google/Runtime"],
)

# A rule that has no srcs and only forwards its immediate deps.
haskell_library(
    name = "foo",
    deps = [
        ":Provider",
        ":cc_with_data",
    ],
)

haskell_test(
    name = "TransitiveDataTest",
    srcs = ["TransitiveDataTest.hs"],
    # Intentionally depend on the forwarding rule:
    tags = ["nofixdeps"],
    deps = [":foo"],
)
