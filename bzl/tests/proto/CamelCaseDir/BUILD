# Checks that haskell_proto_library/aspect work in CamelCased directories.
load("//bzl:def.bzl", "haskell_proto_library", "haskell_test")

proto_library(
    name = "foo_proto",
    srcs = ["foo.proto"],
)

haskell_proto_library(
    name = "foo_haskell_pb",
    deps = [":foo_proto"],
)

haskell_test(
    name = "CamelCaseDirTest",
    srcs = ["CamelCaseDirTest.hs"],
    deps = [
        ":foo_haskell_pb",
        # TODO
    ],
)

# Make sure this works even in the unlikely case that the rule name is upper-cased.
proto_library(
    name = "UpperFoo_proto",
    srcs = ["foo.proto"],
)

haskell_proto_library(
    name = "UpperFoo_haskell_pb",
    deps = [":UpperFoo_proto"],
)

haskell_test(
    name = "CamelCaseDirUpperRuleTest",
    srcs = ["CamelCaseDirTest.hs"],
    deps = [
        ":foo_haskell_pb",
        # TODO
    ],
)
