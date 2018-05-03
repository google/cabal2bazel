# Test that "pass through" rules like cabal_haskell_library forwards transitive
# data dependencies correctly.

load("//bzl:cabal_package.bzl", "cabal_haskell_package")
load("//bzl:def.bzl", "haskell_test")
load(":package_description.bzl", "description")

package(
    features = ["haskell_strict_deps"],
)

cc_library(
    name = "cc_with_data",
    data = ["data/cc_data.txt"],
)

# Note: This cabal_haskell_package creates a cabal_haskell_library rule that
# only forwards a haskell_library rule.
cabal_haskell_package(
    cc_deps = [":cc_with_data"],
    description = description,
)

haskell_test(
    name = "TransitiveDataTest",
    srcs = ["TransitiveDataTest.hs"],
    deps = [":foo"],
)
