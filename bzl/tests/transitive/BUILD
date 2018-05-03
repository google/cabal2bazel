# Test that modules can import modules from implicit (transitive) dependencies.

load("//bzl:def.bzl", "haskell_library", "haskell_test")

package(
    features = ["haskell_strict_deps"],
)

haskell_library(
    name = "Transitive",
    srcs = ["Transitive.hs"],
    # Removing the line below makes TransitiveTest fail.
    features = ["-haskell_strict_deps"],
)

# A module that imports "Transitive".
haskell_library(
    name = "Direct",
    srcs = ["Direct.hs"],
    deps = [":Transitive"],
)

# A module that imports both "Direct" and "Transitive", but only specifies
# "Direct" in the deps.
haskell_test(
    name = "TransitiveTest",
    srcs = ["TransitiveTest.hs"],
    deps = [":Direct"],
)
