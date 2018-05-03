# Test that TemplateHaskell works and can link against multiple layers of
# dependencies.

load("//bzl:def.bzl", "haskell_library", "haskell_test")

package(
    features = ["haskell_strict_deps"],
)

# A library that depends on another one.
haskell_library(
    name = "Dependency",
    srcs = ["Dependency.hs"],
    deps = [":TransitiveDependency"],
)

haskell_library(
    name = "TransitiveDependency",
    srcs = ["TransitiveDependency.hs"],
)

# A library that depends only on "Dependency" but (implicitly) needs to link
# against TransitiveDependency for TemplateHaskell.
haskell_library(
    name = "TestDependencyLib",
    srcs = ["TestDependencyLib.hs"],
    deps = [":Dependency"],
)

haskell_test(
    name = "TestDependencyLibTest",
    srcs = ["TestDependencyLibTest.hs"],
    deps = [":TestDependencyLib"],
)

# A test that depends only on "Dependency" but (implicitly) needs to link
# against TransitiveDependency for TemplateHaskell.
haskell_test(
    name = "TestDependency",
    srcs = ["TestDependency.hs"],
    deps = [":Dependency"],
)
