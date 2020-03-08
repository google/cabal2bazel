# Test that TemplateHaskell works and can link against multiple layers of
# dependencies.
# (It's still broken for code that depends on FFI; see b/19372897.)

load("//bzl:def.bzl", "haskell_library", "haskell_test")

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
    executes_deps_in_compile = True,
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
    executes_deps_in_compile = True,
    deps = [":Dependency"],
)

# A test that puts all of the files in a single rule.
haskell_test(
    name = "TestJoined",
    srcs = [
        "Dependency.hs",
        "TestDependency.hs",
    ],
    executes_deps_in_compile = True,
    deps = [":TransitiveDependency"],
)

haskell_test(
    name = "ANNTest",
    srcs = ["ANNTest.hs"],
)
