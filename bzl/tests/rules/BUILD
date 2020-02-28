# Simple sanity checks of each rule.
# (Note in particular that the other tests tend to use haskell_test instead of
# haskell_binary.)

load("//bzl:def.bzl", "haskell_binary", "haskell_library", "haskell_test")
load("@bazel_skylib//rules:build_test.bzl", "build_test")

haskell_library(
    name = "Library",
    srcs = ["Library.hs"],
)

haskell_binary(
    name = "Binary",
    srcs = ["Binary.hs"],
    deps = [":Library"],
)

haskell_test(
    name = "Test",
    srcs = ["Binary.hs"],
    deps = [":Library"],
)

haskell_test(
    name = "JointTest",
    srcs = [
        "Binary.hs",
        "Library.hs",
    ],
)

# Check that binaries can be forwarded by sh_binary and sh_test (e.g., to
# re-expose them from different BUILD files).
# TODO(judahjacobson): See if we can replace such uses of sh_binary with a more
# robust Skylark rule.
sh_binary(
    name = "BinarySh",
    srcs = [":Binary"],
)

sh_test(
    name = "TestSh",
    srcs = [":Test"],
)

# Test "forwarding" rules that don't have "srcs" but instead reexport
# the modules of their immediate dependencies.
haskell_library(
    name = "Lib1",
    deps = [":Library"],
)

haskell_library(
    name = "Lib2",
    deps = [":Lib1"],
)

haskell_test(
    name = "TestForward",
    srcs = ["Binary.hs"],
    deps = [":Lib1"],
)

haskell_test(
    name = "TestForward2",
    srcs = ["Binary.hs"],
    deps = [":Lib2"],
)

# Test that hidden modules are really hidden.
# Otherwise, depending on both LibHidden and LibExposed would make GHC
# raise an "ambiguous module name" error.
haskell_library(
    name = "LibHidden",
    srcs = ["Library.hs"],
    hidden_modules = ["Library"],
)

haskell_library(
    name = "LibExposed",
    srcs = ["Library.hs"],
)

# Another edge case: when forwarding a library, don't forward its
# hidden modules.
haskell_library(
    name = "LibHiddenForward",
    deps = [":LibHidden"],
)

haskell_test(
    name = "TestHidden",
    srcs = ["Binary.hs"],
    # TODO(b/140051855): Once we teach build_cleaner about hidden_modules, we
    # should try to make this test analyzable.
    tags = ["nofixdeps"],
    deps = [
        ":LibExposed",
        ":LibHidden",
        ":LibHiddenForward",
    ],
)

# A rule that depends on a package exposed by haskell_core_library.
haskell_test(
    name = "CoreLibraryTest",
    srcs = ["CoreLibraryTest.hs"],
    deps = ["//third_party/haskell/ghc"],
)

# A rule that depends on a module forwarded from a haskell_core_library.
haskell_test(
    name = "CoreLibraryForwardingTest",
    srcs = ["CoreLibraryTest.hs"],
    tags = ["nofixdeps"],  # Allow depending on the forwarded library
    deps = [":ghc_forwarded"],
)

haskell_library(
    name = "ghc_forwarded",
    deps = ["//third_party/haskell/ghc"],
)

# A binary with no dependencies should still be able to link against
# the default packages.
haskell_test(
    name = "DefaultPackageTest",
    srcs = ["DefaultPackageTest.hs"],
)

haskell_test(
    name = "PackageImportsTest",
    srcs = ["PackageImportsTest.hs"],
    deps = [":Library"],
)

haskell_library(
    name = "Reexport1",
    reexported_modules = ["Library as Reexport1"],
    deps = [":Library"],
)

haskell_test(
    name = "ReexportTest1",
    srcs = ["ReexportTest1.hs"],
    deps = [":Reexport1"],
)

# Forwarding a reexported module through another library.
haskell_library(
    name = "ForwardReexport1",
    deps = [":Reexport1"],
)

haskell_test(
    name = "ForwardReexportTest",
    srcs = ["ReexportTest1.hs"],
    deps = [":ForwardReexport1"],
)

# Reexporting an already-reexported modue.
haskell_library(
    name = "Reexport2",
    reexported_modules = ["Reexport1 as Reexport2"],
    deps = [":Reexport1"],
)

haskell_test(
    name = "ReReexportTest",
    srcs = ["ReexportTest2.hs"],
    deps = [":Reexport2"],
)

haskell_library(
    name = "ReexportPrelude",
    reexported_modules = ["Prelude as ReexportPrelude"],
)

haskell_test(
    name = "ReexportPreludeTest",
    srcs = ["ReexportPreludeTest.hs"],
    deps = [":ReexportPrelude"],
)

haskell_library(
    name = "UnicodeLib",
    srcs = ["UnicodeLib.hs"],
)

build_test(
    name = "UnicodeLibTest",
    targets = [":UnicodeLib"],
)

# Test that we don't forward modules from default packages.
# TODO(b/148421465): build_cleaner doesn't  understand the "default_packages"
# flag, so we need to tag some of these targets with "nofixdeps".

# A library that's also present in "base".
haskell_library(
    name = "bytestring-overlap",
    srcs = ["Data/ByteString.hs"],
    tags = ["nofixdeps"],
    deps = [
        "//third_party/haskell/ghc:base",
    ],
)

haskell_test(
    name = "NoDefaultPackagesTest",
    srcs = ["NoDefaultPackagesTest.hs"],
    # This setting should prevent a conflict between :bytestring-overlap and the
    # "bytestring" package.
    default_packages = [],
    tags = ["nofixdeps"],
    deps = [
        ":bytestring-overlap",
        "//third_party/haskell/ghc:base",
    ],
)

haskell_library(
    name = "forward-bytestring-overlap",
    deps = [":bytestring-overlap"],
    # Note: this intentionally *doesn't* set default_packages = [];
    # forwarding rules automatically shouldn't reexport default packages.
)

haskell_test(
    name = "NoDefaultPackagesForwardTest",
    srcs = ["NoDefaultPackagesTest.hs"],
    # This setting should prevent a conflict between :bytestring-overlap and the
    # "bytestring" package.
    default_packages = [],
    tags = ["nofixdeps"],
    deps = [
        ":forward-bytestring-overlap",
        "//third_party/haskell/ghc:base",
    ],
)
