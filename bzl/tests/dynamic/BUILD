# Test Haskell targets with the "linkstatic" option.

load("//bzl:def.bzl", "haskell_binary", "haskell_library", "haskell_test")

haskell_library(
    name = "Library",
    srcs = ["Library.hs"],
)

haskell_binary(
    name = "Binary",
    srcs = ["Binary.hs"],
    linkstatic = False,
    deps = [":Library"],
)

haskell_test(
    name = "BinaryRunfilesTest",
    srcs = ["BinaryRunfilesTest.hs"],
    args = ["Binary"],
    data = [":Binary"],
    deps = [
        # TODO
    ],
)

haskell_test(
    name = "Test",
    srcs = ["Binary.hs"],
    linkstatic = False,
    deps = [":Library"],
)

# Link against a library in a different directory, to confirm
# that we wire up the rpath correctly.
haskell_test(
    name = "SubdirTest",
    srcs = ["Binary.hs"],
    linkstatic = False,
    deps = ["//bzl/tests/dynamic/subdir:Library"],
)

haskell_test(
    name = "FfiTest",
    srcs = ["FfiTest.hs"],
    linkstatic = False,
    deps = ["//bzl/tests/dynamic/subdir:CLibrary"],
)

haskell_binary(
    name = "FfiBinary",
    srcs = ["FfiTest.hs"],
    linkstatic = False,
    deps = ["//bzl/tests/dynamic/subdir:CLibrary"],
)

haskell_test(
    name = "FfiBinaryRunfilesTest",
    srcs = ["BinaryRunfilesTest.hs"],
    args = ["FfiBinary"],
    data = [":FfiBinary"],
    deps = [
        # TODO
    ],
)

cc_test(
    name = "cbin",
    srcs = ["bin.cc"],
)

cc_library(
    name = "flag_provider",
    srcs = ["flag_provider.cc"],
    deps = [
        "@com_google_absl//absl/flags:flag",
    ],
    alwayslink = 1,
)

haskell_test(
    name = "FlagProviderDynamicTest",
    srcs = ["FlagProviderTest.hs"],
    linkstatic = False,
    deps = [
        ":flag_provider",
        # TODO
    ],
)

# For comparison, a version of the test that links statically:
haskell_test(
    name = "FlagProviderStaticTest",
    srcs = ["FlagProviderTest.hs"],
    linkstatic = True,
    deps = [
        ":flag_provider",
        # TODO
    ],
)
