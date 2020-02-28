load("//bzl:def.bzl", "haskell_bootstrap_binary", "haskell_test")

# Since other haskell_binary rules will depend on it in their attrs,
# this target can't itself be a haskell_binary.  We break the cycle
# by making it a haskell_bootstrap_binary instead.
haskell_bootstrap_binary(
    name = "CheckModuleNames",
    srcs = ["CheckModuleNames.hs"],
    visibility = ["//visibility:public"],
    # Note: we use this target to build haskell_library targets.
    # As a result, it cannot itself depend on a haskell_library, and can
    # only use core packages like base or ghc.
    deps = [
        # TODO
    ],
)

haskell_test(
    name = "CheckModuleNamesTest",
    srcs = ["CheckModuleNamesTest.hs"],
    data = [
        ":CheckModuleNames",
        "//third_party/haskell/ghc:settings",
    ],
    deps = [
        # TODO
    ],
)
