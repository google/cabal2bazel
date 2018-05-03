# Simple sanity checks of each rule.
# (Note in particular that the other tests tend to use haskell_test instead of
# haskell_binary.)

load("//bzl:def.bzl", "haskell_binary", "haskell_library", "haskell_test")

package(
    features = ["haskell_strict_deps"],
)

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
