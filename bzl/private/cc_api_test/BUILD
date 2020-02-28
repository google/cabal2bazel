# Tests of how we use the C++ Skylark API.

load(":link.bzl", "cc_api_binary")

cc_library(
    name = "lib",
    srcs = ["lib.cc"],
)

cc_api_binary(
    name = "binary",
    deps = [":lib"],
)

cc_binary(
    name = "comparison_binary",
    deps = [":lib"],
)
