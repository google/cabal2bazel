# Description: Redirector build file to switch between GHC versions.

licenses(["unencumbered"])

load("//bzl:config.bzl", "ghc_config")

package(
    default_visibility = ["//visibility:public"],
)

config_setting(
    name = "v8_0_2_k8",
    values = {
        "cpu": "k8",
    },
)

# C/C++ code that needs to include headers from GHC (such as <HsFFI.h>) should
# depend on this target.
cc_library(
    name = "headers",
    deps = select({
        ":v8_0_2_k8": ["//third_party/haskell/ghc/v8_0_2_k8:headers"],
    }),
)

# The GHC library directory is needed for using the GHC API.
# Applications can use //third_party/haskell/ghc-paths:ghc_paths to
# add those files as a dependency and find the correct location.
filegroup(
    name = "library_bundle",
    srcs = select({
        ":v8_0_2_k8": ["//third_party/haskell/ghc/v8_0_2_k8:library-bundle"],
    }),
)

filegroup(
    name = "everything",
    srcs = select({
        ":v8_0_2_k8": ["//third_party/haskell/ghc/v8_0_2_k8:everything"],
    }),
)

filegroup(
    name = "default_packages",
    srcs = [
        ":default_packages_txt",
    ],
)

genrule(
    name = "default_packages_txt",
    srcs = select({
        ":v8_0_2_k8": ["//third_party/haskell/ghc/v8_0_2_k8:default-packages"],
    }),
    outs = ["default-packages.txt"],
    cmd = "cp $< $@",
)

ghc_config(
    name = "config",
    config = select({
        ":v8_0_2_k8": "//third_party/haskell/ghc/v8_0_2_k8:config",
    }),
)
