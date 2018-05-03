# Description: GHC binaries to be used by bazel.
#
# The Haskell compiler in fact has the BSD-like Glasgow Haskell Compiler
# License. However, its outputs are unencumbered, but the output_licenses
# declaration does not work, because the Haskell compiler needs to be in the
# target configuration, although it is a tool. Thus, the license is set to
# unencumbered and this big warning is put here.
load("//bzl:config.bzl", "ghc_config")

licenses(["unencumbered"])

exports_files(["LICENSE"])

# To avoid unwanted dependencies, we declare a visibility that only allows
# //third_party/haskell/ghc to access and forward these targets.
package(
    default_visibility = ["//visibility:legacy_public"],  # FIXME
)

package_group(
    name = "main_ghc",
    packages = ["//third_party/haskell/ghc"],
)

config_setting(
    name = "prof",
    values = {"define": "prof=true"},
)

external_headers = [
]

cc_library(
    name = "headers",
    hdrs = external_headers,
    includes = [
    ],
    textual_hdrs = glob(
        [
        ],
        exclude = external_headers,
    ),
    #visibility = [":main_ghc"],  # FIXME
)

# The whole of GHC for all configurations. This is the target
# directly referenced by bazel (by way of ghc_top flag). The other
# targets are located by doing lookups relative to this target.
# The inclusion of all the bundles here is important, the bazel
# tests depend on this target.
filegroup(
    name = "everything",
    srcs = [
        ":binary-bundle",
        ":default-packages",
        ":ghc",
        ":ghc-pkg",
        ":ghc-pkg-bundle",
        ":haddock_interfaces",
        ":hsc-template",
        ":hsc2hs",
        ":hsc2hs-bundle",
        ":library-bundle",
    ],
    visibility = [
        ":main_ghc",
    ],
)

filegroup(
    name = "haddock_interfaces",
    srcs = glob(["**/*.haddock"]),
)

sh_binary(
    name = "ghc",
    srcs = ["bin/ghc"],
    data = [
        ":binary-bundle",
    ],
)

sh_binary(
    name = "ghc-pkg",
    srcs = ["bin/ghc-pkg"],
    data = [":ghc-pkg-bundle"],
)

sh_binary(
    name = "hsc2hs",
    srcs = ["hsc2hs.sh"],
    data = [":hsc2hs-bundle"],
)

filegroup(
    name = "default-packages",
    srcs = ["default-packages.txt"],
    visibility = ["//third_party/haskell/ghc:__pkg__"],
)

filegroup(
    name = "hsc-template",
    srcs = [],
    data = [":hsc2hs-bundle"],
)

filegroup(
    name = "common-bundle",
    srcs = [
    ] + select({
        # Include profiling enabled files (*.p_hi and *_p.a) only for profiling
        # builds.
        ":prof": [
        ] + glob(
            [
                "**/HS*.o",
                "**/*.hi",
                "**/*.p_hi",
                "**/*.h",
                "**/*.a",
                "lib/ghc-8.0.2/package.conf.d/**",
            ],
            exclude = [],
        ),
        "//conditions:default": glob(
            [
                "**/HS*.o",
                "**/*.hi",
                "**/*.h",
                "**/*.a",
                "lib/ghc-8.0.2/package.conf.d/**",
            ],
            exclude = ["**/*_p.a"],
        ),
    }),
)

filegroup(
    name = "binary-bundle",
    srcs = [":common-bundle"],
)

filegroup(
    name = "library-bundle",
    srcs = [
        ":common-bundle",
    ],
    visibility = [":main_ghc"],
)

filegroup(
    name = "ghc-pkg-bundle",
    srcs = [
    ] + glob(["lib/ghc-8.0.2/package.conf.d/**"]),
)

filegroup(
    name = "hsc2hs-bundle",
    srcs = [
        ":common-bundle",
    ],
)

ghc_config(
    name = "config",
    binutils_path = "third_party/compilers",
    compiler = ":ghc",
    compiler_bundle = [
        ":common-bundle",
        ":ghc-pkg-bundle",  # TODO
    ],
    default_options = [
        "-Wall",
        "-Werror",
        "-Wno-missing-signatures",
        "-Wno-type-defaults",
        "-XNoForeignFunctionInterface",
        "-XScopedTypeVariables",
    ],
    default_packages = ":default-packages",
    ghc_pkg = ":ghc-pkg",
    ghc_pkg_bundle = [":ghc-pkg-bundle"],
    hsc2hs = ":hsc2hs",
    hsc2hs_template = "lib/ghc-8.0.2/template-hsc.h",
    library_bundle = [":library-bundle"],
    version = 802,
    visibility = [":main_ghc"],
)
