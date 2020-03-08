# Description: GHC binaries to be used by blaze.

load("//bzl:config.bzl", "ghc_config")
load(":global_packages.bzl", "global_packages")

licenses(["restricted"])

exports_files([
    "LICENSE",
    "global_packages.bzl",
])

_GHC_ROOT = "lib/ghc-8.6.4/"

# Used by unix-compat:
_UNIX_PACKAGE_INCLUDE = _GHC_ROOT + "unix-2.7.2.2/include/"

external_headers = [
    _GHC_ROOT + "include/HsFFI.h",
    _GHC_ROOT + "include/MachDeps.h",
]

cc_library(
    name = "headers",
    hdrs = external_headers,
    includes = [
        _GHC_ROOT + "include",
        _UNIX_PACKAGE_INCLUDE,
    ],
    textual_hdrs = glob(
        [
            _GHC_ROOT + "include/**/*.h",
            _UNIX_PACKAGE_INCLUDE + "*.h",
        ],
        exclude = external_headers,
    ),
    visibility = ["//third_party/haskell/ghc:main_ghc"],
)

# The whole of GHC for all configurations. This is the target
# directly referenced by blaze (by way of ghc_top flag). The other
# targets are located by doing lookups relative to this target.
# The inclusion of all the bundles here is important, the blaze
# tests depend on this target.
filegroup(
    name = "everything",
    srcs = [
        ":common-bundle",
        ":ghc-pkg-bundle",
        ":haddock_interfaces",
        ":hsc-template",
        ":hsc2hs-bundle",
        ":library-bundle",
    ],
    visibility = [
        "//third_party/haskell/ghc:main_ghc",
    ],
)

filegroup(
    name = "haddock_support",
    srcs = glob([
        "docs/libraries/*.png",
        "docs/libraries/*.gif",
        "docs/libraries/*.css",
        "docs/libraries/*.js",
    ]),
    visibility = ["//third_party/haskell/ghc:main_ghc"],
)

filegroup(
    name = "docs",
    srcs = glob(["docs/**/*"]),
    visibility = ["//third_party/haskell/ghc:main_ghc"],
)

filegroup(
    name = "haddock_interfaces",
    srcs = glob(["docs/libraries/**/*.haddock"]),
)

filegroup(
    name = "hsc-template",
    srcs = [_GHC_ROOT + "template-hsc.h"],
    data = [":hsc2hs-bundle"],
)

filegroup(
    name = "settings",
    srcs = [
        _GHC_ROOT + "llvm-passes",
        _GHC_ROOT + "llvm-targets",
        _GHC_ROOT + "platformConstants",
        _GHC_ROOT + "settings",
    ],
    visibility = ["//third_party/haskell/ghc:main_ghc"],
)

filegroup(
    name = "common-bundle",
    srcs = [
        _GHC_ROOT + "bin/ghc",
        _GHC_ROOT + "bin/ghc-pkg",
        _GHC_ROOT + "bin/hsc2hs",
        _GHC_ROOT + "ghc-usage.txt",
        _GHC_ROOT + "bin/unlit",
        _GHC_ROOT + "ghci-usage.txt",
        ":settings",
    ] + glob(
        [
            "**/HS*.o",
            "**/*.hi",
            "**/*.dyn_hi",
            "**/*.h",
            "**/*.a",
            "**/*.so",
            "**/*.so.*",
            _GHC_ROOT + "package.conf.d/**",
        ],
        exclude = ["**/*_p.a"],
    ),
    visibility = ["//third_party/haskell/ghc:main_ghc"],
)

filegroup(
    name = "prof-bundle",
    srcs = [
        # Uses -fexternal-interpreter when compiling with -prof.
        _GHC_ROOT + "bin/ghc-iserv",
        _GHC_ROOT + "bin/ghc-iserv-prof",
    ] + glob(
        [
            "**/*.p_hi",
            "**/*_p.a",
        ],
    ),
)

filegroup(
    name = "library-bundle",
    srcs = [
        ":common-bundle",
    ],
    visibility = ["//third_party/haskell/ghc:main_ghc"],
)

filegroup(
    name = "ghc-pkg-bundle",
    srcs = glob([
        _GHC_ROOT + "package.conf.d/**",
        # ghc-pkg is dynamically linked against these libraries.
        _GHC_ROOT + "**/*.so",
        _GHC_ROOT + "**/*.so.*",
    ]),
)

filegroup(
    name = "shared_libraries",
    srcs = glob([
        "**/*.so",
        "**/*.so.*",
    ]),
)

filegroup(
    name = "hsc2hs-bundle",
    srcs = [
        _GHC_ROOT + "bin/hsc2hs",
        _GHC_ROOT + "template-hsc.h",
        ":common-bundle",
    ],
)

ghc_config(
    name = "config",
    compiler = _GHC_ROOT + "bin/ghc",
    compiler_bundle = [
        ":common-bundle",
        ":ghc-pkg-bundle",
    ],
    default_options = [
        "-Wall",
        "-Werror",
        "-Wno-missing-signatures",
        "-Wno-type-defaults",
        "-Wno-trustworthy-safe",
        "-Wincomplete-uni-patterns",
        "-Wincomplete-record-updates",
        "-XNoForeignFunctionInterface",
        "-XScopedTypeVariables",
        # Turn off StarIsType, which appears to be the long-term
        # goal of GHC.  We can use Data.Kind.Type in our code instead
        # of "*".  This approach also lets us write "a * b" at the type level
        # unambiguously.
        "-XNoStarIsType",
    ],
    docdir = package_name() + "/docs/libraries",
    ghc_pkg = _GHC_ROOT + "bin/ghc-pkg",
    ghc_pkg_bundle = [":ghc-pkg-bundle"],
    global_packages = global_packages,
    hsc2hs = _GHC_ROOT + "bin/hsc2hs",
    hsc2hs_template = _GHC_ROOT + "template-hsc.h",
    library_bundle = [":library-bundle"],
    library_root = package_name() + "/" + _GHC_ROOT,
    prof_bundle = [":prof-bundle"],
    shared_libraries = [":shared_libraries"],
    unlit = _GHC_ROOT + "bin/unlit",
    version = "8.6.4",
    visibility = ["//third_party/haskell/ghc:main_ghc"],
)
