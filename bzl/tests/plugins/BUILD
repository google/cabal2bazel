load(
    "//bzl:def.bzl",
    "haskell_library",
    "haskell_test",
)

haskell_test(
    name = "PluginsTest",
    srcs = ["PluginsTest.hs"],
    plugins = [
        # TODO: ghc-typelits-knownnat
    ],
)

haskell_library(
    name = "PluginsLib",
    srcs = ["PluginsLib.hs"],
    plugins = [
        # TODO: ghc-typelits-knownnat
    ],
)

haskell_test(
    name = "PluginsLibTest",
    srcs = ["PluginsLibTest.hs"],
    deps = [":PluginsLib"],
)
