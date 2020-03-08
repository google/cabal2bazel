"""Definitions to support multiple GHC versions.
"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")

# The list of wired-up GHC builds.
# Note that two builds of GHC may have the same major version number;
# for example, if we're doing a minor version bump, or if we
# built GHC with a different configuration.
_GHC_VERSIONS = {
    "8.6": struct(
        major_version = "8.6",
        package = "//third_party/haskell/ghc/v8_6_4",
        src_package = "//third_party/haskell/ghc/vendor_src/v8_6_4",
    ),
}

# The build of GHC that we will use without any explicit "--define GHC=..."
_DEFAULT_VERSION = "8.6"

def ghc_config_settings():
    """Function to generate config_setting rules."""
    native.config_setting(
        name = "v8.6",
        define_values = {
            "GHC": "8.6",
        },
    )

# A map keyed by config_settings (as defined by ghc_config_settings())
# which can be used to construct a select() statement.
GHC_VERSION_SETTINGS = dicts.add(
    {
        "//third_party/haskell/ghc:v" + k: _GHC_VERSIONS[k]
        for k in _GHC_VERSIONS
    },
    # Blaze doesn't have a defaulting mechanism for "--define",
    # so add an explicit entry to this map.
    {"//conditions:default": _GHC_VERSIONS[_DEFAULT_VERSION]},
)

def select_by_ghc_version(version_map):
    """Constructs a select() statement for different GHC versions.

    Useful for third-party packages that need to choose their own
    version based on the version of GHC.

    This function only allows selecting by major GHC version,
    since minor versions are generally API-compatible with each other.

    The map must specify a value for the default GHC version.  Any other GHC
    versions not explicitly specified will also use that default value.

    Example:

        haskell_library(
            name = "singletons",
            deps = select_by_ghc_version({
                "8.4": ["//third_party/haskell/singletons/v2_4_1:singletons"],
                "8.0": ["//third_party/haskell/singletons/v2_2_2:singletons"],
            }),
        )

    Args:
      version_map: A map keyed by strings which are major GHC versions (e.g., "8.6").

    Returns:
      A select() statement with one entry for each built GHC.
    """
    return select(dict_by_ghc_version(version_map))

def dict_by_ghc_version(version_map):
    return {
        k: version_map.get(
            GHC_VERSION_SETTINGS[k].major_version,
            version_map[_DEFAULT_VERSION],
        )
        for k in GHC_VERSION_SETTINGS
    }

def unless_ghc_version(version):
    """Tests whether the given GHC version is the default.

    Useful for marking targets as "disabled" so TAP doesn't build them.

    Example usage:

      cabal_haskell_package(
          description = description,
          disabled = unless_ghc_version("8.6"),
      )

    Args:
      version: A string; GHC major version; e.g., "8.6".

    Returns:
      A boolean; True iff the given version is *not* the default.
    """

    # Sanity-check that this is a know version.
    if version not in _GHC_VERSIONS:
        fail("Unknown version " + version)
    return _DEFAULT_VERSION != version
