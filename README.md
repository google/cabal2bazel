# cabal2bazel

A tool to help with fetching Cabal packages from Hackage and importing
them as packages into cabal2bazel.


Usage:
  cabal2bazel --fetch <cabal-package-identifier>

Examples:
  cabal2bazel --fetch binary
  cabal2bazel --fetch binary-0.5.0.2
  cabal2bazel --recursive --fetch lens

The process for installing third-party Haskell packages is specified at:
  https://opensource.google.com/docs/thirdparty/haskell/
cabal2bazel does not (yet) capture all the subtleties of this process,
and is no substitute for code review. But it will automate the bulk of
chores, including:

  - fetching the Cabal package;
  - unpacking it into an appropriate directory;
  - ensuring the license file is in the expected location;
  - generating a BUILD file with suitable rules.
  - if --recursive is specified, traversing graph of all dependencies of
    selected package and importing all of them.

cabal2bazel attempts to map the dependencies of a Cabal library or
executable to the 'deps' attribute of the resulting Bazel target.

Things that still need manual attention include:
  - optimization modes need to be removed from ghcopts, because Bazel
    chooses them itself;
  - cabal2bazel adds '-Wwarn' to all rules' ghcopts; this might not be
    desired;
  - correct choice of license attributes should be checked;
  - generated dependencies might need to be adjusted to point to specific
    versions of the packages depended upon.

NOTE: You will need an appropriate version of 'cabal' to be available
on your path; cabal2bazel uses it for downloading and unpacking packages.

NOTE: Sometimes it is necessary to edit the cabal file and re-run
cabal2bazel. In such cases rerun cabal2bazel as follows to resume
installation:

  cabal2bazel --wire-up <package-name>

Example:
  cabal2bazel --wire-up //third_party/haskell/binary/v0_5_0_2

