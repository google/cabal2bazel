# Copyright 2020 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Functions for passing arguments to GHC."""

load(
    "//bzl/private:cc.bzl",
    "compile_command",
)

def _ghc_options_library_root(config):
    """Options that should be passed to command-line invocations of GHC.

    They are not used by the GHC API, so should not be included in the
    haskell.CompileInfo.option proto field.

    In normal GHC installations, these arguments are passed by the
    $PREFIX/bin/ghc shell script.  However, to automate the installation
    and configuration of GHC, we don't use that script and call
    $PREFIX/lib/ghc-*/bin/ghc directly.

    Args:
      config: A HaskellCompilerInfo provider.

    Returns:
      A list of string arguments to be passed to GHC.
    """

    return [
        "-B" + config.library_root,
    ]

def _ghc_options_common(cc_toolchain, compilation_mode, global_package_ids, default_options):
    """Options that should be passed to all invocations of GHC."""
    cc = compile_command(
        cc_toolchain,
        # Since the compiler and assembler will only be looking
        # at generated code, any warnings will be useless noise.
        # In particular, generated FFI stub code causes warnings
        # that are turned into errors with the default C
        # compilation flags! So we suppress as many warnings as we
        # can, and make the remaining ones no longer errors.
        user_compile_flags = ["-w", "-Wno-error"],
    )

    # TODO(judahjacobson): Some of these are redundant with cc.arguments
    options = [
        "-optl-no-canonical-prefixes",
        "-optl--sysroot=" + cc_toolchain.toolchain.sysroot,
        "-v0",  # Keep silent (unless later -v options override this)
        "-pgmP",
        cc.executable,
        "-pgmc",
        cc.executable,
        "-pgma",
        cc.executable,
        "-pgml",
        cc.executable,
        "-no-user-package-db",
        # Always build with -fPIC to enable --config=hardened and, eventually,
        # asan/tsan/etc. (b/24772551)  Note that we also build the compiler
        # itself with -fPIC.
        "-fPIC",
        "-optc-fPIC",
        "-fdiagnostics-color=always",
        # Fail on invalid Haddock comments:
        "-haddock",
    ]
    if compilation_mode == "opt":
        options += ["-O2"]

    options += ["-opta" + o for o in cc.arguments]
    options += ["-optc" + o for o in cc.arguments]

    # Force the compiler to run in preprocessor mode when invoked as such:
    options += ["-optP" + o for o in ["-E", "-undef", "-traditional"]]

    # Disable debug symbols to make the compiler output more deterministic.
    # The temporary directory used by some intermediate files contains the PID;
    # for example, "/tmp/ghc9421_0/ghc_4.c".  The basename "ghc_4.c" is
    # deterministic, but the temporary directory is not and may be
    # included in the output file's debug information.
    # For details, see:
    # https://git.haskell.org/ghc.git/commitdiff/7a82b77691fb90c4af6863673f10e454a449739e
    options += ["-optc-g0"]

    # TODO(judahjacobson): Pass more specific link-time options to clang
    options += ["-optl-Wno-unused-command-line-argument"]

    # Prevent peeking into the current directory.  (Ordered here to match the
    # previous native rules; it can also probably go earlier.)
    options += ["-i"]

    # Make our build rules "strict", by requiring any imported modules to be
    # from packages explicitly specified with the -package-id flag.
    # This flag also makes GHC allow exposing two packages of the same name,
    # which would ...
    options.append("-hide-all-packages")

    # Custom flags for this version of GHC.
    options += default_options

    # Prevent clang from complaining about a file that already has debug info in
    # it.  This option needs to go last in order to override earlier -g flags.
    options += ["-opta-g0"]

    return options

ghc_options = struct(
    common = _ghc_options_common,
    library_root = _ghc_options_library_root,
)
