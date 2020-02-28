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

"""Functions for compiling Haskell source files."""

load("//bzl/private:action.bzl", "multi_action")
load("//bzl/private:archive.bzl", "create_wrapper_shared_cc_lib")
load(
    "//bzl/private:cc.bzl",
    "get_dynamic_libraries",
)
load("//bzl/private:ghc_options.bzl", "ghc_options")
load("//bzl/private:package.bzl", "package")
load("//bzl:settings.bzl", "settings")

_PROFILING_OPTIONS = [
    "-prof",
    "-hisuf p_hi",
    "-osuf p_o",
]

def _package_key_options(metadata):
    """Command-line arguments related to the current package key."""
    return [
        "-this-unit-id",
        metadata.key,
        "-optP-DCURRENT_PACKAGE_KEY=\"{}\"".format(metadata.key),
    ]

def _create_output_dirs(ctx, name):
    """Creates a struct of directories capturing output from GHC."""
    return struct(
        hi = ctx.actions.declare_directory(name + ".interfaces"),
        object = ctx.actions.declare_directory(name + ".objects"),
        stub = ctx.actions.declare_directory(name + ".stub"),
        dump = ctx.actions.declare_directory(name + ".dump"),
        mix = ctx.actions.declare_directory(name + ".mix") if ctx.coverage_instrumented() else None,
    )

def _collect_output_dirs(dirs):
    """Collects the outputs that should be declared for an action."""
    return [d for d in [dirs.hi, dirs.object, dirs.stub, dirs.dump, dirs.mix] if d]

def _output_arguments(dirs, generate_code = True):
    """The command-line arguments controlling GHC outputs."""
    return [
        "-no-link",
        "-hidir",
        dirs.hi.path,
        "-odir",
        dirs.object.path,
        "-stubdir",
        dirs.stub.path,
        "-ddump-to-file",
        # Write output files as:
        # path/to/target.dump/dump-simpl
        "-dumpdir",
        dirs.dump.path,
        "-ddump-file-prefix",
        # No prefix leads to a simpler output path:  "path/to/target.dump/dump-simpl"
        "",
    ] + (
        # Also write interface files when not generating code, even though we're not using them yet.
        # If -fwrite-interface wasn't set, GHC would still write the .hi files to a temporary
        # location: https://gitlab.haskell.org/ghc/ghc/issues/16670
        # Keeping them gives the HaskellCheck action some nontrivial output.
        [] if generate_code else ["-fno-code", "-fwrite-interface"]
    ) + (
        # Builds a directory with a ".mix" extension containing all of the .mix
        # files generated by GHC when HPC is enabled
        # (https://wiki.haskell.org/Haskell_program_coverage).
        ["-fhpc", "-hpcdir", dirs.mix.path] if dirs.mix else []
    )

# RTS flags for compiling Haskell source code.
#
# TODO(judahjacobson): This setting was last tuned in 2017 (cl/153585436);
# see whether it's still appropriate or if there's more tuning we should do
#
# Compilation is a batch workload. We can avoid much of garbage collection
# overhead by using more RAM for generation-0 (aka nursery).
# This change alone speeds building up 15% faster (-c opt mode) for target
# '//third_party/haskell/vector/v0_11_0_0:vector-lib'
#
# For background on why it helps see:
#    https://ghc.haskell.org/trac/ghc/ticket/9221
#    http://trofi.github.io/posts/193-scaling-ghc-make.html
#
# In ghc-8 we could also enable parallel ("-j") building mode.
# But it might make builds unstable from run to run which
# will trigger more rebuilds on forge.
# In ghc-7.10 "-j" has almost no positive effect.
_compile_rts_arguments = ["-A256M"]

def compile_sources(
        ctx,
        toolchains,
        metadata,
        deps,
        srcs_options,
        hidden_modules = []):
    """Run the Haskell compiler to produce a library.

    Args:
      ctx: The current rule context.
      toolchains: A struct as returned by def.bzl:_get_toolchains.
      metadata: A struct of package metadata; see package.metadata() for details.
      deps: A struct() of dependencies for compiling this code, of the form
        returned by collect_deps().
      srcs_options: A struct as returned from compile_srcs_options.get().
      hidden_modules: Modules which should not be exposed by this target,
        assuming that it is a library.

    Returns:
      Returns a struct consisting of the following fields:
        hi: A File reference to the directory of *.hi interface files output by GHC.
        checked_hi: A File reference to the directory of *.hi interface files output by GHC
          during the typecheck-only  step.
        object: A File reference to the directory of *.o object files output by GHC.
        prof: A struct of two fields "hi" and "object", with outputs from the profiling build.
          Will be None if we are not building with profiling.
        stub_headers: A list of .h Files output by the compiler from "foreign export"
          declarations.
        wrapper_lib: A shared library that links against the immediate C++ dependencies.
          Will be None if there are no immediate dependencies.
    """
    these_settings = settings.get(ctx)
    dynamic_libraries = get_dynamic_libraries(ctx, deps.transitive.cc)
    inputs = depset(
        toolchains.haskell.compiler_bundle +
        # Fail to build if the immediate dependencies have incorrect names.
        # (But make sure that checking a library's *own* names happens in parallel with
        # its HaskellCompile step, to reduce build latency.)
        deps.immediate_module_names_ok +
        (dynamic_libraries if srcs_options.needs_dynamic_libraries else []),
        transitive = [
            deps.cc_headers,
            deps.transitive.haskell.shared_libs if srcs_options.needs_dynamic_libraries else depset(),
            deps.plugins.shared_libs,
            srcs_options.inputs,
            toolchains.cc.toolchain.all_files,
        ],
    )

    # If profiling is enabled, create an action for the profiling build as well
    # as the non-profiling "vanilla" build. See comment on _profiling_enabled for
    # reasoning.

    # Pass the current label to the compiler, for better error messages.
    compile_env = {"BLAZE_LABEL": str(ctx.label)}

    rts_args = (
        ["+RTS"] + _compile_rts_arguments + srcs_options.rts_arguments + ["-RTS"]
    ) if srcs_options.arguments else []
    arguments = (ghc_options.library_root(toolchains.haskell) +
                 toolchains.haskell.common_options +
                 _package_key_options(metadata) +
                 srcs_options.arguments +
                 rts_args +
                 [s.path for s in srcs_options.srcs])
    # TODO(judahjacobson): unlike in hrepl, we don't explicitly load the C++ libraries
    # with -l, but rather assume that they'll be loaded transitively by the Haskell
    # libraries that link to them.  Passing -l seems to cause link errors in the indexer
    # in some corner cases.
    # When running TemplateHaskell or plugins for compiling code, we generally aren't
    # calling much (or any) C++, so there isn't as much concern about ODR violations
    # as when executing code in hrepl.

    # Codegen, both for static (.o) and dynamic (.dyn_o).
    vanilla_dirs = _create_output_dirs(ctx, ctx.label.name)
    outputs = _collect_output_dirs(vanilla_dirs)

    # The header stub created if the library has any foreign exports.
    stub_headers = []
    if hasattr(ctx.attr, "foreign_exports"):
        stub_headers.extend([ctx.actions.declare_file(
            "{}.stub/{}_stub.h".format(ctx.label.name, name.replace(".", "/")),
        ) for name in ctx.attr.foreign_exports])
    outputs.extend(stub_headers)

    package_compile = package.compile_options(deps)
    ctx.actions.run(
        executable = toolchains.haskell.compiler,
        arguments = arguments + ["-dynamic-too"] + _output_arguments(vanilla_dirs) +
                    these_settings.ghcopts +
                    package_compile.arguments,
        outputs = outputs,
        inputs = depset(transitive = [
            inputs,
            package_compile.inputs,
        ]),
        progress_message = "Compiling Haskell files for " + str(ctx.label),
        mnemonic = "HaskellCompile",
        env = compile_env,
    )

    # Just type-checking, without codegen.
    checked_dirs = _create_output_dirs(ctx, ctx.label.name + ".check")
    package_check = (
        package.compile_options(deps) if srcs_options.needs_dynamic_libraries else package.check_options(deps)
    )
    ctx.actions.run(
        executable = toolchains.haskell.compiler,
        arguments = arguments +
                    _output_arguments(checked_dirs, generate_code = False) +
                    these_settings.ghcopts +
                    package_check.arguments,
        outputs = _collect_output_dirs(checked_dirs),
        inputs = depset(transitive = [
            inputs,
            package_check.inputs,
        ]),
        progress_message = "Checking Haskell files for " + str(ctx.label),
        mnemonic = "HaskellCheck",
        env = compile_env,
    )

    prof = None
    if these_settings.profile:
        prof_dirs = _create_output_dirs(ctx, ctx.label.name + ".prof")
        prof = struct(
            hi = prof_dirs.hi,
            object = prof_dirs.object,
        )
        multi_action(
            ctx = ctx,
            actions = [
                # Copy all of the object files from the vanilla build into the
                # output directory of the profiling build.
                # If one source file using TemplateHaskell depends on another
                # in the same library, GHC will load the ".dyn_o" file from the latter.
                # It looks for the object files in -odir (the output of this rule).
                # Unfortunately, for profiling builds, the dyn_o is in the output of the
                # *vanilla* build.  And, unlike for hi files, there doesn't seem to be
                # a way to tell GHC to look in another location.
                # (The object files are not part of the final build output, so adding to
                # the outputs here is fairly benign.)
                struct(
                    cmd = "/bin/cp",
                    args = ["-R", vanilla_dirs.object.path + "/.", prof_dirs.object.path],
                ),
                struct(
                    cmd = toolchains.haskell.compiler,
                    args = arguments + _output_arguments(prof_dirs) +
                           package_compile.arguments +
                           _PROFILING_OPTIONS +
                           these_settings.ghcopts + these_settings.profile_ghcopts +
                           ["-i" + vanilla_dirs.hi.path],
                ),
            ],
            outputs = _collect_output_dirs(prof_dirs),
            inputs = depset(
                [vanilla_dirs.hi, vanilla_dirs.object] + toolchains.haskell.prof_bundle,
                transitive = [
                    inputs,
                    package_compile.inputs,
                ],
            ),
            progress_message = "Compiling Haskell files with profiling for %s" % str(ctx.label),
            mnemonic = "HaskellCompileProf",
            env = compile_env,
        )

    wrapper_lib = create_wrapper_shared_cc_lib(
        ctx,
        toolchains.cc,
        "{}_cc_wrapper/lib{}_cc_wrapper.so".format(ctx.label.name, metadata.key),
        deps.immediate_cc_dylibs,
    ) if deps.immediate_cc_dylibs else None

    return struct(
        hi = vanilla_dirs.hi,
        checked_hi = checked_dirs.hi,
        object = vanilla_dirs.object,
        prof = prof,
        stub_headers = stub_headers,
        wrapper_lib = wrapper_lib,
    )