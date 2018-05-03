# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Skylark rules for building Haskell code.

This file contains BUILD rules to compile Haskell libraries and binaries.
See the "Args" section of each build rule for info about how to use them.

To use these rules, add a load() statement at the top of your BUILD file; for
example:
load("//bzl:def.bzl", "haskell_library", "haskell_binary")

Currently supported rules:

- haskell_library
- haskell_binary
- haskell_test
- haskell_proto_library

For usage information, see the Args documentation for the definitions of those
rules.

To enable profiling for a build, set the "prof" define variable to true, e.g.

  bazel build --define prof=true //path/to:target
"""

# This extension often writes InstalledPackageInfo files (*.spec) as an
# intermediate step of registering GHC packages. Documentation is available
# here:
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#installed-pkg-info

load("@bazel_skylib//:lib.bzl", "dicts")
load("//bzl:action.bzl", "multi_action", "archiving_action", "new_archive", "unarchived_path")
load("//bzl:metadata.bzl", "package_key")
load("//bzl:providers.bzl", "add_cc_lib", "collect_deps", "forward_deps", "fix_aspect_providers", "new_transitive")

def _output_name(ctx):
  """Get the name prefix for output files for this rule.

  This is usually ctx.label.name, except for proto aspects which are tagged by
  their API version.

  Args:
    ctx: The current rule context.
  Returns:
    A string prefix for output file names.
  """
  if hasattr(ctx.attr, "_api"):
    return ctx.label.name + "." + ctx.attr._api
  else:
    return ctx.label.name

def _get_preprocessor(ctx):
  """Retrieve the "preprocessor" attribute, or return None if it doesn't exist.

  We need to check whether it exists, since some functions run inside
  haskell_proto_aspect which can't define it as an attribute.

  Args:
    ctx: The current rule context.
  Returns:
    The "preprocessor" attribute, or None if it doesn't exist.
  """
  if hasattr(ctx.executable, "preprocessor"):
    return ctx.executable.preprocessor
  else:
    return None

# Getting GHC to link against FFI code is tricky; for example:
# - Two different Haskell packages might link the same cc_library, which
#   could cause a "duplicate symbol" error
# - Two different cc_libraries in different directories might have the same
#   name, which makes it hard to pick both of them up with the "-L" and "-l"
#   flags.
# To solve this, we make every haskell library depend on a dummy "ffi-deps" GHC
# package.  When running GHC, we use the "-package-db" flag to swap in
# different versions of "ffi-deps" for different rule types:
# - When building libraries, we don't do any linking; so "ffi-deps" is a
#   trivallly empty package.
# - When building binaries, "ffi-deps" contains "ld-options" with all the
#   cc_library dependencies and the various linker flags.
# - When running gghci, "ffi-deps" links against an .so with all the
#   (transitive) cc_library dependencies linked together.
# TODO(judahjacobson): Investigate using this to support Template Haskell.  We
# should try to avoid linking a new .so file for each Haskell rule whether or
# not it's actually needed, since that can slow things down if there's a chain
# of transitive rules.  (Note that GHCi and Template Haskell can in fact link
# against .a files, but GHC gets confused by "alwayslink" libraries like //base
# which have the extension ".lo".)
_FFI_DEPS_PACKAGE_NAME = "ffi-deps"

# File extension types of Haskell source files.
_HASKELL_SOURCE_BOOT = [
    ".hs-boot",
    ".lhs-boot",
]

_HASKELL_SOURCE_NONBOOT = [
    ".hs",
    ".lhs",
]

_HASKELL_SOURCE = _HASKELL_SOURCE_BOOT + _HASKELL_SOURCE_NONBOOT

def filter_files(extensions, files):
  """Filters a list of files based on a list of allowed extensions."""
  filtered_files = []
  for f in files:
    for extension in extensions:
      if f.basename.endswith(extension):
        filtered_files.append(f)
        break

  return filtered_files

_PROFILING_OPTIONS = [
    "-prof",
    "-fprof-auto",
    "-hisuf p_hi",
    "-osuf p_o",
]

def _profiling_enabled(ctx):
  """Whether profiling is enabled for 'ctx'.

  Notes on profiling support in general:

  Template Haskell always uses the version of libraries compiled without
  profiling enabled, so we must build both profiling and non-profiling verions
  of libraries when profiling is enabled.
  """
  return "prof" in ctx.var and ctx.var["prof"] == "true"

def _common_ghc_options(ctx, package_name, ghcopts, deps, srcs, profiling):
  """Options that should be passed to all invocations of GHC."""
  cpp = ctx.fragments.cpp
  # TODO: hsc2hs compiles a C file and runs the
  # executable to fill in placeholders in the generated .hs file.
  # For a cross compiler, we should use a HOST=TARGET=x86_64 C compiler.
  # cc = str(cpp.compiler_executable)

  options = [
      "-optc--sysroot=" + str(cpp.sysroot),
      "-v0",  # Keep silent (unless later -v options override this)
      # We need to remove -pass-exit-codes (unknown to clang (driver_is_not_gcc))
      # from "Response Files" (@/tmp/ghcXXXX_X/ghc_X.rsp) produced by GHC before
      # calling driver_is_not_gcc. Since driver_is_not_gcc process command line
      # options but not response files, ghc-8.0.2/bin/ghc invokes
      # ghc-8.0.2/bin/driver_is_not_gcc_wrapper.sh to do the removal.
      # "-pgmP", cc + " -E -undef -traditional",
      # "-pgmc", cc,
      # "-pgma", cc,
      # "-pgml", cc,
      "-no-user-package-db",
      "-package-name", package_name,
      # Always build with -fPIC to enable asan/tsan/etc.  Note that we
      # also build the compiler itself with -fPIC.
      "-fPIC",
      "-optc=-fPIC",
      "-rtsopts"
  ]
  preprocessor = _get_preprocessor(ctx)
  if preprocessor:
    options += ["-F", "-pgmF", preprocessor.path]
  if ctx.var["COMPILATION_MODE"] == "opt":
    options += ["-O2"]
  if profiling:
    options += _PROFILING_OPTIONS

  compiler_options = (cpp.compiler_options([])
                      + cpp.unfiltered_compiler_options([])
                      # Since the compiler and assembler will only be looking
                      # at generated code, any warnings will be useless noise.
                      # In particular, generated FFI stub code causes warnings
                      # that are turned into errors with the default C
                      # compilation flags! So we suppress as many warnings as we
                      # can, and make the remaining ones no longer errors.
                      + ["-w", "-Wno-error"])
  options += ["-opta" + o for o in compiler_options]
  options += ["-optc" + o for o in compiler_options]

  # Prevent peeking into the current directory.  (Ordered here to match the
  # previous native rules; it can also probably go earlier.)
  options += ["-i"]

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
  # will trigger more rebuilds.
  # In ghc-7.10 "-j" has almost no positive effect.
  # Most RTS options are disabled in Stage1Only cross compilers.
  if cpp.cpu == "k8":
    options += ["+RTS", "-A256M", "-RTS"]

  # Input Files related to the given libraries.  This includes the GHC package
  # cache and the *.hi files; other files used by GHC (e.g., during linking)
  # should be passed in via the "inputs" argument.
  for cache in deps.transitive.caches:
    options += ["-package-db", cache.dirname]
  for p in deps.exposed_package_names:
    options += ["-package", p]

  # If this binary doesn't depend on any haskell_library rules, explicitly
  # specify the "ffi-deps" package to make GHC link against it.
  # (If it does depend on a haskell_library, that will pull in ffi-deps
  # automatically, since every haskell_library always depends on ffi-deps.)
  if not deps.transitive.hs_libs:
    options += ["-package", _FFI_DEPS_PACKAGE_NAME]

  options += ["-optP" + f for flag in deps.cpp_flags for f in flag.split(" ")]

  # Custom flags for this version of GHC.
  options += ctx.attr._ghc_config.haskell_config.default_options

  # Custom GHC flags for this build rule.  Put them last so they can override
  # the above options.
  options += [o for opt in ghcopts
              for o in ctx.tokenize(
                  ctx.expand_make_variables(
                      "ghcopts",
                      ctx.expand_location(
                          opt,
                          getattr(ctx.attr, "extra_src_files", [])),
                      {}))]

  # Add source files.
  if not filter_files(_HASKELL_SOURCE_BOOT, srcs):
    # No hs-boot files; just add the srcs.
    options += [s.path for s in srcs]
  else:
    # GHC expects us to pass in the non-boot files (*.hs), and point it to the
    # right directory so it can load the boot files (*hs-boot) automatically
    # based on {-# SOURCE #-} pragmas in the module imports.
    #
    # NOTE: Using "-i{path/to/package}" here is not hermetic when run with
    # --noforge; specifically, we might pick up a .hs-boot file that wasn't
    # specified in the srcs.  It's probably not worth making the build more
    # complicated to handle that case, since hs-boot files are rare.
    srcs_nonboot = filter_files(_HASKELL_SOURCE_NONBOOT, srcs)
    options += ["-i" + ctx.label.package] + [s.path for s in srcs_nonboot]

  # Prevent clang from complaining about a file that already has debug info in
  # it.  This option needs to go last in order to override earlier -g flags.
  options += ["-opta-g0"]

  return options

def _compile_library(ctx, srcs=[], src_archive=None, ghcopts=[], deps=struct()):
  """Run the Haskell compiler to produce a library.

  Args:
    ctx: The current rule context.
    srcs: A list of source Files.
    src_archive: An archive containing source files (optional).
    ghcopts: Additional command-line options to pass to GHC.
    deps: A struct() of dependencies for compiling this code, of the form
      returned by collect_deps().

  Returns:
    Returns a struct consisting of the following fields:
      hi: An archive File for the *.hi interface files output by GHC.
      object: An archive File for the *.o object files output by GHC.
      compile_info: A proto File with information about the compile action,
        stored in a haskell.CompileInfo protocol buffer as text format.
  """
  hi_archive = new_archive(ctx, _output_name(ctx) + ".interfaces")
  object_archive = new_archive(ctx, _output_name(ctx) + ".objects")

  ffi_package_spec = ctx.new_file(_output_name(ctx) + ".ffi-deps-input.spec")
  ctx.file_action(
      output=ffi_package_spec,
      content="\n".join([
          "name: " + _FFI_DEPS_PACKAGE_NAME,
          "id: " + _FFI_DEPS_PACKAGE_NAME,
          "key: " + _FFI_DEPS_PACKAGE_NAME,
      ]))
  ffi_package_cache = _register_package(ctx, _output_name(ctx) + ".ffi-deps",
                                        ffi_package_spec)

  input_archives = list(deps.transitive.hi_archives)

  lib_options = []
  if src_archive:
    input_archives += [src_archive]
    lib_options += [unarchived_path(src_archive.path) + "/**/*.hs"]

  lib_options += [
      "-hidir", unarchived_path(hi_archive.path),
      "-odir", unarchived_path(object_archive.path),
      # Stub files are currently not used, but redirect them so they don't
      # appear in the middle of the source tree with --noforge.
      "-stubdir", (ctx.bin_dir.path + "/" + ctx.label.package
                   + "/" + _output_name(ctx) + ".stub"),
      "-package-db", ffi_package_cache.dirname,
  ]

  config = ctx.attr._ghc_config.haskell_config
  preprocessor = _get_preprocessor(ctx)

  # If profiling is enabled, create an action for the profiling build as well
  # as the non-profiling build. See comment on _profiling_enabled for
  # reasoning.
  non_profiling_common_options = _common_ghc_options(
      ctx, package_key(ctx), ghcopts, deps, srcs, profiling=False)
  option_sets = [non_profiling_common_options + lib_options]
  if _profiling_enabled(ctx):
    profiling_common_options = _common_ghc_options(
        ctx, package_key(ctx), ghcopts, deps, srcs, profiling=True)
    option_sets += [profiling_common_options + lib_options]
    options = profiling_common_options
  else:
    options = non_profiling_common_options

  archiving_action(
      ctx=ctx,
      input_archives=input_archives,
      output_archives=[hi_archive, object_archive],
      actions=(([struct(cmd="shopt -s globstar")] if src_archive else [])
               +[struct(cmd=config.compiler, args=o) for o in option_sets]),
      # TODO: Remove the list() and just pass sets in directly.
      inputs=(config.compiler_bundle
              + list(deps.cc_headers
                     | deps.transitive.caches
                     | deps.transitive.hs_libs
                     | getattr(ctx.files, "extra_src_files", [])
                    )
              + srcs
              + [ffi_package_cache]
              + ([preprocessor] if preprocessor else [])),
      progress_message="Compiling Haskell files for %s" % str(ctx.label),
      mnemonic="HaskellCompile",
  )

  return struct(
      hi=hi_archive,
      object=object_archive,
      compile_info_files=_compile_info_files(
          ctx=ctx,
          extra_input_archives=[src_archive] if src_archive else [],
          deps=deps,
          srcs=srcs,
          options=options),
      dummy_ffi_deps_cache=ffi_package_cache)

def _compile_binary(ctx, out, srcs, deps):
  """Run the Haskell compiler to produce a binary file.

  Args:
    ctx: The current rule context.
    out: The output binary File.
    srcs: A list of source Files.
    deps: A struct() of dependencies for compiling this code, of the form
      returned by collect_deps().
  """
  common_options = _common_ghc_options(
      ctx, "main", ctx.attr.ghcopts, deps, srcs,
      profiling=_profiling_enabled(ctx))
  options = list(common_options)

  ffi_deps = _static_ffi_deps(ctx, deps)
  options += ["-package-db", ffi_deps.package_db]

  if ctx.attr.threaded:
    options += ["-threaded"]
  if ctx.attr.main_is:
    options += ["-main-is", ctx.attr.main_is]

  # Redirect temporary files (e.g., .hi and .o) to a separate folder.
  # We don't use them, but this prevents GHC from writing them next to the
  # source files when --noforge is set.
  options += ["-outputdir", ctx.bin_dir.path + "/" + ctx.label.package
              + "/" + ctx.label.name + ".output"]

  config = ctx.attr._ghc_config.haskell_config
  preprocessor = _get_preprocessor(ctx)

  input_archives = list(deps.transitive.hi_archives)

  archiving_action(
      ctx=ctx,
      input_archives=input_archives,
      actions=[
          struct(cmd=config.compiler,
                 args=options + ["-o", out.path])],
      # TODO: Remove the list() and just pass sets in directly.
      inputs=(config.compiler_bundle + srcs
              + list(deps.transitive.caches | deps.transitive.cc_libs
                     | deps.transitive.hs_libs
                     | deps.cc_headers
                     | ctx.files.extra_src_files
                     | ffi_deps.files)
              + ([preprocessor] if preprocessor else [])),
      progress_message="Compiling Haskell binary %s" % str(ctx.label),
      mnemonic="HaskellCompile",
      outputs=[out],
  )

  return _compile_info_files(
      ctx=ctx,
      deps=deps,
      srcs=srcs,
      options=common_options)

def _register_package(ctx, package_db_name, package_spec):
  """Creates and registers a GHC package for the current target.

  Creates a new GHC package database and registers the given package
  (as specified by package_spec) as the only package in that database.

  Args:
    ctx: The current context.
    package_db_name: The name of the output GHC package database.
    package_spec: A File containing the human-readable "spec" for this GHC
      package.

  Returns:
    Returns the binary file containing the package DB.  It will be of the form
    ".../{package_db_name}/package.cache".  To get the actual package DB, call
    "dirname" on the cache file.
  """
  cache = ctx.new_file(package_db_name + "/package.cache")
  package_db = cache.dirname
  config = ctx.attr._ghc_config.haskell_config
  ghc_pkg = config.ghc_pkg
  multi_action(
      ctx=ctx,
      inputs=[package_spec] + config.ghc_pkg_bundle,
      outputs=[cache],
      actions=[
          # Before calling ghc-pkg, remove the parent directory of the cache
          # file.  That directory is added automatically by ctx.new_file(), but
          # "ghc-pkg init" will fail if it already exists.
          struct(cmd="rm", args=["-rf", package_db]),
          struct(cmd=ghc_pkg, args=["-v0", "init", package_db]),
          # Specify --force to simplify things by preventing ghc-pkg from
          # checking that support files exist (e.g., *.hi files).
          struct(cmd=ghc_pkg, args=["-v0", "register", "--force", "--package-db=" + package_db,
                                    package_spec.path]),
      ],
      progress_message="Building Haskell package %s" % str(ctx.label),
      mnemonic="HaskellPackage",
  )

  return cache

"""Attributes for compiling libraries and linking shared libraries."""

_compilation_attrs = {
    "_ghc_config": attr.label(
        default = Label("//third_party/haskell/ghc:config"),
    ),
    "_runtime_libs": attr.label(
        default = Label("//bzl:runtime_dependencies"),
    ),
}

"""Attributes allowed in haskell_{library,binary,test} rules."""

_library_attrs = dicts.add(_compilation_attrs, {
    "srcs": attr.label_list(allow_files = _HASKELL_SOURCE),
    "deps": attr.label_list(),
    "extra_objs": attr.label_list(),
    "ghcopts": attr.string_list(),
    "data": attr.label_list(
        cfg = "data",
        allow_files = True,
    ),
    "preprocessor": attr.label(
        executable = True,
        cfg = "host",
    ),
    # Internal flags for go/cabal2buildv2:
    # Additional source files that should be available when running the Haskell
    # compiler.  (For example, nonstandard files that should be included by CPP,
    # or .hs-boot files.)
    "extra_src_files": attr.label_list(allow_files = True),
})

def _haskell_library_impl(ctx, ghcopts=[], deps=[], srcs=[], src_archive=None):
  """Common code for building a Haskell library.

  Args:
    ctx: The current rule context.
    ghcopts: Extra options to pass to GHC.
    deps: A struct of dependencies for compiling this code, of the form
          returned by collect_deps().
    srcs: A list of Haskell source files.
    src_archive: (Optional) An archive containing Haskell source files.
  Returns:
    See the documentation of _impl_library().
  """
  lib = _compile_library(ctx=ctx, ghcopts=ghcopts, deps=deps, srcs=srcs,
                         src_archive=src_archive)

  # Give the library archive a name that's unique.
  # GHC uses "-l" to link the Haskell libs, which would otherwise
  # two haskell_library targets with the same name in different directories.
  hs_lib_name = "HS" + package_key(ctx)

  package_spec = ctx.new_file(_output_name(ctx) + ".spec")

  # A list of the names of modules contained by this library, as extracted from
  # the names of the .hi files output by GHC. The module names are
  # comma-separated, without any whitespace padding, e.g.:
  #
  # Foo,Foo.Bar,Foo.Bar.Baz
  modules_list = ctx.new_file(_output_name(ctx) + ".modules")
  archiving_action(
      ctx=ctx,
      input_archives=[lib.hi],
      outputs=[modules_list],
      mnemonic="HaskellCollectModules",
      progress_message="Collecting Haskell library modules for %s" % str(ctx.label),
      actions=[struct(cmd=" | \\\n".join(
          # Get the .hi file names relative to the archive directory.
          # Note: When profiling is enabled, both *.hi and *.p_hi files will be
          # present. This only searches for one extension to avoid getting
          # duplicate module names.
          ["find %s -name '*.%s' -printf \"%%P\\n\""
           % (unarchived_path(lib.hi.path),
              "p_hi" if _profiling_enabled(ctx) else "hi"),
           # Convert filenames into Haskell module names; e.g.,
           # "Foo/Bar/Baz.hs" -> "Foo.Bar.Baz".
           "sed 's|\.hi$||'",
           "sed 's|\.p_hi$||'",
           "tr / .",
           # Put all the modules into a one-line comma-separated list (and
           # *not* ending with a comma).
           "tr '\\n' ,",
           "sed 's|,$||'",
          ]) + " >> " + modules_list.path)],
  )

  config = ctx.attr._ghc_config.haskell_config

  if not deps.exposed_package_names:
    dependency_list = ""
  else:
    dependency_list = "".join([", " + id for id in deps.exposed_package_names])

  strict_deps = any([
      'haskell_strict_deps' in ctx.features,
  ])

  package_name = package_key(ctx)
  # Write the GHC package spec for this library.
  multi_action(
      ctx=ctx,
      inputs=[modules_list, config.default_packages],
      outputs=[package_spec],
      mnemonic="HaskellLibrarySpec",
      progress_message="Writing Haskell library spec for %s" % str(ctx.label),
      actions=[
          struct(cmd="printf '" + "\\n".join([
              "name: " + package_name,
              "id: " + package_name,
              "key: " + package_name,
              "library-dirs: ${pkgroot}",
              "import-dirs: ${pkgroot}/" + unarchived_path(
                  lib.hi.basename),
              "hs-libraries: " + hs_lib_name,
              "exposed: " + str(not strict_deps),
              "exposed-modules: '"
              ]) + " > " + package_spec.path),
          struct(cmd="cat %s >> %s" % (modules_list.path, package_spec.path)),
          struct(cmd="printf '\\ndepends: ' >> " + package_spec.path),
          # Append all the dependencies, separated by commas, and avoiding
          # putting a comma at the end.  (We include *all* of the
          # default-packages since it's hard to tell what was actually needed.)
          struct(cmd="sed 's/ /, /g' %s | tr -d '\\n' >> %s" % (
              config.default_packages.path, package_spec.path)),
          struct(cmd="printf ', %s' >> %s" % (_FFI_DEPS_PACKAGE_NAME, package_spec.path)),
          struct(cmd="echo %s >> %s" % (
              dependency_list, package_spec.path)),

      ],
  )

  package_cache = _register_package(ctx, _output_name(ctx) + ".db", package_spec)

  # If profiling is enabled, we must create both profiling and non-profiling
  # archives. See comment on _profiling_enabled for reasoning.
  lib_archives = []
  suffixes = [("o", ".a")]
  if _profiling_enabled(ctx):
    suffixes += [("p_o", "_p.a")]
  binutils_dir = ctx.bin_dir.path + "/" + config.binutils_path
  for obj_suffix, archive_suffix in suffixes:
    # Combine all the .o files into a single archive.
    lib_archive = ctx.new_file("lib" + hs_lib_name + archive_suffix)
    # ctx.attr.extra_objs should be empty or contain a label `:<package>-cbits` (provided by `cabal_haskell_package`).
    cbits_archive_name = lib_archive.dirname + "/lib" + ctx.label.name + "-cbits.a"
    cbits_archive = []
    # TODO: Figure out why `extra_objs` does not exist for //bzl/tests/proto:BarTest
    if hasattr(ctx.attr, "extra_objs"):
      if len(ctx.attr.extra_objs) >= 2:
        fail("extra_objs with more than 1 elements is not implemented")
      if ctx.attr.extra_objs:
        for extra_obj in ctx.attr.extra_objs:
          for f in extra_obj.files:
            if f.path.endswith(".a") and not f.path.endswith(".pic.a"):
              if cbits_archive:
                fail("Only one archive (specifically lib<package>-cbits.a) is supported. Files: %s" % extra_obj.files)
              cbits_archive = [f]
    archiving_action(
        ctx=ctx,
        outputs=[lib_archive],
        inputs=config.library_bundle + cbits_archive,
        input_archives=[lib.object],
        progress_message="Creating archive for %s" % str(ctx.label),
        mnemonic="HaskellArchive",
        actions= [
            struct(cmd="test -f %s && install -m644 %s %s" % (cbits_archive_name, cbits_archive_name, lib_archive.path)),
            struct(cmd="find %s -name '*.%s' | xargs %s/ar cqsD %s"
                   % (unarchived_path(lib.object.path),
                      obj_suffix,
                      binutils_dir,
                      lib_archive.path))
        ],
    )
    lib_archives.append(lib_archive)

  return struct(
      files=depset([lib.hi, package_cache, package_spec] + lib_archives),
      output_groups={
          "haskell_modules_list": [modules_list],
          "haskell_source_files": depset(
              srcs + ([src_archive] if src_archive else [])
              + getattr(ctx.files, "extra_src_files", [])),
          "haskell_compile_info": lib.compile_info_files,
          "haskell_dummy_ffi_deps": depset([lib.dummy_ffi_deps_cache]),
          "haskell_shared_ffi_deps": _shared_ffi_deps(ctx, deps),
      },
      runfiles=ctx.runfiles(collect_default=True),

      haskell=struct(
          exposed_package_names=depset([package_name]),
          transitive=new_transitive(
              transitive_deps=deps.transitive,
              cache=package_cache,
              hi_archive=lib.hi,
              hs_libs=lib_archives,
              ),
          dummy_ffi_deps_cache=lib.dummy_ffi_deps_cache,
      ),
  )

def _shared_ffi_deps(ctx, deps):
  """Build a ffi-deps package for dynamic linking against all cc_library deps.

  Args:
    ctx: The current rule context.
    deps: A struct() of dependencies for compiling this code, of the form
      returned by collect_deps().
  Returns:
    Returns a set with the following output files:
    - lib{ctx.attr.lib.name}.so, a shared library containing the transitiv
      closure of the cc_library dependencies of the given rule.
    - {ctx.attr.lib.name}.db/package.cache, which has registered a "ffi-deps"
      GHC package that links against the above .so file.
  """
  # TODO(judahjacobson): Linking fails for with -c opt, possibly because we
  # aren't getting the right flags from Skylark.  (Likely PIE-related).  For
  # now, just don't build the shared deps in that setting.
  if ctx.var["COMPILATION_MODE"] == "opt":
    return depset()

  shared_lib_name = _output_name(ctx) + "_shared_ffi_deps"

  config = ctx.attr._ghc_config.haskell_config
  cpp=ctx.fragments.cpp
  shared_lib = ctx.new_file("lib" + shared_lib_name + ".so")

  cc_libs = deps.transitive.cc_libs | ctx.attr._runtime_libs.cc.libs

  ctx.action(
      inputs=list(cc_libs) + config.compiler_bundle,
      outputs=[shared_lib],
      executable=cpp.compiler_executable,
      mnemonic="HaskellCCDeps",
      progress_message="Linking shared Haskell deps for " + str(ctx.label),
      arguments=(
          ["-shared", "-o", shared_lib.path, "-static-libgcc"]
          + cpp.mostly_static_link_options([], True)
          + ["-Wl,-whole-archive"]
          + [l.path for l in cc_libs]
          + ["-Wl,-no-whole-archive"]
          + list(deps.transitive.link_flags)
          + cpp.link_options
      ))

  package_spec = ctx.new_file(shared_lib_name + ".spec")
  ctx.file_action(
      output=package_spec,
      content="\n".join([
          "name: "  + _FFI_DEPS_PACKAGE_NAME,
          "id: " + _FFI_DEPS_PACKAGE_NAME,
          "key: " + _FFI_DEPS_PACKAGE_NAME,
          "library-dirs: ${pkgroot}",
          "extra-libraries: " + shared_lib_name
      ]))
  package_cache = _register_package(ctx, shared_lib_name + ".db",
                                    package_spec)

  return depset([shared_lib, package_spec, package_cache])

def _static_ffi_deps(ctx, deps):
  """Build a ffi-deps package for static linking against all cc_library deps.

  Args:
    ctx: The current rule context.
    deps: A struct() of dependencies for compiling this code, of the form
      returned by collect_deps().
  Returns:
    A struct consisting of the following fields:
      files: Support files for this package.
      package_db: The path to the GHC package DB.
  """
  cpp = ctx.fragments.cpp

  # Link against runtime libraries, deduping against the libs specified
  # by other C++ dependencies.
  link_transitive = add_cc_lib(deps.transitive, ctx.attr._runtime_libs)
  # TODO: Link against the actual "linkstamp" attributes. E.g. by
  # adding a genrule(stamp=1, srcs=[], outs=[something]), make
  # "something" an implicit dependency of every Haskell rule, then use
  # that to embed the linkstamp data into the binaries

  all_link_flags = (
      list(link_transitive.link_flags)
      # Note: we need to pass the "True" argument to fully_static_link_options
      # in order to get the flags "-Wl,--export-dynamic-symbol=...".
      + cpp.fully_static_link_options([], True)
      + cpp.link_options)

  # A cheap workaround for lack of c++ toolchain interfaces, -latomic is forced
  # onto every crosstool link line, though it is usually not relevant for
  # Haskell code. When GHCi is invoked, e.g. through Template Haskell, it will
  # get all -l options and will try to load them dynamically (as .so). But
  # c-compiler-team intends that libatomic should be strictly static. The
  # workaround is to just strip -latomic off our link line.\
  # Note that this would cause a problem if some Haskell library wanted to link
  # against libatomic, regardless of whether TH is involved.
  all_link_flags = [opt for opt in all_link_flags if opt != "-latomic"]

  link_options = (
      # TODO: whole-archive should only be added for always_link=1
      # libraries. Lacking the Skylark feature we apply it everywhere causing a
      # major Haskell binary size increase.
      ["-Wl,--whole-archive"]
      + [lib.path for lib in link_transitive.cc_libs]
      + ["-Wl,--no-whole-archive"]
      + all_link_flags
      )

  linking_package_spec = ctx.new_file(ctx.label.name + ".link.spec")
  ctx.file_action(output=linking_package_spec,
                  content="\n".join(
                      ["name: " + _FFI_DEPS_PACKAGE_NAME,
                       "id: " + _FFI_DEPS_PACKAGE_NAME,
                       "key: " + _FFI_DEPS_PACKAGE_NAME,
                      ] +
                      ["ld-options: \"" + opt.replace("\"", "\\\"")
                       + "\"" for opt in link_options]))
  linking_package_cache = _register_package(
      ctx, ctx.label.name + ".link.db", linking_package_spec)

  return struct(
      files=link_transitive.cc_libs | [linking_package_cache],
      package_db=linking_package_cache.dirname)

def _compile_info_files(ctx, deps=[], extra_input_archives=[], srcs=[], options=[]):
  """Render the text of a haskell.CompileInfo protocol buffer."""
  compile_info = ctx.new_file(_output_name(ctx) + ".HaskellCompile")
  input_archives = deps.transitive.hi_archives | depset(extra_input_archives)
  ctx.file_action(
      output=compile_info,
      content="\n".join(
          ["bin_dir: \"" + ctx.bin_dir.path + "\""]
          + ["input_archive: \"" + a.path + "\"" for a in input_archives]
          + ["source_file: \"" + s.path + "\"" for s in srcs]
          + ["option: \"" + o.replace("\\", "\\\\").replace("\"", "\\\"")
             + "\"" for o in options]
          ))
  return (
      # Force building the immediate dependencies in the "compile_info" output
      # group, by returning all of their dependencies.
      depset([compile_info]) | input_archives | deps.transitive.caches
      | deps.transitive.hs_libs)

def _impl_library(ctx):
  """Implementation of the haskell_library rule.

  Every library outputs the following files:
    - {name}.db/package.cache: A GHC package database containing (only) this
      library
    - {name}.spec: A human-readable version of package.cache, for debugging
    - {name}.modules: A comma-separated list of Haskell module names contained
      by this library
    - {name}.interfaces.tar: An archive containing the GHC interface files for
      the modules in this library
    - {name}.objects.tar: An archive containing the object files for the
      modules in this library
    - libHS{mangled name}.a: Library containing all the modules' object code

  If this library has no source files, then it just re-exposes all the
  immediate dependencies.

  Args:
    ctx: The current rule context.
  Returns:
    files: The set of file output by this rule
    runfiles: The set of supporting runfiles for binaries that depend on this
      rule
    haskell: A struct consisting of the following fields:
      exposed_package_names: The GHC package IDs that are exposed when this rule
        appears in the "deps" field.  Usually it's a single library corresponding
        to the "srcs" of this rule; however if "srcs" is empty then it consists
        of the libraries exposed from the immediate dependencies.
      transitive: Transitive providers for this and all (recursive)
        dependencies.  See the documentation of new_transitive() for more
        details.
      compile_info: A protocol buffer string, human-readably encoded.  See
        haskell_library_compile_info for more details.
      dummy_ffi_deps_cache: A GHC package DB that defines a trivial "ffi-deps"
        package for this library.
  """
  if not ctx.files.srcs:
    return forward_deps(ctx, ctx.attr.deps)

  deps = collect_deps(ctx.attr.deps)

  return _haskell_library_impl(ctx, deps=deps, ghcopts=ctx.attr.ghcopts,
                               srcs=ctx.files.srcs)

"""A BUILD rule for compiling Haskell libraries.

This rule compiles a single Haskell library containing one or more source
files.

Example usage:
haskell_library(
    name = "mylibrary"
    srcs = ["Foo.hs", "Bar.hs"]
)

The Haskell package name for the library is derived from the target path
relative to WORKSPACE (with various substitutions made in order to turn the
target path into a valid Haskell package name). This fact should be of no
interest unless the package is used for building things outside
BUILD framework.

Args:
  name: A unique name within the current BUILD file for this rule.
  srcs: A list of Haskell source files ("*.hs") to compile into the target.
  deps: Optional; A list of Labels of other libraries that this package
    depends on.  This list can contain (Skylark) haskell_library and/or
    cc_library rules.  Any modules imported by the "srcs" must be directly
    exported from one of these haskell_library deps.  The deps will all be
    linked into the final binary target.
  ghcopts: Optional; A list of strings to be added to the GHC compilation
    command.  This is subject to "Make variable" substitution and "Bourne
    shell tokenization" (see go/be for more details).
  preprocessor: Optional; a binary target.  Specifies an executable to be run
    as a GHC preprocessor.  The executable path of the binary target is
    provided to ghc via the -pgmF option.
  data: Optional; a list of Labels for the files needed by this rule at
    runtime.
  **kwargs: Implicit Bazel rule attributes.
"""

haskell_library = rule(
    attrs = _library_attrs,
    fragments = ["cpp"],
    implementation = _impl_library,
)

def _impl_binary(ctx):
  """Implementation of the haskell_binary rule.

  A binary outputs the following files:
    - {name}: The built binary.
    - {name}.spec: A human-readable version of the GHC package used for
        linking this binary, for debugging.

  Args:
    ctx: The current rule context.
  Returns:
    files: The set of files output by this rule, consisting of:
      - {name}, the built binary
      - {name}.spec, a human-readable version of the GHC package that we use
          to link this binary.  (Returned for debugging.)
  """
  deps = collect_deps(ctx.attr.deps)

  out = ctx.outputs.executable
  compile_info_files = _compile_binary(
      ctx=ctx, out=out, srcs=ctx.files.srcs, deps=deps)

  return struct(
      files=depset([out]),
      runfiles=ctx.runfiles(collect_default=True),
      output_groups={
          "haskell_source_files": depset(ctx.files.srcs),
          "haskell_compile_info": compile_info_files,
          "haskell_shared_ffi_deps": _shared_ffi_deps(ctx, deps),
      })

"""Attributes allowed in haskell_binary and haskell_test rules."""

_binary_attrs = dicts.add(_library_attrs, {
    "main_is": attr.string(),
    "threaded": attr.bool(default = True),
})

""" A BUILD rule for compiling an executable binary from Haskell source files.

Example usage:
haskell_binary(
    name = "mybinary",
    srcs = ["Main.hs"],
)

Args:
  name: A unique name within the current BUILD file for this rule.
  srcs: A list of Haskell source files ("*.hs") to compile into the target.
  deps: Optional; A list of Labels of other libraries that this package depends
    on.  This list can contain (Skylark) haskell_library and/or cc_library
    rules.  Any modules imported by the "srcs" must be directly exported from
    one of these haskell_library deps.  The deps will all be linked into the
    final binary target.
  ghcopts: Optional; A list of strings to be added to the GHC compilation
    command.  This is subject to "Make variable" substitution and "Bourne shell
    tokenization" (see go/be for more details).
  data: Optional; a list of Labels for the files needed by this rule at
    runtime.
  main_is: Optional; A string specifying the module that contains the main
    function.
  threaded: Optional; True by default, can be used to disable threaded RTS when
    linking a binary. Do not disable lightly, see go/haskell-deadlock.

"""

haskell_binary = rule(
    attrs = _binary_attrs,
    executable = True,
    fragments = ["cpp"],
    implementation = _impl_binary,
)

""" A BUILD rule for compiling a test from Haskell source files.

Example usage:
haskell_test(
    name = "mylibrary_test",
    srcs = ["Main.hs"],
)

Args:
  name: A unique name within the current BUILD file for this rule.
  srcs: A list of Haskell source files ("*.hs") to compile into the target.
  deps: Optional; A list of Labels of other libraries that this package depends
    on.  This list can contain (Skylark) haskell_library and/or cc_library
    rules.  Any modules imported by the "srcs" must be directly exported from
    one of these haskell_library deps.  The deps will all be linked into the
    final binary target.
  ghcopts: Optional; A list of strings to be added to the GHC compilation
    command.  This is subject to "Make variable" substitution and "Bourne shell
    tokenization" (see go/be for more details).
  data: Optional; a list of Labels for the files needed by this rule at
    runtime.
  main_is: Optional; A string specifying the module that contains the main
    function.
  threaded: Optional; True by default, can be used to disable threaded RTS when
    linking a binary. Do not disable lightly, see go/haskell-deadlock.

"""

haskell_test = rule(
    attrs = _binary_attrs,
    fragments = ["cpp"],
    test = True,
    implementation = _impl_binary,
)

def _capitalize_first_letter(c):
  """Capitalize the first letter of the input.

  Unlike the built-in capitalize() method, doesn't lower-case the other
  characters.  This helps mimic the behavior of proto-lens-protoc, which turns
  Foo/Bar/BAZ.proto into Foo/Bar/BAZ.hs (rather than Foo/Bar/Baz.hs).

  Args:
    c: A non-empty string word.
  Returns:
    The input with the first letter upper-cased.
  """
  return c[0].capitalize() + c[1:]

def _camel_case(comp):
  """Camel-case the input string, preserving any existing capital letters."""
  # Split on both "-" and "_", matching the behavior of proto-lens-protoc.
  return "".join([_capitalize_first_letter(c2)
                  for c1 in comp.split("_") for c2 in c1.split("-")])

def _proto_lens_output_file(path):
  """The output file from proto-lens-protoc when run on the given path."""
  path = path[:-len(".proto")]
  result = "/".join([_camel_case(p) for p in path.split("/")]) + ".hs"
  return "Proto/" + result

def _impl_proto_lens_aspect(target, ctx):
  """Implementation of the haskell_proto_aspect aspect for proto-lens.

  Args:
    target: The current target.
    ctx: The current rule context.
  Returns:
    A compiled haskell_library target (see _impl_library for more details).
  """
  # TODO(judahjacobson): For haskell_deps=strict, this currently forwards
  # everything in _proto_support.  Should we be more strict and only
  # forward the haskell_proto_library deps?
  all_deps = ctx.rule.attr.deps + ctx.attr._proto_support

  # If this proto_library rule has no "srcs", treat it as just
  # forwarding its immediate dependencies.
  if not target.proto.direct_sources:
    return fix_aspect_providers(forward_deps(ctx, all_deps))

  # Accumulate all of the haskell source files to be generated.
  # A file "{package}/foo.proto" will be turned into "{package}/foo.pb.hs".
  hs_files = []
  for src in target.proto.direct_sources:
    # As with the native rules, require the .proto file to be in the same Bazel
    # package as the proto_library rule.  This allows us to put the output .hs
    # file next to the input .proto file.
    # Unfortunately Skylark doesn't let us check the package of the file
    # directly, so instead we just look at its short_path and rely on the
    # proto_library rule itself to check for consistency.
    # We use the file's short_path rather than its dirname/basename in case
    # it's in a subdirectory; for example, if the proto_library rule is in
    # "foo/BUILD" but the .proto file is "foo/bar/baz.proto".
    prefix = ctx.label.package + "/"
    if not src.short_path.startswith(prefix):
      fail("Mismatch between rule context " + str(ctx.label.package)
           + " and source file " + src.short_path)
    if src.basename[-6:] != ".proto":
      fail("bad extension for proto file " + src)
    # Strip off the package and the following "/" from the short_path.
    relative_path = src.short_path
    hs_files += [ctx.new_file(_proto_lens_output_file(relative_path))]

  # Run the protocol compiler to generate the Haskell source files.
  plugin = "protoc-gen-haskell=" + ctx.executable._protoc_gen_haskell.path
  out_root_path = hs_files[0].root.path + "/" + prefix
  ctx.action(
      inputs=([ctx.executable._protoc_gen_haskell]
              + list(target.proto.transitive_sources)),
      outputs=hs_files,
      # This program parses the .proto files and feeds their descriptors into
      # the plugin program (protoc-gen-haskell) to generate Haskell sources.
      executable=ctx.executable._protocol_compiler,
      arguments=(
          # TODO(judahjacobson): For safety, pass --disallow_services to the
          # protocol compiler based on whether the deps specify
          # has_services=1.
          ["--plugin=" + plugin,
           "--haskell_out=no-reexports:" + out_root_path,
          ]
          + ["-I" + s.short_path + "=" + s.path
             for s in target.proto.transitive_sources]
          + [s.short_path for s in target.proto.direct_sources]
          ),
      progress_message=("Generating Haskell proto-lens source files for %s"
                        % str(ctx.label)),
      mnemonic="HaskellProtoLensGen",
  )

  lib = _haskell_library_impl(
      ctx=ctx,
      deps=collect_deps(all_deps),
      srcs=hs_files)

  return fix_aspect_providers(lib)

haskell_proto_lens_aspect = aspect(
    attr_aspects = ["deps"],
    attrs = dicts.add({
        "_api": attr.string(default = "lens"),
        "_protocol_compiler": attr.label(
            executable = True,
            cfg = "host",
            default = Label("@com_google_protobuf//:protoc"),
        ),
        "_protoc_gen_haskell": attr.label(
            executable = True,
            cfg = "host",
            # This won't work!!!
            default = Label("@com_google_protobuf//:protoc"),
        ),
        # Explicit list of dependencies which will be used and exported by all
        # haskell_proto_library_rules.
        # TODO: Be more strict about when they are exported.
        # Also, this should probably export data-default-class rather than
        # data-default, but some packages currently assume the latter.
        "_proto_support": attr.label_list(
            default = [
#                Label("//third_party/haskell/lens_labels:lens-labels"),
#                Label("//third_party/haskell/proto_lens:proto-lens"),
#                Label("//third_party/haskell/data_default:data-default"),
            ],
        ),
    }, _compilation_attrs),
    fragments = ["cpp"],
    implementation = _impl_proto_lens_aspect,
)

def _impl_proto(ctx):
  """Implementation of the haskell_proto_library rule.

  Args:
    ctx: The current rule context.
  Returns:
    A compiled haskell_library target (see _impl_library for more details).
  """
  return forward_deps(ctx, ctx.attr.deps)

_haskell_proto_lens_library = rule(
    attrs = {
        "deps": attr.label_list(
            providers = ["proto"],
            aspects = [haskell_proto_lens_aspect],
        ),
    },
    fragments = ["cpp"],
    implementation = _impl_proto,
)

def haskell_proto_library(name="", deps=[], api="lens", **kwargs):
  """A BUILD rule for compiling Haskell protocol buffer bindings.

  Example usage:

      proto_library(
          name = "foo_proto",
          srcs = ["foo.proto"],
      )

      haskell_proto_library(
          name = "foo_haskell_pb",
          deps = [":foo_proto"],
      )

  Args:
    name: The name of this target.
    deps: A list of proto_library targets for which to generate bindings.
      Note that transitive proto_library dependencies will get dependencies
      automatically generated for them.
    api: Optional: The proto API that we should use.  Defaults to
      lens" for proto-lens.
    **kwargs: Additional arguments for this build rule.
  """
  if api == "lens":
    _haskell_proto_lens_library(name=name, deps=deps, **kwargs)
  else:
    fail("Bad api \"" + api + "\" for haskell_proto_library rule " + name)
