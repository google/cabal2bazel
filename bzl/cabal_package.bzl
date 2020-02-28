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

"""Skylark build rules for cabal haskell packages.

To see all of the generated rules, run:
bazel query --output=build //third_party/haskell/{package}/v{version}:all
"""

load("@bazel_skylib//lib:collections.bzl", "collections")
load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("//third_party/haskell/alex:build_defs.bzl", "genalex")
load("//third_party/haskell/ghc:versions.bzl", "select_by_ghc_version")
load("//third_party/haskell/happy:build_defs.bzl", "genhappy")
load("//bzl/cabal_package:cabal_paths.bzl", "cabal_paths")
load("//bzl/cabal_package:overrides.bzl", "common_package_overrides")
load("//bzl/private:action.bzl", "multi_action")
load("//bzl:binary_with_runfiles.bzl", "binary_runfiles")
load(
    "//bzl:def.bzl",
    "haskell_binary",
    "haskell_library",
)
load("//bzl:hsc2hs.bzl", "hsc2hs")
load("@bazel_skylib//rules:build_test.bzl", "build_test")

_conditions_default = "//conditions:default"

# Extra GHC arguments to undo the defaults which might otherwise break
# some Cabal packages.
_undo_default_options = [
    "-XForeignFunctionInterface",
    "-XNoScopedTypeVariables",
    "-no-haddock",
] + select_by_ghc_version({
    "8.6": ["-XStarIsType"],
    "8.4": [],
})

def _paths_module(desc):
    return "Paths_" + desc.package.pkgName.replace("-", "_")

def _glob_modules(src_dirs):
    """List Haskell files under the given directory.

    Args:
      src_dirs: A list of subdirectories relative to this package.

    Returns:
      A dict mapping pairs of modules and extensions to source files.
    """
    outputs = {}
    for src_dir in src_dirs:
        for f in native.glob([_prefix_dir_base(src_dir) + "**/*"]):
            base, extension = paths.split_extension(f)
            if extension.startswith("."):
                module = paths.relativize(base, src_dir).replace("/", ".")
                if (module, extension) not in outputs:
                    outputs[(module, extension[1:])] = f
    return outputs

def _conditions_dict(d):
    return getattr(d, "select", {_conditions_default: d})

def _get_extra_src_files(desc):
    """Returns paths with DOS separator \\ replaced by /."""
    return [path.replace("\\", "/") for path in desc.extraSrcFiles]

def _generate_module(component_name, module_map, module, cbits, tags):
    """Creates a build rule to generate the given module.

    If the source file (e.g. .hs) already exists, it just returns it.

    Each generated source file is given a suffix to make it unique for each component
    (library, executable, test).  This avoids having duplicate rules
    when the same module is specified for multiple components.

    Args:
      component_name: A string used to uniqify the generated rule
      module_map: A dict from a pair of (module name, extension)
        to filenames.
      module: The name of the module to generate.
      cbits: The cc_library rule for this target.
      tags: Tags to set on the generated build rule.
    """
    rule_name = "_gen_" + component_name + "_" + module
    out_hs = "_gen_" + component_name + "/" + module.replace(".", "/") + ".hs"

    f = module_map.get((module, "hs"))
    if f:
        return f

    f = module_map.get((module, "lhs"))
    if f:
        return f

    # A tag to tell build_cleaner what module is being generated, despite the
    # uniquified filename.
    generated_tags = tags + ["haskell_generated_module=" + module]

    # hsc2hs
    f = module_map.get((module, "hsc"))
    if f:
        hsc2hs(
            name = rule_name,
            src = f,
            out = out_hs,
            deps = [cbits],
            tags = generated_tags,
        )
        return out_hs

    # Alex
    f = module_map.get((module, "x"))
    if f:
        genalex(
            src = f,
            out = out_hs,
            tags = generated_tags,
        )
        return out_hs

    # Happy
    f = module_map.get((module, "y"))
    if not f:
        f = module_map.get((module, "ly"))
    if f:
        genhappy(
            src = f,
            out = out_hs,
            tags = generated_tags,
        )
        return out_hs

    fail("Missing module %s for %s" % (module, component_name))

def _get_build_attrs(
        name,
        build_info,
        desc,
        generated_srcs_dir,
        extra_modules,
        module_path = None,
        cc_deps = [],
        package_overrides = None,
        ghcopts = [],
        tags = []):
    """Get the attributes for a particular library or binary rule.

    Args:
      name: The name of this component.
      build_info: A struct of the Cabal BuildInfo for this component.
      desc: A struct of the Cabal PackageDescription for this package.
      generated_srcs_dir: Location of autogenerated files for this rule,
        e.g., "dist/build" for libraries.
      extra_modules: exposed-modules: or other-modules: in the package description
      module_path: The main file or module, if this rule will produce a binary.
      cc_deps: External cc_libraries that this rule should depend on.
      package_overrides: Override the default location of specific dependencies;
        see cabal_haskell_package for more details.
      ghcopts: Extra GHC options.
      tags: Tags to set on every generated target.

    Returns:
      A dictionary of attributes (e.g. "srcs", "deps") that can be passed
      into a haskell_library or haskell_binary rule.
    """

    # Collect all the source files by their extension.
    # module_map will contain a dictionary from module names ("Foo.Bar")
    # and extensions to the original source file ("src/Foo/Bar.hsc").
    module_map = _glob_modules(build_info.hsSourceDirs + [generated_srcs_dir])

    # Keep track of which modules have been generated for this target, in case
    # we generate the same file for multiple conditions.
    # Maps module names to the target.
    generated_modules = dict()

    # Collect the source files for each module in this Cabal component.
    # srcs is a mapping from "select()" conditions (e.g. //third_party/haskell/ghc:ghc-8.0.2) to a list of source files.
    srcs = {}

    # Keep track of .hs-boot files specially.  GHC doesn't want us to pass
    # them as command-line arguments; instead, it looks for them next to the
    # corresponding .hs files.
    paths_module = _paths_module(desc)
    extra_modules_dict = _conditions_dict(extra_modules)
    other_modules_dict = _conditions_dict(build_info.otherModules)

    cbits_target = name + "-cbits"

    # Track the runtime deps for any Paths_ modules we're using.
    # Use a separate dictionary since the select() keys for the main
    # package dependencies may be different.
    runtime_deps = {}
    for condition in depset(extra_modules_dict.keys() + other_modules_dict.keys()).to_list():
        srcs[condition] = []
        runtime_deps[condition] = []
        for module in (extra_modules_dict.get(condition, []) +
                       other_modules_dict.get(condition, [])):
            if module == paths_module:
                runtime_deps[condition].append(
                    "//bzl/cabal_package:PathDeps",
                )
                srcs[condition] += [":" + paths_module]
            elif module in generated_modules:
                srcs[condition] += generated_modules[module]
            else:
                # Generate a file for this module:
                fs = [_generate_module(name, module_map, module, cbits_target, tags)]

                # Also add boot files, if any:
                if (module, "hs-boot") in module_map:
                    fs += [module_map[(module, "hs-boot")]]
                elif (module, "lhs-boot") in module_map:
                    fs += [module_map[(module, "lhs-boot")]]
                srcs[condition] += fs
                generated_modules[module] = fs

    # Collect the options to pass to ghc.
    extra_ghcopts = ghcopts
    ghcopts = []
    ghcopts += _undo_default_options
    ghcopts += [
        "-X" + ext
        for ext in ([build_info.defaultLanguage] if build_info.defaultLanguage else ["Haskell98"]) +
                   build_info.defaultExtensions +
                   build_info.oldExtensions
    ]

    threaded = False
    ghcopt_blacklist = ["-Wall", "-Wwarn", "-w", "-Werror", "-O", "-O2"]
    for (compiler, opts) in build_info.options:
        if compiler == "ghc":
            ghcopts += [o for o in opts if o not in ghcopt_blacklist]
            if "-threaded" in opts:
                threaded = True
    ghcopts += ["-w", "-Wwarn"]  # -w doesn't kill all warnings...

    # Collect the dependencies.
    deps = {}
    for condition, ps in _conditions_dict(build_info.targetBuildDepends).items():
        deps[condition] = []
        for p in ps:
            if p.name == desc.package.pkgName:
                # Allow executables to depend on the library in the same package.
                deps[condition] += [":" + _fix_rule_name(p.name)]
            elif p.name in package_overrides:
                deps[condition] += [package_overrides[p.name]]
            else:
                deps[condition] += ["//third_party/haskell/" + _fix_rule_name(p.name)]

    ghcopts += ["-optP" + o for o in build_info.cppOptions]

    # Generate a cc_library for this package.
    # TODO(judahjacobson): don't create the rule if it's not needed.
    install_includes = native.glob(
        [
            _prefix_dir(d, f)
            for d in build_info.includeDirs
            for f in build_info.installIncludes
        ],
    )
    headers = depset(
        [f for f in native.glob(_get_extra_src_files(desc))] + install_includes,
    )
    ghcopts += ["-I" + native.package_name() + "/" + d for d in build_info.includeDirs]
    lib_name = name + "-cbits"
    for xs in deps.values():
        xs.append(":" + lib_name)
    native.cc_library(
        name = lib_name,
        srcs = build_info.cSources,
        includes = build_info.includeDirs,
        copts = ([o for o in build_info.ccOptions if not o.startswith("-D")] +
                 ["-w"]),
        defines = [o[2:] for o in build_info.ccOptions if o.startswith("-D")],
        textual_hdrs = headers.to_list(),
        deps = ["//third_party/haskell/ghc:headers"] + cc_deps,
        tags = tags,
    )

    if module_path:
        [full_module_path] = native.glob(
            [_prefix_dir(d, module_path) for d in build_info.hsSourceDirs],
        )
        for xs in srcs.values():
            if full_module_path not in xs:
                xs.append(full_module_path)

    return dicts.add(
        {
            "srcs": select(srcs),
            "deps": select(deps) + select(runtime_deps),
            # We'll list any core packages explicitly in the deps:
            "default_packages": [],
            "ghcopts": ghcopts + extra_ghcopts,
            "extra_src_files": collections.uniq(
                install_includes +
                native.glob(_get_extra_src_files(desc)),
            ),
            # Assume that this target uses TemplateHaskell or a similar
            # functionality.  Alternately we could parse the files for
            # LANGUAGE extensions, but it's not worth the added complexity.
            "executes_deps_in_compile": True,
            # Don't try to distinguish plugins from regular deps, since Cabal
            # doesn't give us that information.  Note there's an open proposal:
            # https://github.com/haskell/cabal/issues/2965
            "allow_deps_as_plugins": True,
        },
        # Attributes that are only relevant for binaries or tests
        {
            # Include the "threaded" attribute, since some binaries don't work
            # with our default (threaded=True)
            "threaded": threaded,
            # Don't enforce our internal conventions for third-party code:
            "skip_module_name_check": True,
        } if module_path else {},
    )

def _prefix_dir_base(d):
    normalized = paths.normalize(d)
    if normalized == ".":
        return ""
    else:
        return normalized + "/"

def _prefix_dir(dir_path, file_path):
    """Prefix the file with the given directory, without extra "/"s."""
    return _prefix_dir_base(dir_path) + file_path

def _impl_haskell_thirdparty_test(ctx):
    shell_file = ctx.actions.declare_file(ctx.label.name)
    extra_srcs_label = ctx.attr.extra_srcs.label

    ctx.actions.expand_template(
        template = ctx.file._template,
        output = shell_file,
        substitutions = {
            "%{binary}": ctx.executable.binary.short_path,
            "%{extra_srcs_dir}": paths.join(extra_srcs_label.package, extra_srcs_label.name),
        },
    )
    return DefaultInfo(
        executable = shell_file,
        runfiles = ctx.runfiles(
            files = [ctx.executable.binary] + ctx.files.extra_srcs + ctx.files.runfiles,
        ),
    )

_haskell_thirdparty_test = rule(
    implementation = _impl_haskell_thirdparty_test,
    test = True,
    attrs = {
        "binary": attr.label(executable = True, cfg = "target"),
        "extra_srcs": attr.label(allow_files = True),
        "runfiles": attr.label(allow_files = True),
        "_template": attr.label(
            allow_single_file = True,
            default = Label("//bzl/cabal_package:test.template"),
        ),
    },
)
"""A wrapper test that sets up the environment for third-party Haskell tests."""

def _impl_collect_srcs(ctx):
    # Collect all srcs into a separate directory,
    # keeping their relative directory structure.
    srcs_dir = ctx.label.name
    srcs = ctx.files.srcs
    srcs_and_outs = [
        (f, ctx.actions.declare_file(
            paths.join(
                srcs_dir,
                paths.relativize(f.path, ctx.label.package),
            ),
        ))
        for f in srcs
    ]
    src_outs = [g for (_, g) in srcs_and_outs]

    # Copy them all in a single action with a single progress message,
    # to avoid spamming the build output.
    multi_action(
        ctx,
        inputs = depset(srcs),
        progress_message = "Collecting source files for " + ctx.label.name,
        outputs = src_outs,
        # Just copy the files rather than soft-linking them.  Since they're
        # source files, we don't expect them to be large.  And using "cp"
        # avoids having to deal with relative paths for the soft link.
        actions = [
            struct(cmd = "/bin/cp", args = [f.path, g.path])
            for (f, g) in srcs_and_outs
        ],
    )

    return DefaultInfo(files = depset(src_outs))

_collect_srcs = rule(
    implementation = _impl_collect_srcs,
    attrs = {"srcs": attr.label_list(allow_files = True)},
)
"""Collects all srcs in a separate directory."""

def _is_unimplementable_test(t):
    """Whether we're not currently able to build this kind of test.

    Some kinds of tests are hard to integrate with Bazel.  In particular:
    if they rely on a custom Setup script that processes the source files.
    Currently, we just skip them.

    Args:
      t: A test-suite

    Returns:
      A boolean.
    """
    for p in t.testBuildInfo.targetBuildDepends:
        # Doctest rules rely on setup-depends: cabal-doctest. However, only
        # blacklist the tests depending directly on "doctest".
        if p.name in ["doctest"]:
            return True
    return False

def _fix_rule_name(name):
    return name.replace("-", "_")

def _render_reexport(r):
    return r.moduleReexportOriginalName + " as " + r.moduleReexportName

def cabal_haskell_package(
        description = None,
        cc_deps = [],
        data = [],
        package_overrides = {},
        cbits = True,
        ghcopts = [],
        test = True,
        target_deps = {},
        target_tags = {},
        linkstatic = True,
        disabled = False):
    """Creates build rules for a Cabal package.

    This macro is used by go/cabal2build, which generates (among other things):
    - A package_description.bzl file that reifies the .cabal file
      into a Starlark struct
    - A BUILD file that calls cabal_haskell_package() with that struct.  The macro
      will generate build rules for the libraries, binaries and tests that were
      originally listed in the .cabal file.

    For example:

        load(":package_description.bzl", "description")
        load(
            "//bzl:cabal_package.bzl",
            "cabal_haskell_package",
        )
        cabal_haskell_package(description = description)

    For many Cabal packages, the default parameters of this macro are sufficient.
    In other cases, you may want to customize that BUILD file by hand and add more
    arguments to cabal_haskell_package. For example:

        cabal_haskell_package(
            description = description,
            # Adds a cc_library dependency:
            cc_deps = ["//third_party/zlib"],
            # Prevents generating build rules for tests:
            test = False,
        )

    Args:
      description: A Skylark struct generated by cabal2build representing a
        .cabal file's contents.
      cc_deps: Additional cc_library targets that this package should
        depend on.
      data: "data" for haskell_library.
      package_overrides: Specify that this rule should use a non-default location
        of some dependencies.  (NOTE: This should be avoided when possible; see
        go/thirdpartyhaskell for details.)  A dictionary mapping string package
        names to labels; for example:
        {"haskell-src-exts": "//third_party/haskell/haskell_src_exts/v1.16.0:haskell_src_exts"}.
      cbits: Whether to insert object files from lib<package>-cbits.a into the produced .a .
      ghcopts: Extra GHC options.
      test: Whether to generate rules for this package's test-suites.
        Defaults to True.
      target_deps: A dict from names of targets to additional deps for them (list of labels).
      target_tags: A dict from names of targets to additional tags for them (list of strings).
      linkstatic: The value of the "linkstatic" attribute for each binary and test rule.
      disabled: If true, tag every target as to prevent it from being built
        automatically.  For example, this setting allows us to import packages and their
        dependencies into Piper in separate, parallel CLs.
    """

    if disabled:
        tags = [
            "notap",  # Don't run on TAP.
            "manual",  # Don't run with "bazel build {package}/..." or "{package}:all".
            "no_grok",  # Don't index with Grok.
            "nobuilder",  # Don't build with Tricorder.
        ]
    else:
        tags = []

    # Tell build_cleaner not to analyze these targets; it can't modify them.
    tags += ["nofixdeps"]

    # Include global overrides, such as haskell_core_library rules.
    package_overrides = dicts.add(common_package_overrides, package_overrides)

    name = description.package.pkgName

    data = data + native.glob([_prefix_dir(description.dataDir, d) for d in description.dataFiles])

    # TODO(judahjacobson): don't generate a Paths_ library unless it's needed.
    cabal_paths(
        name = _paths_module(description),
        out = _paths_module(description) + ".hs",
        package = name.replace("-", "_"),
        version = [int(v) for v in description.package.pkgVersion.split(".")],
        data_dir = description.dataDir,
        tags = tags,
    )

    lib = description.library
    if lib and lib.libBuildInfo.buildable:
        lib_attrs = _get_build_attrs(
            name,
            lib.libBuildInfo,
            description,
            "dist/build",
            lib.exposedModules,
            cc_deps = cc_deps,
            package_overrides = package_overrides,
            ghcopts = ghcopts,
            tags = tags,
        )
        lib_deps = lib_attrs.pop("deps", []) + target_deps.get(name, [])

        haskell_library(
            name = _fix_rule_name(name),
            cabal_name = name,
            cabal_version = description.package.pkgVersion,
            data = data,
            deps = lib_deps,
            tags = tags + ["haskell-exposed"] + target_tags.get(name, []),
            hidden_modules = select(_conditions_dict(lib.libBuildInfo.otherModules)),
            reexported_modules = [_render_reexport(r) for r in lib.reexportedModules] if hasattr(lib, "reexportedModules") else [],
            **lib_attrs
        )

        build_test(
            name = name + "_build_test",
            targets = [":" + _fix_rule_name(name)],
            tags = tags,
        )

    for exe in description.executables:
        if not exe.buildInfo.buildable:
            continue
        exe_name = exe.exeName

        # Avoid a name clash with the library.  For stability, make this logic
        # independent of whether the package actually contains a library.
        if exe_name == name:
            exe_name = name + "_bin"
        paths = _paths_module(description)
        binary_tags = tags + target_tags.get(exe_name, [])
        attrs = _get_build_attrs(
            exe_name,
            exe.buildInfo,
            description,
            "dist/build/%s/%s-tmp" % (name, name),
            # Some packages (e.g. happy) don't specify the
            # Paths_ module explicitly.
            [paths] if paths not in exe.buildInfo.otherModules else [],
            module_path = exe.modulePath,
            package_overrides = package_overrides,
            ghcopts = ghcopts,
            tags = binary_tags,
        )
        exe_deps = attrs.pop("deps", []) + target_deps.get(exe_name, [])

        haskell_binary(
            name = _fix_rule_name(exe_name),
            deps = exe_deps,
            tags = binary_tags + ["haskell-exposed"],
            data = data,
            linkstatic = linkstatic,
            **attrs
        )

    # Collect all "source" files, making them available to the tests.
    # Most relevant tests only use extra-src-files, but a few tests
    # (e.g., conduit, or tests using hedgehog) also look for the
    # original source files.
    # Note: *don't* collect data-files, since they're supposed to be
    # accessed via runfiles (via the autogenerated Paths_* module)
    # and we don't want the tests to circumvent that.
    all_source_dirs = (
        [
            d
            for exe in description.executables
            for d in exe.buildInfo.hsSourceDirs
        ] +
        [
            d
            for t in description.testSuites
            for d in t.testBuildInfo.hsSourceDirs
        ] +
        (description.library.libBuildInfo.hsSourceDirs if description.library else [])
    )
    _collect_srcs(
        name = "collected_srcs",
        srcs = native.glob(_get_extra_src_files(description) +
                           [_prefix_dir(d, "**/*") for d in all_source_dirs]),
    )

    if test:
        for t in description.testSuites:
            if not t.testBuildInfo.buildable:
                continue
            interface = t.testInterface
            test_name = t.testName

            # We only support tests of type "exitcode-stdio-1.0".
            # The only other type Cabal currently supports is "detailed-0.9",
            # which isn't used by any of our packages.
            if interface.type != "exitcode-stdio-1.0":
                fail("Unrecognized test " + test_name + " of type: " + interface.type)

            if _is_unimplementable_test(t):
                continue

            all_test_tags = tags + target_tags.get(test_name, [])
            attrs = _get_build_attrs(
                test_name,
                t.testBuildInfo,
                description,
                "dist/build/%s/%s-tmp" % (test_name, test_name),
                [],
                module_path = interface.mainIs,
                package_overrides = package_overrides,
                ghcopts = ghcopts,
                tags = all_test_tags,
            )
            test_deps = attrs.pop("deps", []) + target_deps.get(test_name, [])

            # Wrap the test in a shell script that runs it with the
            # environment that third-party packages expect.
            test_binary_name = test_name + ".binary"
            haskell_binary(
                name = test_binary_name,
                deps = test_deps,
                tags = all_test_tags,
                linkstatic = linkstatic,
                testonly = True,
                **attrs
            )

            test_binary_runfiles = test_binary_name + ".runfiles"
            binary_runfiles(
                name = test_binary_runfiles,
                src = test_binary_name,
                tags = all_test_tags,
                testonly = True,
            )

            real_test_name = _fix_rule_name(test_name)
            _haskell_thirdparty_test(
                name = real_test_name,
                binary = test_binary_name,
                tags = all_test_tags,
                extra_srcs = "collected_srcs",
                runfiles = test_binary_runfiles,
                timeout = "long",
            )
