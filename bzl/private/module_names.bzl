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

"""Checks that modules names match filepaths."""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("//bzl/private:ghc_options.bzl", "ghc_options")
load("//bzl:paths.bzl", "split_dirname")

check_module_names_attrs = {
    "_check_module_names": attr.label(
        executable = True,
        cfg = "host",
        default = Label("//bzl/check_module_names:CheckModuleNames"),
    ),
    "_ghc_settings": attr.label(
        allow_files = True,
        default = Label("//third_party/haskell/ghc:settings"),
    ),
}

def _maybe_unlit(ctx, config, f):
    """Turns a literate Haskell file into a regular Haskell source file.

    Does nothing to files that are not literate Haskell.

    Args:
      ctx: The current rule context
      config: A HaskellCompilerInfo provider
      f: A source File.

    Returns:
      A File.
    """
    name, ext = paths.split_extension(f.basename)
    if ext not in [".lhs", ".lhs-boot"]:
        return f
    f_unlit = ctx.actions.declare_file(ctx.label.name + ".unlit/" + name + ".hs")
    ctx.actions.run(
        executable = config.unlit,
        arguments = [f.path, f_unlit.path],
        inputs = [f],
        outputs = [f_unlit],
        mnemonic = "HaskellUnlit",
    )
    return f_unlit

def check_module_names(ctx, config, srcs_options, main_is = None):
    """Checks that the module names of source files correspond to their paths.

    See guess_module_name for the path->name logic.

    Args:
      ctx: The current rule context
      config: A HaskellCompilerInfo provider.
      srcs_options: A struct of attributes that affect the compilation of Haskell
        source files.
      main_is: The expected name of the module defining "main", if any.

    Returns:
      A File which is empty.  A successful build of that file indicates
        that the module names are as expected.
    """

    guessed_srcs = {
        _maybe_unlit(ctx, config, s): _guess_module_name(s)
        for s in srcs_options.srcs
    }

    module_names_ok = ctx.actions.declare_file(ctx.label.name + ".module-names-ok")

    # CheckModuleNames uses the GHC API, so it needs some GHC-related flags.
    # However, don't pass flags like -package-db or -package which make GHC look for
    # built dependencies. We don't want this action to rely on any file contents.
    modules_ok_opts = (
        ghc_options.library_root(config) +
        config.common_options +
        srcs_options.arguments
    )
    ctx.actions.run_shell(
        command = "{} $@ && touch {}".format(
            ctx.executable._check_module_names.path,
            module_names_ok.path,
        ),
        arguments = [
                        "--source-file={},{}".format(f.path, guess)
                        for f, guess in guessed_srcs.items()
                    ] +
                    (["--main-is=" + main_is] if main_is else []) +
                    ["--ghcopt=" + o for o in modules_ok_opts],
        inputs = guessed_srcs.keys() +
                 [ctx.executable._check_module_names] +
                 ctx.files._ghc_settings,
        outputs = [module_names_ok],
        mnemonic = "HaskellCheckModuleNames",
    )
    return module_names_ok

def _guess_module_name(file):
    """Compute the expected module name of a Haskell source file.

    Looks for the longest suffix of the path that is CamelCased;
    i.e., where each directory starts with a capital letter.

    Args:
      file: A source File.

    Returns:
      A Haskell module name, as a string.

    """
    _, suffix = split_dirname(file.dirname)

    # Append the file's basename, without its extension.
    # The basename may be lower-cased if it's a Main module, but CheckModules
    # will ignore our guess in that case.
    return (paths.join(suffix, paths.split_extension(file.basename)[0])
        .replace("/", "."))

def _parse_reexport_pair(r):
    """Parses an entry of the reexported_modules attribute.

    It is of the form: "Foo as Bar", which means that "Foo" is a module exported
    by a dependency, and this target is reexporting it as "Bar".

    Args:
      r: A string as defined in the reexported_modules attribute.

    Returns:
      A pair (from, to).
    """
    s = r.split(" as ")
    if len(s) != 2:
        fail("Unrecognized reexported module spec " + s)
    return (s[0], s[1])

def render_reexport(m, r):
    """Turns a reeexport into an entry for ghc-pkg's exposed-modules field.

    We also use this format in the haskell_exposed_modules output group.
    Args:
      m: A string module name.
      r: A struct, as in the values of HaskellInfo.exposed_modules.

    Returns:
      A string.
    """
    return "{} from {}:{}".format(
        m,
        r.original_package_key,
        r.original_module,
    )

def _collect_reexports(ctx, config, dep_modules, reexported_modules):
    """Computes the modules reexported by this target.

    Args:
      ctx: The current rule context
      config: A HaskellCompilerInfo provider
      dep_modules: A dict as stored in HaskellInfo.exposed_modules,
        collecting all modules exported by immediate deps of this rule.
      reexported_modules: A list of strings, as in the reexported_modules attribute.

    Returns:
      A dictionary as in HaskellInfo.exposed_modules.
    """

    result = dict()
    for r in reexported_modules:
        (old_module, new_module) = _parse_reexport_pair(r)
        if old_module in dep_modules:
            result[new_module] = dep_modules[old_module]
        elif old_module in config.default_exposed_modules:
            result[new_module] = config.default_exposed_modules[old_module]
        else:
            fail("Reexporting an unknown module: " + r + "," + str(dep_modules))

    return result

def immediate_exports(package_key, module_names):
    """Collects the given modules into a dict as stored in HaskellInfo.exposed_modules."""
    return {m: struct(
        original_module = m,
        original_package_key = package_key,
    ) for m in module_names}

def collect_exposed_modules(
        ctx,
        config,
        metadata,
        dep_exposed_modules,
        hidden_modules,
        reexported_modules,
        srcs):
    """Computes the modules exposed from this library, including reexports.

    Args:
      ctx: The current rule context.
      config: A HaskellCompileInfo provider.
      metadata: The identifiers for the current rule.
      dep_exposed_modules: A list of dicts as stored in HaskellInfo.exposed_modules,
        from the deps of this rule.
      hidden_modules: A list of strings; module names.
      reexported_modules: A list of strings, as defined in the rule attribute.
      srcs: A list of Files.

    Returns:
      A dict as stored in HaskellInfo.exposed_modules.
    """

    compiled_modules = immediate_exports(
        metadata.key,
        [
            m
            for m in [_guess_module_name(s) for s in srcs]
            if m not in hidden_modules
        ],
    )

    # Shortcut to avoid extra work (merging the deps) in the common case:
    if srcs and not reexported_modules:
        return compiled_modules

    merged_deps = dicts.add(*dep_exposed_modules)

    # Forwarding rule
    if not srcs and not reexported_modules:
        return merged_deps

    return dicts.add(
        compiled_modules,
        _collect_reexports(
            ctx,
            config,
            merged_deps,
            reexported_modules,
        ),
    )
