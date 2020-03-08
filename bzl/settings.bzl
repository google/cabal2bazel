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

"""Build settings for configuring Haskell rules."""

load("@bazel_skylib//rules:common_settings.bzl", "BuildSettingInfo")

_PerTargetGhcoptsInfo = provider(
    # Entries is a list of structs with the fields:
    #  - prefixes: A list of prefixes to match
    #  - flags: A list of flags to pass to GHC for targets matching those prefixes.
    fields = ["entries"],
)

def _split(str, delimiter):
    # Work around an edge case: "".split(..) returns [""], not [].
    if not str:
        return []
    return str.split(delimiter)

def _maybe_prepend_slashes(s):
    if s.startswith("//"):
        return s
    else:
        return "//" + s

def _per_target_ghcopts_impl(ctx):
    entries = []
    flag_value = ctx.build_setting_value

    for kv in _split(flag_value, ";"):
        kv_at = kv.split("@")
        if len(kv_at) != 2:
            fail("Unrecognized per_target_ghcopts entry; should be prefix@flag: " + str(kv))
        prefixes = [_maybe_prepend_slashes(p) for p in _split(kv_at[0], ",")]
        flags = _split(kv_at[1], ",")
        for flag in flags:
            if not flag.startswith("-"):
                fail("Unrecognized flag " + str(flag) + "; should start with '-'")
        entries.append(struct(prefixes = prefixes, flags = flags))

    return _PerTargetGhcoptsInfo(entries = entries)

per_target_ghcopts = rule(
    implementation = _per_target_ghcopts_impl,
    build_setting = config.string(flag = True),
)

_GhcoptsInfo = provider(fields = ["options"])

def _ghcopts_impl(ctx):
    setting_value = ctx.build_setting_value
    if setting_value:
        # Don't worry about quoting flags that contain spaces.
        # There's a limited number of flags we might reasonably pass for this
        # use case.
        options = [x for x in setting_value.split(" ") if x != " "]
    else:
        # Work around edge case: "".split(" ")=[""]
        options = []
    return _GhcoptsInfo(options = options)

ghcopts = rule(
    implementation = _ghcopts_impl,
    build_setting = config.string(flag = True),
)

_settings_attributes = {
    "_profile": attr.label(
        default = Label("//settings:profile"),
    ),
    "_profile_ghcopts": attr.label(
        default = Label("//settings:profile_ghcopts"),
    ),
    "_ghcopts": attr.label(
        default = Label("//settings:ghcopts"),
    ),
    "_per_target_ghcopts": attr.label(
        default = Label("//settings:per_target_ghcopts"),
    ),
}

def _settings_get(ctx):
    """Collects compile settings for the current rule.

    These are separate from the compile_srcs arguments because the latter
    get passed along to, e.g., hrepl, but these are specific to the current
    Bazel run.  (hrepl itself can take GHC flags directly on the command-line.)

    Args:
      ctx: The current rule context.

    Returns:
      A struct with the following fields:
      - ghcopts: Flags to pass to GHC when building
      - profile: Whether to build with profiling.
      - profile_ghcopts: A list of flags to pass when profiling.
    """

    # TODO(judahjacobson): Deprecate and remove the --define way (ctx.var) of setting these flags.
    if ctx.var.get("prof", "false") == "true":
        profile = True
        prof_ghcopts_flag = ctx.var.get("prof_ghcopts", "-fprof-auto")
        if prof_ghcopts_flag:
            profile_ghcopts = [o for o in prof_ghcopts_flag.split(" ") if o]
        else:
            profile_ghcopts = []
    elif ctx.attr._profile[BuildSettingInfo].value:
        profile = True
        profile_ghcopts = ctx.attr._profile_ghcopts[_GhcoptsInfo].options
    else:
        profile = False
        profile_ghcopts = None

    ghcopts = [o for o in ctx.attr._ghcopts[_GhcoptsInfo].options]
    if ctx.var.get("haskell_core_dump") == "true":
        ghcopts.append("-ddump-simpl")

    # Apply any per-target flags that match the current label.
    # TODO(judahjacobson): Switch to regexps once Starlark supports
    # them (b/31937774).
    ghcopts += [
        flag
        for entry in ctx.attr._per_target_ghcopts[_PerTargetGhcoptsInfo].entries
        if any([str(ctx.label).startswith(prefix) for prefix in entry.prefixes])
        for flag in entry.flags
    ]

    return struct(
        ghcopts = ghcopts,
        profile = profile,
        profile_ghcopts = profile_ghcopts,
    )

settings = struct(
    attributes = _settings_attributes,
    get = _settings_get,
)
