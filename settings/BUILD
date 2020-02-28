load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")
load(
    "//bzl:settings.bzl",
    "ghcopts",
    "per_target_ghcopts",
)

per_target_ghcopts(
    name = "per_target_ghcopts",
    build_setting_default = "",
    visibility = ["//visibility:public"],
)

bool_flag(
    name = "profile",
    build_setting_default = False,
    visibility = ["//visibility:public"],
)

config_setting(
    name = "profile_mode",
    flag_values = {
        ":profile": "True",
    },
)

ghcopts(
    name = "profile_ghcopts",
    build_setting_default = "-fprof-auto",
    visibility = ["//visibility:public"],
)

ghcopts(
    name = "ghcopts",
    build_setting_default = "",
    visibility = ["//visibility:public"],
)
                                                                                                    

