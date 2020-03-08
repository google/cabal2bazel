load("//bzl:def.bzl", "haskell_proto_library", "haskell_test")
load("@bazel_skylib//rules:build_test.bzl", "build_test")

proto_library(
    name = "foo_proto",
    srcs = ["foo.proto"],
)

proto_library(
    name = "bar_proto",
    srcs = ["bar.proto"],
    deps = [":foo_proto"],
)

# A haskell_proto_library rule that depends (only) transitively on foo_proto.
haskell_proto_library(
    name = "bar_haskell_pb",
    deps = [":bar_proto"],
)

haskell_test(
    name = "BarTest",
    srcs = ["BarTest.hs"],
    deps = [
        ":bar_haskell_pb",
        # TODO
    ],
)

proto_library(
    name = "bar_forwarded_proto",
    deps = [":bar_proto"],
)

haskell_proto_library(
    name = "bar_forwarded_haskell_pb",
    deps = [":bar_forwarded_proto"],
)

haskell_test(
    name = "BarForwardedTest",
    srcs = ["BarTest.hs"],
    deps = [
        ":bar_forwarded_haskell_pb",
        # TODO
    ],
)

# Make sure that proto-lens works with generated .proto files.  It's a little
# more tricky than hprotoc since proto-lens outputs the generated files
# separately instead of as one tarball.
genrule(
    name = "generate_proto_file",
    outs = ["generated.proto"],
    cmd = "echo 'syntax = \"proto2\";message Generated{};' > $@",
)

proto_library(
    name = "generated_proto",
    srcs = ["generated.proto"],
)

haskell_proto_library(
    name = "generated_haskell_pb",
    deps = [":generated_proto"],
)

haskell_test(
    name = "GeneratedTest",
    srcs = ["GeneratedTest.hs"],
    deps = [
        ":generated_haskell_pb",
        # TODO
    ],
)

# Check that we can use .proto files from subdirectories.
proto_library(
    name = "sub_proto",
    srcs = ["sub_dir/sub.proto"],
)

haskell_proto_library(
    name = "sub_haskell_pb",
    deps = [":sub_proto"],
)

haskell_test(
    name = "SubTest",
    srcs = ["SubTest.hs"],
    deps = [
        ":sub_haskell_pb",
        # TODO
    ],
)

# Test that we can handle the camel-casing behavior of proto-lens-protoc.
# In practice, our filenames will not be camel-cased, but path components might.
# And in particular we should handle filenames with consecutive upper-case letters.
proto_library(
    name = "camel_case_proto",
    srcs = ["CamelCASE.proto"],
)

haskell_proto_library(
    name = "camel_case_haskell_pb",
    deps = [":camel_case_proto"],
)

build_test(
    name = "camel_case_test",
    targets = [":camel_case_haskell_pb"],
)

# A proto library containing both a dash and an underscore.
proto_library(
    name = "dashed-file_path_proto",
    srcs = ["dashed-file_path.proto"],
)

haskell_proto_library(
    name = "dashed-file_path_haskell_pb",
    deps = [":dashed-file_path_proto"],
)

haskell_test(
    name = "DashedFilePathTest",
    srcs = ["DashedFilePathTest.hs"],
    deps = [
        ":dashed-file_path_haskell_pb",
        # TODO
    ],
)
