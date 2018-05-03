load("//bzl:def.bzl", "haskell_proto_library", "haskell_test")
load("//bzl:hackage.bzl", "hackage")

package(
    features = ["haskell_strict_deps"],
)

proto_library(
    name = "foo_proto",
    srcs = ["foo.proto"],
)

proto_library(
    name = "bar_proto",
    srcs = ["bar.proto"],
    deps = [":foo_proto"],
)

haskell_proto_library(
    name = "bar_haskell_pb_lens",
    api = "lens",
    deps = [":bar_proto"],
)

haskell_test(
    name = "BarLensTest",
    srcs = ["BarLensTest.hs"],
    deps = [
        ":bar_haskell_pb_lens",
        #        hackage("data-default"),
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
    name = "generated_haskell_pb_lens",
    api = "lens",
    deps = [":generated_proto"],
)

haskell_test(
    name = "GeneratedLensTest",
    srcs = ["GeneratedLensTest.hs"],
    deps = [
        ":generated_haskell_pb_lens",
        #        hackage("data-default"),
    ],
)

# Check that we can use .proto files from subdirectories.
proto_library(
    name = "sub_proto",
    srcs = ["sub_dir/sub.proto"],
)

haskell_proto_library(
    name = "sub_haskell_pb_lens",
    api = "lens",
    deps = [":sub_proto"],
)

haskell_test(
    name = "SubLensTest",
    srcs = ["SubLensTest.hs"],
    deps = [
        ":sub_haskell_pb_lens",
        #        hackage("data-default"),
    ],
)

# Test that we can handle the camel-casing behavior of proto-lens-protoc.
# In practice, google3 filenames will not be camel-cased, but path components might.
# And in particular we should handle filenames with consecutive upper-case letters.
proto_library(
    name = "camel_case_proto",
    srcs = ["CamelCASE.proto"],
)

haskell_proto_library(
    name = "camel_case_haskell_pb",
    api = "lens",
    deps = [":camel_case_proto"],
)

# A proto library containing both a dash and an underscore.
proto_library(
    name = "dashed-file_path_proto",
    srcs = ["dashed-file_path.proto"],
)

haskell_proto_library(
    name = "dashed-file_path_haskell_pb",
    api = "lens",
    deps = [":dashed-file_path_proto"],
)

haskell_test(
    name = "DashedFilePathTest",
    srcs = ["DashedFilePathTest.hs"],
    deps = [
        ":dashed-file_path_haskell_pb",
        #        hackage("data-default"),
    ],
)
