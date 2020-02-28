# Test that we can link against dependent cc_libraries.

load("//bzl:def.bzl", "haskell_binary", "haskell_library", "haskell_test")

# A C++ library that depends on another library.
cc_library(
    name = "foo_c",
    srcs = ["foo.cc"],
    deps = [
        "@com_google_absl//absl/strings:str_format",
    ],
)

# A Haskell binary that depends directly on the C++ library.
haskell_test(
    name = "SimpleFFI",
    srcs = ["SimpleFFI.hs"],
    deps = [":foo_c"],
)

haskell_library(
    name = "Foo",
    srcs = ["Foo.hs"],
    deps = [":foo_c"],
)

# A Haskell binary that depends transitively on the C++ library.
haskell_test(
    name = "TransitiveFFI",
    srcs = ["TransitiveFFI.hs"],
    deps = [":Foo"],
)

# A library that's tagged with linkstatic, which means Bazel won't
# generate a DSO for it.
cc_library(
    name = "foo_c_static",
    srcs = ["foo.cc"],
    linkstatic = True,
    deps = [
        "@com_google_absl//absl/strings:str_format",
    ],
)

# Check that dynamic binaries can still link against linkstatic=True libraries.
haskell_test(
    name = "DynamicBinStaticLib",
    srcs = ["SimpleFFI.hs"],
    linkstatic = False,
    deps = [":foo_c_static"],
)

# Corner case: it should be OK to specify cc_deps in "forwarding" libraries
# like FooForwarding that don't specify any srcs.
haskell_library(
    name = "FooForwardingDep",
    srcs = ["Foo.hs"],
)

haskell_library(
    name = "FooForwarding",
    deps = [
        # Order is important.  (See FooForwardingCcFirst.)
        ":Foo",
        ":foo_c",
    ],
)

haskell_test(
    name = "ForwardingFFI",
    srcs = ["TransitiveFFI.hs"],
    # Intentionally depend on the forwarding rule:
    tags = ["nofixdeps"],
    deps = [":FooForwarding"],
)

# Double-check that a different order of deps (with the cc_library first)
# doesn't confuse the build rules. For details, see cl/258815193.
haskell_library(
    name = "FooForwardingCcFirst",
    deps = [
        # Order is important.
        ":foo_c",
        ":Foo",
    ],
)

haskell_test(
    name = "ForwardingFFICcFirst",
    srcs = ["TransitiveFFI.hs"],
    # Intentionally depend on the forwarding rule:
    tags = ["nofixdeps"],
    deps = [":FooForwardingCcFirst"],
)

# Test that alwayslink libraries are linked correctly.
# This class defines a foo() function internally.
cc_library(
    name = "provider",
    srcs = ["provider.cc"],
    alwayslink = 1,
)

# This class uses the foo() function without depending directly on
# foo_provider, assuming that it will get linked in correctly.
# (This is a simple way to check that linking included foo_provider,
# which is necessary in particular for libraries use registration.)
cc_library(
    name = "user",
    srcs = ["user.cc"],
    # NOTE: foo_provider is *not* specified in the deps, but it
    # is used internally.
)

# Another user cc_library that comes before "provider" alphabetically (to
# satisfy buildifier which wants the below "deps" lists to be sorted).
cc_library(
    name = "another_user",
    srcs = ["user.cc"],
)

# Test that the order that deps are specified doesn't matter.  Otherwise we
# might accidentally get it right in this test but wrong in some other rules.
haskell_test(
    name = "AlwaysLinkTest",
    srcs = ["AlwaysLinkTest.hs"],
    deps = [
        # provider comes before user
        ":provider",
        ":user",
    ],
)

haskell_test(
    name = "AlwaysLinkTest2",
    srcs = ["AlwaysLinkTest.hs"],
    deps = [
        # provider comes after another_user
        ":another_user",
        ":provider",
    ],
)

# Test that CPP works with cc_library headers.
# This is generally bad practice in our build, but is needed
# by some third_party packages.
haskell_test(
    name = "HeadersTest",
    srcs = ["HeadersTest.hs"],
    deps = [
        ":HeadersLib",
        "//test_support:header_provider",
    ],
)

haskell_library(
    name = "HeadersLib",
    srcs = ["HeadersLib.hs"],
    deps = [
        "//test_support:header_provider",
    ],
)

# Check that we handle "foreign import wrapper" directives and the
# "*_stub.h" files they output.
# Currently we don't actually do anything with such files yet; but if we run
# this test with --noforge, it shouldn't dump a "Stub_stub.h" file in the
# middle of the source tree.
haskell_library(
    name = "Stub",
    srcs = ["Stub.hs"],
)

# Test to make sure "Stub" is actually compiled by TAP
haskell_test(
    name = "StubTest",
    srcs = ["StubTest.hs"],
    deps = [":Stub"],
)

# Test that we can link against cc_libraries built with "-g"
cc_library(
    name = "foo_c_debug",
    srcs = ["foo.cc"],
    copts = ["-g"],
    deps = [
        "@com_google_absl//absl/strings:str_format",
    ],
)

haskell_test(
    name = "SimpleFFIDebug",
    srcs = ["SimpleFFI.hs"],
    deps = [":foo_c_debug"],
)

# Test that direct C dependencies can be used in TemplateHaskell.
cc_library(
    name = "extra_objs_direct",
    srcs = ["extra_objs_direct.cc"],
)

haskell_library(
    name = "ExtraObjsLib",
    srcs = ["ExtraObjsLib.hs"],
    deps = [":extra_objs_direct"],
)

haskell_test(
    name = "ExtraObjsTest",
    srcs = ["ExtraObjsTest.hs"],
    executes_deps_in_compile = True,
    deps = [":ExtraObjsLib"],
)

# Test that transitive FFI deps can be used in TemplateHaskell.
cc_library(
    name = "transitive_objs",
    srcs = ["transitive_objs.cc"],
    deps = [":three"],
)

cc_library(
    name = "three",
    srcs = ["three.cc"],
    hdrs = ["three.h"],
)

haskell_library(
    name = "TransitiveObjsLib",
    srcs = ["ExtraObjsLib.hs"],
    deps = [":transitive_objs"],
)

haskell_test(
    name = "TransitiveObjsTest",
    srcs = ["ExtraObjsTest.hs"],
    executes_deps_in_compile = True,
    deps = [":TransitiveObjsLib"],
)

# Tests that TemplateHaskell can transitively use C++ deps
# through another module in the same compile action.
# (This step is tricky because, in order for one module to
# run compiled code from another, GHC creates a temporary
# shared library out of the latter (in this case, out of
# ExtraObjsForward).
haskell_test(
    name = "ExtraObjsJoinedTest",
    srcs = [
        "ExtraObjsForward.hs",
        "ExtraObjsJoinedTest.hs",
    ],
    executes_deps_in_compile = True,
    deps = [":ExtraObjsLib"],
)

# A test that uses TemplateHaskell to load a library that
# depends transitively on //base, which has a fair number of
# idiosyncrasies.
haskell_test(
    name = "TemplateHaskellBaseTest",
    srcs = ["TemplateHaskellBaseTest.hs"],
    executes_deps_in_compile = True,
    deps = [":Foo"],
)

# This script forces the linker to use a (trivial) linker script.
# TODO(judahjacobson): Make the linker script nontrivial to confirm that
# we're actually honoring the cc_library's linkopts.
haskell_test(
    name = "LinkInputsTest",
    srcs = ["LinkInputsTest.hs"],
    deps = ["linker_script_cc"],
)

cc_library(
    name = "linker_script_cc",
    linkopts = ["-Wl,--dynamic-list,$(location :dynamic.lds)"],
    deps = ["dynamic.lds"],
)
