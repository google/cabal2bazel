# Test that we can link against dependent cc_libraries.

load("//bzl:def.bzl", "haskell_library", "haskell_test")

package(
    features = ["haskell_strict_deps"],
)

# A C++ library that depends on another library.
cc_library(
    name = "foo_c",
    srcs = ["foo.cc"],
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

# Corner case: it should be OK to specify cc_deps in "forwarding" libraries
# like FooForwarding that don't specify any srcs.
haskell_library(
    name = "FooForwardingDep",
    srcs = ["Foo.hs"],
)

haskell_library(
    name = "FooForwarding",
    deps = [
        ":Foo",
        ":foo_c",
    ],
)

haskell_test(
    name = "ForwardingFFI",
    srcs = ["TransitiveFFI.hs"],
    deps = [":FooForwarding"],
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
# This is generally bad practice for code in google3, but is needed
# by some third_party packages.
haskell_test(
    name = "HeadersTest",
    srcs = ["HeadersTest.hs"],
    deps = [
        ":HeadersLib",
        ":header_provider",
    ],
)

haskell_library(
    name = "HeadersLib",
    srcs = ["HeadersLib.hs"],
    deps = [
        ":header_provider",
    ],
)

cc_library(
    name = "header_provider",
    hdrs = ["include/provider.hh"],
    includes = ["include"],
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

# Test to make sure "Stub" is actually compiled.
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
)

haskell_test(
    name = "SimpleFFIDebug",
    srcs = ["SimpleFFI.hs"],
    deps = [":foo_c_debug"],
)
