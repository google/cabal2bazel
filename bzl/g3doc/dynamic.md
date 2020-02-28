# Dynamic Linking in the Haskell Build Rules

go/haskell-dynamic

[TOC]

## Overview

Our Haskell build rules use dynamic shared objects (DSOs, i.e., `lib*.so`) in
several ways. This document describes why we need DSOs and how they integrate
with other parts of the build.

By default, `haskell_binary` rules produce static binaries with no dependencies
on DSOs outside of system libraries such as `libc` (but not `libc++`). This
behavior is the same as for `cc_binary`, which [defaults][linkstatic] to
`linkstatic=True` and only links against static library archives (`lib*.a`).

[linkstatic]: https://docs.bazel.build/versions/master/be/c-cpp.html#cc_binary.linkstatic

However, the build rules use DSOs in three ways. They support multiple features
of the Haskell compiler (GHC):

-   To support Haskell language extensions that enable compile-time
    metaprogramming, such as `TemplateHaskell`.
-   To let the [hrepl](https://github.com/google/hrepl) interpreter load library code at
    runtime.
-   To build dynamic Haskell binaries with `linkstatic=False`. (This is
    uncommon, but necessary in some cases; see
    [Choosing the linker way](#Choosing) below for details.

The DSOs may be of both `haskell_library` and `cc_library` dependencies built by
Bazel.

### hrepl

Although GHC is a binary compiler, it can also run as an [interpreter], called
GHCi. The "hrepl" tool integrates GHCi with the Bazel BUILD rules.

[interpreter]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html

hrepl/GHCi improve developer workflow in several key ways. First, reloading the
modules in the interpreter is faster than re-running `bazel build`, giving much
quicker feedback about compilation errors. Second, executing arbitrary code from
within the interpreter is faster and more convenient than writing and compiling
explicit one-off source files (as well as any associated BUILD rules). Third,
GHCi provides several ways to interactively browse the library APIs, for example
via the `:info` or `:type` commands.

Enabling those features places several requirements on hrepl:

-   Interpreter sessions should be based on existing Bazel build rules such as
    `haskell_library`, and should not need additional configuration. For
    example, it should not expect developers to write a new BUILD target that
    defines the scope of the interpreter session.
-   Interpreter sessions should be able to load several Haskell rules at once.
    As above, this feature allows developers to start such one-off sessions
    without writing a new BUILD target.
-   The interpreter should support Haskell targets that depend transitively on
    `cc_library` rules. Our codebase as well as some third-party Haskell
    packages use C/C++ to interact with other libraries (e.g., Stubby) and to
    optimize low-level code.

GHCi works by taking a Haskell package dependency scope (defined by the
`-package` and `-package-db` flags) and a list of Haskell source files to
interpret. It first loads any dependencies via their compiled library archives.
Then, it compiles the given source files to bytecode or to object code
(uncommon, generally to support certain language features).

### Compile-Time Metaprogramming

Several features of GHC enable compile-time metaprogramming. GHC implements
those features by loading and running library object code in the middle of its
compilation steps.

-   [Template Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell),
    and the related
    [QuasiQuotes](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-QuasiQuotes),
    allows writing Haskell functions that generate code in the middle of
    compiling a file.
-   [Compiler plugins](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins)
    allow users to adjusts GHC's behavior.
-   [Source annotations](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#source-annotations)
    (ANN pragmas) attach data to identifiers in source code.

Some examples of how we have used this in practice:

-   Plugins such as [ghc-typelits-extra] improve GHC's type checking and
    inference.
-   The [lens] package uses TemplateHaskell to automatically generate lenses
    for individual record fields.
-   Some third-party packages either use Template Haskell themselves, or depend
    transitively on another library that does.

[ghc-typelits-extra]: https://hackage.haskell.org/package/ghc-typelits-extra
[lens]: https://hackage.haskell.org/package/lens/docs/Control-Lens-TH.html

### Dynamic GHC programs

GHC has two ways to load code at compile time or in the interpreter. For
historical details, see
[The Dynamic Linking Debate](https://gitlab.haskell.org/ghc/ghc/wikis/dynamic-linking-debate).
This section will summarize the current behavior.

-   Originally, GHC implemented its own linker as part of its runtime system,
    which loaded static object files (`*.a` or `*.o`) directly. This approach
    tended to be cumbersome and fragile.
-   In 2014 (version 7.8), GHC made it possible to use the system dynamic linker
    instead. That approach uses system calls like `dlopen` to access compiled
    code from DSOs.

GHC's current version uses the system dynamic library by default. The static
mode is still available for custom builds of the compiler, but it has known
issues and its codebase is not well-understood.

Our build rules use the dynamic linker, as of Q1 2019. Originally we used the
static way, since our rules predated the relevant changes to GHC. That old
approach caused us to run into several issues and ultimately blocked the upgrade
to GHC version 8.6 until the switch. See b/124259169 for details.

#### Choosing the linker way

GHC distributions contain two versions of its runtime library (`rts`):

-   As a static archive, whose implementation uses the custom static linker.
-   As a DSO, whose implementation uses the system dynamic linker.

In other words, GHC chooses between the two codepaths based on the linkage of
the `ghc` binary executable. If `ghc` is statatically linked against `rts`, it
will use the custom static linker at runtime. Similarly, a dynamically-linked
version of `ghc` (the default) will use the dynamic linker at runtime. It is not
clear whether this behavior is a hard requirement of how GHC works, or just a
simplification in its build system. This behavior also applies to any other
binary using GHC as a library to compile code: its runtime behavior (e.g., for
Template haskell) will depend on whether that binary itself is linked statically
or dynamically against `rts`.

[GHC API]: http://hackage.haskell.org/package/ghc

As a result, we build the Haskell CodeSearch indexer with `linkstatic=False` so
that it uses the same mode for TemplateHaskell as the regular Bazel builds.
