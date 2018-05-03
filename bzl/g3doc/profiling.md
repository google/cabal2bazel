# Profiling Haskell Binaries

To enable profiling for a build, pass the `--define prof=true` flag to bazel.

```bash
bazel build --define prof=true //path/to:target
```

The flags `-prof -fprof-auto -rtsopts` will be passed to the haskell compiler.

When profiling is enabled, pass the `-p` RTS option to the binary to write
profiling data to `[binary_name].prof`.

```bash
bazel-bin/path/to/target +RTS -p
less target.prof
```

For more details, see
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html.
