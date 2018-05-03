# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Helper functions for managing providers between Haskell Skylark rules.

Every haskell_library rule has a "haskell" provider which is a struct with
several Haskell-specific fields.  "Transitive" data (which is the union of all
recursive dependencies) is stored in the "haskell.transitive" subfield.
"""

def new_transitive(transitive_deps=struct(), hi_archive=None, cache=None, hs_libs=[]):
  """Make a struct which is suitable for the "haskell.transitive" provider.

  The transitive fields are:
    hi_archives: An archive file for each library's .hi files (see
      archive.bzl for more details)
    caches: A GHC package database cache file for each Haskell library
    hs_libs: An archive file (i.e., "lib*.a") for each Haskell library
    cc_libs: The set of library files from all (recursive) cc_library
      dependencies
    link_flags: The set of flags needed to link the cc_library dependencies

  TODO(judahjacobson): We treat link_flags as a set in order to dedup them
  between transitive providers, but this might break linkopts that depend on the
  order within a single cc_library rule.  It's unclear whether this breaks
  anything in practice.

  Args:
    transitive_deps: A struct of transitive fields (as specified above), from
      the dependencies of this rule.
    hi_archive: (Optional): An archive file for this library's .hi files.
    cache: A GHC package database cache file for this library.
    hs_libs: A list of object archives for this library.
  Returns:
    A struct of transitive fields.
  """
  return struct(
      hi_archives=(transitive_deps.hi_archives | [hi_archive]
                   if hi_archive else transitive_deps.hi_archives),
      caches=(transitive_deps.caches | [cache]
              if cache else transitive_deps.caches),
      hs_libs=(transitive_deps.hs_libs | hs_libs),
      cc_libs=transitive_deps.cc_libs,
      link_flags=transitive_deps.link_flags,
  )

def add_cc_lib(transitive_deps=struct(), cc_dep=struct()):
  """Make a new "haskell.transitive" struct by adding a cc_library target.

  Args:
    transitive_deps: A struct of transitive fields.
    cc_dep: A cc_library dependency.
  Returns:
    A new transitive fields struct which includes the given cc_library.
  """
  return struct(
      hi_archives=transitive_deps.hi_archives,
      caches=transitive_deps.caches,
      hs_libs=transitive_deps.hs_libs,
      cc_libs=transitive_deps.cc_libs | cc_dep.cc.libs,
      link_flags=transitive_deps.link_flags | cc_dep.cc.link_flags)


def collect_deps(deps):
  """Group and combine the given Haskell and C/C++ dependencies.

  Args:
    deps: A list of rules which can be either Skylark haskell_library or native
    cc_library rules.

  Returns:
    A struct consisting of the following fields:
      exposed_package_names: The set of GHC package IDs for haskell_library rules
        that appeared directly in the deps.
      transitive: The union of the transitive providers for all Haskell
        dependencies.
      cc_headers: The set of C/C++ headers from the immediate cc_library
        dependencies (as well as their own transitive dependencies).  They may
        be needed by CPP.
      cpp_flags: Flags for running CPP in order to use the cc_headers when
        building the current rule.
  """
  # Properties of this particular rule only:
  exposed_package_names = depset()
  cc_headers = depset()
  cpp_flags = []
  # "Transitive" properties:
  hi_archives = depset()
  caches = depset()
  hs_libs = depset(order="topological")
  cc_libs = depset(order="topological")
  link_flags = depset()

  for dep in deps:
    if hasattr(dep, "haskell"):
      # haskell_library rule
      exposed_package_names = (
          exposed_package_names | dep.haskell.exposed_package_names)
      # Forward all of the transitive providers:
      hi_archives = hi_archives | dep.haskell.transitive.hi_archives
      caches = caches | dep.haskell.transitive.caches
      hs_libs = hs_libs | dep.haskell.transitive.hs_libs
      cc_libs = cc_libs | dep.haskell.transitive.cc_libs
      link_flags = link_flags | dep.haskell.transitive.link_flags
    elif hasattr(dep, "cc"):
      # cc_library rule
      cc_headers = cc_headers | dep.cc.transitive_headers
      cpp_flags += dep.cc.compile_flags
      # Forward the relevant transitive providers:
      cc_libs = cc_libs | dep.cc.libs
      link_flags = link_flags | dep.cc.link_flags
    else:
      fail("Target can't be used in \"deps\" field: " + str(dep.label))

  return struct(
      exposed_package_names=exposed_package_names,
      transitive=struct(
          hi_archives=hi_archives,
          caches=caches,
          hs_libs=hs_libs,
          cc_libs=cc_libs,
          link_flags=link_flags),
      cc_headers=cc_headers,
      cpp_flags=cpp_flags,
  )

# List of output groups relevant to haskell rules/aspects
_haskell_output_groups = [
    "haskell_source_files",
    "haskell_compile_info",
    "haskell_dummy_ffi_deps",
    "haskell_shared_ffi_deps",
]

def forward_deps(ctx, deps, extra_providers={}):
  """Return a struct that forwards the providers of the immediate Haskell deps.

  Args:
    ctx: The current rule context.
    deps: A list of Haskell library rules (haskell_library,
      haskell_proto_library or haskell_proto_aspect) which should be forwarded
      by this rule.
    extra_providers: A dictionary of providers which should be passed in
    addition to the standard ones from haskell_library.

  Returns:
    A struct consisting of the following fields:
      files: The union of the output files from all immediate dependencies.
      output_groups: The union of the output groups from all immediate
        dependencies.
      runfiles: The runfiles for all dependencies.
      haskell: A struct consisting of the following fields:
        exposed_package_names: The set of GHC package IDs for haskell_library rules
          that appeared directly in the deps.
        transitive: The union of the transitive providers for all Haskell
          dependencies.
      Plus: any entries of extra_providers.
  """
  files = depset()
  output_groups = {}
  # Forward files and output_groups from haskell_library rules in the deps.
  # (We don't explicitly forward files from cc_library deps, to match the behavior
  # of regular haskell_library rules.  They'll come from providers via the call
  # to collect_deps below.)
  for dep in deps:
    if hasattr(dep, "haskell"):
      # Aspects return files through their "haskell" provider; see
      # fix_aspect_provider() below.
      if hasattr(dep.haskell, "files"):
        files = files | dep.haskell.files
      else:
        files = files | dep.files

      if hasattr(dep, "output_groups"):
        for group in _haskell_output_groups:
          if not group in dep.output_groups:
            continue
          if group in output_groups:
            output_groups[group] = dep.output_groups[group] | output_groups[group]
          else:
            output_groups[group] = dep.output_groups[group]

  haskell_deps = collect_deps(deps)

  return struct(
      files=files,
      runfiles=ctx.runfiles(collect_default=True),
      output_groups=output_groups,
      haskell=struct(
          exposed_package_names=haskell_deps.exposed_package_names,
          transitive=haskell_deps.transitive),
      **extra_providers)

def fix_aspect_providers(lib):
  """Change a struct of Haskell library providers to be returnable from an aspect.

  Args:
    lib: A struct of Haskell library providers, e.g., as returned from
      forward_deps().
  Returns:
    A struct of providers which is valid to return from an aspect.
  """
  return struct(
      # Don't forward runfiles, since they're not supported by aspects.
      output_groups=lib.output_groups,
      haskell=struct(
          # Aspects don't forward the "files" provider, so pass it through
          # the haskell sub-struct instead.
          files=lib.files,
          exposed_package_names=lib.haskell.exposed_package_names,
          transitive=lib.haskell.transitive))
