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

"""Package description auto-generated from foo.cabal by cabal2build.

"""
description = struct(
    package = struct(pkgName = "foo", pkgVersion = "1"),
    license = "BSD3",
    licenseFiles = ["LICENSE"],
    copyright = "",
    maintainer = "haskell@google.com",
    author = "Foo",
    stability = "",
    testedWith = [
        ("ghc", "==7.6.3"), ("ghc", "==7.8.4"), ("ghc", "==7.10.1")
    ],
    homepage = "http://example.com/foo",
    pkgUrl = "",
    bugReports = "http://bugs/",
    sourceRepos = [],
    synopsis = "Foo.",
    description = "Fake description.",
    category = "General",
    customFieldsPD = [],
    buildDepends = [],
    specVersionRaw = None,
    buildType = "Simple",
    library = struct(
        exposedModules = [],
        reexportedModules = [],
        requiredSignatures = [],
        exposedSignatures = [],
        libExposed = True,
        libBuildInfo = struct(
            buildable = True,
            buildTools = [],
            cppOptions = [],
            ccOptions = [],
            ldOptions = [],
            pkgconfigDepends = [],
            frameworks = [],
            cSources = [],
            jsSources = [],
            hsSourceDirs = ["."],
            otherModules = ["Paths_foo"],
            defaultLanguage = "Haskell2010",
            otherLanguages = [],
            defaultExtensions = [],
            otherExtensions = [],
            oldExtensions = [],
            extraLibs = [],
            extraGHCiLibs = [],
            extraLibDirs = [],
            includeDirs = [],
            includes = [],
            installIncludes = [],
            options = [],
            profOptions = [],
            sharedOptions = [],
            customFieldsBI = [],
            targetBuildDepends = [],
            targetBuildRenaming = None
        )
    ),
    executables = [],
    testSuites = [],
    benchmarks = [],
    dataFiles = ["foo.txt"],
    dataDir = "data",
    extraSrcFiles = [],
    extraTmpFiles = [],
    extraDocFiles = []
)
