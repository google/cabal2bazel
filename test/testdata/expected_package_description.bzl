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

"""Package description auto-generated from awesome.cabal by cabal2build.

Configured with Cabal flags:
  flag2: True
  flag1: False
"""
description = struct(
    package = struct(pkgName = "awesome", pkgVersion = "1.2"),
    license = "BSD3",
    licenseFiles = ["LICENSE"],
    copyright = "",
    maintainer = "user@example.org",
    author = "John Doe",
    stability = "",
    testedWith = [
        ("ghc", "==7.6.3"), ("ghc", "==7.8.4"), ("ghc", "==7.10.1")
    ],
    homepage = "http://example.com/awesome",
    pkgUrl = "",
    bugReports = "http://bugs/",
    sourceRepos = [
        struct(
            repoKind = "RepoHead",
            repoType = "Git",
            repoLocation = "git://github.com/awesome/awesome.git",
            repoModule = None,
            repoBranch = None,
            repoTag = None,
            repoSubdir = None
        )
    ],
    synopsis = "An awesome library.",
    description = "Fake description.\nAnother description line.\n\nNew paragraph.",
    category = "General",
    customFieldsPD = [],
    buildDepends = [
        struct(name = "array", version = ">=1.3 && <1.4"),
        struct(name = "containers", version = ">=1.3 && <1.4"),
        struct(name = "lens", version = ">=1.3 && <1.4"),
        struct(name = "package2", version = ">=1.3 && <1.4")
    ],
    specVersionRaw = None,
    buildType = "Simple",
    setupBuildInfo = None,
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
            extraFrameworkDirs = [],
            cSources = [],
            jsSources = [],
            hsSourceDirs = ["."],
            otherModules = [],
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
            targetBuildDepends = [
                struct(name = "array", version = ">=1.3 && <1.4"),
                struct(name = "containers", version = ">=1.3 && <1.4"),
                struct(name = "lens", version = ">=1.3 && <1.4"),
                struct(name = "package2", version = ">=1.3 && <1.4")
            ],
            targetBuildRenaming = []
        )
    ),
    executables = [],
    testSuites = [],
    benchmarks = [],
    dataFiles = [],
    dataDir = "",
    extraSrcFiles = ["README.markdown"],
    extraTmpFiles = [],
    extraDocFiles = []
)
