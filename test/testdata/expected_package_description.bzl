"""Package description auto-generated from awesome.cabal by cabal2build.

Configured with Cabal flags:
  flag1: False
  flag2: True
"""
description = struct(
    specVersionRaw = ">=1.10",
    package = struct(pkgName = "awesome", pkgVersion = "1.2"),
    licenseRaw = "BSD3",
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
    bugReports = "http://b/",
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
    buildTypeRaw = "Simple",
    setupBuildInfo = None,
    library = struct(
        libName = None,
        exposedModules = [],
        reexportedModules = [],
        signatures = [],
        libExposed = True,
        libBuildInfo = struct(
            buildable = True,
            buildTools = [],
            buildToolDepends = [],
            cppOptions = [],
            asmOptions = [],
            cmmOptions = [],
            ccOptions = [],
            cxxOptions = [],
            ldOptions = [],
            pkgconfigDepends = [],
            frameworks = [],
            extraFrameworkDirs = [],
            asmSources = [],
            cmmSources = [],
            cSources = [],
            cxxSources = [],
            jsSources = [],
            hsSourceDirs = ["."],
            otherModules = [],
            virtualModules = [],
            autogenModules = [],
            defaultLanguage = "Haskell2010",
            otherLanguages = [],
            defaultExtensions = [],
            otherExtensions = [],
            oldExtensions = [],
            extraLibs = [],
            extraGHCiLibs = [],
            extraBundledLibs = [],
            extraLibFlavours = [],
            extraLibDirs = [],
            includeDirs = [],
            includes = [],
            installIncludes = [],
            options = [],
            profOptions = [],
            sharedOptions = [],
            staticOptions = [],
            customFieldsBI = [],
            targetBuildDepends = [
                struct(name = "array", version = ">=1.3 && <1.4"),
                struct(name = "containers", version = ">=1.3 && <1.4"),
                struct(name = "lens", version = ">=1.3 && <1.4"),
                struct(name = "package2", version = ">=1.3 && <1.4")
            ],
            mixins = []
        )
    ),
    subLibraries = [],
    executables = [],
    foreignLibs = [],
    testSuites = [],
    benchmarks = [],
    dataFiles = [],
    dataDir = "",
    extraSrcFiles = ["README.markdown"],
    extraTmpFiles = [],
    extraDocFiles = []
)
