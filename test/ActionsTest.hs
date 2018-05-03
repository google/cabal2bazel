-- Copyright 2018 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--    https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}
module Main where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Version (makeVersion)
import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Language.Haskell.Extension
import System.FilePath
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, assertEqual)

import Google.Google3.Tools.Cabal2Build
import Google.Google3.Tools.Cabal2Build.Description (descriptionFileContents)

main = do
    defaultMain
        [ testCase "defaultPackages" testDefaultPackages
        , testCase "loadCabalFile" testLoadCabalFile
        , testCase "generatePackageDescriptionFile"
              testGeneratePackageDescriptionFile
        ]

testDefaultPackages = do
    defaults <- loadDefaultPackages
    assertBool "Default packages must contain 'base'" $
        Text.pack "base" `elem` defaults

testDataDir = "test/testdata"
baseDir = testDataDir </> "awesome-1.2"

testLoadCabalFile = do
    (packageDesc, flags) <-
        loadCabalFile UseDefaults defaultVersion (baseDir </> "awesome.cabal")
    let depVersionRange =
            intersectVersionRanges
                (unionVersionRanges
                    (thisVersion (makeVersion [1,3]))
                    (laterVersion (makeVersion [1,3])))
                (earlierVersion (makeVersion [1,4]))
    let expectedDesc = PackageDescription
            { package = PackageIdentifier
                { pkgName = PackageName "awesome"
                , pkgVersion = makeVersion [1,2]
                }
            , license = BSD3
            , licenseFiles = ["LICENSE"]
            , copyright = ""
            , maintainer = "user@example.org"
            , author = "John Doe"
            , stability = ""
            , testedWith =
                [ (GHC, thisVersion (makeVersion [7,6,3]))
                , (GHC, thisVersion (makeVersion [7,8,4]))
                , (GHC, thisVersion (makeVersion [7,10,1]))
                ]
            , homepage = "http://example.com/awesome"
            , pkgUrl = ""
            , bugReports = "http://bugs/"
            , sourceRepos = [ SourceRepo
                { repoKind = RepoHead
                , repoType = Just Git
                , repoLocation = Just "git://github.com/awesome/awesome.git"
                , repoModule = Nothing
                , repoBranch = Nothing
                , repoTag = Nothing
                , repoSubdir = Nothing
                } ]
            , synopsis = "An awesome library."
            , description = "Fake description.\n" ++
                "Another description line.\n\nNew paragraph."
            , category = "General"
            , customFieldsPD = []
            , buildDepends =
                [ Dependency (PackageName "array") depVersionRange
                , Dependency (PackageName "containers") depVersionRange
                , Dependency (PackageName "lens") depVersionRange
                , Dependency (PackageName "package2") depVersionRange
                ]
            , specVersionRaw = Right $
                unionVersionRanges
                    (thisVersion (makeVersion [1,10]))
                    (laterVersion (makeVersion [1,10]))
            , buildType = Just Simple
            , setupBuildInfo = Nothing
            , library = Just Library
                { exposedModules = []
                , reexportedModules = []
                , requiredSignatures = []
                , exposedSignatures = []
                , libExposed = True
                , libBuildInfo = BuildInfo
                    { buildable = True
                    , buildTools = []
                    , cppOptions = []
                    , ccOptions = []
                    , ldOptions = []
                    , pkgconfigDepends = []
                    , frameworks = []
                    , extraFrameworkDirs = []
                    , cSources = []
                    , hsSourceDirs = ["."]
                    , otherModules = []
                    , defaultLanguage = Just Haskell2010
                    , otherLanguages = []
                    , defaultExtensions = []
                    , otherExtensions = []
                    , oldExtensions = []
                    , extraLibs = []
                    , extraLibDirs = []
                    , includeDirs = []
                    , includes = []
                    , installIncludes = []
                    , Distribution.PackageDescription.options = []
                    , customFieldsBI = []
                    , targetBuildDepends =
                        [ Dependency (PackageName "array") depVersionRange
                        , Dependency (PackageName "containers") depVersionRange
                        , Dependency (PackageName "lens") depVersionRange
                        , Dependency (PackageName "package2") depVersionRange
                        ]
                    , jsSources = []
                    , extraGHCiLibs = []
                    , profOptions = []
                    , sharedOptions = []
                    , targetBuildRenaming = Map.empty
                    }
                }
            , executables = []
            , testSuites = []
            , benchmarks = []
            , dataFiles = []
            , dataDir = ""
            , extraSrcFiles = ["README.markdown"]
            , extraTmpFiles = []
            , extraDocFiles = []
            }
    assertEqual "pacakge description" expectedDesc packageDesc

    let expectedFlags = [(FlagName "flag2", True), (FlagName "flag1", False)]
    assertEqual "flags" expectedFlags flags

testGeneratePackageDescriptionFile = do
    (packageDesc, flags) <-
        loadCabalFile UseDefaults defaultVersion (baseDir </> "awesome.cabal")
    let actual = descriptionFileContents flags packageDesc
    expected <- readFile (testDataDir </> "expected_package_description.bzl")
    assertEqual "packageDescription" expected actual
