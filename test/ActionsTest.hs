-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main where

import Data.List (sortOn)
import qualified Data.Map as Map
import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Language.Haskell.Extension
import System.FilePath
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import Google.Google3.Tools.Cabal2Build.Actions
    ( loadCabalFile
    , generateOwnersFileContent
    )
import Google.Google3.Tools.Cabal2Build.Configuration (makeFlagName)
import Google.Google3.Tools.Cabal2Build.Description (descriptionFileContents)
import Google.Google3.Tools.Cabal2Build.Stackage (Snapshot(..))

main = do
    defaultMain
        [ testCase "loadCabalFile" testLoadCabalFile
        , testCase "generatePackageDescriptionFile"
              testGeneratePackageDescriptionFile
        ]

testDataDir = "test/testdata"
baseDir = testDataDir </> "awesome-1.2"

testLoadCabalFile = do
    let plan = Snapshot
                { corePackageVersions = Map.empty
                , ghcVersion = defaultGhcVersion
                , snapshotPackages = Map.empty
                }
    (packageDesc, flags) <-
        loadCabalFile [] plan (baseDir </> "awesome.cabal")
    let depVersionRange =
            intersectVersionRanges
                (orLaterVersion (mkVersion [1,3]))
                (earlierVersion (mkVersion [1,4]))
    let expectedDesc = PackageDescription
            { package = PackageIdentifier
                { pkgName = mkPackageName "awesome"
                , pkgVersion = mkVersion [1,2]
                }
            , licenseRaw = Right BSD3
            , licenseFiles = ["LICENSE"]
            , copyright = ""
            , maintainer = "user@example.org"
            , author = "John Doe"
            , stability = ""
            , testedWith =
                [ (GHC, thisVersion (mkVersion [7,6,3]))
                , (GHC, thisVersion (mkVersion [7,8,4]))
                , (GHC, thisVersion (mkVersion [7,10,1]))
                ]
            , homepage = "http://example.com/awesome"
            , pkgUrl = ""
            , bugReports = "http://b/"
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
            , specVersionRaw = Right $ orLaterVersion (mkVersion [1,10])
            , buildTypeRaw = Just Simple
            , subLibraries = []
            , foreignLibs = []
            , setupBuildInfo = Nothing
            , library = Just mempty
                { libExposed = True
                , libBuildInfo = mempty
                    { buildable = True
                    , hsSourceDirs = ["."]
                    , defaultLanguage = Just Haskell2010
                    , targetBuildDepends =
                        [ Dependency (mkPackageName "array") depVersionRange
                        , Dependency (mkPackageName "containers") depVersionRange
                        , Dependency (mkPackageName "lens") depVersionRange
                        , Dependency (mkPackageName "package2") depVersionRange
                        ]
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
    assertEqual "package description" expectedDesc packageDesc

    let expectedFlags = [(makeFlagName "flag1", False), (makeFlagName "flag2", True)]
    assertEqual "flags" expectedFlags (sortOn fst flags)

testGeneratePackageDescriptionFile = do
    let plan = Snapshot
                { corePackageVersions = Map.empty
                , ghcVersion = defaultGhcVersion
                , snapshotPackages = Map.empty
                }
    (packageDesc, flags) <-
        loadCabalFile [] plan (baseDir </> "awesome.cabal")
    let actual = descriptionFileContents flags packageDesc
    expected <- readFile (testDataDir </> "expected_package_description.bzl")
    assertEqual "packageDescription" expected actual

testGenerateOwnersFileContent = do
    let owner = "default-owner"
    content <- generateOwnersFileContent owner
    let firstLine = head . lines $ content
    -- The passed owner must be in the first line.
    assertEqual "owners" owner firstLine

-- Currently unused:
defaultGhcVersion :: Version
defaultGhcVersion = mkVersion [8]
