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

import qualified Data.Map as Map
import qualified Distribution.Package as Package
import Distribution.Version (mkVersion)
import System.FilePath
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import Google.Google3.Tools.Cabal2Build.Actions
import Google.Google3.Tools.Cabal2Build.Build
import Google.Google3.Tools.Cabal2Build.Stackage

mkPackageName :: String -> Package.PackageName
mkPackageName = Package.mkPackageName

main = do
    defaultMain [ testCase "getDependencies" testGetDependencies ]

baseDir = "test/testdata/awesome-1.2"

testGetDependencies = do
    (packageDesc, _) <-
        loadCabalFile [] testPlan (baseDir </> "awesome.cabal")
    let expectedDeps = map mkPackageName ["array", "containers", "lens", "package2"]
    assertEqual "dependencies" expectedDeps $
        getDependencies packageDesc

testPlan :: Snapshot
testPlan = Snapshot
    { corePackageVersions = Map.empty
    , ghcVersion = mkVersion [8]
    , snapshotPackages = Map.empty
    }

