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

import qualified Distribution.License as Cabal
import Distribution.Version (mkVersion)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import Google.Google3.Tools.Cabal2Build.License

main = do
    defaultMain [ testCase "getDependencies" testLicenseText ]

caseApache = Cabal.Apache Nothing
caseGPL = Cabal.GPL Nothing
caseLGPL_1_0 = Cabal.LGPL (Just (mkVersion [1,0]))
caseLGPL_2_1 = Cabal.LGPL (Just (mkVersion [2,1]))
caseLGPL_3 = Cabal.LGPL (Just (mkVersion [3]))
caseLGPL = Cabal.LGPL Nothing
caseMPL_2_0 = Cabal.MPL (mkVersion [2])
caseUnknownFoo = Cabal.UnknownLicense "Foo"

testLicenseText = do
  -- Check various version forms.
  assertEqual "Explicit trailing .0" (showCabalLicense caseLGPL_1_0) "LGPL-1.0"
  assertEqual "Implicit trailing .0" (showCabalLicense caseLGPL_3) "LGPL-3.0"
  assertEqual "Multi-digit" (showCabalLicense caseLGPL_2_1) "LGPL-2.1"
  assertEqual "Unversioned" (showCabalLicense caseLGPL) "LGPL"

  -- Check spellings of generated license names.
  assertEqual "Apache" (showCabalLicense caseApache) "Apache"
  assertEqual "BSD2" (showCabalLicense Cabal.BSD2) "BSD-2-Clause"
  assertEqual "BSD3" (showCabalLicense Cabal.BSD3) "BSD-3-Clause"
  assertEqual "BSD4" (showCabalLicense Cabal.BSD4) "BSD-4-Clause"
  assertEqual "GPL" (showCabalLicense caseGPL) "GPL"
  assertEqual "ISC" (showCabalLicense Cabal.ISC) "ISC"
  assertEqual "MIT" (showCabalLicense Cabal.MIT) "MIT"
  assertEqual "MPL" (showCabalLicense caseMPL_2_0) "MPL-2.0"
  assertEqual "PubDomain" (showCabalLicense Cabal.PublicDomain) "PublicDomain"
  assertEqual "Unknown/foo" (showCabalLicense caseUnknownFoo) "Unknown/Foo"

