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

module Main (main) where

import Control.Exception (Exception, try)
import qualified Data.Map as M
import Google.Google3.Tools.Cabal2Build.Download
  ( PackageJSON (..),
    PkgVerificationError (..),
    verifyPackage',
  )
import Test.Framework (defaultMain)
import Test.Framework.Providers.API (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@?=), assertFailure)
import Test.HUnit.Lang (Assertion)

assertThrows :: (Exception e, Eq e) => e -> IO a -> Assertion
assertThrows expected action = do
  result <- try action
  case result of
    Left thrown -> thrown @?= expected
    Right _ ->
      assertFailure $ "No exception thrown, expected: " ++ show expected

fakePackagePath :: FilePath
fakePackagePath =
  "test/testdata/verify_package/fake_package.txt"

fakePackageHash :: String
fakePackageHash =
  "cf75eb023a0d96d7f5af5449f87a4667f126f0e3356b9545fe990dbee78d5697"

testVerifyPackage :: Assertion
testVerifyPackage = do
  let f = fakePackagePath
  let pkg = PackageJSON (M.fromList [("SHA256", fakePackageHash)]) 29
  verifyPackage' f pkg

testVerifyPackageWrongFileSize :: Assertion
testVerifyPackageWrongFileSize = do
  let f = fakePackagePath
  let pkg = PackageJSON (M.fromList [("SHA256", fakePackageHash)]) 30
  assertThrows (PkgVerificationError "Wrong file size: expected 30, got 29") $
    verifyPackage' f pkg

testVerifyPackageNoSHA256 :: Assertion
testVerifyPackageNoSHA256 = do
  let f = fakePackagePath
  let pkg = PackageJSON M.empty 29
  assertThrows
    ( PkgVerificationError
        "SHA256 is not found in package JSON: PackageJSON \
        \{packageHashes = fromList [], packageSize = 29}"
    )
    $ verifyPackage' f pkg

testVerifyPackageWrongSHA256 :: Assertion
testVerifyPackageWrongSHA256 = do
  let f = fakePackagePath
  let pkg = PackageJSON (M.fromList [("SHA256", "badsha256hash")]) 29
  assertThrows
    ( PkgVerificationError $
        "Wrong SHA256: expected badsha256hash, got " ++ fakePackageHash
    )
    $ verifyPackage' f pkg

main :: IO ()
main =
  defaultMain
    [ testGroup
        "verifyPackage'"
        [ testCase "ok" testVerifyPackage,
          testCase "wrong file size" testVerifyPackageWrongFileSize,
          testCase "no sha256" testVerifyPackageNoSHA256,
          testCase "wrong sha256" testVerifyPackageWrongSHA256
        ]
    ]
