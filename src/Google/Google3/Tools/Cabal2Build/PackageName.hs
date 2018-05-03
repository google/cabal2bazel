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

{-# LANGUAGE OverloadedStrings #-}

-- | Defines conversion between Cabal package names and
--   Google3 package names.
module Google.Google3.Tools.Cabal2Build.PackageName (
  google3packageNameForCabalPackageName,
  google3packageNameForCabalPackageId) where

import qualified Data.Text as Text
import qualified Distribution.Package as Cabal
import qualified Distribution.Version as Cabal
import qualified Google.Google3 as Google3

-- | Converts an unversioned Cabal package name to a valid Google3 package
--   name. Dashes become underscores.
--
--   e.g. "utf8-string" becomes "//third_party/haskell/utf8_string"
google3packageNameForCabalPackageName :: Cabal.PackageName
                                      -> Google3.PackageName
google3packageNameForCabalPackageName (Cabal.PackageName name) =
  Google3.PackageName [
    "third_party",
    "haskell",
    Text.pack $ map mungeChar name
    ]
  where mungeChar '-' = '_'
        mungeChar x = x

-- | Converts a versioned Cabal package name to a valid Google3 package
--   name.
--   e.g. "utf8-string-0.3.6" becomes "//third_party/haskell/utf8_string/v0_3_6"
google3packageNameForCabalPackageId :: Cabal.PackageIdentifier
                                    -> Google3.PackageName
google3packageNameForCabalPackageId (Cabal.PackageIdentifier name version)
  = Google3.subpackageName unversionedPackageName [versionSegment version]
    where unversionedPackageName = google3packageNameForCabalPackageName name
          versionSegment (Cabal.Version branch tags) =
            Text.cons 'v' $
            Text.intercalate "_" (map Text.pack $
                                  map show branch ++ tags)
