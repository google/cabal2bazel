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

{-# LANGUAGE OverloadedStrings #-}

-- | Defines conversion between Cabal package names and
--   Google3 package names.
module Google.Google3.Tools.Cabal2Build.PackageName (
  google3packageNameForCabalPackageName,
  google3packageNameForCabalPackageId,
  cabalVersionForSegment) where

import qualified Data.Text as Text
import qualified Distribution.Package as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal
import qualified Google.Google3.Name as Google3
import Text.Read (readMaybe)

-- | Converts an unversioned Cabal package name to a valid Google3 package
--   name. Dashes become underscores.
--
--   e.g. "utf8-string" becomes "//third_party/haskell/utf8_string"
google3packageNameForCabalPackageName :: Cabal.PackageName
                                      -> Google3.PackageName
google3packageNameForCabalPackageName name =
  Google3.PackageName [
    "third_party",
    "haskell",
    Text.pack $ map mungeChar (Cabal.display name)
    ]
  where mungeChar '-' = '_'
        mungeChar x = x

-- | Converts a versioned Cabal package name to a valid Google3 package
--   name.
--   e.g. "utf8-string-0.3.6" becomes "//third_party/haskell/utf8_string/v0_3_6"
google3packageNameForCabalPackageId :: Cabal.PackageIdentifier
                                    -> Google3.PackageName
google3packageNameForCabalPackageId (Cabal.PackageIdentifier name version)
  = Google3.subpackageName unversionedPackageName [versionSegment]
    where unversionedPackageName = google3packageNameForCabalPackageName name
          versionSegment =
            Text.cons 'v' $
            Text.intercalate "_" $
            map (Text.pack . show) branch
          branch =  Cabal.versionNumbers version

-- | Converts a Google3 version directory name into a Cabal version.
-- For example, "v0_3_6" becomes 0.3.6.
cabalVersionForSegment :: Text.Text -> Maybe Cabal.Version
cabalVersionForSegment v = do
    ('v', v') <- Text.uncons v
    vs <- mapM (readMaybe . Text.unpack)
            $ Text.splitOn "_" v'
    return $ Cabal.mkVersion vs
