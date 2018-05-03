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

-- | Utilities related to the names of Google3 packages and targets.

module Google.Google3.Name where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath

-- | A package name such as //foo/bar, represented as a list of
--   path segments.
newtype PackageName = PackageName [Text]
  deriving (Eq, Ord)
instance Show PackageName where
  show = ("//" ++) . google3relativePath

-- | An unqualified target name.
newtype TargetName = TargetName { unTargetName :: Text }
  deriving (Eq, Ord)
instance Show TargetName where
  show = Text.unpack . unTargetName

-- | The fully qualified label of a target: its name qualifed by its package.
data Label = Label PackageName TargetName
  deriving (Eq, Ord)
instance Show Label where
  show (Label pn tn) = show pn ++ ":" ++ show tn

-- > parsePackageName "//foo/bar" == PackageName ["foo", "bar"]
parsePackageName :: Text -> PackageName
parsePackageName = PackageName . dropWhile Text.null . Text.splitOn "/"

-- | Note that this expects a fully qualified label, so things like
--   ":target_name" or "target_name" won't work.
-- > parseLabel "//foo/bar:baz" == Label (PackageName ["foo", "bar"])
-- >                                     (TargetName "baz")
parseLabel :: Text -> Label
parseLabel l = Label (parsePackageName x) (TargetName $ Text.tail y)
  where (x, y) = Text.break (== ':') l

-- | Glorified list concatenation.
-- > subPackageName (PackageName "foo") ["bar", "baz"] ==
-- >     PackageName ["foo", "bar", "baz"]
subpackageName :: PackageName -> [Text] -> PackageName
subpackageName (PackageName segs) moreSegs = PackageName (segs ++ moreSegs)

-- | Package //foo/bar is found at foo/bar relative to a Google3 root.
google3relativePath :: PackageName -> FilePath
google3relativePath (PackageName p) = foldr1 (</>) . fmap Text.unpack $ p

-- | Package directories' names must only contain alphanumeric
-- characters, dashes, underscores and forward slashes.
isValidPackageDirectoryName :: Text -> Bool
isValidPackageDirectoryName = Text.all isValidChar where
  isValidChar a = isAlphaNum a || a `elem` ("-_/" :: String)
