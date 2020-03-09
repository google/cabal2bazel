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

-- | Functions for a Stackage snapshot and for unpacking
-- Cabal packages.
{-# LANGUAGE OverloadedStrings #-}
-- Unfortunately, we need a couple of orphan instances of FromJSON to get
-- aeson/yaml to parse the snapshot.  Luckily, this module is near the
-- top of the import tree so it shouldn't conflict with anything else.
{-# OPTIONS_GHC -Wno-orphans #-}
module Google.Google3.Tools.Cabal2Build.Stackage
    ( getSnapshot
    , Snapshot(..)
    , SnapshotPackage(..)
    , unpackPackage
    , fetchCabalFileContents
    ) where

import Control.Exception (throwIO)
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson.Types (FromJSONKey(..), FromJSONKeyFunction(..), Parser)
import Distribution.Version (Version)
import Data.Yaml (FromJSON(..), decodeEither', withObject, withText, (.:))
import qualified Distribution.Text as Cabal
import Distribution.Package (PackageName, PackageId, pkgName)
import Distribution.PackageDescription
import System.FilePath ((</>), (<.>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.Process (callProcess, readProcess)

import Google.Google3.Tools.Cabal2Build.Configuration (makeFlagName)
import Google.Google3.Tools.Cabal2Build.Download (downloadUrl, verifyPackage)
import Google.Google3.Tools.Cabal2Build.Options (Resolver(..))

-- | A stackage snapshot which is a curated list of packages that are
-- compatible with each other.  This data structure corresponds to the YAML
-- file format that Stackage provides.
data Snapshot = Snapshot
    { corePackageVersions :: M.Map PackageName Version
    , snapshotPackages :: M.Map PackageName SnapshotPackage
    , ghcVersion :: Version
    }

-- | An individual package in the snapshot.
data SnapshotPackage = SnapshotPackage
    { snapshotPackageVersion :: Version
    , snapshotPackageFlags :: M.Map FlagName Bool
    }

instance FromJSON Snapshot where
    parseJSON = withObject "Snapshot" $ \o -> do
        sys <- o .: "system-info"
        coreVersions <- sys .: "core-packages"
        ghcVers <- sys .: "ghc-version"
        pkgs <- o .: "packages"
        return Snapshot { corePackageVersions = coreVersions
                         , snapshotPackages = pkgs
                         , ghcVersion = ghcVers
        }

instance FromJSON SnapshotPackage where
    parseJSON = withObject "SnapshotPackage" $ \o ->
        SnapshotPackage
            <$> (o .: "version")
            <*> ((o .: "constraints") >>= (.: "flags"))

-- | Download and parse the Stackage snapshot for the given resolver.
getSnapshot :: Resolver -> IO Snapshot
getSnapshot resolver = withSystemTempFile "snapshot" $ \f h -> do
    putStrLn $ "Downloading Stackage snapshot " ++ unResolver resolver
    hClose h
    downloadUrl (resolverUrl resolver) f
    contents <- B.readFile f
    case decodeEither' contents of
        Left err -> throwIO err
        Right x -> return x

-- | Reads the contents of a URL into a value in memory.
readUrl
    :: String -- ^ The URL to download from
    -> IO B.ByteString
readUrl url = do
    putStrLn $ "Downloading " ++ show url
    -- Disable the curl progress prompt, since it can be noisy.
    T.encodeUtf8 . T.pack
        <$> readProcess "curl" ["--silent", "--location", url] ""

resolverUrl :: Resolver -> String
resolverUrl (Resolver name) = prefix ++ "/" ++ name ++ ".yaml"
  where
    -- Locations copied from:
    -- https://github.com/commercialhaskell/stack/blob/ac2c759d5841f798080dbd3b24d7a0f55990d936/src/Stack/Config/Urls.hs
    prefix
      | "lts-" `L.isPrefixOf` name
          = "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
      | "nightly-" `L.isPrefixOf` name
          = "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"
      | otherwise = error $ "Unrecognized resolver " ++ show name

instance FromJSON PackageName where
    parseJSON = withText "PackageName" simpleParser

instance FromJSONKey PackageName where
    fromJSONKey = cabalKeyTextParser

instance FromJSON FlagName where
    parseJSON = fmap makeFlagName . parseJSON

instance FromJSONKey FlagName where
    fromJSONKey = FromJSONKeyText (makeFlagName . T.unpack)

instance FromJSON Version where
    parseJSON = withText "Version" simpleParser

cabalKeyTextParser :: Cabal.Text a => FromJSONKeyFunction a
cabalKeyTextParser = FromJSONKeyTextParser simpleParser

simpleParser :: Cabal.Text a => T.Text -> Parser a
simpleParser t = case Cabal.simpleParse (T.unpack t) of
                        Just v -> pure v
                        Nothing -> fail $ "Unable to parse: "
                                            ++ show t

-- | Fetch the given Cabal package, and unpack it to the given location.
unpackPackage :: PackageId -> FilePath -> IO ()
unpackPackage pid dest
    = withSystemTempDirectory ("cabal2build-" ++ ident ++ ".") $ \f -> do
      putStrLn $ "Downloading package " ++ ident
      let url = "https://hackage.haskell.org/package/"
                          ++ ident ++ "/" ++ ident ++ ".tar.gz"
      print url
      let tarDest = f </> ident <.> "tar.gz"
      downloadUrl url tarDest
      verifyPackage pid tarDest
      callProcess "tar" ["-xzf", tarDest, "-C", dest]
 where
   ident = Cabal.display pid

-- | Fetch the contents of the .cabal file from Hackage.
fetchCabalFileContents :: PackageId -> IO B.ByteString
fetchCabalFileContents pid = readUrl $
      "https://hackage.haskell.org/package/"
           ++ ident ++ "/" ++ name ++ ".cabal"
  where
    ident = Cabal.display pid
    name = Cabal.display $ pkgName pid
