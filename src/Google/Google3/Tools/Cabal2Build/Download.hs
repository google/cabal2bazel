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

{-# LANGUAGE DeriveGeneric #-}

-- | Module for downloading and verification of downloaded packages.
module Google.Google3.Tools.Cabal2Build.Download
  ( downloadUrl,
    verifyPackage,
    -- Following are just for testing.
    PackageJSON (..),
    PkgVerificationError (..),
    verifyPackage',
  )
where

import Control.Exception (Exception, displayException, throwIO)
import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256, hashlazy)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BS
import Data.Map ((!?), Map)
import Distribution.Package (PackageId, PackageIdentifier (..))
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Utils (fromUTF8LBS)
import Distribution.Types.PackageName (unPackageName)
import GHC.Generics (Generic)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Posix.Files (fileSize, getFileStatus)
import System.Process (callProcess)
import Text.Printf (printf)

-- | Downloads from the given URL and stores in the given file.
-- Throws an 'IOError' on download failures.
downloadUrl ::
  -- | The URL to download from
  String ->
  -- | The destination file
  FilePath ->
  IO ()
downloadUrl url path = do
  putStrLn $ "Downloading " ++ show url
  callProcess "curl" ["--silent", "--location", "--fail", "--output", path, url]

cabalHashesUrl :: String -> String -> String
cabalHashesUrl name version =
  printf
    "https://raw.githubusercontent.com/commercialhaskell/all-cabal-hashes\
    \/hackage/%s/%s/%s.json"
    name
    version
    name

data PackageJSON
  = PackageJSON
      { packageHashes :: Map String String,
        packageSize :: Int
      }
  deriving (Generic, Show)

instance J.FromJSON PackageJSON where
  parseJSON =
    J.genericParseJSON J.defaultOptions {J.fieldLabelModifier = J.camelTo2 '-'}

newtype PkgVerificationError = PkgVerificationError String
  deriving (Eq, Show)

instance Exception PkgVerificationError where
  displayException (PkgVerificationError msg) = "PkgVerificationError: " ++ msg

-- | Verifies the downloaded package tarball.
-- Verification is done by comparing the hash of the file with the one from
-- all-cabal-hashes.
verifyPackage :: PackageId -> FilePath -> IO ()
verifyPackage pkgId downloaded = do
  putStrLn $ "Verifying package " ++ prettyShow pkgId
  let name = unPackageName . pkgName $ pkgId
      version = prettyShow . pkgVersion $ pkgId
  withSystemTempFile "cabal2build" $ \f h -> do
    hClose h
    downloadUrl (cabalHashesUrl name version) f
    c <- BS.readFile f
    case J.decode c of
      Nothing ->
        throwIO
          $ PkgVerificationError
          $ "Cannot decode package json: " ++ fromUTF8LBS c
      Just pkg -> verifyPackage' downloaded pkg

verifyPackage' :: FilePath -> PackageJSON -> IO ()
verifyPackage' downloaded pkg = do
  let expectedSize = packageSize pkg
  size <- fromIntegral . fileSize <$> getFileStatus downloaded
  unless (size == expectedSize)
    $ throwIO
    $ PkgVerificationError
    $ printf "Wrong file size: expected %d, got %d" expectedSize size
  case packageHashes pkg !? "SHA256" of
    Nothing ->
      throwIO
        $ PkgVerificationError
        $ "SHA256 is not found in package JSON: " ++ show pkg
    Just expectedHash -> do
      hash <- sha256 downloaded
      unless (expectedHash == hash)
        $ throwIO
        $ PkgVerificationError
        $ printf "Wrong SHA256: expected %s, got %s" expectedHash hash

sha256 :: FilePath -> IO String
sha256 f = do
  bs <- BS.readFile f
  return $ show (hashlazy bs :: Digest SHA256)
