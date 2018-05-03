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
module Google.Utils.DirectoryUtils where

import Control.Exception (bracket)
import Control.Monad ((<=<), when)
#if __GLASGOW_HASKELL__ < 800
import Data.Bool (bool)
#endif
import Data.Maybe (fromMaybe)
import System.Directory
  ( createDirectoryIfMissing
#if __GLASGOW_HASKELL__ < 800
  , doesDirectoryExist
  , doesFileExist
#endif
  , getDirectoryContents
  , getTemporaryDirectory
  , removeFile
  )
import System.Environment (lookupEnv)
import System.IO (Handle, hClose, hIsOpen, openTempFile)

-- | @mkdir -p@
createPath :: FilePath -> IO ()
createPath = createDirectoryIfMissing True

#if __GLASGOW_HASKELL__ < 800
-- | Returns true if and only if the given path is a file or a directory.
doesPathExist :: FilePath -> IO Bool
doesPathExist p = doesFileExist p >>= bool (doesDirectoryExist p) (return True)
#endif

-- | Returns the contents of a directory, except for '.' and '..'.
getSimpleDirectoryContents :: FilePath -> IO [FilePath]
getSimpleDirectoryContents =
  return . filter (not . special) <=< getDirectoryContents
  where special e = e `elem` [".", ".."]

-- | Return a directory suitable for storing temporary files.
getTempDir :: IO FilePath
getTempDir = fromMaybe <$> getTemporaryDirectory <*> lookupEnv "TEST_DIR"

-- | Run a computation with a temporary file named from the given
-- template.  The file is cleaned up automatically.
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile template = bracket openTemp cleanupTemp . uncurry
  where
    openTemp = getTempDir >>= flip openTempFile template
    cleanupTemp (path, h) = do
      open <- hIsOpen h
      when open $ hClose h
      removeFile path
