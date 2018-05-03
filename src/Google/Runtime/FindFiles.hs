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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
-- |File system traversal helpers.

module Google.Runtime.FindFiles
  ( findFiles
  , findInTestDirectory
  , findSrcDir
  , findSrcDirOrDie
  , findParentRunfilesDir
  , Path) where

import Control.Monad (filterM)
import Data.List (findIndices, isPrefixOf, isSuffixOf)
import System.Directory (doesDirectoryExist, getCurrentDirectory,
                         getDirectoryContents)
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>), joinPath, splitPath, normalise)

-- |A pair comprising a directory and a file name.
type Path = (FilePath, FilePath)

-- |Looks for the files satisfying the given predicate under the given
-- directory.
findFiles :: (Path -> IO Bool) -> FilePath -> IO [Path]
findFiles predicate path = do
  children <- filter (`notElem` [".", ".."]) <$> getDirectoryContents path
  localMatches <- filterM predicate ((path,) <$> children)
  childDirs <- filterM doesDirectoryExist ((path </>) <$> children)
  childMatches <- mapM (findFiles predicate) childDirs
  return $ localMatches ++ concat childMatches

-- |Finds the file with the given base name under TEST_SRCDIR. There must
--  be one and only one such file or else the function calls error.
findInTestDirectory :: FilePath -> IO FilePath
findInTestDirectory fileName = do
  testSrcDir <- getEnv "TEST_SRCDIR"
  foundFiles <- findFiles (return . (== fileName) . snd) testSrcDir
  case foundFiles of
    [(dir, file)] -> return $ dir </> file
    _ -> error $ "Expected one file named " ++ fileName ++
                 " in " ++ testSrcDir ++ ". Found " ++ show foundFiles

-- |Determines the location of the top of the source tree. If there is
-- TEST_SRCDIR environment variable - it gets used with "google3/"
-- appended. Otherwise the current directory is traversed up until
-- there is "google3".
findSrcDir :: IO (Maybe FilePath)
findSrcDir = lookupEnv "TEST_SRCDIR" >>= \ case
    Just dir -> return $ Just (dir </> "google3")
    Nothing -> do
      components <- splitPath <$> getCurrentDirectory
      let matches = findIndices (isPrefixOf "google3") components
      case matches of
        [] -> return Nothing
        _ -> return . Just $ joinPath (take (last matches + 1) components)

-- |Determines the location of the top of the source tree via
-- 'findSrcDir', otherwise it throws an error.
findSrcDirOrDie :: IO FilePath
findSrcDirOrDie = findSrcDir >>= \ case
    Just path -> return path
    Nothing -> do
      cwd <- getCurrentDirectory
      error $ "Needs to run in google3, not " ++ cwd

-- |Determines the eventual location of the parent last ".runfiles"
-- directory. This happens when the binary is part of a "data"
-- dependency of another target, in which case the binary itself (and
-- all its dependencies as well) lives already under a single, merged
-- runfiles directory (see
findParentRunfilesDir :: FilePath -> Maybe FilePath
findParentRunfilesDir binary =
  let components = splitPath $ normalise binary
      matches = findIndices (".runfiles/" `isSuffixOf`) components
  in  case matches of
        [] -> Nothing
        _ -> Just $ joinPath (take (last matches + 1) components)
