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
-- | Deals with google3 build and deployment issues.

module Google.Runtime.BuildHelper (
    findRunfilesDir,
    findRunfilesDirOrDie,
    findHostDependency,
    findHostDependencyOrDie,
    ) where

import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (guard, msum, mzero)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.List (elemIndices)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment.FindBin(getProgPath)
import System.FilePath
    ( (</>)
    , dropTrailingPathSeparator
    , joinPath
    , splitDirectories
    , takeDirectory
    , takeFileName
    )

import Google.Runtime.FindFiles (findSrcDir, findParentRunfilesDir)

-- | Get the runfiles directory for the current binary.  Checks common places
-- where host dependencies live in different environments.
findRunfilesDir :: IO (Maybe FilePath)
findRunfilesDir = runMaybeT $ do
    binary <- liftIO getProgPath
    let candidates :: [MaybeT IO FilePath]
        candidates = [
          -- Nominal case of prebuilt binary with runfiles directory.
          return $ binary ++ ".runfiles" </> "google3",
          -- Rarer case of prebuilt binary which itself lives in a runfiles
          -- directory.
          MaybeT $ return $ (</> "google3") <$> findParentRunfilesDir binary,
          -- Development fallback for ghci.
          (</> "bazel-bin/") <$> MaybeT findSrcDir,
          -- Walk up to google3/ starting from current binary location.
          -- Use splitDirectories to remove the trailing slash from each
          -- component; we don't care about eliding multiple slashes in a row.
          do parts <- splitDirectories . (</> binary)
                          <$> liftIO getCurrentDirectory
             let matches = elemIndices "google3" parts
             case matches of
                 [] -> mzero
                 _ ->  return $ joinPath (take (last matches + 1) parts)
          ]
    result <- dropTrailingPathSeparator <$> msum (map checkExists candidates)
    -- We drop the final "google3" in order to be consistent with other
    -- languages.
    -- But first, sanity check that it's actually there.
    guard (takeFileName result == "google3")
    return $ takeDirectory result

checkExists :: MaybeT IO FilePath -> MaybeT IO FilePath
checkExists action = do
    path <- action
    liftIO (doesDirectoryExist path) >>= guard
    return path

-- | A version of findRunfilesDirOrDie that throws an error if the directory
-- cannot be found.
findRunfilesDirOrDie :: IO FilePath
findRunfilesDirOrDie = findRunfilesDir >>= \case
    -- Use throwIO to raise an error more eagerly.
    Nothing -> throwIO (ErrorCall "Unable to locate runfiles dir")
    Just f -> return f

-- | Resolves the given host dependency stem (starting from google3) to a full
-- path.  Checks common places where host dependencies live in different
-- environments.
--
-- The result is equivalent to @runfiles </> "google3" </> f@, where @runfiles@
-- is the result of calling 'findRunfilesDir' and 'f' is the input to this
-- function.
findHostDependency :: FilePath -> IO (Maybe FilePath)
findHostDependency f = fmap (fmap (</> "google3" </> f)) findRunfilesDir

-- | A version of 'findHostDependency' that throws an error if the file is not
-- found.
findHostDependencyOrDie :: FilePath -> IO FilePath
findHostDependencyOrDie f = fmap (</> "google3" </> f) findRunfilesDirOrDie
