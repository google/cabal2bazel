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

{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

-- | Tool to help with fetching Cabal packages from Hackage and installing
--   them as Google3 packages under //third_party/haskell.
--
--   This module is largely about interacting with the Real World, including
--   the user.

module Main (main) where

import Control.Monad (void)
import Options.Applicative (progDesc, execParser, info)

import Google.Google3.Name (parsePackageName)
import Google.Google3.Tools.Cabal2Build.Actions
import Google.Google3.Tools.Cabal2Build.Options
import Google.Google3.Tools.Cabal2Build.Stackage

import Google.Google3.VCSClient (g3Dir, vcsForGivenDirectory)

main = do
  let infoMod = progDesc "cabal2bazel"
  opts <- execParser $ info options infoMod
  -- TODO(judahjacobson): Locate the top-level directory.
  vcs <- vcsForGivenDirectory "."
  putStrLn ("Using google3 directory: " <> g3Dir vcs)
  case operation opts of
    Install cabalPackageName -> do
      name <- parseNameOrId cabalPackageName
      snapshot <- getSnapshot (resolverOpt opts)
      fetchAndInstall opts snapshot vcs name

    WireUp path -> do
      snapshot <- getSnapshot (resolverOpt opts)
      wireUpInstalledPackage opts snapshot vcs path

    ReplaceOwner google3packageName ->
       void $ replaceOwner opts vcs $
        parsePackageName google3packageName
