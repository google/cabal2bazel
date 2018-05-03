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

{-# LANGUAGE OverloadedStrings, LambdaCase, NoMonomorphismRestriction #-}

-- | Tool to help with fetching Cabal packages from Hackage and installing
--   them as packages under //third_party/haskell.
--
--   See 'usage' for a more detailed explanation.
--
--   This module is largely about interacting with the Real World, including
--   the user.

module Main where

import Data.Monoid ((<>))
import Control.Monad (void)
import Options.Applicative hiding (Success)
import Data.Text (unpack)
import Google.Google3 (parsePackageName)

import Google.Google3.Tools.Cabal2Build

usage = unlines [
    "A tool to help with fetching Cabal packages from Hackage and installing",
    "them as packages under //third_party/haskell."
  ]

main = do
  let parseInfo = info (options <**> helper)
        ( fullDesc
          <> progDesc usage
          <> header "cabal2bazel" )
  opts <- execParser parseInfo
  defaultPackages <- loadDefaultPackages

  case operation opts of
    Install cabalPackageName ->
      fetchAndInstall opts defaultPackages cabalPackageName >>= \ case
          (Success _)      -> return ()
          AlreadyInstalled -> fail $ "Package '" ++
                                     unpack cabalPackageName ++
                                     "' already installed."
          InvalidCabalName -> fail $ "Unintelligible Cabal package name '"
                                     ++ unpack cabalPackageName ++ "'."
          UnpackFailed     -> fail $ "Couldn't unpack package '" ++
                                     unpack cabalPackageName ++ "'."
    WireUp google3packageName ->
      void $ wireUpPackage opts defaultPackages $
        parsePackageName google3packageName

    ReplaceOwner google3packageName ->
       void $ replaceOwner opts $
        parsePackageName google3packageName
