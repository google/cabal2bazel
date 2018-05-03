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

-- | Tools for dealing with Cabal packages' configuration flags.
--   In fact, nothing in here has anything to do with Google
--   packages, so it could be moved to a more general
--   Cabal-related tree.
module Google.Google3.Tools.Cabal2Build.Configuration (
  Cabal.FlagAssignment,
  targetPackageDesc,
  requestFlags) where

import Data.Char (toLower)
import Distribution.Compiler
import qualified Distribution.PackageDescription as Cabal (
  FlagAssignment,
  FlagName(..),
  GenericPackageDescription,
  PackageDescription)
import qualified Distribution.PackageDescription.Configuration as Cabal (
  finalizePackageDescription)
import Distribution.System
import qualified Distribution.Version as Cabal (Version)
import System.IO (hFlush, stdout)

-- | Finalizes a package description for the target configuration.
targetPackageDesc :: Cabal.GenericPackageDescription
                  -> Cabal.Version  -- ^ GHC version to target.
                  -> Cabal.FlagAssignment
                  -> (Cabal.PackageDescription, Cabal.FlagAssignment)
targetPackageDesc pkg ghcVersion explicitFlags =
  let compilerInfo = unknownCompilerInfo (CompilerId GHC ghcVersion) NoAbiTag
      Right (desc, flags) =
          Cabal.finalizePackageDescription explicitFlags
                                           (const True)
                                           (Platform X86_64 Linux)
                                           compilerInfo
                                           []
                                           pkg
  in (desc, flags)

-- | Prompts the user to supply values for configuration flags.
requestFlags :: Cabal.FlagAssignment     -- ^ Default flags.
             -> IO Cabal.FlagAssignment  -- ^ User specified flags.
requestFlags = mapM requestFlag where
  requestFlag (Cabal.FlagName flag, defaultValue) = do
    putStr $ flag ++ " [" ++ defaultStr defaultValue ++ "] "
    hFlush stdout
    value <- map toLower <$> getLine >>= \ case
        "y" -> return True
        "n" -> return False
        ""  -> return defaultValue
        _   -> fail "Must specify y or n, or Enter for default"
    return (Cabal.FlagName flag, value)
  defaultStr True = "Y"
  defaultStr False = "N"
