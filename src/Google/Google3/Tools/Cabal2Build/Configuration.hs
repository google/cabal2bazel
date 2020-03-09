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

-- TODO(judahjacobson): Fix the deprecated use of finalizePackageDescription.
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE LambdaCase #-}

-- | Tools for dealing with Cabal packages' configuration flags.
--   In fact, nothing in here has anything to do with Google
--   packages, so it could be moved to a more general
--   Cabal-related tree.
module Google.Google3.Tools.Cabal2Build.Configuration (
  FlagAssignmentList,
  targetPackageDesc,
  requestFlags,
  displayFlagName,
  makeFlagName) where

import Data.Char (toLower)
import Distribution.Compiler
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal (
  finalizePackageDescription)
import Distribution.System
import qualified Distribution.Text as Cabal (display)
import qualified Distribution.Version as Cabal (Version)
import System.IO (hFlush, stdout)

type FlagAssignmentList = [(Cabal.FlagName, Bool)]

mkFlagAssignment :: FlagAssignmentList -> Cabal.FlagAssignment
mkFlagAssignment = Cabal.mkFlagAssignment

unFlagAssignment :: Cabal.FlagAssignment -> FlagAssignmentList
unFlagAssignment = Cabal.unFlagAssignment

displayFlagName :: Cabal.FlagName -> String
displayFlagName = Cabal.display

makeFlagName :: String -> Cabal.FlagName
makeFlagName =
  -- Cabal lowercases the flag name before mkFlagName. Mimic that.
  -- See b/134632412 and https://github.com/haskell/cabal/issues/5043.
  Cabal.mkFlagName . map toLower

-- | Finalizes a package description for the target configuration.
targetPackageDesc :: Cabal.GenericPackageDescription
                  -> Cabal.Version  -- ^ GHC version to target.
                  -> FlagAssignmentList
                  -> (Cabal.PackageDescription, FlagAssignmentList)
targetPackageDesc pkg ghcVersion explicitFlags =
  let compilerInfo = unknownCompilerInfo (CompilerId GHC ghcVersion) NoAbiTag
   in case Cabal.finalizePackageDescription (mkFlagAssignment explicitFlags)
                                            (const True)
                                            (Platform X86_64 Linux)
                                            compilerInfo
                                            []
                                            pkg
      of
        Right (desc, flags) -> (desc, unFlagAssignment flags)
        Left deps ->
          error $ "targetPackageDesc: missing dependencies: " ++ show deps

-- | Prompts the user to supply values for configuration flags.
requestFlags :: FlagAssignmentList     -- ^ Default flags.
             -> IO FlagAssignmentList  -- ^ User specified flags.
requestFlags = mapM requestFlag where
  requestFlag (flag, defaultValue) = do
    putStr $ displayFlagName flag ++ " [" ++ defaultStr defaultValue ++ "] "
    hFlush stdout
    value <- map toLower <$> getLine >>= \ case
        "y" -> return True
        "n" -> return False
        ""  -> return defaultValue
        _   -> fail "Must specify y or n, or Enter for default"
    return (flag, value)
  defaultStr True = "Y"
  defaultStr False = "N"
