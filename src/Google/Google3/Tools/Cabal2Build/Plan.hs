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

-- | Computes the packages and dependencies that need to be installed.
module Google.Google3.Tools.Cabal2Build.Plan
    ( Plan
    , PlanPackage(..)
    , createRecursivePlan
    , createPlanPackage
    , planPackageFromCabalContents
    ) where

import Control.Monad (filterM, unless)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.Maybe (maybeToList)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription.Parsec as Parsec
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal
import Google.Utils.List (ordNubOn)
import System.Process (callProcess)

import Google.Google3.Tools.Cabal2Build.Stackage
    (Snapshot(..), SnapshotPackage(..), fetchCabalFileContents)
import Google.Google3.Tools.Cabal2Build.Configuration
    (FlagAssignmentList, targetPackageDesc)


type Plan = [PlanPackage]

data PlanPackage = PlanPackage
    { planPackageCabalFileContents :: ByteString
      -- ^ Raw contents of the .cabal file.
    , planPackageDesc :: Cabal.PackageDescription
      -- ^ The package description, which has resolved all of the conditionals
      -- (for example: flags) in the .cabal file.
    , planPackageFlags :: FlagAssignmentList
      -- ^ A record of the values chosen for the flags in this package.
      -- Includes "default" values that were not explicitly set by
      -- the user.
    }

-- | Creates a plan for a single package.  Downloads the .cabal file from
-- Hackage and configures it.
createPlanPackage
    :: Cabal.PackageIdentifier
    -> Snapshot
    -> FlagAssignmentList
    -> IO PlanPackage
createPlanPackage pid snapshot customFlags =
    fetchCabalFileContents pid
        >>= planPackageFromCabalContents snapshot customFlags

-- | Configures a .cabal file from its raw contents.
planPackageFromCabalContents
    :: Snapshot
    -> FlagAssignmentList
    -> ByteString -- ^ Contents of the .cabal file
    -> IO PlanPackage
planPackageFromCabalContents snapshot customFlags cabalContents = do
    genericDesc <-
        case Parsec.runParseResult $
                    Parsec.parseGenericPackageDescription cabalContents of
          (warnings, Left (version, errors))
              -> error $ "Error parsing .cabal file: "
                        ++ show (version, warnings, errors)
          (warnings, Right desc)
              -> do
                    unless (null warnings) $
                        putStrLn $ "Warnings parsing .cabal file: "
                                      ++ show warnings
                    return desc
    -- Configure the package with the flag settings from Stackage.
    -- Override those settings with the user-specified FlagAssignment.
    let (desc, flags) = targetPackageDesc genericDesc (ghcVersion snapshot)
                               $ Map.toList
                               $ flip Map.union (Map.fromList customFlags)
                               $ maybe Map.empty snapshotPackageFlags
                               $ Map.lookup (Cabal.packageName genericDesc)
                                     (snapshotPackages snapshot)
    return PlanPackage
        { planPackageCabalFileContents = cabalContents
        , planPackageDesc = desc
        , planPackageFlags = flags
        }

-- | Creates a plan containing the given package and its dependencies, by
-- downloading their .cabal files from Hackage and configuring them.
createRecursivePlan
    :: Snapshot
    -> FlagAssignmentList
    -> Cabal.PackageIdentifier
    -> (Cabal.Dependency -> IO Bool)  -- ^ dependency absence checker
    -> IO Plan
createRecursivePlan snapshot flags pid0 isDepMissing = do
    corePackages <- readCorePackages
    evalStateT (addPackageId pid0)
        (Set.fromList corePackages)
  where
    maybeAddPackageDep dep = do
        alreadyAdded <- get
        if Cabal.depPkgName dep `Set.member` alreadyAdded
            then return []
            else do
                missing <- lift $ isDepMissing dep
                if missing
                    then lift (resolveAndCheckPackageId snapshot dep)
                            >>= addPackageId
                    else return []
    addPackageId pid = do
        pkg <- lift $ createPlanPackage pid snapshot flags
        modify' $ Set.insert (Cabal.pkgName pid)
        neededDeps <- lift $ filterM isDepMissing
                        $ getDependencies $ planPackageDesc pkg
        (pkg :) . concat <$> mapM maybeAddPackageDep neededDeps

-- | Returns the list of dependencies of the given package.
getDependencies :: Cabal.PackageDescription -> [Cabal.Dependency]
getDependencies packageDesc = ordNubOn (Text.pack . Cabal.display) $
  concatMap Cabal.targetBuildDepends $
  map Cabal.libBuildInfo (maybeToList $ Cabal.library packageDesc) ++
  map Cabal.buildInfo (Cabal.executables packageDesc)

-- | Gets the packages that our build takes from GHC's core package DB,
-- and thus don't need a version installed in third_party/haskell.
readCorePackages :: IO [Cabal.PackageName]
readCorePackages = do
    callProcess "blaze"
        ["build", "tools/build_defs/haskell/cabal_package:core_packages"]
    map Cabal.mkPackageName
        . lines
        <$> readFile "blaze-bin/tools/build_defs/haskell/cabal_package/core_packages"

-- | Looks up the given package in the Stackage snapshot by name, and checks
-- whether it satisfies the dependency's version range.
resolveAndCheckPackageId
    :: MonadFail m => Snapshot -> Cabal.Dependency -> m Cabal.PackageId
resolveAndCheckPackageId snapshot dep@(Cabal.Dependency name range) = let
    pid = resolvePackageId snapshot name
    in if Cabal.pkgVersion pid `Cabal.withinRange` range
        then return pid
        else fail $ "Stackage version " ++ Cabal.display pid
                    ++ " does not satisfy the dependency " ++ Cabal.display dep

-- | Looks up the version of the given package in the Stackage snapshot.
resolvePackageId :: Snapshot -> Cabal.PackageName -> Cabal.PackageId
resolvePackageId snapshot n = Map.findWithDefault err n
                            $ Cabal.PackageIdentifier n
                                            . snapshotPackageVersion
                            <$> snapshotPackages snapshot
  where err = error $ "The Stackage snapshot doesn't contain the package "
                    ++ Cabal.display n
                    ++ "; consider fetching it with an explicit version, "
                    ++ "e.g.,  cabal2build --fetch foo-X.Y.Z"
