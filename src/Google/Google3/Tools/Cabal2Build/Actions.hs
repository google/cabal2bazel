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

{-# LANGUAGE OverloadedStrings, LambdaCase, NoMonomorphismRestriction #-}

module Google.Google3.Tools.Cabal2Build.Actions (
    loadCabalFile,
    wireUpInstalledPackage,
    fetchAndInstall,
    generateOwnersFileContent, -- just for testing
    replaceOwner,
    parseNameOrId,
    parseId,
    ) where

import Control.Monad (unless, when)
import qualified Data.ByteString as B
import Data.List (delete, isPrefixOf, nub, sortBy, (\\))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Text as Cabal (Text, display, simpleParse)
import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.Version as Cabal
import qualified Google.Google3.Name as Google3
import qualified Google.Google3.Package as Google3
import Google.Google3.VCSClient (VCSClient(..), withOpenFile)
import Safe (headMay)
import System.Process
import System.Directory
import System.FilePath
import System.Posix.User (getEffectiveUserName)
import System.IO.Temp (withSystemTempDirectory)

import Google.Google3.Tools.Cabal2Build.Build
import Google.Google3.Tools.Cabal2Build.Configuration
import Google.Google3.Tools.Cabal2Build.Description
import Google.Google3.Tools.Cabal2Build.Options
import Google.Google3.Tools.Cabal2Build.PackageName
import Google.Google3.Tools.Cabal2Build.Plan
import Google.Google3.Tools.Cabal2Build.Stackage
import Google.Google3.Tools.Cabal2Build.VersionAgnosticThirdParty
    ( createOrUpdateWrapperPackage )
import Google.Utils.DirectoryUtils
import qualified Google.Google3.Buildifier as Buildifier
import qualified Google.Utils.Expn as Expn
import Google.Utils.Pick (partialPick)

haskellMdbGroup = "mdb.haskell-team"

resolvePackageId :: Snapshot -> Cabal.PackageName -> Cabal.PackageId
resolvePackageId snapshot n = Map.findWithDefault err n
                            $ Cabal.PackageIdentifier n
                                            . snapshotPackageVersion
                            <$> snapshotPackages snapshot
  where err = error $ "The Stackage snapshot doesn't contain the package "
                    ++ Cabal.display n
                    ++ "; consider fetching it with an explicit version, "
                    ++ "e.g.,  cabal2build --fetch foo-X.Y.Z"

absPath :: VCSClient -> Google3.PackageName -> FilePath
absPath vcs = Google3.google3absolutePath (g3Dir vcs)

-- | Fetches a Cabal package, and unpacks it into a suitable places
--   in the third_party/haskell tree. The Cabal package directory's
--   contents are left pristine. In particular, no METADATA or BUILD
--   files are produced; that is left for a later stage.
--   Overwrites the contents of the .cabal file with the value from the
--   PlanPackage, which may be a more recent Hackage revision.
--
--   The result is the name of the Google3 package that was created
--   for the version that was fetched, such as:
--     //third_party/haskell/binary/v0_5_0_2
installPackageInGoogle3
    :: VCSClient -> PlanPackage -> IO Google3.PackageName
installPackageInGoogle3 vcs pkg =
  withSystemTempDirectory "cabal2build." $ \ tmpPath -> do
    createPath tmpPath
    let pkgId = Cabal.package $ planPackageDesc pkg
    unpackPackage pkgId tmpPath
    let versionedPkgName = google3packageNameForCabalPackageId pkgId
        versionedPath = absPath vcs versionedPkgName
    pathExists <- doesPathExist versionedPath
    if pathExists
      then error $ "Package " ++ show versionedPath
                      ++ " already exists."
      else do putStrLn $ "Installing '" ++ Cabal.display pkgId ++
                         "' as '" ++ versionedPath ++ "'"
              createPath $ takeDirectory versionedPath
              -- Call "mv" rather than renamePath.  A simple rename may fail,
              -- since the temporary file may be on a different device than the
              -- destination (e.g. CitC).
              callProcess "mv"
                  [tmpPath </> Cabal.display pkgId, versionedPath]
              -- Overwrite the .cabal file, in case we got a different
              -- revision:
              B.writeFile
                   (versionedPath </> Cabal.display (Cabal.packageName pkgId)
                                  <.> "cabal")
                   (planPackageCabalFileContents pkg)
              -- Write the METADATA file.  Do that here, and not when
              -- wiring up the package, to avoid churning last_upgrade_date
              -- if we re-wire it up (e.g., if we're importing it disabled).
              subPackage <- isSubPackage versionedPath
              unless subPackage
                    $ generateMetadata versionedPath
                        (planPackageDesc pkg)
              addPackagePath vcs versionedPath versionedPath
              return versionedPkgName

-- | Finds the filename of an arbitrary .cabal file in a directory. Assuming
--   that each Cabal package only contains a single .cabal file, this will
--   find that file.
--
--   The path returned is prefixed by the directory's own path.
firstCabalFile :: FilePath     -- ^ A path to a directory.
               -> IO FilePath  -- ^ A path to a .cabal file found therein.
firstCabalFile parentDir =
  (parentDir </>) . (\ case []  ->
                                error ("No cabal file found in " ++ parentDir)
                            x:_ -> x) . filter isDotCabalFile <$>
  getSimpleDirectoryContents parentDir
    where isDotCabalFile filename = takeExtension filename == ".cabal"

-- | Reads a .cabal file and prompts the user to configure any flags in the
--   package, producing a configured package.
loadCabalFile :: FlagAssignmentList
              -> Snapshot
              -> FilePath
              -> IO (Cabal.PackageDescription, FlagAssignmentList)
loadCabalFile flags snapshot f = do
  genericPkg <- Cabal.readGenericPackageDescription Verbosity.normal f
  -- Configure the package with the flag settings from Stackage.
  -- Override those settings with the user-specified FlagAssignment.
  return $ targetPackageDesc genericPkg (ghcVersion snapshot)
         $ Map.toList
         $ flip Map.union (Map.fromList flags)
         $ maybe Map.empty snapshotPackageFlags
         $ Map.lookup (Cabal.packageName genericPkg)
               (snapshotPackages snapshot)

-- | If the Cabal license file is not named LICENSE, rename it.
-- If we can't detect the license file, print a warning.
createLicenseFileIfMissing ::
    VCSClient -> FilePath -> Cabal.PackageDescription -> IO ()
createLicenseFileIfMissing vcs google3packageDir cabalPackageDesc =
    case Cabal.licenseFiles cabalPackageDesc of
        ["LICENSE"] -> return ()
        [l] -> do
                let license = "LICENSE"
                    oldLicensePath = google3packageDir </> l
                    newLicensePath = google3packageDir </> license
                oldExists <- doesFileExist oldLicensePath
                newExists <- doesFileExist newLicensePath
                -- Check if it's already been moved by a previous run:
                if newExists && not oldExists
                  then putStrLn $
                          "Assuming " ++ oldLicensePath ++ " has already "
                          ++ "been moved to " ++ newLicensePath
                  else do
                    putStrLn $ "Moving " ++ oldLicensePath ++ " to "
                                  ++ newLicensePath
                    cmdMove vcs [oldLicensePath, newLicensePath]
        ls -> putStrLn $ "Warning: unable to detect license file: found "
                          ++ show ls

generateBuildFile
    :: Google3.PackageName  -- ^ of the package having access (typically parent)
    -> Cabal.PackageDescription
    -> FlagAssignmentList
    -> EnabledOption
    -> IO (String, [Google3.License])
generateBuildFile accessorPackage cabalPackageDesc flags enabled = do
  user <- getEffectiveUserName
  let -- Visibility is restricted to the versionless package by default.
      attrs = Google3.PackageAttributes {
        Google3.packageAttributesDefaultVisibility = [
           Google3.Label accessorPackage (Google3.TargetName "__pkg__")
           ]
        }
      google3package = google3packageForCabalPackage cabalPackageDesc flags
                          attrs enabled user
  let contents = show . Google3.pPrint . Google3.packageSyntax $ google3package
  return (contents, Google3.packageLicenses google3package)

generateMetadata :: FilePath
                 -> Cabal.PackageDescription
                 -> IO ()
generateMetadata google3packageDir cabalPackageDesc = do
  let metadataFilepath = google3packageDir </> "METADATA"
  putStrLn $ "Generating " ++ metadataFilepath
  Text.writeFile metadataFilepath ""

-- | Generates a METADATA file of type:VERSIONS, which is for a directory that
-- includes (potentially multiple) versioned packages.
generateVersionsMetadata :: VCSClient
                         -> FilePath
                         -> Cabal.PackageDescription
                         -> IO ()
generateVersionsMetadata vcs google3VersionsDir _cabalPackageDesc = do
  let metadataFilepath = google3VersionsDir </> "METADATA"
  exists <- doesFileExist metadataFilepath
  if exists then
    putStrLn $ "Keeping existing " ++ metadataFilepath
  else do
    putStrLn $ "Generating " ++ metadataFilepath
    Text.writeFile metadataFilepath ""
    cmdAdd vcs [metadataFilepath]

-- | Generates an OWNERS file for the package. Adds current caller and
-- the given owner to the file.
generateOwnersFile :: String
                   -> VCSClient
                   -> FilePath
                   -> IO ()
generateOwnersFile owner vcs google3packageDir = do
  let ownerFilepath = google3packageDir </> ".." </> "OWNERS"
  exists <- doesFileExist ownerFilepath
  if exists then
    putStrLn $ "Keeping existing " ++ ownerFilepath
  else do
      putStrLn $ "Generating " ++ ownerFilepath
      content <- generateOwnersFileContent owner
      writeFile ownerFilepath content
      cmdAdd vcs [ownerFilepath]

generateOwnersFileContent :: String -> IO String
generateOwnersFileContent owner = do
    user <- getEffectiveUserName
    return $ unlines $ nub [owner, user]

emailLocal :: String -> String
emailLocal = takeWhile (/= '@')

replacementUser :: String -> [String] -> IO String
replacementUser replacedUser existingUsers = return replacedUser

-- | Write a BUILD-related, version-specific file.  Add it if it
-- didn't exist before.  Otherwise, try integrating it against the previous
-- version (if any).
addOrUpdatePackageFile
    :: VCSClient
    -> FilePath  -- ^ Versioned path
    -> String    -- ^ File name
    -> String    -- ^ File contents
    -> IO ()
addOrUpdatePackageFile vcs versionedDir name contents = do
    let path = versionedDir </> name
    putStrLn $ "Generating " ++ path
    exists <- doesFileExist path
    writeFile path contents
    Buildifier.call [path]
    unless exists $ addPackagePath vcs versionedDir path

-- List all old packages, in reverse order.
listOtherVersions :: FilePath -> IO [FilePath]
listOtherVersions google3packageDir =
   filter (/= google3packageDir) . sortBy (flip compare) . lines
      <$> readProcess "find"
            [ takeDirectory google3packageDir
            , "-mindepth", "1"
            , "-maxdepth", "1"
            , "-name", "v*"
            , "-type", "d"
            ] ""

-- | Fetches the Cabal package and any dependencies (if recursive), and installs
-- them in Google3.
fetchAndInstall
  :: Options
  -> Snapshot
  -> VCSClient
  -> Either Cabal.PackageId Cabal.PackageName  -- ^ Cabal package to install.
  -> IO ()
fetchAndInstall opts snapshot vcs cabalPackage = do
    plan <- createPlan
    putStrLn "Planning to install: "
    mapM_ (putStrLn . Cabal.display . Cabal.package . planPackageDesc) plan
    unless (dryRunOpt opts == DryRun) $ mapM_ processPackage plan
  where
    processPackage pkg = do
        google3Path <- installPackageInGoogle3 vcs pkg
        wireUpPackage opts vcs pkg google3Path
    pkgId = either id (resolvePackageId snapshot) cabalPackage
    createPlan
        | recursiveOpt opts == Recursive
            = createRecursivePlan snapshot (cabalFlags opts) pkgId
                  (isMissingSufficientVersion vcs)
        | otherwise = (: []) <$> createPlanPackage pkgId snapshot
                                    (cabalFlags opts)

-- | Whether the given dependency is satisfied by a package already
-- installed in google3.
isMissingSufficientVersion :: VCSClient -> Cabal.Dependency -> IO Bool
isMissingSufficientVersion vcs (Cabal.Dependency name range) = do
    let basePackage = Google3.google3absolutePath (g3Dir vcs)
                          (google3packageNameForCabalPackageName name)
    exists <- doesDirectoryExist basePackage
    if not exists
        then return True
        else not . any (`Cabal.withinRange` range)
                 . mapMaybe (cabalVersionForSegment . Text.pack)
                 . filter (not . isPrefixOf ".")
                 <$> getDirectoryContents basePackage


-- | Parses a string which is either a Cabal package ID or a package name.
-- Throws an error if unable to parse it.
parseNameOrId :: Text -> IO (Either Cabal.PackageId Cabal.PackageName)
parseNameOrId s =
    -- Parse PackageName first, since Cabal's parser for PackageId accepts
    -- package names with no versions.
    case parseText s of
       Just n -> return $ Right n
       Nothing ->
           case parseText s of
               Just i -> return $ Left i
               Nothing -> fail $ "Unintelligible Cabal package " ++ show s

-- | Parses a string which is a Cabal package ID.
-- Throws an error if unable to parse it.
parseId :: Text -> IO Cabal.PackageId
parseId s = case parseText s of
              Just pid
                  -- Cabal will happily parse "foo" as a package name
                  -- with no version.  Reject that case explicitly.
                  | null $ Cabal.versionNumbers $ Cabal.pkgVersion pid
                      -> fail $ "Cabal package ID has no version: " ++ show s
                  | otherwise -> return pid
              Nothing -> fail $ "Unintelligible Cabal package " ++ show s

parseText :: Cabal.Text a => Text -> Maybe a
parseText = Cabal.simpleParse . Text.unpack

-- | Add Google files to an already fetched Cabal package.
wireUpInstalledPackage
    :: Options
    -> Snapshot
    -> VCSClient
    -> FilePath  -- ^ user-provided path to directory to wire-up
    -> IO ()
wireUpInstalledPackage opts snapshot vcs path = do
    pkg <- firstCabalFile path
           >>= B.readFile
           >>= planPackageFromCabalContents snapshot (cabalFlags opts)
    -- Turns the path into a rooted label despite potentialy relative path.
    pkgName <-
        Google3.parsePackageName . Text.pack . makeRelative (g3Dir vcs)
        <$> canonicalizePath path
    wireUpPackage opts vcs pkg pkgName

-- | Add Google files to an already fetched Cabal package, given that we have
-- parsed and flattened its .cabal file contents.
wireUpPackage :: Options
              -> VCSClient
              -> PlanPackage
              -> Google3.PackageName  -- ^ Package to wire up.
              -> IO ()
wireUpPackage Options { ownerOpt = ownerText
                      , keepOpt = keep
                      , enabledOpt = enabled
                      } vcs pkg google3pkgName = do
  let owner = Text.unpack ownerText
      google3pkgDir = absPath vcs google3pkgName
      cabalPackageDesc = planPackageDesc pkg
      flagConfig = planPackageFlags pkg
      -- Goes one directory up because that's the structure of
      -- third_party/haskell.
      dirName (Google3.PackageName ps) = Google3.PackageName (init ps)
      parent = dirName google3pkgName
      parentDir = absPath vcs parent
  createLicenseFileIfMissing vcs google3pkgDir cabalPackageDesc
  (buildContents, licenses)
      <- generateBuildFile parent cabalPackageDesc flagConfig enabled
  addOrUpdatePackageFile vcs google3pkgDir "BUILD" buildContents
  addOrUpdatePackageFile vcs google3pkgDir packageDescriptionFileName
      $ descriptionFileContents flagConfig cabalPackageDesc
  generateVersionsMetadata vcs parentDir cabalPackageDesc
  subPackage <- isSubPackage google3pkgDir
  unless subPackage $ do
    generateOwnersFile owner vcs google3pkgDir
    when (enabled == Enabled) $ do
      when (keep == Cleanup) $
        listOtherVersions google3pkgDir
          >>= mapM_ (deleteFilesRecursively vcs)
      createOrUpdateWrapperPackage vcs google3pkgName licenses

-- Test whether this is a subdirectory of another package, say, a git repository
-- that we imported with copybara that contains several Haskell packages.
-- For now, the logic is: check whether there's already a METADATA file at the
-- level above us, which is not of type VERSIONS.
isSubPackage :: FilePath -> IO Bool
isSubPackage f = do
    let metadataPath = takeDirectory f </> "METADATA"
    exists <- doesFileExist metadataPath
    if not exists
        then return False
        else not . isVersionsMetadata <$> Text.readFile metadataPath

-- TODO(judahjacobson): the correct implementation requires an internal protobuf.
isVersionsMetadata :: Text.Text -> Bool
isVersionsMetadata _contents = False

-- | Replaces an owner in an existing package.
replaceOwner :: Options
             -> VCSClient
             -> Google3.PackageName  -- ^ Package to replace an owner in.
             -> IO ()
replaceOwner Options {ownerOpt = ownerText} vcs google3pkgName = do
  let owner = Text.unpack ownerText
      google3pkgDir = absPath vcs google3pkgName
      ownerFilepath = google3pkgDir </> "OWNERS"
  putStrLn $ "Generating " ++ ownerFilepath
  existingOwners <- nub . lines <$> readFile ownerFilepath
  if owner `elem` existingOwners then do
    newOwner <- replacementUser owner existingOwners
    putStrLn $ "Got a new owner: " ++ newOwner
    let newOwners = newOwner : delete owner existingOwners
    if length newOwners < 2 || owner `elem` newOwners then
      putStrLn $ "Replacement failed with invalid list: " ++ show newOwners
    else
      withOpenFile vcs ownerFilepath $
        writeFile ownerFilepath $ unlines $ nub newOwners
  else
    putStrLn $ "Skipping since " ++ owner ++ " is not in " ++ show existingOwners

-- | Recursively adds all the given files, or under the given directories.
-- Integrates them with the previous version if possible.
--
-- More specifically: look for the latest .../vA_B_C/{foo}, and use
-- hg copy (or the equivalent) to integrate their history with the
-- newly-generated file.
addPackagePath :: VCSClient -> FilePath -> FilePath -> IO ()
addPackagePath vcs google3pkgDir path = do
  prevVersion <- headMay <$> listOtherVersions google3pkgDir
  case prevVersion of
    Nothing -> return ()
    Just v -> putStrLn $ "Integrating " ++ path ++ " against " ++ v
  files <- readProcess "find" [path, "-type", "f"] ""
              >>= mapM (findPrevFileVersion prevVersion) . lines
  -- Batch up all adds for efficiency.
  cmdAdd vcs [f | (f, Nothing) <- files]
  sequence_
      [cmdIntegrate vcs [g, f] | (f, Just g) <- files]
 where
  findPrevFileVersion :: Maybe FilePath -> FilePath -> IO (FilePath, Maybe FilePath)
  findPrevFileVersion Nothing f = return (f, Nothing)
  findPrevFileVersion (Just prev) f = do
    let base = makeRelative google3pkgDir f
        oldFile = prev </> base
    exists <- doesFileExist oldFile
    return (f, if exists then Just oldFile else Nothing)

-- | Recursively deletes all files under the given directory.
deleteFilesRecursively :: VCSClient -> FilePath -> IO ()
deleteFilesRecursively vcs path = do
    -- List the files explicitly, rather than specializing the "recursive add"
    -- syntax to each client type.
    readProcess "find" [path, "-type", "f"] ""
        >>= cmdDelete vcs . lines
    -- Afterwards, clean up by deleting the directories too.  Note:
    -- Mercurial (hg) with the "purge" extension will delete the
    -- directories automatically, so check first whether they still
    -- exist.
    stillExists <- doesDirectoryExist path
    when stillExists $ removeDirectoryRecursive path
