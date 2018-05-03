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

module Google.Google3.Tools.Cabal2Build.Actions (
    loadCabalFile,
    loadDefaultPackages,
    wireUpPackage,
    fetchAndInstall,
    replaceOwner,
    generateOwnersFile,
    PromptOption(Prompt, UseDefaults),
    InstallResult(Success, AlreadyInstalled, InvalidCabalName, UnpackFailed))
    where

import Control.Monad (when)
import Data.List (sortBy, (\\), delete, nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal (
    readPackageDescription)
import qualified Distribution.Text as Cabal (display, simpleParse)
import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.Version as Cabal
import qualified Google.Google3 as Google3
import System.Process
import System.Directory
import System.FilePath
import System.Posix.User (getEffectiveUserName)
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)

import Google.Google3.Tools.Cabal2Build.Build
import Google.Google3.Tools.Cabal2Build.Configuration
import Google.Google3.Tools.Cabal2Build.Description
import Google.Google3.Tools.Cabal2Build.Options
import Google.Google3.Tools.Cabal2Build.PackageName
import Google.Google3.Tools.VersionAgnosticThirdParty
    (VCSClient, cmdAdd, vcsForWorkingDirectory, createOrUpdateWrapperPackages,
     withOpenFiles)
import Google.Runtime.BuildHelper (findHostDependencyOrDie)
import Google.Utils.DirectoryUtils
import qualified Google.Google3.Buildifier as Buildifier
import qualified Google.Utils.Expn as Expn
import Google.Utils.Pick (partialPick)

haskellTeam = "haskell-team"

-- | Result of attempting to install a Cabal package.
data InstallResult = Success Google3.PackageName
                   | AlreadyInstalled
                   | InvalidCabalName
                   | UnpackFailed

-- | Fetches a Cabal package, and unpacks it into a suitable places
--   in the third_party/haskell tree. The Cabal package directory's
--   contents are left pristine. In particular, no BUILD
--   files are produced; that is left for a later stage.
--
--   The result is the name of the Google3 package that was created
--   for the version that was fetched, such as:
--     //third_party/haskell/binary/v0_5_0_2
installPackageInGoogle3 :: Cabal.PackageIdentifier
                        -> IO InstallResult
installPackageInGoogle3 pkgId =
  withSystemTempDirectory "cabal2build." $ \ tmpPath -> do
    createPath tmpPath
    -- TODO: Use Cabal directly, to ensure an appropriate version
    -- is used.
    readProcess "cabal" ["unpack", "-d", tmpPath, Cabal.display pkgId] []
      >>= putStrLn

    -- Examining the unpacked directory name is an easy way to establish the
    -- versioned package name in the case that no version was explicitly
    -- specified.
    sortBy (flip compare) . mapMaybe Cabal.simpleParse <$>
      getSimpleDirectoryContents tmpPath >>= \ case
        []          -> return UnpackFailed
        realPkgId:_ ->
          do let basePkgName = google3packageNameForCabalPackageName $
                                   Cabal.pkgName realPkgId
                 basePath = Google3.google3relativePath basePkgName
                 versionedPkgName = google3packageNameForCabalPackageId
                                      realPkgId
                 versionedPath = Google3.google3relativePath versionedPkgName

             pathExists <- doesPathExist versionedPath
             if pathExists
               then return AlreadyInstalled
               else do putStrLn $ "Installing '" ++ Cabal.display realPkgId ++
                                  "' as '" ++ versionedPath ++ "'"
                       createPath basePath
                       readProcess "mv" [tmpPath </> Cabal.display realPkgId
                                        , versionedPath] [] >>= putStrLn
                       return $ Success versionedPkgName

-- | Finds the filename of an arbitrary .cabal file in a directory. Assuming
--   that each Cabal package only contains a single .cabal file, this will
--   find that file.
--
--   The path returned is prefixed by the directory's own path.
firstCabalFile :: FilePath     -- ^ A path to a directory.
               -> IO FilePath  -- ^ A path to a .cabal file found therein.
firstCabalFile parentDir =
  (parentDir </>) . (\ case []  -> error ("No cabal file found in" ++ parentDir)
                            x:_ -> x) . filter isDotCabalFile <$>
  getSimpleDirectoryContents parentDir
    where isDotCabalFile filename = takeExtension filename == ".cabal"

-- | Reads a .cabal file and prompts the user to configure any flags in the
--   package, producing a configured package.
loadCabalFile :: PromptOption
              -> Cabal.Version
              -> FilePath
              -> IO (Cabal.PackageDescription, FlagAssignment)
loadCabalFile prompt ghcVersion f = do
  genericPkg <- Cabal.readPackageDescription Verbosity.normal f
  -- Configure the package with no flags specified, to determine the
  -- default values of the flags.
  let (_, defaultFlags) = targetPackageDesc genericPkg ghcVersion []
  -- Allow the user to override the defaults and configure the package again,
  -- with the user's chosen flags.
  targetPackageDesc genericPkg ghcVersion <$>
      (case prompt of
        Prompt -> requestFlags
        UseDefaults -> return) defaultFlags

loadDefaultPackages :: IO [Text]
loadDefaultPackages = do
  dir <- getCurrentDirectory
  file <- findHostDependencyOrDie "third_party/haskell/ghc/default-packages.txt"
  map packageName . mapMaybe Cabal.simpleParse . words <$>
      readFile (dir </> file)
    where packageName (Cabal.PackageIdentifier (Cabal.PackageName n) _) =
              Text.pack n

-- | Prompts the user to supply second owner username.
requestUsername :: String     -- ^ Default suggested username.
                -> IO String  -- ^ User specified username.
requestUsername def = do
  putStr $ "Please provide second owner [" ++ def ++ "]:"
  hFlush stdout
  response <- getLine
  return $! if null response then def else response

generateBuildFile :: FilePath
                  -> BzlOption
                  -> Cabal.PackageDescription
                  -> FlagAssignment
                  -> [Text]
                  -> IO ()
generateBuildFile google3packageDir bzl cabalPackageDesc flags defaultPackages = do
  let setupFile = "Setup.hs"
  setupExists <- doesFileExist $ google3packageDir </> setupFile
  let buildFilename = google3packageDir </> "BUILD"
      -- Visibility is restricted to the versionless package by default.
      attrs = Google3.PackageAttributes {
        Google3.packageAttributesDefaultVisibility = [
           Google3.Label
               (Google3.PackageName versionlessPackage)
               (Google3.TargetName "__pkg__")
           ]
        }
      versionlessPackage = map Text.pack (splitPath $ takeDirectory google3packageDir)
      google3package = google3packageForCabalPackage cabalPackageDesc flags bzl attrs
                          [setupFile | setupExists] defaultPackages
  putStrLn $ "Generating " ++ buildFilename
  writeFile buildFilename
            (show . Google3.pPrint . Google3.packageSyntax $ google3package)
  Buildifier.call [buildFilename]


-- | Generates an OWNERS file for the package. Uses current caller as
--   first owner and picks random person from haskell-team and
--   asks to confirm.
generateOwnersFile :: IO String
                   -> FilePath
                   -> IO ()
generateOwnersFile chooseOwner google3packageDir = do
  let ownerFilepath = google3packageDir </> ".." </> "OWNERS"
  exists <- doesFileExist ownerFilepath
  if exists then
    putStrLn $ "Keeping existing " ++ ownerFilepath
  else do
      putStrLn $ "Generating " ++ ownerFilepath
      writeFile ownerFilepath =<< unlines <$> sequence [getEffectiveUserName,
                                                        chooseOwner]

emailLocal :: String -> String
emailLocal = takeWhile (/= '@')

randomOtherUser :: IO String
randomOtherUser = Expn.tryExpand haskellTeam >>= \case
    Right xs@(_:_) -> do user <- getEffectiveUserName
                         emailLocal <$> partialPick (delete user xs)
    _ -> return ""

replacementUser :: String -> [String] -> IO String
replacementUser replacedUser existingUsers =
    Expn.tryExpand haskellTeam >>= \case
        Right xs@(_:_) ->
            emailLocal <$> partialPick (xs \\ (replacedUser:existingUsers))
        _ -> return replacedUser

generatePackageDescriptionFile :: FilePath
                               -> FlagAssignment
                               -> VCSClient
                               -> Cabal.PackageDescription
                               -> IO ()
generatePackageDescriptionFile google3packageDir flags vcs cabalPackageDesc = do
  let packageDescriptionFilePath
          = google3packageDir </> packageDescriptionFileName
  putStrLn $ "Generating " ++ packageDescriptionFilePath
  writeFile packageDescriptionFilePath
      $ descriptionFileContents flags cabalPackageDesc
  cmdAdd vcs [packageDescriptionFilePath]

-- | Fetches the Cabal package and installs it in Google3.
fetchAndInstall
  :: Options
  -> [Text]           -- ^ Default packages included in every Haskell build.
  -> Text             -- ^ Cabal package name to install.
  -> IO InstallResult
fetchAndInstall opts defaultPackages cabalPackageName =
  case Cabal.simpleParse (Text.unpack cabalPackageName) of
    Nothing             -> return InvalidCabalName
    Just cabalPackageId -> do
      r <- installPackageInGoogle3 cabalPackageId
      case r of
          Success google3pkgName ->
              wireUpPackage opts defaultPackages google3pkgName >>=
              when (recursiveOpt opts) .
              mapM_ (fetchAndInstall opts defaultPackages)
          _ -> return ()
      return r

-- | Add Google files to an already fetched Cabal package.
wireUpPackage :: Options
              -> [Text]               -- ^ Default packages included in every
                                      --   Haskell build.
              -> Google3.PackageName  -- ^ Package to wire up.
              -> IO [Text]
wireUpPackage (Options { promptOpt = prompt
                       , ownerOpt = owner
                       , bzlOpt = bzl
                       , ghcVersionOpt = ghcVersion
                       }) defaultPackages google3pkgName = do
  let chooseOwner | Text.null owner = randomOtherUser >>= case prompt of
                                          Prompt -> requestUsername
                                          UseDefaults -> return
                  | otherwise = return (Text.unpack owner)
      google3pkgDir = Google3.google3relativePath google3pkgName
  (cabalPackageDesc, flagConfig) <-
      firstCabalFile google3pkgDir >>= loadCabalFile prompt ghcVersion
  vcs <- vcsForWorkingDirectory
  when (bzl == UseBzl) $
      generatePackageDescriptionFile google3pkgDir flagConfig vcs
          cabalPackageDesc
  generateBuildFile google3pkgDir bzl cabalPackageDesc flagConfig defaultPackages
  generateOwnersFile chooseOwner google3pkgDir

  createOrUpdateWrapperPackages vcs [google3pkgName]
  return $ getDependencies cabalPackageDesc \\ defaultPackages


-- | Replaces an owner in an existing package.
replaceOwner :: Options
             -> Google3.PackageName  -- ^ Package to replace an owner in.
             -> IO ()
replaceOwner Options {ownerOpt = ownerText} google3pkgName = do
  let owner = Text.unpack ownerText
      google3pkgDir = Google3.google3relativePath google3pkgName
      ownerFilepath = google3pkgDir </> "OWNERS"
  putStrLn $ "Generating " ++ ownerFilepath
  existingOwners <- nub . lines <$> readFile ownerFilepath
  if owner `elem` existingOwners then do
    newOwner <- replacementUser owner existingOwners
    putStrLn $ "Got a new owner: " ++ newOwner
    let newOwners = newOwner : delete owner existingOwners
    if length newOwners < 2 || owner `elem` newOwners then
      putStrLn $ "Replacement failed with invalid list: " ++ show newOwners
    else do
      vcs <- vcsForWorkingDirectory
      withOpenFiles vcs [ownerFilepath] $
        writeFile ownerFilepath $ unlines $ nub newOwners
  else
    putStrLn $ "Skipping since " ++ owner ++ " is not in " ++ show existingOwners

