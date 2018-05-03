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

{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections, ScopedTypeVariables #-}
-- | Code to make sure every version-specific third-party Haskell package in
--   Google3 has a properly configured version-agnostic package.
module Google.Google3.Tools.VersionAgnosticThirdParty where

import Control.Exception (catch, throwIO)
import Control.Monad (filterM, forM, forM_, unless)
import Data.Function (on)
import Data.List (groupBy, isPrefixOf, partition, sortBy)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Tuple.Extra (fst3)
import System.Directory (doesFileExist)
import System.Environment (getProgName, getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), addExtension, takeBaseName)
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Error (mkIOError, userErrorType)
import System.Process (rawSystem, readProcess, readProcessWithExitCode)

import qualified Google.Google3.Name as Google3 (
  Label(..),
  PackageName(..),
  TargetName(..),
  google3relativePath,
  parseLabel,
  parsePackageName)
import qualified Google.Google3.Package as Google3 (
  Dependency(..),
  Package(..),
  PackageAttributes(..),
  Rule(..),
  RuleClass,
  Source(..),
  dependencyExpr,
  packageSyntax, pPrint)
import qualified Google.Google3.Buildifier as Buildifier
import Google.Utils.ReadPUtils
import Data.Version (Version)
import qualified Data.Version (parseVersion)
import Google.Google3.Tools.Cabal2Build.Build(ordNub)

usage name = unlines [
  "Helps maintain third_party/haskell/*/BUILD.",
  "",
  "For each versioned third-party package specified, creates or updates",
  "the BUILD file for a corresponding versionless package. For each",
  "relevant target declared by the versioned package, a wrapper target",
  "is declared in the versionless package.",
  "",
  "p4 add/edit and buildifier are also run on the resulting BUILD files",
  "",
  "Usage:",
  "  " ++ name ++ " [versioned_package ...]",
  "  " ++ name ++ " --all",
  "",
  "Example:",
  "  " ++ name ++ " //third_party/haskell/hslogger/v1_0_7",
  "",
  "The above example will create or edit //third_party/haskell/hslogger/BUILD",
  "so that it refers to targets in //third_party/haskell/hslogger/v1_0_7."
  ]

-- | Generic command action.
type Command = [FilePath] -> IO ()

-- | VCS Client type, holding the commands to run.
data VCSClient = VCSClient
  { cmdAdd    :: Command -- ^ Add files to VCS command.
  , cmdEdit   :: Command -- ^ Opens files for edit (before editing them).
  , cmdRecord :: Command -- ^ Records that a file has been edited (after).
  , cmdDelete :: Command -- ^ Marks files for removal.
  }

-- Part one: Find all the version-specific Google3 third-party haskell packages.

groupSortBy :: Ord a1 => (a -> a1) -> [a] -> [[a]]
groupSortBy f = groupBy ((==) `on` f) . sortBy (comparing f)

-- | Don't consider the package for the compiler itself.
interesting (Google3.PackageName ("third_party":"haskell":"ghc":_)) = False
interesting _ = True

versionedPackages :: IO [Google3.PackageName]
versionedPackages = filter interesting .
                    fmap (Google3.parsePackageName . Text.pack . ("//" ++)) .
                    lines <$>
                    readProcess "find" [ "third_party/haskell"
                                       , "-mindepth", "2"
                                       , "-maxdepth", "2"
                                       , "-type", "d"
                                       , "-name", "v*"] ""

parseVersion :: Text -> Maybe Version
parseVersion = parseUniquely (parseFully Data.Version.parseVersion) .
               Text.unpack

splitVersion p@(Google3.PackageName ps) = (init ps,
                                           (p, extractVersion (last ps)))
 where
  extractVersion = parseVersion . replaceUnderscores . stripLeadingV
  stripLeadingV = Text.tail
  replaceUnderscores = Text.map (\c -> if c == '_' then '.' else c)

-- | Given a list of versioned package names, returns only the latest version
--   of each Cabal package represented in the list.
latestVersions :: [Google3.PackageName] -> [Google3.PackageName]
latestVersions = map head . groupVersions

-- | Given a list of versioned package names, returns all but the latest
--   version of each Cabal package represented in the list.
olderVersions :: [Google3.PackageName] -> [Google3.PackageName]
olderVersions = concatMap tail . groupVersions

-- | Helper for @latestVersions@ and @olderVersions@.
--   Given a list of versioned package names, groups different versions of the
--   same Cabal package together, and sorts each group by decreasing version.
groupVersions :: [Google3.PackageName] -> [[Google3.PackageName]]
groupVersions vPkgs = [map fst . sortBy (flip $ comparing snd) $
                       snd <$> pkgGrp |
                       pkgGrp <- groupSortBy fst
                                 [splitVersion vPkg | vPkg <- vPkgs]]

-- Part two: create a wrapper package for each versioned package.

bazelQuery :: Google3.PackageName -> IO Text
bazelQuery package = Text.pack <$> readProcess "bazel" [
             "query", "--noshow_loading_progress", "--noannounce_rc",
             show (Google3.Label package $ Google3.TargetName "all"),
             "--output=label_kind"] ""

isInteractiveSession :: IO Bool
isInteractiveSession = (`elem` ["ghc", "ghci"]) . takeBaseName <$>
                       getExecutablePath

getRunfilesDir :: IO FilePath
getRunfilesDir = flip addExtension "runfiles" <$> getExecutablePath

getExecRoot :: IO FilePath
getExecRoot = do
  interactive <- isInteractiveSession
  if interactive
  then return "bazel-bin"
  else (</> "google3") <$> getRunfilesDir

directDependents :: Google3.Label -> IO [Google3.Label]
directDependents label = do
  execRoot <- getExecRoot
  output <- Text.pack <$>
            readProcess
            (execRoot </> "devtools/deps/depserver/query/depends_on")
            ["--direct", show label] ""
  return [Google3.parseLabel . head . Text.words $ line |
          line <- Text.lines output,
          not $ Text.null line]

ruleKinds :: Google3.PackageName -> IO [(Google3.Label, Google3.RuleClass)]
ruleKinds = fmap parse . bazelQuery
  where parse = mapMaybe (collect . Text.words) . Text.lines
        collect [kind, "rule", label] = Just (Google3.parseLabel label, kind)
        collect _ = Nothing

-- Bazel won't even load a package under third_party if it doesn't have a
-- licenses() statement in it. So the appropriate thing to do is to put a
-- big fat warning on these packages.
--
-- In the longer term, it'll maybe make sense to move to the one-
-- version-per-package model that the rest of the third_party world seems
-- to have adopted, in which case this problem will go away.
preamble = [
  "Note: Description, license file and so on are given by version-",
  "specific packages, not here.",
  "",
  "WARNING! The code you want probably has a more restrictive license.",
  "This Google3 package was automatically generated within Google and",
  "serves only to wrap a specific version of a third-party package.",
  "This package contains no third-party code itself, so it has no license.",
  "Please look at the wrapped package for license details of the real",
  "third-party code."
  ]

licenseWarning = "!!!WARNING!!! (See above)"

unversionedPackageName :: Google3.PackageName -> Google3.PackageName
unversionedPackageName (Google3.PackageName package) =
    Google3.PackageName (init package)

haskellBinary, haskellLibrary, filegroup, cabalHaskellLibrary,
    internalCabalHaskellLibrary, shBinary :: Text
haskellBinary = "haskell_binary"
haskellLibrary = "haskell_library"
filegroup = "filegroup"
cabalHaskellLibrary = "cabal_haskell_library"
internalCabalHaskellLibrary = "_cabal_haskell_library"
shBinary = "sh_binary"

wrapperRule :: Google3.Label -> Google3.RuleClass -> Maybe Google3.Rule
wrapperRule label@(Google3.Label _ target) kind
    | kind == haskellBinary
        = Just (rule shBinary)
            { Google3.ruleSrcs = [Google3.LabelSource label] }
    | kind == haskellLibrary
        =  Just (rule haskellLibrary)
            { Google3.ruleDeps = [Google3.Labelled label] }
    | kind == internalCabalHaskellLibrary
        = Just (rule cabalHaskellLibrary)
            { Google3.ruleOtherAttrs =
                [("library", Google3.dependencyExpr $ Google3.Labelled label)]
            }
    | kind == filegroup
        = Just (rule filegroup)
            { Google3.ruleSrcs = [Google3.LabelSource label] }
    | otherwise = Nothing
  where
    rule ruleClass = Google3.Rule {
        Google3.ruleClass = ruleClass,
        Google3.ruleName = target,
        Google3.ruleSrcs = [],
        Google3.ruleDeps = [],
        Google3.ruleOtherAttrs = []
    }

wrapperPackage :: Google3.PackageName
               -> [(Google3.Label, Google3.RuleClass)]
               -> Google3.Package
wrapperPackage packageName
               rulesAndKinds =
  Google3.Package
    (unversionedPackageName packageName)
    (Text.unlines preamble)
    (Google3.PackageAttributes [Google3.parseLabel "//visibility:public"])
    (Just licenseWarning)
    ["unencumbered"]  -- packageLicenses
    ""                -- packageLicenseFile
    rules
    []
 where
  rules = mapMaybe (uncurry wrapperRule) rulesAndKinds'
  -- If we're doing go/cabal2buildv2 (--bzl flag to cabal2build), ignore any
  -- intermediate haskell_library rules generated by the cabal_haskell_package
  -- macro and only forward the cabal_haskell_library rule.
  -- TODO(judahjacobson): Once we remove the old way, simplify this logic by
  -- just removing the "haskell_library" case from wrapperRule.
  rulesAndKinds'
      | any ((== internalCabalHaskellLibrary) . snd) rulesAndKinds
          = filter ((/= haskellLibrary) . snd) rulesAndKinds
      | otherwise = rulesAndKinds

makeWrapperPackage :: Google3.PackageName
                    -> IO Google3.Package
makeWrapperPackage latestVersion = wrapperPackage latestVersion <$>
                                   ruleKinds latestVersion

buildFile' :: Google3.PackageName -> FilePath
buildFile' pkg = Google3.google3relativePath pkg </> "BUILD"

buildFile :: Google3.Package -> FilePath
buildFile = buildFile' . Google3.packageName

writeBuildFile :: Google3.Package -> IO ()
writeBuildFile pkg = do
  writeFile bfile (show . Google3.pPrint . Google3.packageSyntax $ pkg)
  Buildifier.call [bfile]
    where bfile = buildFile pkg

partitionM :: (Monad m, Functor m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f xs = uncurry ((,) `on` map fst) . partition snd . zip xs <$>
                  mapM f xs

-- | Like @rawSystem@ but throws an informative exception upon failure.
checkRawSystem :: String -> [String] -> IO ()
checkRawSystem cmd args = rawSystem cmd args >>= \ case
    ExitSuccess -> return ()
    ExitFailure r ->
        throwIO (mkIOError userErrorType ("rawSystem: " ++ cmd ++
                                          ' ':unwords (map show args) ++
                                          " (exit " ++ show r ++ ")")
                 Nothing Nothing)

noop = return . const ()

p4op op files = unless (null files) $ checkRawSystem "p4" (op:files)
p4add = p4op "add"
p4edit = p4op "edit"
p4record = noop
p4delete = p4op "delete"

hgop op files = unless (null files) $ checkRawSystem "hg" (op:files)
hgAdd = hgop "add"
hgEdit = noop
hgRecord = hgAdd
hgDelete = hgop "rm"


-- | Helper function to run an IO action in an edit/record environment.
withOpenFiles :: VCSClient -> [FilePath] -> IO a -> IO a
withOpenFiles client files action = do
  cmdEdit client files
  v <- action
  cmdRecord client files
  return v

createOrUpdateWrapperPackages client packages = do
  wrappers <- mapM makeWrapperPackage packages
  let wrapperBuildFiles = map buildFile wrappers
  (editFiles, addFiles) <- partitionM doesFileExist wrapperBuildFiles
  withOpenFiles client editFiles $ do
    mapM_ writeBuildFile wrappers
    -- It's not a disaster if this fails.
    Buildifier.run wrapperBuildFiles >>= \case
      Left er -> putStrLn $ "Warning: Buildifier failed with " ++ show er
      Right _ -> return ()
  cmdAdd client addFiles

-- Part three: point dependent targets at versionless rules, wherever
-- possible.

wrapperLabel (Google3.Label packageName target) =
    Google3.Label (unversionedPackageName packageName) target

-- | Given a versioned label, return a pair of (label, dependent labels),
--   excluding any dependents from the same Cabal package (any version, or
--   the versionless wrapper).
findRuleDependents :: Google3.Label -> IO (Google3.Label, [Google3.Label])
findRuleDependents label = do
  dependents <- directDependents label
  return (label, filter (not . sameCabalPackage) dependents)
 where
  sameCabalPackage dependent = packageOf dependent `isOrIsBelow`
                               packageOf (wrapperLabel label)
  packageOf (Google3.Label p _) = p
  (Google3.PackageName x) `isOrIsBelow` (Google3.PackageName y) =
      y `isPrefixOf` x

-- | Given a versioned package, find the relevant labels in it, and return
--   pairs of (label, dependent labels), excluding corresponding versionless
--   labels.
findPackageDependents :: Google3.PackageName
                      -> IO [(Google3.Label, [Google3.Label])]
findPackageDependents pkg = catch (findPackageDependents' pkg)
                                  (findDependentsFailed pkg :: IOError -> IO [(Google3.Label, [Google3.Label])])
 where
  findPackageDependents' package = do
      rks <- ruleKinds package
      let rulesToProcess = [rule | (rule, kind) <- rks,
                            isJust (wrapperRule rule kind)]
      filter (not . null . snd) <$> mapM findRuleDependents rulesToProcess
  findDependentsFailed oldPackage _ = do
    hPrint stderr oldPackage
    mapM_ (hPutStrLn stderr . ("  " ++)) [
        "Warning: ignoring this package because its dependents could",
        "not be determined."]
    -- In practice, this code path is only taken when a subprocess fails, and
    -- that subprocess' exit has already documented the reason. So printing
    -- the exception details here would just be noise.
    return []

dependentAttr :: Google3.Label -> Google3.Label -> String -> IO Bool
dependentAttr dependent dependency attr = do
  rawOutput <- Text.pack <$>
               readProcess "bazel"
                           ["query",
                            unwords [show dependency, "intersect",
                                     "labels(", show attr, ",",
                                     show dependent, ")"]
                           ] "" `catch` (\(e:: IOError) -> print e >> return [])
  return $ dependency `elem` [Google3.parseLabel line |
                              line <- Text.lines rawOutput]

updateDependent :: Google3.Label -> Google3.Label -> Google3.Label -> IO ()
updateDependent dependent oldDep newDep = do
  attrsToFix <- filterM (dependentAttr dependent oldDep) ["srcs", "deps"]
  if null attrsToFix
  then hPutStrLn stderr $ unwords [
            "Warning:", show dependent, "doesn't seem to depend on",
            show newDep, "in any way that can be fixed automatically."]
  else forM_ attrsToFix $ \attr -> rawSystem
                                   "bazel" ["edit", "replace_value",
                                            show dependent, attr,
                                            show oldDep, show newDep]

-- | Pretty print a key and its values.
ppKeyValues :: (Show a, Show b) => (a, [b]) -> IO ()
ppKeyValues (k, vs) = Text.putStrLn $
                      Text.unlines (showText k : map (("  " <>) . showText) vs)

-- | Prints a list of latest-versioned third-party targets which have direct
--   dependents (other than their versionless wrapper targets), along with
--   those dependents.
--
--   These are candidates for cleanup - in most cases, the dependents can
--   instead depend on the versionless wrapper target.
printExplicitVersionDependents :: IO ()
printExplicitVersionDependents = do
  allDeps <- latestVersions <$> versionedPackages >>= mapM findPackageDependents
  mapM_ ppKeyValues [(fst $ head grp,
                      sortBy (comparing show) $ concatMap snd grp) |
                     grp <- groupSortBy (show . fst) $ concat allDeps]

updateDependents :: VCSClient -> [Google3.PackageName] -> IO ()
updateDependents client packages = do
  allDependents <- concat <$> mapM findPackageDependents packages
  let allDependentBuildFiles =
          ordNub [buildFile' pkg |
                  Google3.Label pkg _ <- concatMap snd allDependents]
  withOpenFiles client allDependentBuildFiles $
    sequence_ [updateDependent dependent label (wrapperLabel label) |
               (label, dependents) <- allDependents,
               dependent <- dependents]

-- | Finds versions of third-party packages which aren't the most recent
--   ones installed and which have no dependents.
findObsoleteVersions :: IO [Google3.PackageName]
findObsoleteVersions = olderVersions <$> versionedPackages >>=
                       filterM noDependents
 where
  noDependents :: Google3.PackageName -> IO Bool
  noDependents = fmap (null . concat . snd . unzip) . findPackageDependents

-- | Opens for delete versions of third-party packages which aren't the most
--   recent ones installed and which have no dependents.
deleteObsoleteVersions :: VCSClient -> IO ()
deleteObsoleteVersions client = do
  obsoletePackages <- findObsoleteVersions
  cmdDelete client
    [Google3.google3relativePath p </> "..." | p <- obsoletePackages]

-- | Finds packages which depend on versions of third-party packages which
--   aren't the most recent ones installed.
findLaggingDependents :: IO [(Google3.PackageName, [Google3.PackageName])]
findLaggingDependents = do
  oldPackages <- olderVersions <$> versionedPackages
  (fmap . filter) (not . null . snd) . forM oldPackages $ \oldPackage -> do
      dependents <- map snd <$> findPackageDependents oldPackage
      return (oldPackage, ordNub [p | Google3.Label p _ <- concat dependents])

printLaggingDependents :: IO ()
printLaggingDependents = findLaggingDependents >>= mapM_ ppKeyValues

vcsForWorkingDirectory :: IO VCSClient
vcsForWorkingDirectory = do
  isHg <- fst3 <$>
    readProcessWithExitCode "hg" ["stat"] ""
  isP4 <- fst3 <$>
    readProcessWithExitCode "p4" ["pending"] ""
  return $ case (isHg, isP4) of
    (ExitSuccess, _) -> VCSClient  hgAdd  hgEdit  hgRecord  hgDelete
    (_, ExitSuccess) -> VCSClient   p4add   p4edit   p4record   p4delete
    _ -> error "Working directory is not a hg/p4 client"

showText :: Show a => a -> Text
showText = Text.pack . show
