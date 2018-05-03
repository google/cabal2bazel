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

{-# LANGUAGE OverloadedStrings #-}

-- | Defines conversion between Cabal package descriptions
--   (in .cabal files) and Google3 package descriptions
--   (in BUILD files).
module Google.Google3.Tools.Cabal2Build.Build
  ( google3packageForCabalPackage
  , getDependencies
  , packageDescriptionFileName
  , ordNub
  ) where

import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Text as Cabal (display)
import qualified Google.Google3 as Google3
import Google.Google3.Package (Expr(..), Stmt(..))
import qualified Data.Set as S

import Google.Google3.Tools.Cabal2Build.Options (BzlOption(..))
import Google.Google3.Tools.Cabal2Build.PackageName
    (google3packageNameForCabalPackageId)
import Google.Google3.Tools.Cabal2Build.License (showCabalLicense)


import System.FilePath ((</>))

google3licenseForCabalLicense :: Cabal.License -> Google3.License
google3licenseForCabalLicense l = case l of
  Cabal.GPL  _   -> "restricted"
  Cabal.LGPL _   -> "restricted"
  Cabal.MPL _    -> "restricted"
  Cabal.BSD2     -> "notice"
  Cabal.BSD3     -> "notice"
  Cabal.BSD4     -> "notice"
  Cabal.MIT      -> "notice"
  Cabal.Apache _ -> "notice"
  Cabal.ISC      -> "notice"
  Cabal.PublicDomain -> "unencumbered (DONOTSUBMIT: CHECK THIS)"
  Cabal.AllRightsReserved -> "restricted"
  Cabal.OtherLicense -> "restricted"
  Cabal.UnknownLicense name -> "restricted (DONOTSUBMIT: Find appropriate license " <>
                               "type for '" <> Text.pack name <> "')"
  _ ->
      "restricted (DONOTSUBMIT: Find appropriate license)"

-- | Calculates most of the Google3 build rule that corresponds to a Cabal
--   library or executable. The rule class and its name are left undefined;
--   'ruleForLibrary' and 'ruleForExecutable' provide them differently.
--
--   This function gives a rule with:
--
--     * a glob for each source directory;
--
--     * a Google3 dependency for each Cabal dependency
--
--     * ghcopts
ruleForBuildInfo :: [Text] -> Cabal.BuildInfo -> [Text] -> Google3.Rule
ruleForBuildInfo filesToExclude info defaultPackages =
  let srcs = [Google3.GlobSource Google3.Glob {
                Google3.globIncludes = map (Text.pack . globForSourceDir) $
                                           Cabal.hsSourceDirs info,
                Google3.globExcludes = if "." `elem` Cabal.hsSourceDirs info
                                          then filesToExclude
                                          else []
             }]
      globForSourceDir "." =       recursiveGlobSuffix
      globForSourceDir d   = d </> recursiveGlobSuffix
      recursiveGlobSuffix = "**" </> "*.hs"
      mkdep (Cabal.Dependency (Cabal.PackageName l) _) =
          Google3.Hackage (Text.pack l)
      inDefaultPackages (Cabal.Dependency (Cabal.PackageName l) _) =
          Text.pack l `elem` defaultPackages
      deps = map mkdep $ filter (not . inDefaultPackages)
                 (Cabal.pkgconfigDepends info ++ Cabal.targetBuildDepends info)
      extensionFlag e = "-X" ++ Cabal.display e
      ghcOpts = let rawOpts = Cabal.hcOptions Cabal.GHC info ++
                              map extensionFlag (Cabal.defaultExtensions info ++
                                                 Cabal.oldExtensions info) ++
                              map ("-optP" ++) (Cabal.cppOptions info) ++
                              map ("-optc" ++) (Cabal.ccOptions info) ++
                              map ("-optl" ++) (Cabal.ldOptions info) ++
                              ["-Wwarn"] in
                if null rawOpts
                then []
                else [("ghcopts", Google3.stringListSyntax
                                  (map Text.pack rawOpts))]
  in Google3.Rule {
       Google3.ruleClass = undefined,
       Google3.ruleName = undefined,
       Google3.ruleSrcs = srcs,
       Google3.ruleDeps = deps,
       Google3.ruleOtherAttrs = ghcOpts
     }

newStmts :: [Google3.Stmt]
newStmts =
    [ SExpr $ Call "load"
        (map LitString [":" <> Text.pack packageDescriptionFileName,
                        "description"])
        []
    , SExpr $ Call "load"
        (map LitString ["//tools/build_defs/haskell:cabal_package.bzl",
                        "cabal_haskell_package"]) []
    , SExpr $ Call "cabal_haskell_package" [] [("description", Var "description")]
    ]

-- | The name of the auto-generated package description file, relative to the
-- Cabal package.
packageDescriptionFileName :: FilePath
packageDescriptionFileName = "package_description.bzl"

-- | Calculates the Google3 build rule that corresponds to a Cabal package.
-- It will generate a library and test rule.
ruleForLibrary :: [Text] -> [Text] -> Text -> Cabal.Library -> Google3.Rule
ruleForLibrary filesToExclude defaultPackages libName lib =
  (ruleForBuildInfo filesToExclude (Cabal.libBuildInfo lib) defaultPackages) {
      Google3.ruleClass = "cabal_haskell_package",
      Google3.ruleName = Google3.TargetName libName
    }

-- | Calculates the Google3 build rule that corresponds to a Cabal executable.
ruleForExecutable :: [Text] -> [Text] -> Cabal.Executable -> Google3.Rule
ruleForExecutable filesToExclude defaultPackages exe =
  (ruleForBuildInfo filesToExclude (Cabal.buildInfo exe) defaultPackages) {
      Google3.ruleClass = "haskell_binary",
      Google3.ruleName = Google3.TargetName $ Text.pack $ Cabal.exeName exe
    }

google3packageCommentForCabalPackage :: Cabal.PackageDescription
                                     -> Cabal.FlagAssignment
                                     -> Text
google3packageCommentForCabalPackage packageDesc flags =
  Text.intercalate "\n\n"
  [ "Description: " <> (Text.pack . Cabal.display . Cabal.package) packageDesc
  , Text.pack $ Cabal.description packageDesc] <> flagsDesc flags
  where flagsDesc [] = Text.empty
        flagsDesc fs = Text.unlines $
            "\n\nConfigured with Cabal flags:" : map flagDesc fs
        flagDesc (Cabal.FlagName flag, value) =
            (Text.concat . map Text.pack) ["  ", flag, ": ", show value]

google3packageForCabalPackage
    :: Cabal.PackageDescription
    -> Cabal.FlagAssignment
    -> BzlOption
    -> Google3.PackageAttributes  -- ^ Attributes for the package
    -> [Text]  -- ^ Top-level source files that should be excluded from source
               --   globs (e.g., Setup.hs).
    -> [Text]  -- ^ Packages that are included by default and do not need to be
               --   added as deps.
    -> Google3.Package
google3packageForCabalPackage desc flags bzl attrs excludeFiles defaultPackages =
  let packageId = Cabal.package desc
      (Cabal.PackageName name) = Cabal.pkgName packageId
      libName
          -- Disambiguate libraries and binaries that have the same name.
          | any (\exe -> Cabal.exeName exe == name)
              $ Cabal.executables desc = name ++ "-lib"
          | otherwise = name
  in Google3.Package {
       Google3.packageName = google3packageNameForCabalPackageId packageId,
       Google3.packageComment =
           google3packageCommentForCabalPackage desc flags,
       Google3.packageAttributes = attrs,
       Google3.packageRules = case bzl of
          NoUseBzl ->
             map (ruleForLibrary excludeFiles defaultPackages (Text.pack libName))
             (maybeToList $ Cabal.library desc) ++
             map (ruleForExecutable excludeFiles defaultPackages)
             (Cabal.executables desc)
          UseBzl -> [],
       Google3.packageLicenses = [
           google3licenseForCabalLicense $ Cabal.license desc],
       Google3.packageLicenseComment = Just $ Text.pack $
           (showCabalLicense $ Cabal.license desc),
       Google3.packageLicenseFile =
           Text.pack . head $ Cabal.licenseFiles desc ++ [""],
       Google3.packageStmts = case bzl of
          UseBzl -> newStmts
          NoUseBzl -> []
     }

-- | Removes duplicates, preserving order.
ordNub :: (Ord a) => [a] -> [a]
ordNub =
  let go _ [] = []
      go s (x:xs) = if x `S.member` s
        then go s xs
        else x : go (S.insert x s) xs
  in go S.empty

-- | Returns the list of dependencies of the given package.
getDependencies :: Cabal.PackageDescription -> [Text]
getDependencies packageDesc = ordNub $ concatMap getDependencies' $
  map Cabal.libBuildInfo (maybeToList $ Cabal.library packageDesc) ++
  map Cabal.buildInfo (Cabal.executables packageDesc)
    where getDependencies' info = map getPackageName $
            Cabal.pkgconfigDepends info ++ Cabal.targetBuildDepends info
          getPackageName (Cabal.Dependency (Cabal.PackageName l) _) =
            Text.pack l
