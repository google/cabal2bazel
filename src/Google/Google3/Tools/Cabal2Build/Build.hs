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

{-# LANGUAGE OverloadedStrings #-}

-- | Defines conversion between Cabal package descriptions
--   (in .cabal files) and Google3 package descriptions
--   (in BUILD files).
module Google.Google3.Tools.Cabal2Build.Build
  ( google3packageForCabalPackage
  , getDependencies
  , packageDescriptionFileName
  ) where

import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Distribution.License as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Text as Cabal (display)
import qualified Google.Google3.Package as Google3
import Google.Google3.Package (Expr(..), Stmt(..))
import Google.Utils.List (ordNubOn)

import Google.Google3.Tools.Cabal2Build.Configuration
    ( FlagAssignmentList
    , displayFlagName
    )
import Google.Google3.Tools.Cabal2Build.Options (EnabledOption(..))
import Google.Google3.Tools.Cabal2Build.PackageName
    (google3packageNameForCabalPackageId)

-- TODO(judahjacobson): This currently uses Distribution.License for simplicity.
-- Eventually, consider switching to Distribution.SPDX.License (a much larger
-- enum), which is the internal representation of  the "license" field for
-- .cabal files with cabal-version 2.2 or higher.
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
  Cabal.AGPL _ -> "not allowed (DONOTSUBMIT: AGPL license is not allowed)"
  Cabal.UnspecifiedLicense ->
      "restricted (DONOTSUBMIT: Find appropriate license)"

newStmts
    :: EnabledOption
    -> String  -- ^ Username
    -> [Google3.Stmt]
newStmts enabled user =
    [ SExpr $ Call "load"
        (map LitString [":" <> Text.pack packageDescriptionFileName,
                        "description"])
        []
    , SExpr $ Call "load"
        (map LitString ["//tools/build_defs/haskell:cabal_package.bzl",
                        "cabal_haskell_package"]) []
    ]
    ++
    (case enabled of
          Enabled -> []
          Disabled -> [SComment $ disabledTODO user])
    ++
    [ SExpr $ Call "cabal_haskell_package" []
            $ ("description", Var "description")
              : [("disabled", Var "True") | enabled == Disabled]
    ]

-- | The name of the auto-generated package description file, relative to the
-- Cabal package.
packageDescriptionFileName :: FilePath
packageDescriptionFileName = "package_description.bzl"

google3packageCommentForCabalPackage :: Cabal.PackageDescription
                                     -> FlagAssignmentList
                                     -> Text
google3packageCommentForCabalPackage packageDesc flags =
  Text.intercalate "\n\n"
  [ "Description: " <> (Text.pack . Cabal.display . Cabal.package) packageDesc
  , Text.pack $ Cabal.description packageDesc] <> flagsDesc flags
  where flagsDesc [] = Text.empty
        flagsDesc fs = Text.unlines $
            "\n\nConfigured with Cabal flags:" : map flagDesc fs
        flagDesc (flag, value) =
            (Text.concat . map Text.pack)
                ["  ", displayFlagName flag, ": ", show value]

disabledTODO :: String -> Text
disabledTODO user =
    "TODO(" <> Text.pack user <> "): Enable this package."
    <> " It has been temporarily disabled."

google3packageForCabalPackage
    :: Cabal.PackageDescription
    -> FlagAssignmentList
    -> Google3.PackageAttributes  -- ^ Attributes for the package
    -> EnabledOption
    -> String -- ^ Username
    -> Google3.Package
google3packageForCabalPackage desc flags attrs enabled user =
  Google3.Package {
       Google3.packageName = google3packageNameForCabalPackageId
                                 (Cabal.package desc),
       Google3.packageComment =
           google3packageCommentForCabalPackage desc flags,
       Google3.packageAttributes = attrs,
       Google3.packageRules = [],
       Google3.packageLicenses =
           [google3licenseForCabalLicense $
           -- Note: licenseFromSPDX is lossy, but it defaults to returning
           -- UnknownLicense which we can just edit by hand later.
           -- We use licenseRaw here rather than license so that for
           -- cabal-version less than 2.2, we can preserve the
           -- Distribution.License as-is.
               either Cabal.licenseFromSPDX id $
               Cabal.licenseRaw desc],
       -- The license file for third-party code *must* be named LICENSE.
       -- We'll rely on the wire-up step to rename it as needed.
       Google3.packageLicenseFile = "LICENSE",
       Google3.packageStmts = newStmts enabled user
     }

-- | Returns the list of dependencies of the given package.
getDependencies :: Cabal.PackageDescription -> [Cabal.PackageName]
getDependencies packageDesc = ordNubOn (Text.pack . Cabal.display) $
  concatMap getDependencies' $
  map Cabal.libBuildInfo (maybeToList $ Cabal.library packageDesc) ++
  map Cabal.buildInfo (Cabal.executables packageDesc)
    where
      getDependencies' info = map getPackageName $
            Cabal.targetBuildDepends info
      getPackageName (Cabal.Dependency n _) = n
