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

module Google.Google3.Tools.Cabal2Build.Options where

import Control.Applicative ((<|>), many)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Distribution.PackageDescription (FlagName)
import Options.Applicative (Parser, flag, help, metavar, long, showDefault,
                            strOption, value)
import Google.Google3.Tools.Cabal2Build.Configuration (makeFlagName)

-- | Operation to perform.
data Operation = Install Text      -- ^ Install Cabal package.
               | WireUp FilePath   -- ^ Add Google files to existing
                                   -- directory with unpacked Cabal
                                   -- package.
               | ReplaceOwner Text -- ^ Replace an owner in a package.

-- | Whether to keep old versions of packages.
data KeepOption = Keep | Cleanup
    deriving Eq

-- | Whether to enable the package and generate a version-agnostic BUILD file.
data EnabledOption = Disabled | Enabled
    deriving (Eq, Show)

-- | How far down the package dependency graph to go.
data DepthOption
    = Recursive  -- ^ Fetch every transitive depenency
    | Immediate  -- ^ Only this target
    deriving Eq

-- | Command line options.
data Options = Options
    { operation     :: Operation
    , recursiveOpt  :: DepthOption
    , ownerOpt      :: Text
    , resolverOpt   :: Resolver
    , keepOpt       :: KeepOption
    , cabalFlags    :: [(FlagName, Bool)]
    , enabledOpt    :: EnabledOption
    , dryRunOpt     :: DryRunOption
    }

data DryRunOption
    = DryRun
    | ReallyRun
    deriving Eq

-- | An identifier for a version of the Stackage build plan.
-- The string is either of the form "lts-*" or "nightly-*".
newtype Resolver = Resolver { unResolver :: String }

defaultResolver :: String
defaultResolver = "lts-13.3"

defaultPrimaryOwner :: String
defaultPrimaryOwner = "default-owner"

options :: Parser Options
options = Options <$> (install <|> wireUp <|> replaceOwner)
                  <*> recursive <*> owner
                  <*> resolver <*> keep <*> many cabalFlag <*> enabled
                  <*> dryRun
    where install = Install . Text.pack
              <$> strOption (
                  long "fetch" <>
                  metavar "HACKAGE_PACKAGE" <>
                  help "Haskell package to fetch and install")
          wireUp = WireUp
              <$> strOption (
                  long "wire-up" <>
                  metavar "VERSIONED_DIR" <>
                  help  "Google3 package to wire up")
          replaceOwner = ReplaceOwner . Text.pack
              <$> strOption (
                  long "replace-owner" <>
                  help  "Google3 package to replace an owner in")
          recursive = flag Immediate Recursive
              (long "recursive" <> help "Recursively install dependencies")
          dryRun = flag ReallyRun DryRun
              (long "dry-run" <>
               help "Stop before actually installing packages")
          owner = Text.pack
              <$> strOption (
                  long "owner" <>
                  help ("Primary owner of packages. " ++
                        "Current user will be added as the secondary owner.") <>
                  value defaultPrimaryOwner <>
                  showDefault)
          resolver :: Parser Resolver
          resolver = Resolver
              <$> strOption
              (long "resolver" <>
               help "Which version of Stackage to use for this package" <>
               value defaultResolver <> showDefault)
          keep = flag Cleanup Keep
                    (long "keep" <> help "Don't delete old versions")
          cabalFlag :: Parser (FlagName, Bool)
          cabalFlag = makeCabalFlag
              <$> strOption (
                  long "flag" <>
                  help "Cabal flag to override the Stackage default")
          makeCabalFlag :: String -> (FlagName,Bool)
          makeCabalFlag ('-':s) = (makeFlagName s, False)
          makeCabalFlag ('+':s) = (makeFlagName s, True)
          makeCabalFlag s = (makeFlagName s, True)
          enabled = flag Enabled Disabled
                    (long "disabled" <>
                     help ("Disable and don't generate a version-agnostic "
                           ++ "BUILD file."))
