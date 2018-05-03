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

module Google.Google3.Tools.Cabal2Build.Options where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Options.Applicative.Builder
import Options.Applicative.Help.Chunk (Chunk(..))
import qualified Options.Applicative.Help.Core as HelpCore
import Options.Applicative.Internal (ParseError(ShowHelpText))
import Options.Applicative.Types
    ( OptName(..)
    , OptProperties
    , ParseError(..)
    , Parser
    , ParserInfo
    )
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal

defaultVersion :: Cabal.Version
defaultVersion = Cabal.Version [8, 0, 2] []

-- | Operation to perform.
data Operation = Install Text  -- ^ Install Cabal package.
               | WireUp Text   -- ^ Add Google files to existing directory with
                               --   unpacked Cabal package.
               | ReplaceOwner Text -- ^ Replace an owner in a package.

-- | Whether to prompt the user or use defaults.
data PromptOption = Prompt | UseDefaults

-- | Whether to generate files for go/cabal2buildv2.
-- TODO(judahjacobson): Remove the non-bzl way after everything's switched
-- over.
data BzlOption = UseBzl | NoUseBzl
    deriving Eq

-- | Command line options.
data Options = Options
    { operation     :: Operation
    , recursiveOpt  :: Bool
    , promptOpt     :: PromptOption
    , ownerOpt      :: Text
    , bzlOpt        :: BzlOption
    , ghcVersionOpt :: Cabal.Version
    }

options :: Parser Options
options = Options <$> (install <|> wireUp <|> replaceOwner) <*> recursive <*> prompt <*> owner
                  <*> bzl <*> ghcVersion
    where install = Install . Text.pack
              <$> strOption (
                  long "fetch" <>
                  help "Haskell package to fetch and install")
          wireUp = WireUp . Text.pack
              <$> strOption (
                  long "wire-up" <>
                  help  "Google3 package to wire up")
          replaceOwner = ReplaceOwner . Text.pack
              <$> strOption (
                  long "replace-owner" <>
                  help  "Google3 package to replace an owner in")
          recursive = flag False True
              (long "recursive" <> help "Recursively install dependencies")
          prompt = flag Prompt UseDefaults
              (long "use-defaults" <>
              help "Whether to use defaults instead of prompting the user")
          owner = Text.pack
              <$> strOption (
                  long "owner" <>
                  help "Other owner of packages, asks interactively or chooses \
                       \randomly if empty" <> value "" <> showDefault)
          bzl = flag UseBzl NoUseBzl
              (long "no-bzl" <>
               help "Whether to not generate files for go/cabal2buildv2")
          ghcVersion :: Parser Cabal.Version
          ghcVersion = option (maybeReader Cabal.simpleParse)
              (long "ghc-version" <>
               help "Which version of to evaluate the cabal file for" <>
               value defaultVersion <> showDefault)
