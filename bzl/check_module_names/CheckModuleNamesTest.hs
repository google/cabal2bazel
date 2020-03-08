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

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Text.Lazy (Text)
import GHC.Stack
import Google.Runtime.BuildHelper
import Google.Test.TF (googleTest)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process.Typed (readProcess, proc, setWorkingDir)
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit (assertFailure)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

checkModuleNamesPath :: FilePath
checkModuleNamesPath =
    "tools/build_defs/haskell/check_module_names/CheckModuleNames"

assertModuleNamesOk :: [String] -> [(FilePath, Text)] -> IO ()
assertModuleNamesOk = assertModuleNames Nothing

assertModuleNamesError :: [Text] -> [String] -> [(FilePath, Text)] -> IO ()
assertModuleNamesError = assertModuleNames . Just

assertModuleNames
    :: HasCallStack
    => Maybe [Text]  -- ^ If set, expect the following error messages
    -> [String] -- ^ Command-line arguments
    -> [(FilePath, Text)] -- ^ Input files
    -> IO ()
assertModuleNames expectedErrs args files =
    withSystemTempDirectory "check-module-names-test" $ \tmp -> do
        forM_ files $ \(path, contents) -> T.writeFile (tmp </> path) contents
        bin <- findHostDependencyOrDie checkModuleNamesPath
        libdir <- findHostDependencyOrDie GHC_PATHS_LIBDIR
        (ec, out, err) <- readProcess $ setWorkingDir tmp $ proc bin
              $ ("--ghcopt=-B" ++ libdir) : args
        unless (B.null out) $
            assertFailure $
                "CheckModuleNames shouldn't write to stdout; found: "
                ++ show out
        case (ec, expectedErrs) of
          (ExitSuccess, Nothing) -> return ()
          (ExitFailure _, Nothing) ->
              assertFailure $
                  "CheckModuleNames unexpectedly failed with output: "
                  ++ show err
          (ExitSuccess, Just _) ->
              assertFailure "CheckModuleNames unexpectedly succeeded"
          (ExitFailure _, Just errs) -> let
              outT = T.decodeUtf8 err
              missingErrs = filter (not . (`T.isInfixOf` outT)) errs
              in unless (null missingErrs) $
                  assertFailure $
                      "CheckModuleNames didn't print the expected messages: "
                      ++ show missingErrs
                      ++ "; got: " ++ show outT

main = googleTest
    [ testCase "no modules" $ assertModuleNamesOk [] []
      -- Note: this binary is agnostic to the filepath<->module name convention.
      -- It relies on the build rules to pass it the "guess" via --source-file.
    , testCase "match" $ assertModuleNamesOk
        ["--source-file=foo.hs,Bar.Baz"]
        [("foo.hs", "module Bar.Baz where")]
    , testCase "no match" $ assertModuleNamesError
        ["foo.hs:1:8-10:\n    Expected: Bar\n         Got: Baz"]
        ["--source-file=foo.hs,Bar"]
        [("foo.hs", "module Baz where")]
    , testGroup "main_is"
        [ testCase "No main_is" $ assertModuleNamesError
            ["foo.hs:1:8-11:\n    Expected: Bar\n         Got: Main"]
            ["--source-file=foo.hs,Bar"]
            [("foo.hs", "module Main where")]
        , testCase "Main" $ assertModuleNamesOk
            ["--source-file=foo.hs,Bar", "--main-is=Main"]
            [("foo.hs", "module Main where")]
        , testCase "ok" $ assertModuleNamesOk
            ["--source-file=foo.hs,Bar", "--main-is=Bar"]
            [("foo.hs", "module Bar where")]
        , testCase "missing" $ assertModuleNamesError
            ["Error: No source file defined the expected main_is \"Baz\"; "
             <> "got:\n  foo.hs:1:8-10: Bar"]
            ["--source-file=foo.hs,Bar", "--main-is=Baz"]
            [("foo.hs", "module Bar where")]
        ]
    , testCase "ghcopt" $ assertModuleNamesOk
        ["--ghcopt=-XMagicHash", "--source-file=foo.hs,Bar"]
        [("foo.hs", "module Bar where\nimport A (magic#)")]
    ]
