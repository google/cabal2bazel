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

{-# LANGUAGE LambdaCase #-}
-- | Utility module to interact with "expn" tool used to expand group
-- membership.
module Google.Utils.Expn (tryExpand, expand) where

import Control.Monad ((>=>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

-- | Tries expanding a group, returns either exit code with outputs or
--   a list of emails in the group.
tryExpand :: String -> IO (Either (Int, String, String) [String])
tryExpand group = readProcessWithExitCode "/usr/bin/expn" [group] "" <$$> \ case
    (ExitSuccess, output, _)           -> Right $ lines output
    (ExitFailure code, output, stderr) -> Left (code, output, stderr)
  where (<$$>) = flip fmap

-- | The same as tryExpand, but errors out if it fails.
expand :: String -> IO [String]
expand = tryExpand >=> either (error . show) return
