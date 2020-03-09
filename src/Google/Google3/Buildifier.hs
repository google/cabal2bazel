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

-- | Utility to format BUILD files using Buildifier tool.

module Google.Google3.Buildifier (call) where

import System.Process (callProcess)

import Google.Runtime.BuildHelper (findHostDependencyOrDie)

-- | Runs buildifier on files with the specified paths.
call :: [FilePath] -> IO ()
call args = do
    bin <- findHostDependencyOrDie "devtools/buildifier/buildifier"
    callProcess bin ("--lint=fix" : args)
