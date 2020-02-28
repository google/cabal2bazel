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

module Main where

import RunfilesLib

-- | Check that we can read the runfiles from this binary rule and its
-- dependencies.
-- Don't bother using any special logic to find the runfiles dir,
-- since the Haskell code for that isn't ported to Skylark yet.
-- In general, anyway, we'll only run this test from "bazel test" or on
-- Forge, where no special logic is necessary.
main = mapM_ (\f -> readFile ("tools/build_defs/haskell/tests/attrs/" ++ f)
                  >>= putStrLn)
        [libFileName, "file_c.txt", "file_main.txt"]
