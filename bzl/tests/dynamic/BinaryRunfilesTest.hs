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

-- | This test checks that a linkstatic=False binary can be run
-- in isolation, using its .runfiles directory to load the shared libraries it
-- depends on.
--
-- Binary
-- Binary.runfiles/{WORKSPACE}/...
--
-- This file organization might happen in the deployment of such a binary.
module Main (main) where

import Google.Runtime.BuildHelper (findRunfilesDirOrDie)
import System.Directory (createDirectory, setCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO.Temp -- (withSystemTempDirectory)
import System.Process (callProcess)

main :: IO ()
main = do
    -- This test is parameterized on the particular binary that we're testing.
    [binaryName] <- getArgs
    runfiles <- findRunfilesDirOrDie
    let binary = runfiles </>
                  "cabal2bazel/tools/build_defs/haskell/tests/dynamic" </>
                  binaryName
    withSystemTempDirectory "BinaryRunfilesTest" $ \tmp -> do
      -- Move into a temporary directory, to confirm that we're only using
      -- guidance from the binary and not the setup of this test.
      let workingDir = tmp </> "working"
      createDirectory workingDir
      setCurrentDirectory workingDir

      -- Run the binary as it is in the runfiles directory.  This is the same
      -- setup as in a "bazel test" of a target, other than using a different
      -- working directory.
      callProcess binary []

      -- Run the binary in a separate directory, with a parallel ".runfiles"
      -- directory next to it.
      -- Note: the .runfiles contains all the runfiles of this *test*, which is
      -- strictly greater than just of the binary that the test uses.  But
      -- that shouldn't matter for this purpose.
      let isolatedBinary = tmp </> binaryName
      callProcess "cp" ["-R", binary, isolatedBinary]
      callProcess "cp" ["-R", runfiles, isolatedBinary ++ ".runfiles"]
      callProcess isolatedBinary []
