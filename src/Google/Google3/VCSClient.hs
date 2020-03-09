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

module Google.Google3.VCSClient
    ( VCSClient(..)
    , vcsForGivenDirectory
    , withOpenFile
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Tuple.Extra (fst3)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.IO.Error (mkIOError, userErrorType)
import qualified System.Process as Proc

-- | Generic command action.
type Command = [FilePath] -> IO ()

-- | VCS Client type, holding the commands to run and the directory it
-- works in.
data VCSClient = VCSClient
  { cmdAdd        :: Command -- ^ Add files to VCS command.
  , cmdIntegrate  :: Command -- ^ Marks a file as a copy of a previous one,
                             --   preserving VCS history.
  , cmdRecord     :: Command -- ^ Records that a file has been edited (after).
  , cmdDelete     :: Command -- ^ Marks files for removal.
  , cmdMove       :: Command -- ^ Moves an already-tracked file.
  , cmdFix        :: Command -- ^ Autoformats the given files.
  , g3Dir         :: FilePath  -- ^ Top level google3 directory.
  }

-- | Like @rawSystem@ but throws an informative exception upon failure.
checkRawSystem :: FilePath -> String -> [String] -> IO ()
checkRawSystem workDir cmd args = do
    ec <- runProcessIn workDir cmd args
    case ec of
        ExitSuccess -> return ()
        ExitFailure r ->
            throwIO (mkIOError userErrorType ("rawSystem: " ++ cmd ++
                                              ' ':unwords (map show args) ++
                                              " (exit " ++ show r ++ ")")
                     Nothing Nothing)

runProcessIn :: FilePath -> String -> [String] -> IO ExitCode
runProcessIn workDir cmd args = do
    let process = (Proc.proc cmd args) { Proc.cwd = Just workDir }
    Proc.withCreateProcess process $ \_ _ _ ph -> Proc.waitForProcess ph

noop :: a -> IO ()
noop = return . const ()

type CmdRunner = String -> [String] -> IO()

hgClient :: FilePath -> VCSClient
hgClient g3dir = VCSClient
    { cmdAdd = hgop "add"
      -- Use --force to overwrite any existing, committed files.
    , cmdIntegrate = hgop "cp" . (["--after", "--force"] ++)
    , cmdRecord = hgop "add"
    , cmdDelete = hgop "rm"
    , cmdMove = hgop "mv"
    , cmdFix = hgop "fix"
    , g3Dir = g3dir
    }
  where
    hgop :: CmdRunner
    hgop op files = unless (null files) $ checkRawSystem g3dir "hg" (op:files)

-- | Runs an action that creates a file, and adds it if it didn't exist
-- beforehand.
withOpenFile :: VCSClient -> FilePath -> IO a -> IO a
withOpenFile client file action = do
    existed <- doesFileExist file
    r <- action
    unless existed $ cmdAdd client [file]
    return r

-- | Returns a 'VCSClient' for use with the given directory (e.g. hg).
vcsForGivenDirectory :: FilePath -> IO VCSClient
vcsForGivenDirectory g3dir = do
    let runProcessSilently cmd args = do
            let process = (Proc.proc cmd args) { Proc.cwd = Just g3dir }
            fst3 <$> Proc.readCreateProcessWithExitCode process ""
    isHg <- runProcessSilently "hg" ["stat"]
    return $ case isHg of
        ExitSuccess -> hgClient g3dir
        _ -> error "Working directory is not an hg client."
