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

{- | This program checks that the module names for a library target
match their filepaths.

To test locally:

  bazel run tools/build_defs/haskellcollect_modules:CheckModuleNames \
     --ghcopt=OPTION --source-file=MODULE,FILE ...

For each MODULE:FILE pair, it uses the GHC API to parse the source FILE
and compare its module name against MODULE.

If it succeeds, it finishes without error. Otherwise, it prints an error
message to stderr.

-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Monad (unless, forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- Use GetOpt instead of optparse-applicative, so that we only depend on "base".
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import qualified DynFlags
import qualified GHC
import qualified HeaderInfo
import qualified Outputable
import qualified StringBuffer

-- | Configuration options for this program.
data Options = Options
    { optGhcopts :: [String]
    , optSourceFiles :: [(FilePath, GHC.ModuleName)]
      -- ^ The expected module name for each source file.
    , optMainIs :: Maybe String
    }

defaultOptions :: Options
defaultOptions = Options
    { optGhcopts = []
    , optSourceFiles = []
    , optMainIs = Nothing
    }

-- | The list of known command-line flags.  Each one produces a function
-- that modifies the current set of options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["ghcopt"]
      (ReqArg (\o opts -> opts { optGhcopts =  o : optGhcopts opts })
              "OPTION")
      "GHC options"
  , Option [] ["source-file"]
     (ReqArg (\f opts -> opts { optSourceFiles = parseSourceFileOpt f
                                   : optSourceFiles opts })
             "MODULE,FILE")
     "A pair of a module and a source file."
  , Option [] ["main-is"]
     (ReqArg (\m opts -> opts { optMainIs = Just m })
            "MODULE")
     "The name of the module defining the 'main' function."
  ]

parseSourceFileOpt :: String -> (FilePath, GHC.ModuleName)
parseSourceFileOpt s = case break (== ',') s of
    (f, ',':m) -> (f, GHC.mkModuleName m)
    _ -> error $ "Couldn't not parse --source-file option: " ++ show s

-- | Gets the options specified on the command-line.
getOptions :: IO Options
getOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, [], []) -> return $ foldr ($) defaultOptions o
        _ -> error $ usageInfo "CheckModuleNames: invalid usage:"
                                        options

-- | Runs a single GHC API session with the given command-line options.
-- Gets the libdir by looking for the first argument of "-B..."
withDynFlags :: [String] -> (DynFlags.DynFlags -> IO a) -> IO a
withDynFlags ghcOpts act =
    GHC.runGhc (Just libDir) $
        GHC.handleSourceError h $ do
            dflags <- DynFlags.getDynFlags
            (dflags', _, _) <- GHC.parseDynamicFlags dflags (map GHC.noLoc rest)
            liftIO $ act dflags'
  where
    (libDir, rest) = getLibDir ghcOpts
    h e = do
        GHC.printException e
        liftIO exitFailure


-- | Finds the first "-B..." flag and return its argument.
getLibDir :: [String] -> (String, [String])
getLibDir ss = case partitionEithers $ map f ss of
    ([], _) -> error "Unable to find libdir"
    (libs, rest) -> (last libs, rest)
  where
    f ('-':'B':s) = Left s
    f s = Right s

parseModuleNameFromFile :: GHC.DynFlags -> FilePath -> IO (GHC.Located GHC.ModuleName)
parseModuleNameFromFile dflags sourceFile = do
    -- Approximate preprocessing the file by stripping out files
    -- with leading "#".  Remember, we're only trying to extract the module name.
    contents <- T.decodeUtf8 <$> B.readFile sourceFile
    let filtered = dropCppLines contents
    let buf = StringBuffer.stringToStringBuffer $ T.unpack filtered
    (dflags' , _, _) <- GHC.parseDynamicFlags dflags
            $ HeaderInfo.getOptions dflags buf sourceFile
    (_, _, modName) <- HeaderInfo.getImports dflags' buf sourceFile sourceFile
    return modName

dropCppLines :: T.Text -> T.Text
dropCppLines = T.unlines . map maybeFilter . T.lines
  where
    -- Make error messages reference back to the original file:
    maybeFilter s
        | Just ('#', _) <- T.uncons s = T.empty
        | otherwise = s

main :: IO ()
main = do
    hSetEncoding stderr utf8
    opts <- getOptions
    withDynFlags (reverse $ optGhcopts opts) $ \dflags -> do
        -- `parsed` is a map from actual module names (located in a specific
        -- file) to the expected ("guessed") module based on the filepath.
        parsed :: Map (GHC.Located GHC.ModuleName) GHC.ModuleName
            <- fmap Map.fromList . forM (optSourceFiles opts) $ \(f, m) ->
                    (, m) <$> parseModuleNameFromFile dflags f
        parsedMainOk <- assertMainOk (optMainIs opts) dflags parsed
        let mismatch =
                Map.filterWithKey (\m' m -> m /= GHC.unLoc m') parsedMainOk
        unless (Map.null mismatch) $ failWithMessageLines $
            "Error: module names do not match filepaths:"
            {- HLINT ignore "Redundant flip" -}
            : flip map (Map.toList mismatch)
                (\(m', m) ->
                    "  " ++ Outputable.showPpr dflags (GHC.getLoc m') ++
                    ":\n" ++
                    "    Expected: " ++ GHC.moduleNameString m ++ "\n" ++
                    "         Got: " ++ GHC.moduleNameString (GHC.unLoc m'))

-- | Validates the expected main module against the parsed module names.
-- Filters out any files whose path/module name are *not* expected to
-- match, in particular if they are the "Main" module.
assertMainOk
    :: Maybe String
    -> GHC.DynFlags
    -> Map (GHC.Located GHC.ModuleName) k
    -> IO (Map (GHC.Located GHC.ModuleName) k)
assertMainOk Nothing _ modules = pure modules -- Not a binary
assertMainOk (Just "Main") dflags modules = assertSingleMain dflags modules
assertMainOk (Just mainIs) dflags modules
    | mainIs `notElem` map (GHC.moduleNameString . GHC.unLoc)
                            (Map.keys modules)
        = failWithMessageLines
            $ ("Error: No source file defined the expected main_is "
                    ++ show mainIs ++ "; got:")
            : map (\m -> "  " ++ showModuleName dflags m)
                (Map.keys modules)
    | otherwise = pure modules

-- | Checks that exactly one module has the name "Main", and if so
-- removes it.  Otherwise, prints an error and fails the process.
assertSingleMain
    :: GHC.DynFlags
    -> Map (GHC.Located GHC.ModuleName) k
    -> IO (Map (GHC.Located GHC.ModuleName) k)
assertSingleMain dflags modules =
    if Map.size mains == 1
        then return rest
        else failWithMessageLines
            $ "Error: expected exactly one \"Main\" module; got:"
            : map (\m -> "  " ++ showModuleName dflags m)
                (Map.keys modules)
  where
    (mains, rest) = Map.partitionWithKey (\m _ -> isMain m) modules
    isMain m = GHC.moduleNameString (GHC.unLoc m) == "Main"

showModuleName :: GHC.DynFlags -> GHC.Located GHC.ModuleName -> String
showModuleName dflags m =
    Outputable.showPpr dflags (GHC.getLoc m)
        ++ ": " ++ GHC.moduleNameString (GHC.unLoc m)

failWithMessageLines :: [String] -> IO a
failWithMessageLines messageLines = do
    -- Add a blank line before and after to set it off from Bazel's other
    -- output:
    hPutStrLn stderr ""
    mapM_ (hPutStrLn stderr) messageLines
    hPutStrLn stderr ""
    exitFailure
