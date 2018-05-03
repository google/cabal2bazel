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

{-|
Module: Google.utils.Env
Description:  Convinience functions to work with environment variables.
-}

module Google.Utils.Env
    ( readMay
    , maybeFormatEnv
    , lookupReadMaybe
    ) where

import Control.Monad      ( guard )
import Data.Maybe         ( listToMaybe )
import System.Environment ( lookupEnv )

-- | A version of 'read' that returns 'Nothing' on failure instead of
-- 'error' @"..."@.
readMay :: Read a => String -> Maybe a
readMay s = listToMaybe $ do
    (parse, remain) <- reads s
    guard (null remain)
    return parse

-- | Try to read an environment variable into @a@.
lookupReadMaybe :: Read a => String -> IO (Maybe a)
lookupReadMaybe n = (readMay =<<) <$> lookupEnv n

-- | This looks up an environment variable and applies the formatting function
-- if the variable exists.
maybeFormatEnv :: String           -- ^ The environment variable.
               -> (String -> IO a) -- ^ How to format the variable into @a@.
               -> IO (Maybe a)
maybeFormatEnv envVar format =
    lookupEnv envVar >>= maybe (return Nothing) (fmap Just . format)
