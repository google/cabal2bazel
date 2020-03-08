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

{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad (guard)
import GHC.Conc (numCapabilities)
import Language.Haskell.TH (litE, Lit(..))

main :: IO ()
main = do
    -- Record the number of capabilities at compile-time.
    -- Note: don't *check* the value at compile-time, since that would unnecessary
    -- break analysis tools like Grok.
    let compileTimeCapabilities = $(litE $ IntegerL $ toEnum numCapabilities)
    putStrLn $ "Number of capabilities: " ++ show compileTimeCapabilities
    guard (compileTimeCapabilities == 2)
