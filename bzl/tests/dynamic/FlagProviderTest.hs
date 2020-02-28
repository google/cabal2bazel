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

{-# LANGUAGE OverloadedStrings #-}
-- | Tests that we run the constructors for alwayslink dependencies.
-- Nothing in this module explicitly links against the C++ library
-- that defines the "flag_provider_test_flag"; we're relying on the constructor
-- in that library to define the flag automatically.
module Main (main) where

import Control.Monad (guard, void)
import Google.Base.Flags (getAll, Info(..))

main :: IO ()
main = do
    allFlags <- getAll
    guard (any isExpectedTestFlag allFlags)

isExpectedTestFlag :: Info -> Bool
isExpectedTestFlag i = fiName i == "flag_provider_test_flag"
