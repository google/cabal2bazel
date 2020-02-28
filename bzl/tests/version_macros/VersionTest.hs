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
module Main (main) where

import Control.Monad (guard)
import Dependency ()

-- Check that we get macros for default packages:
versionBase :: Bool
#if MIN_VERSION_base(1,2,3)
versionBase = True
#else
versionBase = False
#endif

-- Check that we get macros for dependencies with the
-- package_name/package_version attribute:
dependencyVersionMet, dependencyVersionTooLow :: Bool
#if MIN_VERSION_dependency(1,2,3)
dependencyVersionMet = True
#else
dependencyVersionMet = False
#endif

#if !MIN_VERSION_dependency(1,2,4)
dependencyVersionTooLow = True
#else
dependencyVersionTooLow = False
#endif

main = do
    guard versionBase
    guard dependencyVersionMet
    guard dependencyVersionTooLow
    guard (VERSION_dependency == "1.2.3")
