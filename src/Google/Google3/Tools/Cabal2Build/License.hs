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

module Google.Google3.Tools.Cabal2Build.License (showCabalLicense) where

import Data.List (intercalate)
import Distribution.Version (Version(..))
import qualified Distribution.License as Cabal

-- | Representation of Cabal license specifications in generated Google
--   files (specifically BUILD). Where possible we generate names from
--   https://spdx.org/licenses/.
showCabalLicense :: Cabal.License -> String
showCabalLicense (Cabal.AGPL v) = "AGPL" ++ maybe "" showVersion v
showCabalLicense (Cabal.Apache v) = "Apache" ++ maybe "" showVersion v
showCabalLicense Cabal.BSD2 = "BSD-2-Clause"
showCabalLicense Cabal.BSD3 = "BSD-3-Clause"
showCabalLicense Cabal.BSD4 = "BSD-4-Clause"
showCabalLicense (Cabal.GPL v) = "GPL" ++ maybe "" showVersion v
showCabalLicense Cabal.ISC = "ISC"
showCabalLicense (Cabal.LGPL v) = "LGPL" ++ maybe "" showVersion v
showCabalLicense Cabal.MIT = "MIT"
showCabalLicense (Cabal.MPL v) = "MPL" ++ showVersion v
showCabalLicense (Cabal.UnknownLicense s) = "Unknown/" ++ s
showCabalLicense l = show l

-- | Translate a Distribution.Version to an SPDX compatible string.
showVersion :: Version -> String
showVersion (Version [] _) = ""
showVersion (Version [x] _) = "-" ++ show x ++ ".0"
showVersion (Version xs _) = "-" ++ intercalate "." (map show xs)
