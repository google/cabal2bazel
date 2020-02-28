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
{-# LANGUAGE ForeignFunctionInterface #-}
module ExtraObjsLib (triple) where

import Foreign.C (CInt(..))

triple :: Integer -> Integer
triple = fromIntegral . c_triple . fromIntegral

foreign import ccall "triple" c_triple :: CInt -> CInt

-- Test that we can use TemplateHaskell in a module with FFI.
-- Note that GHC has a staging restriction that prevents the splice
-- from using functions defined or imported directly in this module.
$(return [])
