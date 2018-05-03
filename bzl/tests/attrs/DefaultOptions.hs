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

-- Test that we use the ghc_config rule's "default_options" attr
-- when compiling Haskell code.
-- This test in particular checks that we've enabled ScopedTypeVariables,
-- which is currently a default option.
-- If we remove ScopedTypeVariables as a default option, this test may start
-- failing, in which case we should check for a different flag.
module Main where

main = myId $ return ()

-- NOTE: this tests that we use the config default_options field.
-- Specifically, it checks that we enable ScopedTypeVariables.  Without that
-- option, we'd get an error like "Illegal symbol '.' in type".
-- If that behavior ever changes, this test will need to be updated.
myId :: forall a . a -> a
myId = id

