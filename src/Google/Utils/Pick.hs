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

-- | Utility module with a naive implementation of a random selection
-- from a list.
module Google.Utils.Pick (partialPick, pick) where

import System.Random (randomRIO)

-- | Very naive implementation of selecting a random element from a
--   list. Errors out on empty list.
partialPick :: [a] -> IO a
partialPick [] = error "unsafePick: empty list"
partialPick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- | The same as partialPick, but returns Maybe instead.
pick :: [a] -> IO (Maybe a)
pick [] = return Nothing
pick xs = fmap Just $ partialPick xs
