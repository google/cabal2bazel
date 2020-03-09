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

{-# LANGUAGE BangPatterns #-}

module Google.Utils.List (ordNub, ordNubOn, hashNub) where

import Data.Hashable (Hashable (..))
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set

-- | Removes duplicates, preserving order.
ordNub :: (Ord a) => [a] -> [a]
ordNub = ordNubOn id

-- | Removes duplicates, preserving order.  Two elements are considered
-- duplicates if they map to the same 'b'.
ordNubOn :: (Ord b) => (a -> b) -> [a] -> [a]
ordNubOn f =
    let go _ [] = []
        go s (x:xs) = let y = f x
                      in if y `Set.member` s
                           then go s xs
                           else x : go (Set.insert y s) xs
    in go Set.empty

hashNub :: (Eq a, Hashable a) => [a] -> [a]
hashNub = nub_impl HashSet.insert HashSet.member HashSet.empty
{-# INLINE hashNub #-}

nub_impl :: (a -> set a -> set a) -> (a -> set a -> Bool) -> set a -> [a] -> [a]
nub_impl insert member = go
  where go _ [] = []
        go !xs (y:ys)
          | member y xs =     go xs ys
          | otherwise   = y : go (insert y xs) ys
{-# INLINE nub_impl #-}
