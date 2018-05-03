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

module Google.Utils.ReadPUtils where

import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP

-- | Given a parser, produces another parser that behaves the same, but that
--   only succeeds if it consumes the entire input.
parseFully :: ReadP a -> ReadP a
parseFully p = p <* eof

-- | Returns just the first match of a parser on some input, or Nothing if
--   there were no matches at all.
parseUniquely :: ReadP a -> String -> Maybe a
parseUniquely p = listToMaybe . map fst . readP_to_S p
