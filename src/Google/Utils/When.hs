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

-- | Utilities for using 'Bool's, 'Maybe's, and 'Either's to control 'Monad's.
--
-- Alternatively, a module giving names to a few common ways to combine
-- 'maybe', 'either', 'return', 'pure', 'id', and 'mzero'.

module Google.Utils.When
    (
    -- * Monad
    -- ** Bool
    -- | This is simply re-exported from Control.Monad.
      when
    -- ** Maybe
    , whenJust
    , whenNothing
    -- ** Either
    , whenLeft
    , whenRight
    , whenLeft_
    , whenRight_
    -- * MonadPlus
    -- ** Maybe
    , filterJust
    -- ** Either
    , filterLeft
    , filterRight
    ) where

import Control.Monad (MonadPlus(mzero), when, void)

-- | Run an action using the contents of a @Maybe a@ only if it is 'Just'.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x m = maybe (return ()) m x

-- | Use the given action to provide a fallback value when given 'Nothing'.
-- The fallback value is last to allow do-notation to be used comfortably.  For
-- example:
--
-- > whenNothing maybeUser $ do
-- >   uid <- askUserName
-- >   fetchUserFromDatabase uid
whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing x ma = maybe ma pure x

-- | Run an action only when the given @Either a b@ is @'Left' _@.
whenLeft :: Applicative f => Either a b -> (a -> f b) -> f b
whenLeft x mb = either mb pure x

-- | Run an action only when the given @Either a b@ is @'Right' _@.
whenRight :: Applicative f => Either a b -> (b -> f a) -> f a
whenRight x ma = either pure ma x

-- | Equivalent of 'whenLeft' where the continuation returns @()@.  Note that
-- 'whenLeft' can't be used directly for this case because it unifies the
-- 'Right' type and the continuation result type.
whenLeft_ :: Applicative f => Either a b -> (a -> f ()) -> f ()
whenLeft_ = whenLeft . void

-- | Equivalent of 'whenRight' where the continuation returns @()@.
whenRight_ :: Applicative f => Either a b -> (b -> f ()) -> f ()
whenRight_ = whenRight . either (const $ Left ()) Right

-- | Use the value in 'Just' if present, or 'mzero' otherwise.  Particularly
-- useful in 'STM' where 'TVars' containing 'Maybe's are commonly used.
--
-- For example, an equivalent definition of
-- 'Control.Concurrent.STM.TMVar.readMVar' is:
--
-- > readTMVar (TMVar tvar) = filterJust (readTVar tvar)
filterJust :: MonadPlus m => m (Maybe a) -> m a
filterJust = (>>= maybe mzero return)

-- | Use the value in 'Left' if present, or 'mzero' otherwise.
filterLeft :: MonadPlus m => m (Either a b) -> m a
filterLeft = (>>= either return (const mzero))

-- | Use the value in 'Right' if present, or 'mzero' otherwise.
filterRight :: MonadPlus m => m (Either a b) -> m b
filterRight = (>>= either (const mzero) return)
