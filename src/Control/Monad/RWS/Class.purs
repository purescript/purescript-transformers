-- | This module defines the `MonadRWS` type class and its instances.

module Control.Monad.RWS.Class
  ( MonadRWS
  , module Control.Monad.Trans
  , module Control.Monad.Reader.Class
  , module Control.Monad.Writer.Class
  , module Control.Monad.State.Class
  ) where

import Prelude

import Data.Monoid

import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

-- | `MonadRWS r w s` combines the operations and laws of the `MonadReader r`,
-- | `MonadWriter w` and `MonadState s` type classes.
-- |
-- | An implementation is provided for `RWST`, and for other monad transformers
-- | defined in this library.
class (Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) <= MonadRWS r w s m
