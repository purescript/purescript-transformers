-- | This module defines the `MonadRWS` type class and its instances.

module Control.Monad.RWS.Class
  ( class MonadRWS
  , module Control.Monad.Trans
  , module Control.Monad.Reader.Class
  , module Control.Monad.Writer.Class
  , module Control.Monad.State.Class
  ) where

import Control.Monad.Reader.Class (class MonadReader, ask, local, reader)
import Control.Monad.State.Class (class MonadState, get, gets, modify, put, state)
import Control.Monad.Trans (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, censor, listen, listens, pass, tell, writer)


-- | `MonadRWS r w s` combines the operations and laws of the `MonadReader r`,
-- | `MonadWriter w` and `MonadState s` type classes.
-- |
-- | An implementation is provided for `RWST`, and for other monad transformers
-- | defined in this library.
class (MonadReader r m, MonadWriter w m, MonadState s m) <= MonadRWS r w s m | m -> r w s
