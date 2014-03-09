module Control.Monad.Reader.Class where

import Prelude
import Control.Monad.Reader.Trans

class MonadReader r m where
  ask :: m r
  local :: forall a. (r -> r) -> m a -> m a

reader :: forall r m a. (Monad m, MonadReader r m) => (r -> a) -> m a
reader f = ask >>= return <<< f

instance monadReaderArr :: MonadReader r ((->) r) where
  ask = id
  local = (>>>)

instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m) where
  ask = ReaderT return
  local = withReaderT
