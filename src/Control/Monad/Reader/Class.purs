module Control.Monad.Reader.Class where

import Control.Monad.Trans
import Control.Monad.Reader.Trans
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.RWS.Trans
import Control.Monad.State.Trans
import Control.Monad.Writer.Trans
import Data.Monoid

import qualified Control.Monad.RWS as RWS

class MonadReader r m where
  ask :: m r
  local :: forall a. (r -> r) -> m a -> m a

reader :: forall r m a. (Monad m, MonadReader r m) => (r -> a) -> m a
reader f = ask >>= return <<< f

instance monadReaderFun :: MonadReader r ((->) r) where
  ask = id
  local = (>>>)

instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m) where
  ask = ReaderT return
  local = withReaderT

instance monadReaderErrorT :: (Monad m, Error e, MonadReader r m) => MonadReader r (ErrorT e m) where
  ask = lift ask
  local f = mapErrorT (local f)

instance monadReaderMaybeT :: (Monad m, MonadReader r m) => MonadReader r (MaybeT m) where
  ask = lift ask
  local f = mapMaybeT (local f)

instance monadReaderWriterT :: (Monad m, Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask = lift ask
  local f = mapWriterT (local f)

instance monadReaderStateT :: (Monad m, MonadReader r m) => MonadReader r (StateT s m) where
  ask = lift ask
  local f = mapStateT (local f)

instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m) where
  ask = RWS.ask
  local = RWS.local
