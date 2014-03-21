module Control.Monad.State.Class where

import Prelude
import Control.Monad.Trans
import Control.Monad.State.Trans
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Reader.Trans
import Control.Monad.Writer.Trans
import Data.Monoid
import Data.Tuple

class MonadState s m where
  state :: forall a. (s -> (Tuple a s)) -> m a

get :: forall m s. (Monad m, MonadState s m) => m s
get = state \s -> Tuple s s

gets :: forall s m a. (Monad m, MonadState s m) => (s -> a) -> m a
gets f = state \s -> Tuple (f s) s

put :: forall m s. (Monad m, MonadState s m) => s -> m {}
put s = state \_ -> Tuple {} s

modify :: forall s m. (Monad m, MonadState s m) => (s -> s) -> m {}
modify f = state \s -> Tuple {} (f s)

instance monadStateStateT :: (Monad m) => MonadState s (StateT s m) where
  state f = StateT $ return <<< f

instance monadStateStateT1 :: (Monad m, MonadState s m) => MonadState s (StateT s1 m) where
  state f = lift (state f)
  
instance monadStateErrorT :: (Monad m, Error e, MonadState s m) => MonadState s (ErrorT e m) where
  state f = lift (state f)
  
instance monadStateMaybeT :: (Monad m, MonadState s m) => MonadState s (MaybeT m) where
  state f = lift (state f)

instance monadStateReaderT :: (Monad m, MonadState s m) => MonadState s (ReaderT r m) where
  state f = lift (state f)

instance monadStateWriterT :: (Monad m, Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  state f = lift (state f)
