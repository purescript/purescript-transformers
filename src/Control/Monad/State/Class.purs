module Control.Monad.State.Class where

import Prelude
import Control.Monad.State.Trans

class MonadState s m where
  state :: forall a. (s -> StateData s a) -> m a
  
get :: forall m s. (Monad m, MonadState s m) => m s
get = state \s -> { state: s, value: s }

gets :: forall s m a. (Monad m, MonadState s m) => (s -> a) -> m a
gets f = state \s -> { state: s, value: f s }

put :: forall m s. (Monad m, MonadState s m) => s -> m {}
put s = state \_ -> { state: s, value: {} }

modify :: forall s m. (Monad m, MonadState s m) => (s -> s) -> m {}
modify f = state \s -> { state: (f s), value: {} }

instance monadStateStateT :: (Monad m) => MonadState s (StateT s m) where
  state f = StateT $ return <<< f
