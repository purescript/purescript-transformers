module Control.Monad.State.Trans where

import Prelude
import Control.Monad.Trans

type StateData s a = { state :: s, value :: a }

data StateT s m a = StateT (s -> m (StateData s a))

instance monadStateT :: (Monad m) => Monad (StateT s m) where
  return a = StateT \s -> return { state: s, value: a }
  (>>=) (StateT x) f = StateT \s -> do
    { state = s', value = v } <- x s
    runStateT (f v) s'
    
instance monadTransStateT :: MonadTrans (StateT s) where
  lift m = StateT \s -> do
    x <- m
    return { state: s, value: x }

runStateT :: forall s m a. StateT s m a -> s -> m (StateData s a)
runStateT (StateT s) = s

withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
withStateT f s = StateT $ runStateT s <<< f
