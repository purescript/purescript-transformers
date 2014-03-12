module Control.Monad.State.Trans where

import Prelude
import Control.Monad.Trans

type StateData s a = { state :: s, value :: a }

data StateT s m a = StateT (s -> m (StateData s a))

runStateT :: forall s m a. StateT s m a -> s -> m (StateData s a)
runStateT (StateT s) = s

evalStateT :: forall s m a. (Monad m) => StateT s m a -> s -> m a
evalStateT m s = runStateT m s >>= \x -> return x.value

execStateT :: forall s m a. (Monad m) => StateT s m a -> s -> m s
execStateT m s = runStateT m s >>= \x -> return x.state

mapStateT :: forall s m1 m2 a b. (m1 (StateData s a) -> m2 (StateData s b)) -> StateT s m1 a -> StateT s m2 b
mapStateT f m = StateT $ f <<< runStateT m

withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
withStateT f s = StateT $ runStateT s <<< f

instance monadStateT :: (Monad m) => Monad (StateT s m) where
  return a = StateT \s -> return { state: s, value: a }
  (>>=) (StateT x) f = StateT \s -> do
    { state = s', value = v } <- x s
    runStateT (f v) s'

instance monadTransStateT :: MonadTrans (StateT s) where
  lift m = StateT \s -> do
    x <- m
    return { state: s, value: x }

liftCatchState :: forall s m e a. (m (StateData s a) -> (e -> m (StateData s a)) -> m (StateData s a)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a
liftCatchState catch m h = StateT $ \s -> catch (runStateT m s) (\e -> runStateT (h e) s)
