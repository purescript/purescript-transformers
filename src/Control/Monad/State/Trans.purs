module Control.Monad.State.Trans where

import Prelude
import Control.Monad.Trans
import Data.Tuple

data StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT s) = s

evalStateT :: forall s m a. (Monad m) => StateT s m a -> s -> m a
evalStateT m s = runStateT m s >>= \(Tuple x _) -> return x

execStateT :: forall s m a. (Monad m) => StateT s m a -> s -> m s
execStateT m s = runStateT m s >>= \(Tuple _ s) -> return s

mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
mapStateT f m = StateT $ f <<< runStateT m

withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
withStateT f s = StateT $ runStateT s <<< f

instance monadStateT :: (Monad m) => Monad (StateT s m) where
  return a = StateT \s -> return $ Tuple a s
  (>>=) (StateT x) f = StateT \s -> do
    Tuple v s' <- x s
    runStateT (f v) s'

instance altStateT :: (Alternative m) => Alternative (StateT s m) where
  empty = StateT $ \_ -> empty
  (<|>) x y = StateT $ \s -> runStateT x s <|> runStateT y s

instance monadTransStateT :: MonadTrans (StateT s) where
  lift m = StateT \s -> do
    x <- m
    return $ Tuple x s

liftCatchState :: forall s m e a. (m (Tuple a s) -> (e -> m (Tuple a s)) -> m (Tuple a s)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a
liftCatchState catch m h = StateT $ \s -> catch (runStateT m s) (\e -> runStateT (h e) s)

liftListenState :: forall s m a w. (Monad m) => (m (Tuple a s) -> m (Tuple (Tuple a s) w)) -> StateT s m a -> StateT s m (Tuple a w)
liftListenState listen m = StateT $ \s -> do
    Tuple (Tuple a s') w <- listen (runStateT m s)
    return $ Tuple (Tuple a w) s'

liftPassState :: forall s m a b w. (Monad m) => (m (Tuple (Tuple a s) b) -> m (Tuple a s)) -> StateT s m (Tuple a b) -> StateT s m a
liftPassState pass m = StateT $ \s -> pass $ do
    Tuple (Tuple a f) s' <- runStateT m s
    return $ Tuple (Tuple a s') f
