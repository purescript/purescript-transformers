module Control.Monad.Coroutine.Class where

import Data.Tuple

import Control.Monad.State.Trans

class (Monad m) <= MonadYield m where
  yield :: m {}

instance monadYieldStateT :: (MonadYield m) => MonadYield (StateT s m) where
  yield = StateT $ \s -> do
    yield
    return $ Tuple {} s

withTrampoline :: forall m a. (MonadYield m) => m a -> m a
withTrampoline action = do
  yield 
  action
