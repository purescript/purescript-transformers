module Control.Monad.Coroutine.Class where

import Data.Tuple
import Data.Monoid

import Control.Monad.State.Trans
import Control.Monad.Writer.Trans

class (Monad m) <= MonadYield m where
  yield :: m {}

instance monadYieldStateT :: (MonadYield m) => MonadYield (StateT s m) where
  yield = StateT $ \s -> do
    yield
    return $ Tuple {} s

instance monadYieldWriterT :: (Monoid w, MonadYield m) => MonadYield (WriterT w m) where
  yield = WriterT $ do
    yield
    return $ Tuple {} mempty

withTrampoline :: forall m a. (MonadYield m) => m a -> m a
withTrampoline action = do
  yield 
  action
