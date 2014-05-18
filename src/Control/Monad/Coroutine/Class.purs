module Control.Monad.Coroutine.Class where

import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Monoid

import Control.Monad.State.Trans
import Control.Monad.Writer.Trans
import Control.Monad.Error.Trans
import Control.Monad.Error
import Control.Monad.Maybe.Trans
import Control.Monad.Reader.Trans

class (Monad m) <= MonadYield m where
  yield :: m {}

instance monadYieldStateT :: (MonadYield m) => MonadYield (StateT s m) where
  yield = StateT $ \s -> do
    yield
    return $ Tuple {} s

instance monadYieldReaderT :: (MonadYield m) => MonadYield (ReaderT w m) where
  yield = ReaderT $ \r -> do
    yield
    return {}

instance monadYieldWriterT :: (Monoid w, MonadYield m) => MonadYield (WriterT w m) where
  yield = WriterT $ do
    yield
    return $ Tuple {} mempty

instance monadYieldErrorT :: (Error e, MonadYield m) => MonadYield (ErrorT e m) where
  yield = ErrorT $ do
    yield
    return $ Right {}

instance monadYieldMaybeT :: (MonadYield m) => MonadYield (MaybeT m) where
  yield = MaybeT $ do
    yield
    return $ Just {}

withTrampoline :: forall m a. (MonadYield m) => m a -> m a
withTrampoline action = do
  yield 
  action
