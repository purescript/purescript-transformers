module Control.Monad.Error.Class where

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Reader.Trans
import Control.Monad.Writer.Trans
import Control.Monad.State.Trans
import Data.Either
import Data.Monoid

class MonadError e m where
  throwError :: forall a. e -> m a
  catchError :: forall a. m a -> (e -> m a) -> m a

instance monadErrorError :: (Error e) => MonadError e (Either e) where
  throwError = Left
  catchError (Left e) h = h e
  catchError (Right x) _ = Right x

instance monadErrorErrorT :: (Monad m, Error e) => MonadError e (ErrorT e m) where
  throwError e = ErrorT $ return (Left e)
  catchError m h = ErrorT $ do
    a <- runErrorT m
    case a of
      Left e -> runErrorT (h e)
      Right x -> return (Right x)

instance monadErrorMaybeT :: (Monad m, MonadError e m) => MonadError e (MaybeT m) where
  throwError e = lift (throwError e)
  catchError = liftCatchMaybe catchError

instance monadErrorReaderT :: (Monad m, MonadError e m) => MonadError e (ReaderT r m) where
  throwError e = lift (throwError e)
  catchError = liftCatchReader catchError

instance monadErrorWriterT :: (Monad m, Monoid w, MonadError e m) => MonadError e (WriterT w m) where
  throwError e = lift (throwError e)
  catchError = liftCatchWriter catchError

instance monadErrorStateT :: (Monad m, MonadError e m) => MonadError e (StateT s m) where
  throwError e = lift (throwError e)
  catchError = liftCatchState catchError
