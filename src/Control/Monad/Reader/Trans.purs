module Control.Monad.Reader.Trans where

import Control.Monad

data ReaderT r m a = ReaderT (r -> m a)

runReader :: forall r m a. ReaderT r m a -> (r -> m a)
runReader (ReaderT x) = x