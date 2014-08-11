module Control.Monad.Trans where

class MonadTrans t where
  lift :: forall m a. (Monad m) => m a -> t m a
