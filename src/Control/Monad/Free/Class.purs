module Control.Monad.Free.Class where

import Prelude

class (Functor f, Monad m) <= MonadFree f m where
  wrap :: forall a. f (m a) -> m a

liftF :: forall f m a. (Functor f, MonadFree f m) => f a -> m a
liftF = wrap <<< map pure
