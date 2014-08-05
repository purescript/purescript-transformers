module Control.Monad.Identity where

import Prelude

newtype Identity a = Identity a

runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x

instance functorIdentity :: Functor Identity where
  (<$>) f m = Identity $ f $ runIdentity m

instance applyIdentity :: Apply Identity where
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance applicativeIdentity :: Applicative Identity where
  pure = Identity

instance bindIdentity :: Bind Identity where
  (>>=) m f = f $ runIdentity m

instance monadIdentity :: Monad Identity
