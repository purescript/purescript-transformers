module Control.Monad.Identity where

import Prelude

data Identity a = Identity a

runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x

instance monadIdentity :: Prelude.Monad Identity where
  return = Identity
  (>>=) m f = f $ runIdentity m
  
instance applicativeIdentity :: Prelude.Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x
    
instance functorIdentity :: Prelude.Functor Identity where
  (<$>) f m = Identity $ f $ runIdentity m
