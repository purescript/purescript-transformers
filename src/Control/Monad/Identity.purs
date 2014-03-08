module Control.Monad.Identity where

import Prelude

data Identity a = Identity a

runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x

instance monadIdentity :: Monad Identity where
  return = Identity
  (>>=) m f = f $ runIdentity m
  
instance applicativeIdentity :: Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x
    
instance functorIdentity :: Functor Identity where
  (<$>) f m = Identity $ f $ runIdentity m
