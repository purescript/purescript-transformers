module Control.Comonad.Env.Trans where

import Control.Comonad
import Control.Comonad.Trans
import Control.Extend
import Data.Tuple

newtype EnvT e w a = EnvT (Tuple e (w a))

runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)
runEnvT (EnvT x) = x

withEnvT :: forall e1 e2 w a. (e1 -> e2) -> EnvT e1 w a -> EnvT e2 w a
withEnvT f (EnvT (Tuple e x)) = EnvT $ Tuple (f e) x

mapEnvT :: forall e w1 w2 a b. (w1 a -> w2 b) -> EnvT e w1 a -> EnvT e w2 b
mapEnvT f (EnvT (Tuple e x)) = EnvT $ Tuple e (f x)

instance functorEnvT :: (Functor w) => Functor (EnvT e w) where
  (<$>) f (EnvT (Tuple e x)) = EnvT $ Tuple e (f <$> x)

instance extendEnvT :: (Extend w) => Extend (EnvT e w) where
  (<<=) f (EnvT (Tuple e x)) = EnvT $ Tuple e (f <$> ((Tuple e >>> EnvT) <<= x))

instance comonadEnvT :: (Comonad w) => Comonad (EnvT e w) where
  extract (EnvT (Tuple e x)) = extract x

instance comonadTransEnvT :: ComonadTrans (EnvT e) where
  lower (EnvT (Tuple e x)) = x
