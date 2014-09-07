module Control.Comonad.Env.Trans where

import Control.Comonad
import Control.Extend
import Data.Tuple

newtype EnvT e w a = EnvT (Tuple e (w a))

runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)
runEnvT (EnvT x) = x

instance functorEnvT :: (Functor w) => Functor (EnvT e w) where
  (<$>) f (EnvT (Tuple e x)) = EnvT $ Tuple e (f <$> x)

instance extendEnvT :: (Extend w) => Extend (EnvT e w) where
  (<<=) f (EnvT (Tuple e x)) = EnvT $ Tuple e (f <$> ((Tuple e >>> EnvT) <<= x))
