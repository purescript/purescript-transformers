module Control.Comonad.Env.Class where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Env.Trans

import Data.Tuple

class (Comonad w) <= ComonadEnv e w where
  ask :: forall a. w a -> e
  local :: forall a. (e -> e) -> w a -> w a

instance comonadEnvTuple :: ComonadEnv e (Tuple e) where
  ask = fst
  local f (Tuple x y) = Tuple (f x) y

instance comonadEnvEnvT :: (Comonad w) => ComonadEnv e (EnvT e w) where
  ask x = fst $ runEnvT x
  local f x = EnvT $ case runEnvT x of
    Tuple x y -> Tuple (f x) y

asks :: forall e1 e2 w a. (ComonadEnv e1 w) => (e1 -> e2) -> w e1 -> e2
asks f x = f $ ask x
