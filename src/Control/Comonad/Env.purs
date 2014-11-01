module Control.Comonad.Env where

import Control.Comonad.Env.Trans
import Data.Identity
import Data.Tuple

type Env e = EnvT e Identity

runEnv :: forall e a. Env e a -> Tuple e a
runEnv x = runIdentity <$> runEnvT x

withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a
withEnv = withEnvT

mapEnv :: forall e a b. (a -> b) -> Env e a -> Env e b
mapEnv = (<$>)

env :: forall e a. e -> a -> Env e a
env e a = EnvT $ Tuple e $ Identity a
