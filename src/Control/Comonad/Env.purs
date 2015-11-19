-- | This module defines the `Env` comonad.

module Control.Comonad.Env where

import Prelude

import Control.Comonad.Env.Trans (EnvT(..), runEnvT, withEnvT)

import Data.Identity (Identity(..), runIdentity)
import Data.Tuple (Tuple(..))

-- | The `Env` comonad is a synonym for the `EnvT` comonad transformer, applied
-- | to the `Identity` monad.
type Env e = EnvT e Identity

-- | Unwrap a value in the `Env` comonad.
runEnv :: forall e a. Env e a -> Tuple e a
runEnv x = runIdentity <$> runEnvT x

-- | Change the environment type in an `Env` computation.
withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a
withEnv = withEnvT

-- | Change the data type in an `Env` computation.
mapEnv :: forall e a b. (a -> b) -> Env e a -> Env e b
mapEnv = map

-- | Create a value in context in the `Env` comonad.
env :: forall e a. e -> a -> Env e a
env e a = EnvT $ Tuple e $ Identity a
