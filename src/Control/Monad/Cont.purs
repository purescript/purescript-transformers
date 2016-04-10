-- | This module defines the `Cont`inuation monad.

module Control.Monad.Cont
  ( Cont
  , cont
  , runCont
  , module X
  ) where

import Prelude

import Control.Monad.Cont.Trans (ContT(ContT), mapContT, runContT, withContT)
import Control.Monad.Cont.Class (callCC) as X
import Data.Identity (Identity(Identity), runIdentity)

-- | The `Cont` monad is a synonym for the `ContT` monad transformer applied to
-- | the `Identity` monad.
type Cont r a = ContT r Identity a

-- | Creates a computation in the `Cont` monad.
cont :: forall a r. ((a -> r) -> r) -> Cont r a
cont f = ContT (\c -> Identity (f (runIdentity <<< c)))

-- | Runs a computation in the `Cont` monad.
runCont :: forall r a. ContT r Identity a -> (a -> r) -> r
runCont cc k = runIdentity (runContT cc (Identity <<< k))

-- | Transform the result of a continuation-passing function.
mapCont :: forall r a. (r -> r) -> Cont r a -> Cont r a
mapCont f = mapContT (Identity <<< f <<< runIdentity)

-- | Transform the continuation passed into the continuation-passing function.
withCont :: forall a b r. ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f = withContT (compose Identity <<< f <<< compose runIdentity)
