-- | This module defines the `RWS` monad.

module Control.Monad.RWS
  ( RWS()
  , rws
  , runRWS
  , evalRWS
  , execRWS
  , mapRWS
  , withRWS
  , module Control.Monad.RWS.Class
  ) where

import Prelude

import Control.Monad.RWS.Class
import Control.Monad.RWS.Trans (RWST(..), RWSResult(), runRWST, mapRWST, withRWST, execRWST, evalRWST)

import Data.Identity (Identity(..), runIdentity)
import Data.Tuple (Tuple())

-- | The `RWS` monad is a synonym for the `RWST` monad transformer, applied
-- | to the `Identity` monad.
type RWS r w s = RWST r w s Identity

-- | Create an action in the `RWS` monad from a function which uses the
-- | global context and state explicitly.
rws :: forall r w s a. (r -> s -> RWSResult s a w) -> RWS r w s a
rws f = RWST \r s -> return $ f r s

-- | Run a computation in the `RWS` monad.
runRWS :: forall r w s a. RWS r w s a -> r -> s -> RWSResult s a w
runRWS m r s = runIdentity $ runRWST m r s

-- | Run a computation in the `RWS` monad, discarding the final state
evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
evalRWS m r s = runIdentity $ evalRWST m r s

-- | Run a computation in the `RWS` monad, discarding the result
execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
execRWS m r s = runIdentity $ execRWST m r s

-- | Change the types of the result and accumulator in a `RWS` action
mapRWS :: forall r w1 w2 s a1 a2. (RWSResult s a1 w1 -> RWSResult s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
mapRWS f = mapRWST (runIdentity >>> f >>> Identity)

-- | Change the type of the context in a `RWS` action
withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
withRWS = withRWST
