-- | This module defines the `State` monad.

module Control.Monad.State
  ( State()
  , runState
  , evalState
  , execState
  , mapState
  , withState
  , module Control.Monad.State.Class
  ) where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class BoundedOrd, class Category, class DivisionRing, class Eq, class Functor, class ModuloSemiring, class Monad, class Num, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, Unit, Ordering(EQ, GT, LT), add, ap, append, apply, asTypeOf, bind, bottom, compare, compose, conj, const, disj, div, eq, flip, id, liftA1, liftM1, map, mod, mul, negate, not, one, otherwise, pure, return, show, sub, top, unit, unsafeCompare, void, zero, (#), ($), (&&), (*), (+), (++), (-), (/), (/=), (<), (<#>), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>=), (>>=), (>>>), (||))

import Control.Monad.State.Class
import Control.Monad.State.Trans (class MonadState, class MonadTrans, StateT(StateT), evalStateT, execStateT, get, gets, lift, mapStateT, modify, put, runStateT, state, withStateT)

import Data.Identity (Identity(..), runIdentity)
import Data.Tuple (Tuple(), fst, snd)

-- | The `State` monad is a synonym for the `StateT` monad transformer, applied
-- | to the `Identity` monad.
type State s = StateT s Identity

-- | Run a computation in the `State` monad
runState :: forall s a. State s a -> s -> Tuple a s
runState s = runIdentity <<< runStateT s

-- | Run a computation in the `State` monad, discarding the final state
evalState :: forall s a. State s a -> s -> a
evalState m s = fst (runState m s)

-- | Run a computation in the `State` monad, discarding the result
execState :: forall s a. State s a -> s -> s
execState m s = snd (runState m s)

-- | Change the type of the result in a `State` action
mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
mapState f = mapStateT (Identity <<< f <<< runIdentity)

-- | Modify the state in a `State` action
withState :: forall s a. (s -> s) -> State s a -> State s a
withState = withStateT
