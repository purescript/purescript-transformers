-- | This module defines the `Writer` monad.

module Control.Monad.Writer
  ( Writer()
  , runWriter
  , execWriter
  , mapWriter
  , module Control.Monad.Writer.Class
  ) where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class BoundedOrd, class Category, class DivisionRing, class Eq, class Functor, class ModuloSemiring, class Monad, class Num, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, Unit, Ordering(EQ, GT, LT), add, ap, append, apply, asTypeOf, bind, bottom, compare, compose, conj, const, disj, div, eq, flip, id, liftA1, liftM1, map, mod, mul, negate, not, one, otherwise, pure, return, show, sub, top, unit, unsafeCompare, void, zero, (#), ($), (&&), (*), (+), (++), (-), (/), (/=), (<), (<#>), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>=), (>>=), (>>>), (||))

import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans (class MonadTrans, class MonadWriter, WriterT(WriterT), censor, execWriterT, lift, listen, listens, mapWriterT, pass, runWriterT, tell, writer)

import Data.Identity (Identity(..), runIdentity)
import Data.Tuple (Tuple(), snd)

-- | The `Writer` monad is a synonym for the `WriterT` monad transformer, applied
-- | to the `Identity` monad.
type Writer w = WriterT w Identity

-- | Run a computation in the `Writer` monad
runWriter :: forall w a. Writer w a -> Tuple a w
runWriter = runIdentity <<< runWriterT

-- | Run a computation in the `Writer` monad, discarding the result
execWriter :: forall w a. Writer w a -> w
execWriter m = snd (runWriter m)

-- | Change the result and accumulator types in a `Writer` monad action
mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
mapWriter f = mapWriterT (Identity <<< f <<< runIdentity)
