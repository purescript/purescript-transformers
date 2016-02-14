-- | This module defines the `MonadRWS` type class and its instances.

module Control.Monad.RWS.Class
  ( class MonadRWS
  , module Control.Monad.Trans
  , module Control.Monad.Reader.Class
  , module Control.Monad.Writer.Class
  , module Control.Monad.State.Class
  ) where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class BoundedOrd, class Category, class DivisionRing, class Eq, class Functor, class ModuloSemiring, class Monad, class Num, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, Unit, Ordering(EQ, GT, LT), add, ap, append, apply, asTypeOf, bind, bottom, compare, compose, conj, const, disj, div, eq, flip, id, liftA1, liftM1, map, mod, mul, negate, not, one, otherwise, pure, return, show, sub, top, unit, unsafeCompare, void, zero, (#), ($), (&&), (*), (+), (++), (-), (/), (/=), (<), (<#>), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>=), (>>=), (>>>), (||))

import Control.Monad.Reader.Class (class MonadReader, ask, local, reader)
import Control.Monad.State.Class (class MonadState, get, gets, modify, put, state)
import Control.Monad.Trans (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, censor, listen, listens, pass, tell, writer)

import Data.Monoid (class Monoid)

-- | `MonadRWS r w s` combines the operations and laws of the `MonadReader r`,
-- | `MonadWriter w` and `MonadState s` type classes.
-- |
-- | An implementation is provided for `RWST`, and for other monad transformers
-- | defined in this library.
class (Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) <= MonadRWS r w s m
