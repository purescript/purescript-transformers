-- | This module defines the `Reader` monad.

module Control.Monad.Reader
  ( Reader()
  , runReader
  , mapReader
  , withReader
  , module Control.Monad.Reader.Class
  ) where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class BoundedOrd, class Category, class DivisionRing, class Eq, class Functor, class ModuloSemiring, class Monad, class Num, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, Unit, Ordering(EQ, GT, LT), add, ap, append, apply, asTypeOf, bind, bottom, compare, compose, conj, const, disj, div, eq, flip, id, liftA1, liftM1, map, mod, mul, negate, not, one, otherwise, pure, return, show, sub, top, unit, unsafeCompare, void, zero, (#), ($), (&&), (*), (+), (++), (-), (/), (/=), (<), (<#>), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>=), (>>=), (>>>), (||))

import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans (class MonadReader, class MonadTrans, ReaderT(ReaderT), ask, lift, local, mapReaderT, reader, runReaderT, withReaderT)

import Data.Identity (Identity(..), runIdentity)

-- | The `Reader` monad is a synonym for the `ReaderT` monad transformer, applied
-- | to the `Identity` monad.
type Reader r = ReaderT r Identity

-- | Run a computation in the `Reader` monad.
runReader :: forall r a. Reader r a -> r -> a
runReader m = runIdentity <<< runReaderT m

-- | Change the type of the context in a `Reader` monad action.
withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader = withReaderT

-- | Change the type of the result in a `Reader` monad action.
mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT $ Identity <<< f <<< runIdentity
