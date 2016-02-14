-- | This module defines the writer monad transformer, `WriterT`.

module Control.Monad.Writer.Trans
  ( WriterT(..), runWriterT, execWriterT, mapWriterT
  , module Control.Monad.Trans
  , module Control.Monad.Writer.Class
  ) where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class BoundedOrd, class Category, class DivisionRing, class Eq, class Functor, class ModuloSemiring, class Monad, class Num, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, Unit, Ordering(EQ, GT, LT), add, ap, append, apply, asTypeOf, bind, bottom, compare, compose, conj, const, disj, div, eq, flip, id, liftA1, liftM1, map, mod, mul, negate, not, one, otherwise, pure, return, show, sub, top, unit, unsafeCompare, void, zero, (#), ($), (&&), (*), (+), (++), (-), (/), (/=), (<), (<#>), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>=), (>>=), (>>>), (||))

import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..), snd)

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, catchError, catchJust, throwError)
import Control.Monad.Reader.Class (class MonadReader, ask, local, reader)
import Control.Monad.Rec.Class (class MonadRec, forever, tailRec, tailRecM, tailRecM2, tailRecM3)
import Control.Monad.State.Class (class MonadState, get, gets, modify, put, state)
import Control.Monad.Trans (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, censor, listen, listens, pass, tell, writer)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus, empty)

-- | The writer monad transformer.
-- |
-- | This monad transformer extends the base monad with a monoidal accumulator of
-- | type `w`.
-- |
-- | The `MonadWriter` type class describes the operations supported by this monad.
newtype WriterT w m a = WriterT (m (Tuple a w))

-- | Run a computation in the `WriterT` monad.
runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT x) = x

-- | Run a computation in the `WriterT` monad, discarding the result.
execWriterT :: forall w m a. (Apply m) => WriterT w m a -> m w
execWriterT m = snd <$> runWriterT m

-- | Change the accumulator and base monad types in a `WriterT` monad action.
mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b
mapWriterT f m = WriterT $ f (runWriterT m)

instance functorWriterT :: (Functor m) => Functor (WriterT w m) where
  map f = mapWriterT $ (<$>) \(Tuple a w) -> Tuple (f a) w

instance applyWriterT :: (Semigroup w, Apply m) => Apply (WriterT w m) where
  apply f v = WriterT $
    let k (Tuple a w) (Tuple b w') = Tuple (a b) (w <> w')
    in k <$> (runWriterT f) <*> (runWriterT v)

instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT $ pure $ Tuple a mempty

instance altWriterT :: (Alt m) => Alt (WriterT w m) where
  alt m n = WriterT $ runWriterT m <|> runWriterT n

instance plusWriterT :: (Plus m) => Plus (WriterT w m) where
  empty = WriterT empty

instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)

instance bindWriterT :: (Semigroup w, Monad m) => Bind (WriterT w m) where
  bind m k  = WriterT $ do
    Tuple a w <- runWriterT m
    Tuple b w' <- runWriterT (k a)
    return $ Tuple b (w <> w')

instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)

instance monadRecWriterT :: (Monoid w, MonadRec m) => MonadRec (WriterT w m) where
  tailRecM f a = WriterT $ tailRecM f' (Tuple a mempty)
    where
    f' (Tuple a w) = do
      Tuple m w1 <- runWriterT (f a)
      return case m of
        Left a -> Left (Tuple a (w <> w1))
        Right b -> Right (Tuple b (w <> w1))

instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)

instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w) where
  lift m = WriterT $ do
    a <- m
    return $ Tuple a mempty

instance monadEffWriter :: (Monad m, Monoid w, MonadEff eff m) => MonadEff eff (WriterT w m) where
  liftEff = lift <<< liftEff

instance monadContWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
  callCC f = WriterT $ callCC $ \c -> runWriterT (f (\a -> WriterT $ c (Tuple a mempty)))

instance monadErrorWriterT :: (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
  throwError e = lift (throwError e)
  catchError m h = WriterT $ catchError (runWriterT m) (\e -> runWriterT (h e))

instance monadReaderWriterT :: (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask = lift ask
  local f = mapWriterT (local f)

instance monadStateWriterT :: (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  state f = lift (state f)

instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer = WriterT <<< return
  listen m = WriterT $ do
    Tuple a w <- runWriterT m
    return $ Tuple (Tuple a w) w
  pass m = WriterT $ do
    Tuple (Tuple a f) w <- runWriterT m
    return $ Tuple a (f w)
