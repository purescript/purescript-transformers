module Control.Monad.Identity.Trans where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, catchError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadWriter, class MonadTell, pass, listen, tell)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Effect.Class (class MonadEffect, liftEffect)

-- | The `IdentityT` monad transformer.
-- |
-- | This monad acts like a placeholder for functions that take a monad
-- | transformer as an argument, similar to `identity` for functions and
-- | `Identity` for monads.
newtype IdentityT m a = IdentityT (m a)

-- | Run a computation in the `IdentityT` monad.
runIdentityT :: forall m a. IdentityT m a -> m a
runIdentityT (IdentityT ma) = ma

-- | Change the result type of a `IdentityT` monad action.
mapIdentityT :: forall m1 m2 a b. (m1 a -> m2 b) -> IdentityT m1 a -> IdentityT m2 b
mapIdentityT f (IdentityT m) = IdentityT (f m)

derive instance eqIdentityT :: (Eq1 m, Eq a) => Eq (IdentityT m a)
derive instance ordIdentityT :: (Ord1 m, Ord a) => Ord (IdentityT m a)
derive instance eq1IdentityT :: (Eq1 m) => Eq1 (IdentityT m)
derive instance ord1IdentityT :: (Ord1 m) => Ord1 (IdentityT m)
derive instance newtypeIdentityT :: Newtype (IdentityT m a) _

instance functorIdentityT :: Functor m => Functor (IdentityT m) where
  map f (IdentityT m) = IdentityT (map f m)

instance applyIdentityT :: Apply m => Apply (IdentityT m) where
  apply (IdentityT mf) (IdentityT ma) = IdentityT (apply mf ma)

instance applicativeIdentityT :: Applicative m => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)

instance altIdentityT :: (Functor m, Alt m) => Alt (IdentityT m) where
  alt (IdentityT x) (IdentityT y) = IdentityT (x <|> y)

instance plusIdentityT :: (Monad m, Plus m) => Plus (IdentityT m) where
  empty = IdentityT empty

instance alternativeIdentityT :: (Monad m, Alternative m) => Alternative (IdentityT m)

instance bindIdentityT :: Bind m => Bind (IdentityT m) where
  bind (IdentityT m) f = IdentityT (m >>= (runIdentityT <<< f))

instance monadIdentityT :: Monad m => Monad (IdentityT m)

instance monadRecIdentityT :: MonadRec m => MonadRec (IdentityT m) where
  tailRecM f a = IdentityT $
    runIdentityT (f a) >>= case _ of
      Loop a' -> runIdentityT (tailRecM f a')
      Done b -> pure b

instance monadZeroIdentityT :: MonadZero m => MonadZero (IdentityT m)

instance monadPlusIdentityT :: MonadPlus m => MonadPlus (IdentityT m)

instance monadTransIdentityT :: MonadTrans IdentityT where
  lift = IdentityT

instance monadEffectIdentityT :: MonadEffect m => MonadEffect (IdentityT m) where
  liftEffect = lift <<< liftEffect

instance monadContIdentityT :: MonadCont m => MonadCont (IdentityT m) where
  callCC f = IdentityT $ callCC \c -> runIdentityT (f (IdentityT <<< c))

instance monadThrowIdentityT :: MonadThrow e m => MonadThrow e (IdentityT m) where
  throwError e = lift (throwError e)

instance monadErrorIdentityT :: MonadError e m => MonadError e (IdentityT m) where
  catchError (IdentityT m) h = IdentityT $ catchError m (runIdentityT <<< h)

instance monadAskIdentityT :: MonadAsk r m => MonadAsk r (IdentityT m) where
  ask = lift ask

instance monadReaderIdentityT :: MonadReader r m => MonadReader r (IdentityT m) where
  local f = mapIdentityT (local f)

instance monadStateIdentityT :: MonadState s m => MonadState s (IdentityT m) where
  state f = lift (state f)

instance monadTellIdentityT :: MonadTell w m => MonadTell w (IdentityT m) where
  tell = lift <<< tell

instance monadWriterIdentityT :: MonadWriter w m => MonadWriter w (IdentityT m) where
  listen = mapIdentityT listen
  pass = mapIdentityT pass

instance foldableIdentityT :: Foldable m => Foldable (IdentityT m) where
  foldl f i (IdentityT m) = foldl f i m
  foldr i f (IdentityT m) = foldr i f m
  foldMap f (IdentityT m) = foldMap f m

instance traversableIdentityT :: Traversable m => Traversable (IdentityT m) where
  traverse f (IdentityT m) = map IdentityT (traverse f m)
  sequence (IdentityT m) = map IdentityT (sequence m)
