-- | This module defines the _exception monad transformer_ `ExceptT`.

module Control.Monad.Except.Trans
  ( ExceptT(..), runExceptT, withExceptT, mapExceptT
  , module Control.Monad.Trans
  , module Control.Monad.Error.Class
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, throwError, catchError)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.RWS.Class (class MonadRWS, class MonadReader, class MonadState, class MonadWriter, ask, censor, get, gets, listen, listens, local, modify, pass, put, reader, state, tell, writer)
import Control.Monad.Trans (class MonadTrans, lift)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)

import Data.Either (Either(..), either)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

-- | A monad transformer which adds exceptions to other monads, in the same way
-- | as `Except`. As before, `e` is the type of exceptions, and `a` is the type
-- | of successful results. The new type parameter `m` is the inner monad that
-- | computations run in.
newtype ExceptT e m a = ExceptT (m (Either e a))

-- | The inverse of `ExceptT`. Run a computation in the `ExceptT` monad.
runExceptT :: forall e m a. ExceptT e m a -> m (Either e a)
runExceptT (ExceptT x) = x

-- | Transform any exceptions thrown by an `ExceptT` computation using the given function.
withExceptT :: forall e e' m a. (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = ExceptT <<< (<$>) (mapLeft f) <<< runExceptT
  where
  mapLeft _ (Right x) = Right x
  mapLeft f (Left x) = Left (f x)

-- | Transform the unwrapped computation using the given function.
mapExceptT :: forall e e' m n a b. (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f m = ExceptT (f (runExceptT m))

instance functorExceptT :: (Functor m) => Functor (ExceptT e m) where
  map f = mapExceptT ((<$>) ((<$>) f))

instance applyExceptT :: (Apply m) => Apply (ExceptT e m) where
  apply (ExceptT f) (ExceptT x) =
    let f' = (<*>) <$> f
        x' = f' <*> x
    in ExceptT x'

instance applicativeExceptT :: (Applicative m) => Applicative (ExceptT e m) where
  pure = ExceptT <<< pure <<< Right

instance bindExceptT :: (Monad m) => Bind (ExceptT e m) where
  bind m k = ExceptT (runExceptT m >>=
                          either (pure <<< Left) (runExceptT <<< k))

instance monadExceptT :: (Monad m) => Monad (ExceptT e m)

instance monadRecExceptT :: (MonadRec m) => MonadRec (ExceptT e m) where
  tailRecM f = ExceptT <<< tailRecM \a -> do
    m <- runExceptT (f a)
    pure case m of
      Left e -> Right (Left e)
      Right (Left a1) -> Left a1
      Right (Right b) -> Right (Right b)

instance altExceptT :: (Semigroup e, Monad m) => Alt (ExceptT e m) where
  alt m n = ExceptT $ do
    rm <- runExceptT m
    case rm of
      Right x -> pure (Right x)
      Left err -> do
        rn <- runExceptT n
        case rn of
          Right x -> pure (Right x)
          Left err' -> pure (Left (err <> err'))

instance plusExceptT :: (Monoid e, Monad m) => Plus (ExceptT e m) where
  empty = throwError (mempty :: e)

instance alternativeExceptT :: (Monoid e, Monad m) => Alternative (ExceptT e m)

instance monadPlusExceptT :: (Monoid e, Monad m) => MonadPlus (ExceptT e m)

instance monadZeroExceptT :: (Monoid e, Monad m) => MonadZero (ExceptT e m)

instance monadTransExceptT :: MonadTrans (ExceptT e) where
  lift m = ExceptT $ do
    a <- m
    pure $ Right a

instance monadEffExceptT :: (MonadEff eff m) => MonadEff eff (ExceptT e m) where
  liftEff = lift <<< liftEff

instance monadContExceptT :: (MonadCont m) => MonadCont (ExceptT e m) where
  callCC f = ExceptT $ callCC $ \c -> runExceptT (f (\a -> ExceptT $ c (Right a)))

instance monadErrorExceptT :: (Monad m) => MonadError e (ExceptT e m) where
  throwError = ExceptT <<< pure <<< Left
  catchError m handler = ExceptT (runExceptT m >>= either (runExceptT <<< handler) (pure <<< Right))

instance monadReaderExceptT :: (MonadReader r m) => MonadReader r (ExceptT e m) where
  ask = lift ask
  local f = mapExceptT (local f)

instance monadStateExceptT :: (MonadState s m) => MonadState s (ExceptT e m) where
  state f = lift (state f)

instance monadWriterExceptT :: (MonadWriter w m) => MonadWriter w (ExceptT e m) where
  writer wd = lift (writer wd)
  listen = mapExceptT $ \m -> do
    Tuple a w <- listen m
    pure $ (\r -> Tuple r w) <$> a
  pass = mapExceptT $ \m -> pass $ do
    a <- m
    pure $ case a of
      Left e -> Tuple (Left e) id
      Right (Tuple r f) -> Tuple (Right r) f

instance monadRWSExceptT :: (Monoid w, MonadRWS r w s m) => MonadRWS r w s (ExceptT e m)
