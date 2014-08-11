module Control.Monad.Maybe.Trans where

import Control.Monad
import Control.Monad.Trans
import Data.Either
import Data.Maybe
import Data.Tuple

newtype MaybeT m a = MaybeT (m (Maybe a))

instance functorMaybeT :: (Monad m) => Functor (MaybeT m) where
  (<$>) = liftA1

instance applyMaybeT :: (Monad m) => Apply (MaybeT m) where
  (<*>) = ap

instance applicativeMaybeT :: (Monad m) => Applicative (MaybeT m) where
  pure = MaybeT <<< pure <<< Just

instance bindMaybeT :: (Monad m) => Bind (MaybeT m) where
  (>>=) x f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)

instance monadMaybeT :: (Monad m) => Monad (MaybeT m)

instance monadTransMaybeT :: MonadTrans MaybeT where
  lift = MaybeT <<< liftM1 Just

runMaybeT :: forall m a. MaybeT m a -> m (Maybe a)
runMaybeT (MaybeT x) = x

mapMaybeT :: forall m1 m2 a b. (m1 (Maybe a) -> m2 (Maybe b)) -> MaybeT m1 a -> MaybeT m2 b
mapMaybeT f = MaybeT <<< f <<< runMaybeT

liftCatchMaybe :: forall m e a. (m (Maybe a) -> (e -> m (Maybe a)) -> m (Maybe a)) -> MaybeT m a -> (e -> MaybeT m a) -> MaybeT m a
liftCatchMaybe catch m h = MaybeT $ catch (runMaybeT m) (runMaybeT <<< h)

liftListenMaybe :: forall m a w. (Monad m) => (m (Maybe a) -> m (Tuple (Maybe a) w)) -> MaybeT m a -> MaybeT m (Tuple a w)
liftListenMaybe listen = mapMaybeT $ \m -> do
  Tuple a w <- listen m
  return $ (\r -> Tuple r w) <$> a

liftPassMaybe :: forall m a w. (Monad m) => (m (Tuple (Maybe a) (w -> w)) -> m (Maybe a)) -> MaybeT m (Tuple a (w -> w)) -> MaybeT m a
liftPassMaybe pass = mapMaybeT $ \m -> pass $ do
  a <- m
  return $ case a of
    Nothing -> Tuple Nothing id
    Just (Tuple v f) -> Tuple (Just v) f

liftCallCCMaybe :: forall m a b. (((Maybe a -> m (Maybe b)) -> m (Maybe a)) -> m (Maybe a)) -> ((a -> MaybeT m b) -> MaybeT m a) -> MaybeT m a
liftCallCCMaybe callCC f = MaybeT $ callCC $ \c -> runMaybeT (f (\a -> MaybeT $ c $ Just a))
