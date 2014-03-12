module Control.Monad.Maybe.Trans where

import Prelude
import Control.Monad
import Control.Monad.Trans
import Data.Either
import Data.Maybe
import Data.Tuple

data MaybeT m a = MaybeT (m (Maybe a))

instance monadMaybeT :: (Monad m) => Monad (MaybeT m) where
  return x = MaybeT $ return $ Just x
  (>>=) x f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)

instance monadTransMaybeT :: MonadTrans MaybeT where
  lift = MaybeT <<< (<$>) Just

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
