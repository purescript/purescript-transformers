module Control.Monad.Maybe.Trans where

import Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.Trans

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
