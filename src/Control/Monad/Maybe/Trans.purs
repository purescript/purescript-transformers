module Control.Monad.Maybe.Trans where

import Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.Trans

data MaybeT m a = MaybeT (m (Maybe a))

instance monadMaybeT :: (Monad m) => Monad (MaybeT m) where
  --return = lift <<< return -- TODO: doesn't type check?
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
