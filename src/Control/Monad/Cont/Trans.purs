-- | This module defines the CPS monad transformer.

module Control.Monad.Cont.Trans
  ( ContT(..), runContT, mapContT, withContT
  , module Control.Monad.Trans
  , module Control.Monad.Cont.Class
  ) where

import Prelude

import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, state)

-- | The CPS monad transformer.
-- |
-- | This monad transformer extends the base monad with the operation `callCC`.
newtype ContT r m a = ContT ((a -> m r) -> m r)

-- | Run a computation in the `ContT` monad, by providing a continuation.
runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
runContT (ContT f) k = f k

-- | Modify the underlying action in a `ContT` monad action.
mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT (\k -> f $ runContT m k)

-- | Modify the continuation in a `ContT` monad action
withContT :: forall r m a b. ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT (\k -> (runContT m) (f k))

instance monadContContT :: (Monad m) => MonadCont (ContT r m) where
  callCC f = ContT (\k -> runContT (f (\a -> ContT (\_ -> k a))) k)

instance functorContT :: (Monad m) => Functor (ContT r m) where
  map f m = ContT (\k -> runContT m (\a -> k $ f a))

instance applyContT :: (Monad m) => Apply (ContT r m) where
  apply f v = ContT (\k -> runContT f $ (\g -> runContT v (\a -> (k $ g a))))

instance applicativeContT :: (Monad m) => Applicative (ContT r m) where
  pure a = ContT (\k -> k a)

instance bindContT :: (Monad m) => Bind (ContT r m) where
  bind m k = ContT (\k' -> runContT m (\a -> runContT (k a) k'))

instance monadContT :: (Monad m) => Monad (ContT r m)

instance monadTransContT :: MonadTrans (ContT r) where
  lift m = ContT (\k -> m >>= k)

instance monadEffContT :: (MonadEff eff m) => MonadEff eff (ContT r m) where
  liftEff = lift <<< liftEff

instance monadReaderContT :: (MonadReader r1 m) => MonadReader r1 (ContT r m) where
  ask = lift ask
  local f c = ContT \k -> do
    r <- ask
    local f (runContT c (local (const (r :: r1)) <<< k))

instance monadStateContT :: (MonadState s m) => MonadState s (ContT r m) where
  state = lift <<< state
