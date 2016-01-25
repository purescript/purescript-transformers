module Control.Monad.Codensity.Trans
  ( CodensityT(..), runCodensityT
  , module Control.Monad.Trans
  ) where

import Prelude

import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Rec.Class (MonadRec, tailRecM)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, state)

-- | The Codensity monad transformer.
-- |
-- | This monad transformer extends the base monad with CPS that supports tailRecM.
newtype CodensityT m a = CodensityT (forall r. (a -> m r) -> m r)

-- | Run a computation in the `CodensityT` monad, by providing a continuation.
runCodensityT :: forall r m a. CodensityT m a -> (a -> m r) -> m r
runCodensityT (CodensityT f) k = f k

instance functorCodensityT :: (Monad m) => Functor (CodensityT m) where
  map f m = CodensityT (\k -> runCodensityT m (\a -> k $ f a))

instance applyCodensityT :: (Monad m) => Apply (CodensityT m) where
  apply f v = CodensityT (\k -> runCodensityT f (\g -> runCodensityT v (\a -> k $ g a)))

instance applicativeCodensityT :: (Monad m) => Applicative (CodensityT m) where
  pure a = CodensityT (\k -> k a)

instance bindCodensityT :: (Monad m) => Bind (CodensityT m) where
  bind m k = CodensityT (\k' -> runCodensityT m (\a -> runCodensityT (k a) k'))

instance monadCodensityT :: (Monad m) => Monad (CodensityT m)

instance monadTransCodensityT :: MonadTrans CodensityT where
  lift m = CodensityT (\k -> m >>= k)

instance monadRecCodensityT :: (MonadRec m) => MonadRec (CodensityT m) where
  tailRecM k a = CodensityT (\k' -> tailRecM (\x -> runCodensityT (k x) return) a >>= k')

instance monadEffCodensityT :: (MonadEff eff m) => MonadEff eff (CodensityT m) where
  liftEff = lift <<< liftEff

instance monadReaderCodensityT :: (MonadReader r m) => MonadReader r (CodensityT m) where
  ask = lift ask
  local f c = CodensityT (\k -> do
    r <- ask
    local f (runCodensityT c (local (const (r :: r)) <<< k))
  )

instance monadStateCodensityT :: (MonadState s m) => MonadState s (CodensityT m) where
  state = lift <<< state
