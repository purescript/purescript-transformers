module Control.Monad.Cod.Trans where

import Prelude

import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Cont.Class
import Control.Monad.Rec.Class (MonadRec, tailRecM)

-- | The Codensity monad transformer.
-- |
-- | This monad transformer extends the base monad with CPS that supports tailRecM.
newtype CodT m a = CodT (forall r. (a -> m r) -> m r)

-- | Run a computation in the `CodT` monad, by providing a continuation.
runCodT :: forall r m a. CodT m a -> (a -> m r) -> m r
runCodT (CodT f) k = f k

instance functorCodT :: (Monad m) => Functor (CodT m) where
  map f m = CodT (\k -> runCodT m (\a -> k $ f a))

instance applyCodT :: (Monad m) => Apply (CodT m) where
  apply f v = CodT (\k -> runCodT f (\g -> runCodT v (\a -> k $ g a)))

instance applicativeCodT :: (Monad m) => Applicative (CodT m) where
  pure a = CodT (\k -> k a)

instance bindCodT :: (Monad m) => Bind (CodT m) where
  bind m k = CodT (\k' -> runCodT m (\a -> runCodT (k a) k'))

instance monadCodT :: (Monad m) => Monad (CodT m)

instance monadRecCodT :: (MonadRec m) => MonadRec (CodT m) where
  tailRecM k a = CodT (\k' -> tailRecM (\x -> runCodT (k x) return) a >>= k')
