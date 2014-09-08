module Control.Comonad.Store.Trans where

import Control.Extend
import Control.Comonad
import Control.Comonad.Trans

import Data.Tuple

newtype StoreT s w a = StoreT (Tuple (w (s -> a)) s)

runStoreT :: forall s w a. StoreT s w a -> Tuple (w (s -> a)) s
runStoreT (StoreT s) = s

instance functorStoreT :: (Functor w) => Functor (StoreT s w) where
  (<$>) f (StoreT (Tuple w s)) = StoreT $ Tuple ((\h -> h >>> f) <$> w) s

instance extendStoreT :: (Extend w) => Extend (StoreT s w) where
  (<<=) f (StoreT (Tuple w s)) = StoreT $ Tuple ((\w' s' -> f $ StoreT $ Tuple w' s') <<= w) s

instance comonadStoreT :: (Comonad w) => Comonad (StoreT s w) where
  extract (StoreT (Tuple w s)) = extract w s

instance comonadTransStoreT :: ComonadTrans (StoreT s) where
  lower (StoreT (Tuple w s)) = (\f -> f s) <$> w
