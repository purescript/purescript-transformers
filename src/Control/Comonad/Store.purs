module Control.Comonad.Store where

import Control.Comonad.Store.Trans
import Data.Identity
import Data.Tuple

type Store s a = StoreT s Identity a

runStore :: forall s a. Store s a -> Tuple (s -> a) s
runStore s = swap (runIdentity <$> (swap $ runStoreT s))

store :: forall s a. (s -> a) -> s -> Store s a
store f x = StoreT $ Tuple (Identity f) x
