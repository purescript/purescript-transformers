module Control.Monad.Bounce.Class where

import Data.Tuple

import Control.Monad.State.Trans

class (Monad m) <= MonadBounce m where
  bounce :: m {}

instance monadBounceStateT :: (MonadBounce m) => MonadBounce (StateT s m) where
  bounce = StateT $ \s -> do
    bounce
    return $ Tuple {} s
