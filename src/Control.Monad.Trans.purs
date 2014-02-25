module Control.Monad.Trans where

import Control.Monad.Eff

class MonadTrans t where
  lift :: forall m a. (Prelude.Monad m) => m a -> t m a
  
class MonadEff m where
  liftEff :: forall eff a. Eff eff a -> m a
