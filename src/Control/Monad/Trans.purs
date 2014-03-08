module Control.Monad.Trans where

import Prelude
import Control.Monad.Eff

class MonadTrans t where
  lift :: forall m a. (Monad m) => m a -> t m a
  
class MonadEff m where
  liftEff :: forall eff a. Eff eff a -> m a
