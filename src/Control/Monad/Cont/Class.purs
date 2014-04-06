module Control.Monad.Cont.Class where

import Prelude
import Control.Monad.Error
import qualified Control.Monad.Cont.Trans as Cont

class MonadCont m where
  callCC :: forall a b. ((a -> m b) -> m a) -> m a

instance monadContContT :: (Monad m) => MonadCont (Cont.ContT r m) where
  callCC = Cont.callCC
