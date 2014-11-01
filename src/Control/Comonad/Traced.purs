module Control.Comonad.Traced where

import Control.Comonad.Traced.Trans
import Data.Identity

type Traced m = TracedT m Identity

runTraced :: forall m a. Traced m a -> m -> a
runTraced = runTracedT >>> runIdentity

traced :: forall m a. (m -> a) -> Traced m a
traced = Identity >>> TracedT
