module Example.Cont where

import Prelude

import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Cont.Trans
import Control.Monad.Trans

main0 :: forall m eff. (Monad m, MonadEff (console :: CONSOLE | eff) m) => ContT _ m _
main0 = callCC \k -> do
  liftEff $ log "Before the continuation"
  k "You should see this."
  k "You should not see this."

main = runContT main0 log
