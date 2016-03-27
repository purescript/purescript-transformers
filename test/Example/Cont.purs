module Example.Cont where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Cont.Trans (ContT, runContT, callCC)

main0 :: forall s m eff. (MonadEff (console :: CONSOLE | eff) m) => ContT s m String
main0 = callCC \k -> do
  liftEff $ log "Before the continuation"
  k "You should see this."
  k "You should not see this."

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = runContT main0 log
