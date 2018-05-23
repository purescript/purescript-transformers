module Example.Cont where

import Prelude

import Control.Monad.Cont.Trans (ContT, runContT, callCC)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

main0 :: forall s m. MonadEffect m => ContT s m String
main0 = callCC \k -> do
  void $ liftEffect $ log "Before the continuation"
  void $ k "You should see this."
  k "You should not see this."

main :: Effect Unit
main = runContT main0 log
