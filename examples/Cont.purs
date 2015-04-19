module Example.Cont where

import Console
import Control.Monad.Cont.Trans
import Control.Monad.Trans

main0 = runContT $ do
  lift (log "a")
  lift (log "b")

main = main0 (\_ -> log "c")
