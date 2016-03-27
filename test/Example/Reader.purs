module Example.Reader where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Reader (Reader, runReader, ask, local)

testReader :: Reader String String
testReader = local (\x -> x <> "!") ask

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log $ runReader testReader "Done"
