module Example.Reader where

import Prelude

import Control.Monad.Effect (Effect)
import Control.Monad.Effect.Console (log)
import Control.Monad.Reader (Reader, runReader, ask, local)

testReader :: Reader String String
testReader = local (\x -> x <> "!") ask

main :: Effect Unit
main = do
  log $ runReader testReader "Done"
