module Example.Reader where

import Prelude

import Control.Monad.Reader (Reader, runReader, ask, local)
import Effect (Effect)
import Effect.Console (log)

testReader :: Reader String String
testReader = local (\x -> x <> "!") ask

main :: Effect Unit
main = do
  log $ runReader testReader "Done"
