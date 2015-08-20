module Example.Reader where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Reader

testReader :: Reader String String
testReader = local (\x -> x ++ "!") ask

main = do
  print $ runReader testReader "Done"
