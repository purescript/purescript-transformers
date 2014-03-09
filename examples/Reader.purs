module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Data.String

testReader :: Reader String String
testReader = local (\x -> x ++ "!") ask

main = do
  Debug.Trace.print $ runReader testReader "Done"
