module Example.Reader where

import Console
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans

testReader :: Reader String String
testReader = local (\x -> x ++ "!") ask

main = do
  print $ runReader testReader "Done"
