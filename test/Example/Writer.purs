module Example.Writer where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans
import Data.Tuple

testWriter :: Writer String Int
testWriter = do
  tell "Hello from testWriter"
  return 42

main = case runWriter testWriter of
  Tuple value output -> do
    print $ output
    print $ value
