module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans
import Data.Monoid
import Data.Tuple

testWriter :: Writer String Number
testWriter = do
  tell "Hello from testWriter"
  return 42

main = do
  let Tuple value output = runWriter testWriter
  Debug.Trace.print $ output
  Debug.Trace.print $ value
