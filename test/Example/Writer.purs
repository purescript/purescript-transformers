module Example.Writer where

import Prelude

import Data.Tuple

import Control.Monad.Eff.Console
import Control.Monad.Writer

testWriter :: Writer String Int
testWriter = do
  tell "Hello from testWriter"
  return 42

main = case runWriter testWriter of
  Tuple value output -> do
    print $ output
    print $ value
