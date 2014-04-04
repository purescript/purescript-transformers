module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Data.Tuple

incState :: forall eff a. State Number {}
incState = modify $ (+) 1

testState :: forall eff a. State Number String
testState = do
  incState
  incState
  incState
  incState
  incState
  incState
  return "Done"

main = do
  case runState testState 0 of
    Tuple value state -> do
      Debug.Trace.print $ "state: " ++ (show state)
      Debug.Trace.print $ "value: " ++ (show value)
