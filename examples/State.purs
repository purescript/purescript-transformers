module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Identity
import Control.Monad.State

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
  let { state = state, value = value } = runState testState 0
  Debug.Trace.print $ "state: " ++ (show state)
  Debug.Trace.print $ "value: " ++ (show value)