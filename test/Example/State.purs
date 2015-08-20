module Example.State where

import Prelude

import Data.Tuple

import Control.Monad.Eff.Console
import Control.Monad.State

incState :: forall eff a. State Int Unit
incState = modify $ (+) 1

testState :: forall eff a. State Int String
testState = do
  incState
  incState
  incState
  incState
  incState
  incState
  return "Done"

main = case runState testState 0 of
  Tuple value state -> do
    log $ "state: " ++ (show state)
    log $ "value: " ++ (show value)
