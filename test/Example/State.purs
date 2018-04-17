module Example.State where

import Prelude

import Control.Monad.Effect (Effect)
import Control.Monad.Effect.Console (log)
import Control.Monad.State (State, runState, modify)

import Data.Tuple (Tuple(..))

incState :: State Int Unit
incState = modify (_ + 1)

testState :: State Int String
testState = do
  incState
  incState
  incState
  incState
  incState
  incState
  pure "Done"

main :: Effect Unit
main = case runState testState 0 of
  Tuple value state -> do
    log $ "state: " <> (show state)
    log $ "value: " <> (show value)
