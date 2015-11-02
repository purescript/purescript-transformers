module Example.StateEff where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Trans (StateT, runStateT, modify, put, gets)
import Control.Monad.Trans (lift)

import Data.Array ((:), uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Partial.Unsafe (unsafePartial)

type Stack r t = StateT (Array Int) (Eff r) t

pop :: forall r. Stack (console :: CONSOLE | r) Int
pop = unsafePartial do
  Just { head: x, tail: xs } <- gets uncons
  lift $ log $ "Popping " <> show x
  put xs
  pure x

push :: forall r. Int -> Stack (console :: CONSOLE | r) Unit
push x = do
  lift $ log $ "Pushing " <> show x
  modify $ (:) x
  pure unit

testState :: forall r. Stack (console :: CONSOLE | r) Int
testState = do
  push 1
  push 2
  push 3
  pop
  pop

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  result <- runStateT testState []
  case result of
    Tuple value state -> do
      log $ "state: " <> (show state)
      log $ "value: " <> (show value)
