module Example.StateEff where

import Prelude

import Control.Monad.State.Trans (StateT, runStateT, modify, put, gets)
import Control.Monad.Trans.Class (lift)
import Data.Array ((:), uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

type Stack t = StateT (Array Int) Effect t

pop :: Stack Int
pop = unsafePartial do
  Just { head: x, tail: xs } <- gets uncons
  lift $ log $ "Popping " <> show x
  put xs
  pure x

push :: Int -> Stack Unit
push x = do
  lift $ log $ "Pushing " <> show x
  _ <- modify (x : _)
  pure unit

testState :: Stack Int
testState = do
  void $ push 1
  void $ push 2
  void $ push 3
  void $ pop
  pop

main :: Effect Unit
main = do
  result <- runStateT testState []
  case result of
    Tuple value state -> do
      log $ "state: " <> (show state)
      log $ "value: " <> (show value)
