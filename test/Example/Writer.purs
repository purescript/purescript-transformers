module Example.Writer where

import Prelude

import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)

testWriter :: Writer String Int
testWriter = do
  tell "Hello from testWriter"
  pure 42

main :: Effect Unit
main = case runWriter testWriter of
  Tuple value output -> do
    log output
    logShow value
