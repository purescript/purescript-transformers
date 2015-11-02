module Example.Writer where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Writer (Writer, tell, runWriter)

import Data.Tuple (Tuple(..))

testWriter :: Writer String Int
testWriter = do
  tell "Hello from testWriter"
  pure 42

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = case runWriter testWriter of
  Tuple value output -> do
    log output
    logShow value
