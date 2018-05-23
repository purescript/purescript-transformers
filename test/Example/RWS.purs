module Example.RWS where

import Prelude

import Control.Monad.Rec.Class (tailRecM, Step(..))
import Control.Monad.RWS.Trans (RWST, runRWST)
import Control.Monad.State (State, execState, put, get)
import Control.Monad.Writer (tell)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console (log)

loop :: Int -> RWST String (Array String) Int Identity Unit
loop n = tailRecM go n
  where
  go 0 = do
    tell [ "Done!" ]
    pure (Done unit)
  go n' = do
    x <- get
    put (x + 1)
    pure (Loop (n' - 1))

loopState :: Int -> State Int Unit
loopState n = tailRecM go n
  where
  go 0 = do
    pure (Done unit)
  go n' = do
    x <- get
    put (x + 1)
    pure (Loop (n' - 1))

main :: Effect Unit
main = do
  t1 <- t
  res1 <- pure $ unwrap (runRWST (loop 1000000) "" 0)
  t2 <- t
  log $ "RWST: " <> show (t2 - t1)
  t3 <- t
  res2 <- pure $ execState (loopState 1000000) 0
  t4 <- t
  log $ "StateT: " <> show (t4 - t3)

foreign import t :: Effect Number
