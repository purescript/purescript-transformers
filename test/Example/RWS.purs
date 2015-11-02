module Example.RWS where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (tailRecM)
import Control.Monad.RWS.Trans (RWST, runRWST)
import Control.Monad.State (State, execState, put, get)
import Control.Monad.Writer (tell)

import Data.Either (Either(..))
import Data.Identity (Identity, runIdentity)

loop :: Int -> RWST String (Array String) Int Identity Unit
loop n = tailRecM go n
  where
  go 0 = do
    tell [ "Done!" ]
    pure (Right unit)
  go n = do
    x <- get
    put (x + 1)
    pure (Left (n - 1))

loopState :: Int -> State Int Unit
loopState n = tailRecM go n
  where
  go 0 = do
    pure (Right unit)
  go n = do
    x <- get
    put (x + 1)
    pure (Left (n - 1))

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  t1 <- t
  res1 <- pure $ runIdentity (runRWST (loop 1000000) "" 0)
  t2 <- t
  log $ "RWST: " <> show (t2 - t1)
  t3 <- t
  res2 <- pure $ execState (loopState 1000000) 0
  t4 <- t
  log $ "StateT: " <> show (t4 - t3)

foreign import t :: forall eff. Eff eff Number
