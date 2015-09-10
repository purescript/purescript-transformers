module Example.RWS where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.RWS
import Control.Monad.RWS.Trans
import Control.Monad.Rec.Class
import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.Writer

import Data.Either
import Data.Identity

loop :: Int -> RWST String (Array String) Int Identity Unit
loop n = tailRecM go n
  where
  go 0 = do
    tell [ "Done!" ]
    return (Right unit)
  go n = do
    x <- get
    put (x + 1)
    return (Left (n - 1))

loopState :: Int -> StateT Int Identity Unit
loopState n = tailRecM go n
  where
  go 0 = do
    return (Right unit)
  go n = do
    x <- get
    put (x + 1)
    return (Left (n - 1))

main = do
  t1 <- t
  res1 <- pure $ runIdentity (runRWST (loop 1000000) "" 0)
  t2 <- t
  print $ "RWST: " ++ show (t2 - t1)
  t3 <- t
  res2 <- pure $ execState (loopState 1000000) 0
  t4 <- t
  print $ "StateT: " ++ show (t4 - t3)

foreign import t :: forall eff. Eff eff Number
