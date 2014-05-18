module Main where

import Debug.Trace

import Data.Tuple

import Control.Monad.Trans

import Control.Monad.Thunk

import Control.Monad.Bounce
import Control.Monad.Bounce.Class

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans

fib :: Number -> Tuple Number Number
fib n = evalBounceTThunk (runStateT (go n) 0)
  where
  go :: Number -> StateT Number (BounceT Thunk) Number
  go 1 = pure 1
  go 2 = pure 1
  go n = do
    bounce
    count <- get
    put (count + 1)
    a <- go (n - 1)
    bounce
    b <- go (n - 2)
    return $ a + b

main = print (fib 20)
