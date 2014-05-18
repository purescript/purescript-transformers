module Main where

import Debug.Trace

import Data.Tuple

import Control.Monad.Trans

import Control.Monad.Thunk

import Control.Monad.Coroutine
import Control.Monad.Coroutine.Class

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans

fib :: Number -> Tuple Number Number
fib n = evalCoroutineTThunk (runStateT (go n) 0)
  where
  go :: Number -> StateT Number (CoroutineT Thunk) Number
  go 1 = pure 1
  go 2 = pure 1
  go n = withTrampoline do
    count <- get
    put (count + 1)
    a <- go (n - 1)
    b <- go (n - 2)
    return $ a + b

main = print (fib 2000)
