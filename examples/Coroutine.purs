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

import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans

fib :: Number -> Tuple (Tuple Number String) Number
fib n = evalCoroutineTThunk (runStateT (runWriterT (go n)) 0)
  where
  go :: Number -> WriterT String (StateT Number (CoroutineT Thunk)) Number
  go 1 = pure 1
  go 2 = pure 1
  go n = withTrampoline do
    tell $ "fib " ++ show n ++ ", "
    count <- get
    put (count + 1)
    a <- go (n - 1)
    b <- go (n - 2)
    return $ a + b

main = print (fib 2000)
