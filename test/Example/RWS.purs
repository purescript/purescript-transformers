module Example.RWS where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.RWS
import Control.Monad.RWS.Trans
import Control.Monad.Rec.Class
import Control.Monad.State
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

main = do
  t1 <- t
  res <- pure $ runIdentity (runRWST (loop 10000) "" 0)
  t2 <- t
  print $ "RWST.state: " ++ show res.state
  print $ "RWST.log: " ++ show res.log
  print $ "t2 - t1 = " ++ show (t2 - t1)

foreign import t :: forall eff. Eff eff Number
