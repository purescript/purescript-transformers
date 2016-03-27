module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

main :: Eff (console :: CONSOLE) Unit
main = do
  Example.Cont.main
  Example.Reader.main
  Example.State.main
  Example.StateEff.main
  Example.Writer.main
  Example.RWS.main
