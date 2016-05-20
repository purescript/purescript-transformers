module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Example.Cont as Cont
import Example.Reader as Reader
import Example.State as State
import Example.StateEff as StateEff
import Example.Writer as Writer
import Example.RWS as RWS

main :: Eff (console :: CONSOLE) Unit
main = do
  Cont.main
  Reader.main
  State.main
  StateEff.main
  Writer.main
  RWS.main
