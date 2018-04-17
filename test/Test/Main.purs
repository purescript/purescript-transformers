module Test.Main where

import Prelude

import Effect (Effect)
import Example.Cont as Cont
import Example.List as List
import Example.Reader as Reader
import Example.RWS as RWS
import Example.State as State
import Example.StateEff as StateEff
import Example.Writer as Writer

main :: Effect Unit
main = do
  Cont.main
  Reader.main
  State.main
  StateEff.main
  Writer.main
  RWS.main
  List.main
