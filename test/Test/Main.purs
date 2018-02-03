module Test.Main where

import Prelude

import Control.Monad.Effect (Effect)

import Example.Cont as Cont
import Example.Reader as Reader
import Example.State as State
import Example.StateEffect as StateEffect
import Example.Writer as Writer
import Example.RWS as RWS
import Example.List as List

main :: Effect Unit
main = do
  Cont.main
  Reader.main
  State.main
  StateEffect.main
  Writer.main
  RWS.main
  List.main
