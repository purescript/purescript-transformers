module Test.Main where

import Prelude
import Control.Monad.Eff

main = do
  Example.Cont.main
  Example.Reader.main
  Example.State.main
  Example.StateEff.main
  Example.Writer.main
