module Control.Comonad.Trans where

import Control.Comonad

class ComonadTrans f where
  lower :: forall w a. (Comonad w) => f w a -> w a
