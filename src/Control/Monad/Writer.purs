-- | This module defines the `Writer` monad.

module Control.Monad.Writer
  ( Writer()
  , runWriter
  , execWriter
  , mapWriter
  , module Control.Monad.Writer.Class
  ) where

import Prelude

import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans (WriterT(), runWriterT, mapWriterT)

import Data.Identity (Identity(..), runIdentity)
import Data.Tuple (Tuple(), snd)

-- | The `Writer` monad is a synonym for the `WriterT` monad transformer, applied
-- | to the `Identity` monad.
type Writer w = WriterT w Identity

-- | Run a computation in the `Writer` monad
runWriter :: forall w a. Writer w a -> Tuple a w
runWriter = runIdentity <<< runWriterT

-- | Run a computation in the `Writer` monad, discarding the result
execWriter :: forall w a. Writer w a -> w
execWriter m = snd (runWriter m)

-- | Change the result and accumulator types in a `Writer` monad action
mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
mapWriter f = mapWriterT (Identity <<< f <<< runIdentity)
