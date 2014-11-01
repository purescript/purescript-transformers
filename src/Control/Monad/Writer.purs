module Control.Monad.Writer where

import Control.Monad.Writer.Trans
import Data.Identity
import Data.Monoid
import Data.Tuple

type Writer w = WriterT w Identity

runWriter :: forall w a. Writer w a -> Tuple a w
runWriter = runIdentity <<< runWriterT

execWriter :: forall w a. Writer w a -> w
execWriter m = snd (runWriter m)

mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
mapWriter f = mapWriterT (Identity <<< f <<< runIdentity)
