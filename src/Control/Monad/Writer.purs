module Control.Monad.Writer where

import Prelude
import Control.Monad.Identity
import Control.Monad.Writer.Trans
import Data.Monoid
import Data.Tuple

type Writer w a = WriterT w Identity a

runWriter :: forall w a. Writer w a -> Tuple a w
runWriter = runIdentity <<< runWriterT

mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
mapWriter f = mapWriterT (Identity <<< f <<< runIdentity)
