module Control.Monad.Writer where

import Prelude
import Control.Monad.Identity
import Control.Monad.Writer.Trans
import Data.Monoid

type Writer w a = WriterT w Identity a

runWriter :: forall w a. Writer w a -> { value :: a, output :: w }
runWriter = runIdentity <<< runWriterT
