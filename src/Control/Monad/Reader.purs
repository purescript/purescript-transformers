module Control.Monad.Reader where

import Control.Monad.Identity
import Control.Monad.Reader.Trans

type Reader r a = ReaderT r Identity a